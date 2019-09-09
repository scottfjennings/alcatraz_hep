
## this file contains code and instructions for converting Alcatraz nest monitoring data to ACR's HEP data format

## all this code should work fine if you acceessed it via the R Project named "alcatraz_hep". If you simply opened the R code file named "alcatraz", then you will have problems with file paths. 


## the data files we get from USGS for Alcatraz are in a funky format.
## they come in a xlsx file, with a sheet for each species (but 2018 data had both species in 1 sheet)
## in each species sheet, there are 3 rows at the top before the data actually start, with the column names occupying the 2nd and 3rd rows (yes, column names spanning 2 rows)
## then the actual nest check data are arranged in a wide format, with a row for each nest and then repeating sets of columns for each nest check (e.g. date, nest contents, etc) stacked horizontally out as far as needed to match the number of checks for each nest
## the number of checks varies between species and between individual nests, so the code below reactive to the nest-specific number of checks


# load the required packages
library(tidyverse) 
library(lubridate)
library(xlsx)
library(stringr)
code_version = 1.6

alcatraz_reader_xlsx <- function(year, worksheet, filename){
  # first is a function to read in the csv files for each species
  # if the data file we got from USGS was named following the standard they used in 2015-17 (e.g. Alcatraz2017data_FINAL), then 'filename' can be left out. I made this function flexible on filename so that we can avoid renaming files that we get from USGS, thus hopefully avoinding confusion about file history and version
  # if the data file has a separate sheet for each species, then this reader function needs to be called multiple times to import each species separately, and 'worksheet' should be the name of the worksheet for the appropriate species
  # if all species are combined on the same sheet, then the function only needs to be called once
  # reading directly from the xlsx is slow but works fine, and is a more streamlined process than saving the xlsx's as csv's before importing to R
  if(missing(filename)) {
    filepath <- paste("data_from_USGS/Alcatraz", year, "data_FINAL.xlsx", sep="")
  } else {
    filepath <- paste("data_from_USGS/", filename, ".xlsx", sep="")
  }
  
  
  ztabl <- read.xlsx(filepath,
                     sheetIndex = worksheet,
                     startRow = 3) %>% 
    select(-starts_with("NA")) # this trims off any random empty columns
  
  return(ztabl)
}

bcnh <- alcatraz_reader_xlsx(year = "2017", worksheet = "BCNH")
sneg <- alcatraz_reader_xlsx("2017", "SNEG")

all_checks_usgs <- alcatraz_reader_xlsx(year = "2018", worksheet = "Sheet1", filename = "Alcatraz2018data_forACR")


#--------------------------------------------------------

alcatraz_nest_infoer <- function(sp.df){
  # next a function to extract info on nest characteristics
  # most of this info is stuff that's not normally collected as part of the HEP protocol, but here's a function to pull it out in case its needed sometime
  # INPUT: "sp.df" is the data frame for a particular species, that we just created with "alcatraz_reader"
nest.info <- sp.df %>% 
  select(NO., Obs., SPP, Colony, Type, Hgt..ft., Easting, Northing, plus.minus..ft.)
return(nest.info)
}

#sneg_info <- alcatraz_nest_infoer(sneg)
#bcnh_info <- alcatraz_nest_infoer(bcnh)

#--------------------------------------------------------

alcatraz_nest_checker <- function(sp.df){
  # now a function to extract the nest check data from the funky wide USGS format to a normal long table
  # most of what we peel out here has a direct match to what's collected in the HEP protocol, but the format is slightly different 
  # when the xlsx file is read, duplicate column names are given sequential trailing numbers.
  # INPUT: "sp.df" is the data frame for a particular species, that we just created with "alcatraz_reader"
#-----
  
# determine how many sets of check columns there are
#num.check.col.groups <- (ncol(sp.df) - 7 - 2)/5
num.check.col.groups <- sp.df %>% 
  select(matches("\\d")) %>% # column names that contain a digit
  summarize(num.groups = ncol(.)/5) # there are 5 standard columns for each nest check
# make a list of numbers representing each set of check columns
num.check.repeats <- seq(1, length.out = num.check.col.groups[1, 1], by = 1)

check0 <- sp.df %>% 
  select(NO., SPP, DATE, Egg, Chick, Age, Notes) %>% 
  mutate_at(c("Egg", "Age", "Chick", "Notes"), as.character) # to avoid warnings upon rbind()ing below

# this sub-function selects just those columns with matching trailing numbers, with the trailing number specified with "check.repeat"
checker <- function(check.repeat){
  dot.check.repeat <- paste(".", check.repeat, sep = "")
check <- sp.df %>% 
  select(NO., SPP, ends_with(dot.check.repeat), -contains("X.")) %>% 
  rename_all(~sub(dot.check.repeat, '', .x)) %>% # get rid of the digits in column names
  mutate_at(c("Egg", "Age", "Chick", "Notes"), as.character) # to avoid warnings upon rbind()ing below
return(check)
}

# now run checker() on each repeated set of check columns IDed in num.check.repeats()
checks1 <- map_df(num.check.repeats, checker)
# bind to the first check set of columns and do some cleaning
checks <- rbind(check0, checks1) %>% 
  mutate_all(trimws) %>% 
  mutate(Egg = sub("\\+$", "", Egg),
         Egg = sub(">", "", Egg),
         Egg = sub("<", "", Egg),
         Chick = sub(">", "", Chick),
         Chick = sub("<", "", Chick)) %>%
  mutate_at(c("Egg", "Age", "Chick"), as.numeric) %>% # this isn't explicit, but converting to numeric is a shortcut way to convert dashes to NA
  mutate(Notes = tolower(Notes)) %>% 
  filter(!is.na(DATE)) %>% 
  filter(!is.na(SPP)) %>% 
  unique() %>% 
  arrange(NO., DATE)

return(checks)
}

sneg_checks <- alcatraz_nest_checker(sneg)
bcnh_checks <- alcatraz_nest_checker(bcnh)

all_checks <- alcatraz_nest_checker(all_checks_usgs)


## if working with multiple species dataframes, COMBINE THEM NOW.
# the functions below will work with single species data frames, but combining now makes for fewer function calls below
# rbind() can take >2 objects
all_checks <- rbind(sneg_checks, bcnh_checks)
rm(sneg, sneg_checks, bcnh, bcnh_checks)

##-------------------------------------------------------------
all_dup_check_checker <- function(all_checks) {
  # now checking for unique records for the same nest on the same day (i.e. conflicting data for the same date)
  # can view output to figure out which fields are different
  
  # if there are no records in the output of dup_check_checker(), then the output of alcatraz2HEPer() is ready to be written to Alcatraz_ready4screening/
  # otherwise, dup_check_paster(), dup_stage_fixer() and/or dup_chicks_fixer(), below, will be needed
  
  foo <- all_checks %>% 
    group_by(SPP, NO., DATE) %>% 
    summarise(num.dups = n())%>%
    ungroup() %>% 
    filter(num.dups > 1) %>% 
    mutate(NO. = as.character(NO.)) %>% 
    left_join(all_checks, by = c("SPP", "NO.", "DATE")) %>% 
    arrange(NO., DATE)
}
all_checks_dups <- all_dup_check_checker(all_checks)

all_dup_check_paster <- function(all_checks){
  # if duplicate-date, unique-data checks exist, they most likely have different notes. There may also be conflicting info in Egg, Chick and Age
  # this function currently pastes together the 'Egg', 'Chick', 'Age' and 'Notes' from duplicate nest checks on the same day, but with different values for these fields.
  tester2 <- all_checks %>% 
  mutate(Egg = ifelse(is.na(Egg), "", Egg),
         Chick = ifelse(is.na(Chick), "", Chick),
         Age = ifelse(is.na(Age), "", Age),
         Notes = ifelse(is.na(Notes), "", Notes)) %>% 
  group_by(SPP, NO., DATE) %>% 
  mutate(Egg = (trimws(paste(Egg, collapse = ';'))),
         Chick = (trimws(paste(Chick, collapse = ';'))),
         Age = (trimws(paste(Age, collapse = ';'))),
         Notes = (trimws(paste(Notes, collapse = ' ')))) %>% 
  distinct() %>% 
  mutate(Egg = ifelse(Egg == "" | Egg == ";", NA, Egg),
         Chick = ifelse(Chick == "" | Chick == ";", NA, Chick),
         Age = ifelse(Age == "" | Age == ";", NA, Age),
         Notes = ifelse(Notes == "", NA, Notes)) %>% 
  arrange(NO., SPP, DATE) %>% 
  data.frame()
}


all_dups_pasted <- all_dup_check_paster(all_checks)

##-------------------------------------------------------------
## now 3 separate funtions to deal with the pasted Egg, Chick and Age fields generated by all_dup_check_paster(), retaining the max value of the pasted values (and/or the non-NA value if 1 was NA)
  # dup_stage_fixer(), dup_chicks_fixer() and dup_age_fixer() can be run in any order with outputs of prior functions used as inputs for next functions, but note the order and resulting output names I've used in this workflow
  
  # INPUTS: 
  # alc_dups_pasted = the output from dup_check_scrubber() OR the output from dup_*_fixer()
  
  # max.dups = the maximum number of duplicate records, which can be obtained from the output of all_dup_check_checker()
    # this only needs to be filled if max.dups != 2
  
  # the default behavior of this function is to replace the original 'stage' field with the new one generated by this function
  # however, the user can specify replace.stage = "N" to view both the old (pasted) and newly generated stage fields, to ensure that averything worked correctly

dup_fixer <- function(zall_dups_pasted, dup.field, max.dups = 2){
  
  foo <- zall_dups_pasted %>% 
    select(dup.field)
  
  dupper <- data.frame(str_split_fixed(foo[,1], ";", max.dups)) %>%
    mutate_all(as.character) %>%
    mutate_all(as.numeric) %>% 
    mutate(dup.field.good = do.call(pmax, c(., list(na.rm=TRUE)))) 
  
  fixed <- zall_dups_pasted %>% 
    cbind(., select(dupper, dup.field.good))

  return(fixed)
}
# it is recommended to compare the old (pasted) and newly generated stage fields to make sure this function behaved as expected, and that the expected behavior is appropriate...
all_dups_pasted_eggFixed <- dup_fixer(zall_dups_pasted = all_dups_pasted, dup.field = "Egg", max.dups = 4) %>% 
  rename(Egg.good = dup.field.good)
# once that is confirmed, then the same object can be overwritten, now replacing the old stage with the new
all_dups_pasted_eggFixed <- all_dups_pasted_eggFixed %>% 
  select(NO., SPP, DATE, Egg.good, Chick, Age, Notes, -Egg) %>% 
  rename(Egg = Egg.good)

#--- now repeat for Chick and Age fields
all_dups_pasted_egg_chickFixed <- dup_fixer(zall_dups_pasted = all_dups_pasted_eggFixed, dup.field = "Chick", max.dups = 4) %>% 
  rename(Chick.good = dup.field.good)
all_dups_pasted_egg_chickFixed <- all_dups_pasted_egg_chickFixed %>% 
  select(NO., SPP, DATE, Egg, Chick.good, Age, Notes, -Chick) %>% 
  rename(Chick = Chick.good)
#---
all_dups_pasted_egg_chick_ageFixed <- dup_fixer(zall_dups_pasted = all_dups_pasted_egg_chickFixed, dup.field = "Age", max.dups = 4) %>% 
  rename(Age.good = dup.field.good)
all_dups_Fixed <- all_dups_pasted_egg_chick_ageFixed %>% 
  select(NO., SPP, DATE, Egg, Chick, Age.good, Notes, -Age) %>% 
  rename(Age = Age.good)


rm(all_dups_pasted_eggFixed, all_dups_pasted_egg_chickFixed, all_dups_pasted_egg_chick_ageFixed, all_dups_pasted, all_checks_dups)


##-----------------------------------------------------------------------------------------

zoof <- table(all_checks$Notes) %>% data.frame()
zoof2 <- zoof %>% filter(!grepl("egg", Var1) & 
                           !grepl("chick", Var1) & 
                           !grepl("empty", Var1) & 
                           !grepl("destroy", Var1) & 
                           !grepl("hatch", Var1) & 
                           !grepl("renest", Var1) & 
                           !grepl("re-nest", Var1) & 
                           !grepl("not checked", Var1) & 
                           !grepl("depredated", Var1) & 
                           !grepl("dead", Var1) & 
                           !grepl("dying", Var1) & 
                           !grepl("away", Var1) & 
                           !grepl("left nest", Var1) & 
                           !grepl("gone", Var1) & 
                           !grepl("incub", Var1)) %>% 
  arrange(-Freq)

fooz <- all_checks %>% 
  filter(!is.na(Notes))


notes_extracter <- function(sp_checks){
  # fill the Egg or Chick fields with data extracted from the notes field, as appropriate
  # many records with no info in Egg or Chick are nonetheless valuable because the notes specify that the nest was empty (Egg and Chick = 0); is valuable info for nest screening, allowing a nest's record to end with an affirmative failure rather than just no data
  # this function fills in Egg and Chick for some of the most common such occurences
  # warnings about NAs introduced by coercion are OK
  # this function adds a 'keeper' field, which flags records as having real data that we want to keep, or as being empty records that are relicts of the USGS table structure
  # the user can double check the records with keeper == "N" in the output to make sure the rules for exclusion worked correctly for the current data file
  # alc_noter() below can help ID new records (with different notes) that may need to be included in this function
  # INPUT: sp_checks is the species-specific df created by alcatraz_nest_checker()
sp_checks <- all_dups_Fixed
  sp_checks2 <- sp_checks %>%  
  mutate(notes.failed = ifelse(
                          #(is.na(Egg) & is.na(Chick)) & 
                         ((str_detect(Notes, "empty") & !str_detect(Notes, "missed")) | 
                            str_detect(Notes, "destroyed") | 
                            str_detect(Notes, "depred") | 
                            str_detect(Notes, "renest") | 
                            str_detect(Notes, "re-nest") |
                            (str_detect(Notes, "see") & str_detect(Notes, "\\d")) |
                            (str_detect(Notes, "now") & str_detect(Notes, "\\d"))), "Y", "N"),
         notes.failed = ifelse(is.na(Notes), "N", notes.failed)) 
  
  sp_checks3 <- sp_checks2 %>% 
    mutate(Egg2 = ifelse(grepl("not checked", Notes) | 
                           grepl("not found", Notes) | 
                           grepl("missed", Notes), 9, Egg),
           Chick2 = ifelse(grepl("not checked", Notes) | 
                           grepl("not found", Notes) | 
                           grepl("missed", Notes), 9, Chick),
           Egg2 = ifelse(notes.failed == "Y", 0, Egg2),
           Chick2 = ifelse(is.na(Chick2) & str_detect(Notes, "chick"), 8, Chick2),
           Chick2 = ifelse(Chick2 == 8 & str_detect(Notes, "no") & str_detect(Notes, "chick"), 9, Chick2),
           Chick2 = ifelse(is.na(Chick2) & notes.failed == "Y", 0, Chick2),
           Egg2 = ifelse(is.na(Egg2), Egg, Egg2),
           Chick2 = ifelse(is.na(Chick2), Chick, Chick2)) 
  
  sp_checks4 <- sp_checks3 %>% 
    select(-Egg, -Chick) %>%
    select(NO., SPP, DATE, Egg = Egg2, Chick = Chick2, Age, Notes, notes.failed)  %>% 
    mutate(keeper = ifelse(is.na(Egg) & is.na(Chick), "N", "Y")) %>%
    arrange(NO., DATE) %>%
    distinct()
                           
}


all_checks_extracted<- notes_extracter(all_dups_Fixed)



#--------------------------------------------------------
## IMPORTANT MANUAL STEP HERE: examine the ...checks_extracted outputs to make sure no valuable/valid records have been classified with keeper == "N". 

#Probably just need to look at records that have Notes
keeperN_with_noter <- function(sp_checks_extracted) {
  alc_keeperN_with_notes <- sp_checks_extracted %>% 
    filter(keeper == "N", !is.na(Notes))
}
alc_keeperN_with_notes <- keeperN_with_noter(all_checks_extracted)


# can do a manual edit if there is still good data in alc_keeperN_with_notes, changing keeper to Y and changing Egg, Chick, etc as appropriate
alc_keeperN_with_notes_edited <- edit(alc_keeperN_with_notes)
all_checks_extracted_edited <- rbind(all_checks_extracted, alc_keeperN_with_notes_edited) %>% 
  arrange(SPP, NO., DATE)
# then use all_checks_extracted_edited below

alc_noter <- function(sp_checks) {
  # a helper function to check what the most common notes are and compare to the notes that are specified in notes_extracter() to make sure we're dealing with the important ones
  sp_notes <- sp_checks %>% 
  select(Notes) %>% 
  mutate(Notes = sub("\\.$", "", Notes),
         Notes = tolower(Notes)) %>% 
  table() %>% 
  data.frame() %>% 
  arrange(-Freq)
}
bcnh_notes <- alc_noter(bcnh_checks)


trim_keepers <- function(sp_checks_extracted){
# and now filter to have just the keepers
  sp_checks_trimmed <- sp_checks_extracted %>% 
    filter(keeper == "Y") %>% 
    select(-keeper)
}


all_checks_trimmed <- trim_keepers(all_checks_extracted)
all_checks_trimmed <- trim_keepers(all_checks_extracted_edited)

#--------------------------------------------------------

alcatraz2HEPer <- function(sp_checks){
  # finally convert check data from the Alcatraz format (massaged by above functions) to the HEP_screening format, including addition and filling of HEP-specific fields (status, stage, confidence)
  # note that there are some species-specific assumptions built in to the generation of Stage
  # if GREG or GBHE ever nest on Alcatraz the assignment of stages in this function will need to be changed to include stage 3

# create a small df with some of the nest stage boundaries
stages <- data.frame(SPP = as.character(c("BCNH", "SNEG")),
                     end.stg2 = c(10, 10),
                     end.stg4 = c(15, 14))  

zsp_checks <- sp_checks %>% 
  left_join(., stages, by = c("SPP"))



# now the meat of the function to generate the HEP_screening data structure

hep_checks <- zsp_checks %>%
  setNames(tolower(names(.))) %>% 
  rename(ch.age = age) %>% 
  mutate(date = ymd(date),
         adults=as.numeric(""),
         stage = as.numeric(""),
         stage = ifelse((chick == 0 | is.na(chick)) & egg > 0, 1, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & ch.age <= end.stg2, 2, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & ch.age > end.stg2 & ch.age < end.stg4, 4, stage),
         stage = ifelse(chick > 0 & !is.na(chick) & ch.age > end.stg4, 5, stage),
         confidence = "",
         status = "A",
         status = ifelse(egg == 0 & chick == 0, "I", status),
         status = ifelse(egg == 0 & is.na(chick), "I", status),
         status = ifelse(is.na(egg) & chick == 0, "I", status),
         status = ifelse(notes == "not checked" & !is.na(notes) |
                        (egg == 9 & chick == 9) |
                        (egg == 9 & is.na(chick)) |
                        (is.na(egg) & chick == 9), "P", status)) %>% 
  select(date, nest = no., spp, status, adults, stage, chicks = chick, confidence, notes) %>% 
  arrange(nest, spp, date) %>% 
  unique() 
return(hep_checks)
}


alc_hep <- alcatraz2HEPer(all_checks_trimmed)

## as a quick check to make sure 'stage' and 'status' have been filled correctly, these filters should return 0 records
filter(alc_hep, status == "A", is.na(stage), is.na(chicks)) # there should be no records with status == "A" but no data for stage or chicks
filter(alc_hep, status == "I", !is.na(stage)) # there should be no records with status == "I" and valid data for stage
filter(alc_hep, status == "I", stage > 0, chicks > 0) # there should be no records with status == "I" and valid data for stage and chicks

year <- 2017
char_date <- as.Date(Sys.time()) %>% as.character() %>% gsub("-", "", .)
file.path <- paste("Alcatraz_ready4screening/alcatraz", year, "_4screening_codeV", code_version, "_", char_date, ".csv", sep = "")
write.csv(alc_hep, file.path, row.names = F)
###

alc_hep <- read.csv("Alcatraz_ready4screening/alcatraz2018_4screening20190908.csv")

nest_viewer <- function(zspp, znest, znest.exact = FALSE) {
  if(znest.exact == FALSE) {
  alc_hep %>% 
    filter(spp == zspp & grepl(znest, nest)) %>% 
    arrange(date)
  } else {
      alc_hep %>% 
    filter(spp == zspp, nest == znest) %>% 
    arrange(date)
  }
}


nest_viewer_all_checks <- function(zspp, znest, znest.exact = FALSE) {
  if(znest.exact == FALSE) {
  all_checks %>% 
    filter(SPP == zspp & grepl(znest, NO.)) %>% 
    arrange(DATE)
  } else {
      all_checks %>% 
    filter(SPP == zspp, NO. == znest) %>% 
    arrange(DATE)
  }
}


