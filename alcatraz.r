
## this file contains code and instructions for converting Alcatraz nest monitoring data to ACR's HEP data format

## all this code should work fine if you acceessed it via the R Project named "alcatraz_hep". If you simply opened the R code file named "alcatraz", then you will have problems with file paths. 


## the data we get from USGS for Alcatraz is in a funky format.
## it comes in a xlsx file, with a sheet for each species
## in each species sheet, there are 3 rows at the top before the data actually start, with the column names occupying 2 rows
## then the actual nest check data are arranged in a wide format, with a row for each nest and then repeating sets of columns for each nest check (e.g. date, nest contents, etc) stacked horizontally out as far as needed to match the number of checks for each nest
## the number of checks varies between species and between individual nests, so the code below reactive to the nest-specific number of checks


# load the required packages
library(tidyverse) 
library(lubridate)
library(xlsx)
library(stringr)


alcatraz_reader_xlsx <- function(year, species){
  # first is a function to read in the csv files for each species
  # this should set the file name correctly, but double check that the data we got from USGS was named following the standard they used in 2015-17 (e.g. Alcatraz2017data_FINAL)
  # reading directly from the xlsx is slow but works fine
  filename <- paste("alcatraz", year, "/Alcatraz", year, "data_FINAL.xlsx", sep="")
  ztabl <- read.xlsx(filename,
                     sheetIndex = species,
                     startRow = 3) %>% 
    select(-starts_with("NA")) # this trims off any random empty columns
  
  return(ztabl)
}

bcnh <- alcatraz_reader_xlsx("2017", "BCNH")
sneg <- alcatraz_reader_xlsx("2017", "SNEG")



#--------------------------------------------------------

#--------------------------------------------------------

alcatraz_nest_infoer <- function(sp.df){
  # next a function to extract info on nest characteristics
  # most of this info is stuff that's not normally collected as part of the HEP protocol, but here's a function to pull it out in case its needed sometime
  # INPUT: "sp.df" is the data frame for a particular species, that we just created with "alcatraz_reader"
nest.info <- sp.df %>% 
  select(NO., Obs., SPP, Colony, Type, Hgt..ft., Easting, Northing, plus.minus..ft.)
return(nest.info)
}

sneg_info <- alcatraz_nest_infoer(sneg)
bcnh_info <- alcatraz_nest_infoer(bcnh)

#--------------------------------------------------------

alcatraz_nest_checker <- function(sp.df){
  # now a function to extract the nest check data from the funky wide USGS format to a normal long table
  # most of what we peel out here has a direct match to what's collected in the HEP protocol, but the format is slightly different 
  # when the xlsx file is read, duplicate column names are given sequential trailing numbers.
  # INPUT: "sp.df" is the data frame for a particular species, that we just created with "alcatraz_reader"
#-----
# determine how many sets of check columns there are
num.check.col.groups <- (ncol(sp.df) - 7 - 2)/5
# make a list of numbers representing each set of check columns
num.check.repeats <- seq(1, length.out = num.check.col.groups - 1, by = 1)

check0 <- sp.df %>% 
  select(NO., SPP, DATE, Egg, Chick, Age, Notes) %>% 
  mutate_at(c("Egg", "Age", "Chick", "Notes"), as.character) # to avoid warnings upon rbind()ing below

# this sub-function selects just those columns with matching trailing numbers, with the trailing number specified with "check.repeat"
checker <- function(check.repeat){
  dot.check.repeat <- paste(".", check.repeat, sep = "")
check <- sp.df %>% 
  select(NO., SPP, contains(dot.check.repeat), -contains("X.")) %>% 
  rename_all(~sub(dot.check.repeat, '', .x)) %>% 
  mutate_at(c("Egg", "Age", "Chick", "Notes"), as.character) # to avoid warnings upon rbind()ing below
return(check)
}
foo8 <- checker(8)
# now run checker() on each repeated set of check columns IDed in num.check.repeats()
checks1 <- map_df(num.check.repeats, checker)
# bind to the first check set of columns and do some cleaning
checks <- rbind(check0, checks1)%>% 
  unique() %>% 
  arrange(NO., DATE)

return(checks)
}

sneg_checks <- alcatraz_nest_checker(sneg)
bcnh_checks <- alcatraz_nest_checker(bcnh)

##-----------------------------------------------------------------------------------------

notes_extracter <- function(sp_checks){
  # fill the Egg or Chick fields with data extracted from the notes fiels, as appropriate
  # many records with no info in Egg or Chick are nonetheless valuable because the notes specify that the nest was empty (Egg and Chick = 0); is valuable info for nest screening, allowing a nest's record to end with an affirmative failure rather than just no data
  # this function fills in Egg and Chick for some of the most common such occurences
  # warnings about NAs introduced by coercion are OK
  # this function adds a 'keeper' field, which flags records as having real data that we want to keep, or as being empty records that are relicts of the USGS table structure
  # the user can double check the records with keeper == "N" in the output to make sure the rules for exclusion worked correctly for the current data file
  # alc_noter() below can help ID new records (with different notes) that may need to be included in this function
  # INPUT: sp_checks is the species-specific df created by alcatraz_nest_checker()
  sp_checks2 <- sp_checks %>% 
  mutate_all(trimws) %>% 
  mutate(Egg = sub("\\+$", "", Egg)) %>%
  mutate_at(c("Egg", "Age", "Chick"), as.numeric) %>% # this isn't explicit, but converting to numeric is a shortcut way to convert dashes to NA
  mutate(Notes = tolower(Notes)) %>% 
  mutate(notes.failed = ifelse((is.na(Egg) & is.na(Chick)) & 
                         ((str_detect(Notes, "empty") & !str_detect(Notes, "missed")) | 
                            str_detect(Notes, "destroyed") | 
                            str_detect(Notes, "renest") | 
                            str_detect(Notes, "re-nest") |
                            (str_detect(Notes, "see") & str_detect(Notes, "\\d")) |
                            (str_detect(Notes, "now") & str_detect(Notes, "\\d"))), "Y", NA), 
         Egg2 = ifelse(is.na(Egg) & notes.failed == "Y", 0, Egg),
         Chick2 = ifelse(is.na(Chick) & str_detect(Notes, "chick"), 8, Chick),
         Chick2 = ifelse(Chick2 == 8 & str_detect(Notes, "no") & str_detect(Notes, "chick"), 9, Chick2),
         Chick2 = ifelse(is.na(Chick2) & notes.failed == "Y", 0, Chick2)) %>% 
  select(-Egg, -Chick) %>% 
  select(NO., SPP, DATE, Egg = Egg2, Chick = Chick2, Age, Notes)  %>% 
  mutate(keeper = ifelse(is.na(Egg) & is.na(Chick), "N", "Y")) %>% 
  arrange(NO., DATE) %>% 
  distinct()
                           
}
sneg_checks_extracted <- notes_extracter(sneg_checks)
bcnh_checks_extracted <- notes_extracter(bcnh_checks)

## IMPORTANT MANUAL STEP HERE: examine the ...checks_extracted outputs to make sure no valuable/valid records have been classified with keeper == "N"

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

bcnh_checks_trimmed <- trim_keepers(bcnh_checks_extracted)
sneg_checks_trimmed <- trim_keepers(sneg_checks_extracted)

#--------------------------------------------------------

alcatraz2HEPer <- function(sp_checks, zspp){
  # finally convert check data from the Alcatraz format (massaged by above functions) to the HEP_screening format, including addition and filling of HEP-specific fields (status, stage, confidence)
  # note that there are some species-specific assumptions built in to the generation of Stage
  # if GREG or GBHE ever nest on Alcatraz the assignment of stages in this function will need to be changed to include stage 3
  
# create a small df with some of the nest stage boundaries
stages <- data.frame(spp = c("BCNH", "SNEG"),
                     end.stg2 = c(10, 10),
                     end.stg4 = c(15, 14))  
# filter to the current species
sp.stages <- filter(stages, spp == zspp)

# now the big sub-function to generate the HEP_screening data structure
hep_checks <- sp_checks %>%
  setNames(tolower(names(.))) %>% 
  rename(ch.age = age) %>% 
  mutate(date = ymd(date),
         adults=as.numeric(""),
         stage = NA,
         stage = ifelse(chick == 0 & egg > 0, 1, stage),
         stage = ifelse(chick > 0 & ch.age <= sp.stages$end.stg2, 2, stage),
         stage = ifelse(chick>0 & ch.age > sp.stages$end.stg2 & ch.age < sp.stages$end.stg4, 4, stage),
         stage = ifelse(chick>0 & ch.age > sp.stages$end.stg4, 5, stage),
         confidence = "",
         status = ifelse(egg == 0 & chick == 0, "I", "A")) %>% 
  arrange(no., date) %>% 
  select(date, nest = no., spp, status, adults, stage, chicks = chick, confidence, notes) %>% 
  unique()   

return(hep_checks)
}



sneg_hep <- alcatraz2HEPer(sneg_checks_trimmed, "SNEG")
bcnh_hep <- alcatraz2HEPer(bcnh_checks_trimmed, "BCNH")


by_day_check_checker <- function(sp_hep) {
  #one final check for unique records for the same nest on the same day
  # can view output to figure out which fields are different
  foo <- data.frame(table(sp_hep$nest, sp_hep$date)) %>% 
    rename(nest = Var1, date = Var2, num.checks = Freq) %>% 
    filter(num.checks > 1) %>% 
    mutate(nest = as.character(nest),
           date = ymd(date)) %>% 
    left_join(sp_hep, by = c("nest", "date")) %>% 
    arrange(nest, date)
  
}
bcnh_dups <- by_day_check_checker(bcnh_hep)
sneg_dups <- by_day_check_checker(sneg_hep)


dup_check_scrubber <- function(sp_hep){
  # if dup unique checks exist, they most likely have different notes.
  # this function currently pastes together the notes from duplicate nest checks on the same day, but with different notes.
  # function can be modified if other fields don't match
  tester2 <- sp_hep %>% 
  mutate(notes = ifelse(is.na(notes), "", notes)) %>% 
  group_by(nest, date) %>% 
  mutate(notes = (trimws(paste(notes, collapse = ' ')))) %>% 
  distinct() %>% 
  mutate(notes = ifelse(notes == "", NA, notes))
}

bcnh_hep_noDups <- dup_check_scrubber(bcnh_hep)
sneg_hep_noDups <- dup_check_scrubber(sneg_hep)

# HEP_screening code uses file with all species for a colony together.
## combine SNEG and BCNH

alcatraz_sneg_bcnh=rbind(sneg_hep, bcnh_hep) %>% 
  arrange(nest, spp, date)

writefile <- paste("hep_screening/field_data/", year, "/alcatraz/alcatraz_sneg_bcnh_HEPscreen", year, ".csv", sep="")
write.csv(alcatraz_sneg_bcnh, writefile, row.names = F)


