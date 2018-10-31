
## this file contains code and instructions for converting Alcatraz nest monitoring data to our HEP data format

## all this code should work fine if you acceessed it via the R Project named "Alcatraz". If you simply opened the R code file named "alcatraz", then you will have problems with file paths. 


## the data we get for Alcatraz is in a pretty funky format.
## it comes in a xlsx file, with a sheet for each species
## in each species sheet, there are 3 rows at the top before the data actually start
## then the actual nest check data are arranged in a wide format, with a row for each nest and then sets of columns for each nest check (e.g. date, nest contents, etc) stacked horizontally out as far as needed to match the number of checks for each nest
## the number of checks varies between species and between individual nests, so I tried to make the code below as reactive as possible to number of checks

## while xlsx can be read in to R, the data behave a little better if you first make csv files for each species.
## this can be done by 
# 1. opening the xlsx, which should be named AlcatrazYEARdata_FINAL,
# 2. click on the sheet tab for a species, 
# 3. do [file -> save as] and replace the FINAL in the file name with the lowercase 4-letter species code (e.g. Alcatraz2017data_sneg), and setting save as type to CSV(Comma delimited)

## once this is done for each species, the following code can be run

# load the required packages
library(tidyverse) 
library(lubridate)


# first is a function to read in the csv files for each species
# this should set the file name correctly, but double check that the data we got from USGS or whoever was named following the standard they used in 2015-17 (e.g. Alcatraz2017data_FINAL)

alcatraz_reader <- function(year, species){
filename <- paste("alcatraz", year, "/Alcatraz", year, "data_", species, ".csv", sep="")
ztabl <- read.csv(filename,
                  colClasses = "character") %>% 
  select(-starts_with("X")) # this trims off any random empty columns
return(ztabl)
}

# call the funtion for each species, specifying the year you're working with
sneg <- alcatraz_reader("2017", "sneg")
bcnh <- alcatraz_reader("2017", "bcnh")

# in all the following functions, "sp.df" is the data frame for a particular species; these data frames are what we just created with "alcatraz_reader"

#--------------------------------------------------------
# next a function to extract info on nest characteristics
# most of this info is stuff that's not normally collected as part of the HEP protocol
# it will be retained in a separate file
alcatraz_nest_infoer <- function(sp.df){
nest.info <- sp.df %>% 
  select(NO., Obs., SPP, Colony, Type, Hgt..ft., Easting, Northing, plus.minus..ft.)
return(nest.info)
}

# call the function for each species
sneg_info <- alcatraz_nest_infoer(sneg)
bcnh_info <- alcatraz_nest_infoer(bcnh)

#--------------------------------------------------------
# now a function to extract the nest check data
# most of what we peel out here has a direct match to what's collected in the HEP protocol, but the format is slightly different 
alcatraz_nest_checker <- function(sp.df){
# determine how many sets of check columns there are
num.check.col.groups <- (ncol(sp.df) - 7 - 2)/5
# make a list of numbers representing each 
num.check.repeats <- seq(1, length.out = num.check.col.groups - 1, by = 1)

check0 <- sp.df %>% 
  select(NO., SPP, DATE, Egg, Chick, Age, Notes)

checker <- function(check.repeat){
  dot.check.repeat <- paste(".", check.repeat, sep = "")
check <- sp.df %>% 
  select(NO., SPP, contains(dot.check.repeat), -contains("X.")) %>% 
  rename_all(~sub(dot.check.repeat, '', .x))
return(check)
}


checks1 <- map_df(num.check.repeats, checker)

checks <- rbind(check0, checks1) %>% 
  unique() %>% 
  drop_na(Egg, Chick, Age)

return(checks)
}

sneg_checks <- alcatraz_nest_checker(sneg)
bcnh_checks <- alcatraz_nest_checker(bcnh)


#--------------------------------------------------------
# now a function to convert check data from the Alcatraz format to the HEP_screening format
# note that there are some species-specific assumptions built in to the generation of Stage
# also note that while the drop_na command removes records with no real data, some of these records might have info in the notes that may be valuable for some non-hep related projects (i.e. specification that a nest was missed on a particular day)
alcatraz2HEPer <- function(zchecks, zspp){
# create a small df with some of the nest stage boundaries
stages <- data.frame(spp = c("BCNH", "SNEG"),
                     end.stg1 = c(10, 10),
                     end.stg2 = c(14, 13))  
# filter to the current species
sp.stages <- filter(stages, spp == zspp)

# now the big chunk to generate the HEP_screening data structure
hep.checks <- zchecks %>%
  setNames(tolower(names(.))) %>% 
  rename(ch.age = age) %>% 
  mutate(date = mdy(date),
         egg=as.numeric(egg),
         chick=as.numeric(chick),
         adults=as.numeric(""),
         ch.age=as.numeric(ch.age),
         stage=ifelse(chick == 0 & egg == 0, 9,
               ifelse(chick == 0 & egg > 0, 1, 
                      ifelse(chick > 0 & ch.age <= sp.stages$end.stg1, 2, 
                             ifelse(chick>0 & ch.age > sp.stages$end.stg1 & ch.age < sp.stages$end.stg2, 4, 5)))),
         confidence = "",
         status = ifelse(stage == 9, "I", "A"),
         egg = ifelse(egg == "", NA, egg),
         chick = ifelse(chick == "", NA, chick),
         #ch.age = ifelse(ch.age == "" | ch.age == "-" |ch.age == "n/a" | ch.age == "N/A", NA, ch.age),
         notes = ifelse(notes == "", NA, notes)) %>% 
  arrange(no., date) %>% 
  mutate(dropper = ifelse((is.na(egg) & is.na(chick) & is.na(ch.age) & is.na(notes)), 1, 0)) %>% 
  filter(dropper == 0) %>% 
  select(date, nest = no., spp, status, adults, stage, chicks = chick, confidence, notes) %>% 
  unique()   

return(hep.checks)
}



sneg_hep <- alcatraz2HEPer(sneg_checks, "SNEG")
bcnh_hep <- alcatraz2HEPer(bcnh_checks, "BCNH")



## combine SNEG and BCNH

alcatraz_sneg_bcnh=rbind(sneg_hep, bcnh_hep)

writefile <- paste("hep_screening/field_data/", year, "/alcatraz/alcatraz_sneg_bcnh_HEPscreen", year, ".csv", sep="")
write.csv(alcatraz_sneg_bcnh, writefile, row.names = F)

alcatraz_sneg_bcnh <- alcatraz_sneg_bcnh %>% 
  arrange(nest, spp, date)
