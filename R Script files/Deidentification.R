#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/22/2016
# Local Depends: none
# Local Files: ./Original Data Files/*.csv, ./Original Data Files/*.xlsx
#
# Description: In order to be able to use data files for this project on
# multiple computers through Git, I need to deidentify the data. This means that
# all of the subject IDs need to be newly and randomly assigned and
# participation date and DOB need to be reduced to ages and removed.

# Additionally, because participants already have local IDs, this ID system
# needs to be consistent throughout the all of the CSVs imported. Finally, a
# local copy of the original and new IDs needs to be stored on campus and not
# synced between systems.
#
# This file is meant to be sourced as needed when the participant database and
# data files are updated.

#### Load libraries ####
library(openxlsx)
library(dplyr)

#### Set seed ####
set.seed(98657426)

#### Load first data frame ####
# The participant information sheet is Page 1 of Participant Information.xlsx.
# We will use this page to get participant IDs from which to generate random
# IDs. Additionally, participation date and DOB will be converted to ages.

participant.info <- read.xlsx("./Original Data Files/Participant Information.xlsx",
                              sheet = 1, detectDates = TRUE)

#### Get participant IDs ####
subject.ids <- participant.info %>% 
  distinct(participant) %>% 
  select(participant)

random.ids <- sample(1:1000, nrow(subject.ids), replace = FALSE)

subject.ids <- mutate(subject.ids, 
                      id = random.ids,
                      block = ifelse(participant %% 2 == 1, "Second", "First")) %>%
  rename(subject = participant) 

#### Deidentify participant info ####
participant.info <- rename(participant.info, subject = participant) %>%
  left_join(subject.ids) %>%
  mutate(age = (date - dob)/365) %>%
  select(-subject, -date, -dob) %>%
  select(id, block, everything())

#### Deidentify remaining participant information sheets ####
# This step will deidentify the rest of the participant information as well as
# write each sheet in the original .xlsx file to a CSV for future use.

sheet.names <- getSheetNames("./Original Data Files/Participant Information.xlsx")
  
for (i in 2:length(sheet.names)){
  tmp <- read.xlsx("./Original Data Files/Participant Information.xlsx",
                   sheet = i) %>%
    rename(subject = participant) 
  
  tmp <- deidentify(tmp, subject.ids)
  
  write.csv(tmp,
            paste("./Data Files/", sheet.names[i], ".csv", sep = ""),
            sep = ",",
            row.names = FALSE,
            col.names = TRUE)
}

write.csv(participant.info, "./Data Files/Paricipant Info.csv",
          sep = ",",
          row.names = FALSE,
          col.names = TRUE)


#### Load outcomes data and deidentify ####
outcome.files <- list.files(path = "./Original Data Files", 
                            pattern = ".csv")

for (i in 1:length(outcome.files)){
  tmp <- read.csv(paste("./Original Data Files/", outcome.files[i], sep = ""),
                  header = TRUE)
  colnames(tmp) <- tolower(colnames(tmp))
  
  tmp <- prepSubs(tmp)
  tmp <- deidentify(tmp, subject.ids)
  
  write.csv(tmp,
            paste("./Data Files/", outcome.files[i], sep = ""),
            row.names = FALSE,
            col.names = TRUE)
}

#### Write master list of IDs to file ####
subject.ids <- arrange(subject.ids,
                       subject)
write.csv(subject.ids,
          "./Original Data Files/Subject IDs.csv",
          row.names = FALSE)

#### Remove all objects ####
rm(list = c("tmp", "subject.ids", "participant.info", "random.ids",
             "sheet.names", "outcome.files", "i"))

