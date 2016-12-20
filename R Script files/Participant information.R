#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/22/2016
# Local Depends: Deidentification.R
# Local Files: ./Data Files/Participant Info.csv, ./Data Files/Diagnosed Concussions.csv,
# ./Data Files/Suspected Concussion.csv, # ./Data Files/Concussion Symptoms.csv
#
# Description: This script will compile information about the participants
# demographic and injury history. The purpose at present (11/22) is to pick out
# key variables necessary for the exploratory analyses.

#### Load libraries ####
require(tidyverse)

#### Read data ####
participant.info <- read.csv("./Data Files/Participant Info.csv",
                             header = TRUE, sep = ",")

suspected.concussions <- read.csv("./Data Files/Suspected Concussions.csv",
                                  header = TRUE, sep = ",")

diagnosed.concussions <- read.csv("./Data Files/Diagnosed Concussions.csv",
                                  header = TRUE, sep = ",")

concussion.symptoms <- read.csv("./Data Files/Concussion Symptoms.csv",
                                header = TRUE, sep = ",")

participant.order <- read.csv("./Data Files/Order.csv",
                              header = TRUE, sep = ",")

#### Create minimal participant frame ####
participants <- participant.info %>%
  select(id, block, gender, hand, height, weight, gamer, age) %>%
  left_join(diagnosed.concussions) %>%
  select(-age.first, -physician:-Notes) %>%
  rename(diagnosed.recent = age.recent) %>%
  left_join(suspected.concussions) %>%
  select(-age.first, -total.reported, -Notes) %>%
  rename(suspected.recent = age.recent) %>%
  left_join(concussion.symptoms) %>%
  left_join(participant.order) %>%
  select(id, block, order, gender, hand, height, weight, gamer, age, 
         diagnosed.number, suspected.number, LOC, LOC.number, LOC.number2, 
         seizure, amnesia, retrograde.duration1, retrograde.duration2, anterograde.duration1,
         anterograde.duration2, anterograde.duration3, anterograde.duration4,
         sx.current, diagnosed.recent, suspected.recent) %>%
  mutate(concussion.number = diagnosed.number + suspected.number,
         prior.concussion = ifelse(concussion.number != 0, "Yes", "No"),
         LOC.number = ifelse(is.na(LOC.number), 0, LOC.number),
         LOC.number2 = ifelse(is.na(LOC.number2), 0, LOC.number2),
         retrograde.duration1 = ifelse(retrograde.duration1 == 0, NA, retrograde.duration1),
         anterograde.duration1 = ifelse(anterograde.duration1 == 0, NA, anterograde.duration1)) %>%
  group_by(id) %>%
  mutate(total.loc = sum(LOC.number, LOC.number2),
         total.retrograde = sum(!is.na(retrograde.duration1), !is.na(retrograde.duration2)),
         total.anterograde = sum(!is.na(anterograde.duration1), !is.na(anterograde.duration2),
                                 !is.na(anterograde.duration3), !is.na(anterograde.duration4))) %>%
  ungroup()

#### Remove unnecessary dataframes ####
rm(list = c("participant.info", "diagnosed.concussions", 
            "suspected.concussions", "concussion.symptoms", "participant.order"))
