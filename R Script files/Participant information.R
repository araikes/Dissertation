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
  select(-age.first:-Notes) %>%
  left_join(suspected.concussions) %>%
  select(-age.first:-Notes) %>%
  left_join(concussion.symptoms) %>%
  left_join(participant.order) %>%
  select(id, block, order, gender, hand, height, weight, gamer, age, 
         diagnosed.number, suspected.number, LOC, seizure, amnesia, sx.current) %>%
  mutate(concussion.number = diagnosed.number + suspected.number,
         prior.concussion = ifelse(concussion.number != 0, "Yes", "No"))

#### Remove unnecessary dataframes ####
rm(list = c("participant.info", "diagnosed.concussions", 
            "suspected.concussions", "concussion.symptoms", "participant.order"))
