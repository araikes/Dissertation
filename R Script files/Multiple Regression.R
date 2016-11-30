#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/30/2016
# Local Depends: 
# Local Files: ./Data Files/*.csv
#
# Description: Multiple regression.R is the multiple regression for my
# dissertation. This file will contain all of the commands for running,
# plotting, and interpretting the multiple regressions used for each outcome.

#### Create clean workspace ####
rm(list = ls())

#### Source additional files ####

# Load helper functions
source("./R Script files/Helper functions.R")

# Load participant information
source("./R Script files/Participant information.R")

#### Load additional libraries ####
require(tidyverse)
require(furniture)
require(relaimpo)

#### Load outcomes data ####
mmse.data <- read.csv("./Data Files/MMSE.csv",
                      header = TRUE, sep = ",")

mmse.detrended.data <- read.csv("./Data Files/Detrended MMSE.csv",
                                header = TRUE, sep = ",")

dfa.data <- read.csv("/Data Files/DFA.csv",
                     header = TRUE, sep = ",")

avp.data <- read.csv("./Data Files/AvP.csv",
                     header = TRUE, sep = ",")

trial.summary <- read.csv("./Data Files/Trial Summary.csv",
                          header = TRUE, sep = ",")

#### Identify participants and trials to be excluded ####
# Removal is based on criteria:
# 1. Number of visible points per trial is > 20%
# 2. Trials do not include areas of 0 slope, indicating removal of the finger from the load cell.
# 3. Participants are removed if the number of valid trials < 6.
bad.trials <- read.csv("./Data Files/Excluded Trials.csv",
                       header = TRUE, sep = ",")

valid.trials <- trial_inclusion(trial.summary, bad.trials)

rm(list = "bad.trials")

#### 