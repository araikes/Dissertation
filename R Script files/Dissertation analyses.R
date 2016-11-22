#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/22/2016
#
# Description: This is the master analytical file. The purpose of this file is
# to source various other files and functions related to the analyses contained
# in my dissertation. The expectation is for simple coding where calls are made
# either to other files or to functions contained in Helper functions.R so that
# there is a sensible workflow.
#
# Libraries will be loaded on demand by files and functions.

#### Load helper functions ####
source("./R Script files/Helper functions.R")

#### Deidentify data ####
source("./R Script files/Deidentification.R")

#### Trial validation ####
source("./R Script files/Trial Validation.R")
