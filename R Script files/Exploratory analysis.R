#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/14/2016
# Local Depends: Trial Validation.R
# Local Files: ./Data Files/Raw_Vision.csv, ./Data Files/MMSE_Vision.csv,
# ./Data Files/Detrended_Vision.csv
#
# Description: This is an exploratory analysis of the nonlinear metrics for the
# visual-motor tracking task. At present, only MMSE is computed. This file
# depends on Trial Validation.R to import the RMSE for the trials.
#
# This exploratory analysis will focus primarily on graphical representations of
# variables to assess the overall coherence of the dataset. Primary plots will be
# boxplots.
#
# TODO: Compute spectral properties and DFA for trials

#### Load libraries ####
# This will be accomplished by "require" to avoid unnecessary loading.
require(tidyverse)
require(ggplot2)

#### Read in outcomes data ####
mmse.data <- read.csv("./Data Files/MMSE.csv", header = TRUE, sep = ",")
mmse.detrended.data <- read.csv("./Data Files/Detrended MMSE.csv", header = TRUE, sep = ",")
dfa.data <- read.csv("./Data Files/DFA.csv", header = TRUE, sep = ",")
avp.data <- read.csv("./Data Files/AvP.csv", header = TRUE, sep = ",")