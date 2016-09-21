#### File Description ####
# Author: Adam Raikes
# Initial Date: 09/21/2016
# Local Depends: none
# Local Files: ./Data Files/Raw_Vision.csv
#
# Description: Trial Validation.R imports the raw data file for the
# vision-condition at a maximum voluntary contraction (MVC) value of 40%. This
# raw data file contains 2500 data points per person per trial, a subject
# identifier, a trial identifier, a condition identifier (deprecated), and a 
# center value.
#
# Trials will be validated in the following manner.
# 1.) Center value will be transformed from voltage to Newtons. A prior linear
# regression was computed for this purpose.
# 2.) Window scale from the trials (35%-45% MVC) will be converted to Newtons.
# 3.) All trial values will be converted from voltage to Newtons.
# 4.) Trial root mean square error (RMSE) in Newtons from the target line
# (center) will be computed per trial.
# 5.) The percentage of data points within the window scale will be computed per
# trial.
# 6.) Trials will be plotted per person and annotated with RMSE and percentage
# of points in view.
# 7.) Plots will be exported for visual analysis.
#
# Trials will be considered valid if > 70% (17.5 out of 25 sec) of the data
# points were within 35-45% MVC.

#### Libraries ####
# Import necessary libraries
# For purposes here, necessary libraries include tidyverse for dplyr and tidyr functions
# and ggplot2 for plotting.

library(tidyverse)
library(ggplot2)

# Load data
rawtrace <- read.csv("Raw_MVC10.csv") %>%
  select(-X) %>%
  group_by(subject, condition, trial) %>%
  gather(xvals, yvals, -subject:-center) 

rawtrace$subject <- gsub("S","", rawtrace$subject)
rawtrace$trial <- gsub("tr", "", rawtrace$trial)
rawtrace$xvals <- gsub("X", "", rawtrace$xvals)

rawtrace$subject <- as.numeric(rawtrace$subject)
rawtrace$trial <- as.numeric(rawtrace$trial)
rawtrace$xvals <- as.numeric(rawtrace$xvals)

vision <- filter(rawtrace, condition == "cond2")

ggplot(data = vision, aes(x = xvals, y = yvals)) +
  geom_line() +
  facet_grid(subject ~ trial, scale= "free") +
  geom_hline(aes(yintercept = center))
  
