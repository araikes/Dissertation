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

#### Load data ####
# Data is located in ./Data Files as a CSV file
raw.data <- read.csv("./Data Files/Raw_Vision.csv")

# View data
head(raw.data)
ncol(raw.data)

# Fix column names
colnames(raw.data)[2502:2505] <- c("subject", "condition", "trial", "center")
head(raw.data)

# Reshape to long format
raw.data.long <- raw.data %>%
  select(-X) %>%
  group_by(subject, condition, trial) %>%
  gather(xvals, yvals, -subject:-center)

# Remove condition column
# This was necessary in the pilot stage but is no longer required.
raw.data.long <- raw.data.long %>%
  ungroup() %>%
  select(-condition)

#### Edit subjects, trials, and xvals ####
# As of right now, the subject, trial, and xval columns are strings. These
# unusable in their current form. These values will be transformed from string
# to numeric using regex functions.
raw.data.long$subject <- gsub("S","", raw.data.long$subject)
raw.data.long$trial <- gsub("tr", "", raw.data.long$trial)
raw.data.long$xvals <- gsub("X", "", raw.data.long$xvals)

raw.data.long$subject <- as.numeric(raw.data.long$subject)
raw.data.long$trial <- as.numeric(raw.data.long$trial)
raw.data.long$xvals <- as.numeric(raw.data.long$xvals)

head(raw.data.long)

#### Reorder data frame ####
# Right now, the data frame is organized by subject, trial, point. This means
# that all of the 0 time point values come first for all participants, then 1,
# etc. This is impractical and unviewable. Dataframe will be resorted to haveall
# of the points for a single participant's trial list in succession.

raw.data.long <- group_by(raw.data.long, subject, trial) %>%
  arrange(xvals)

#### Voltage to Newtons ####
# The force transducer reports the force in voltage. A calibration set was
# created to convert from the voltage to Newtons. This regression is not
# presented here but the coefficients are used.

raw.data.long <- raw.data.long %>%
  mutate(center.N = 15.2713 + 26.199*center,
         newtons = 15.2713 + 26.199*yvals)

#### Get screen scale values ####
# The target line (center) was at 40% MVC. The screen was scaled to 35-45% MVC.
# In order to determine how much of each trial was within view, the lower and
# upper screen bounds need to be computed.

raw.data.long <- mutate(raw.data.long,
                        screen.lower = -0.58 + 0.35*((center + 0.58)/0.4),
                        screen.upper = -0.58 + 0.45*((center + 0.58)/0.4),
                        screen.lower.N = 0.35*(center.N/0.4),
                        screen.upper.N = 0.45*(center.N/0.4))

#### Compute valid points ####
# Valid points are those which fall between screen.lower and screen.upper.
# For validation purposes, this will be computed for voltage and Newtons
raw.data.long <- mutate(
  raw.data.long,
  valid.point.volts = ifelse(yvals >= screen.lower &
                         yvals <= screen.upper,
                       1, 0),
  valid.point.newtons = ifelse(newtons >= screen.lower.N &
                           newtons <= screen.upper.N, 1 , 0))

force.valid <- raw.data.long %>%
  group_by(subject, trial, center, center.N) %>%
  summarise(count = n(),
            valid.volts = sum(valid.point.volts)/count,
            valid.newtons = sum(valid.point.newtons)/count) %>%
  mutate(diff = valid.newtons - valid.volts)

range(force.valid$valid.volts)
range(force.valid$valid.newtons)
range(force.valid$diff)

trials.valid <- force.valid %>%
  group_by(subject, center, center.N) %>%
  filter(valid.newtons >= 0.7) %>%
  summarise(count = n())

vision <- filter(rawtrace, condition == "cond2")

ggplot(data = vision, aes(x = xvals, y = yvals)) +
  geom_line() +
  facet_grid(subject ~ trial, scale= "free") +
  geom_hline(aes(yintercept = center))
  
