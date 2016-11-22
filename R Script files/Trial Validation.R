#### File Description ####
# Author: Adam Raikes
# Initial Date: 09/21/2016
# Local Depends: none
# Local Files: ./Data Files/Raw Data.csv
#
# Description: Trial Validation.R imports the raw data file for the
# vision-condition at a maximum voluntary contraction (MVC) value of 40%. This
# raw data file contains 2500 data points per person per trial, a id
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


#### Libraries ####
# Import necessary libraries
# For purposes here, necessary libraries include tidyverse for dplyr and tidyr functions
# and ggplot2 for plotting.

require(tidyverse)

#### Load data ####
# Data is located in ./Data Files as a CSV file
raw.data <- read.csv("./Data Files/Raw Data.csv")

# View data
head(raw.data)
ncol(raw.data)

# Reshape to long format
raw.data.long <- raw.data %>%
  select(-x) %>%
  select(id, block, trial, center, everything()) %>%
  group_by(id, block, trial, center) %>%
  gather(xvals, yvals, -id:-center)

#### Convert xvals ####
# Need to strip the prepended "x" from the values
raw.data.long$xvals <- as.numeric(gsub("[A-z]", "", raw.data.long$xvals))
head(raw.data.long)

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
                        screen.lower.N = 15.2713 + 26.199*screen.lower,
                        screen.upper.N = 15.2713 + 26.199*screen.upper)

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
  group_by(id, trial, center, center.N) %>%
  summarise(count = n(),
            valid.volts = sum(valid.point.volts)/count,
            valid.newtons = sum(valid.point.newtons)/count)

#### Compute RMSE ####
# Create a summary data frame with id, trial, center.N, and RMSE. This
# frame will ultimately be merged with demographic frames, MMSE frames, DFA
# frames, etc.
trial.summary <- raw.data.long %>%
  group_by(id, trial, center.N) %>%
  summarise(rmse.N = sqrt(mean((newtons-center.N)^2)),
            rmse.V = sqrt(mean((yvals-center)^2))) %>%
  ungroup() %>%
  mutate(valid.newtons = force.valid$valid.newtons,
         valid.volts = force.valid$valid.volts)

#### Create validation plots ####
# These plots will plot the raw trace from 4sec-29sec. The first 4 and last 1
# sec were dropped in the original data processing pipeline. These plots will
# also annotate onto them the RMSE and the number of points inside the window
# for full evaluation.

# Get vector of unique id IDs to loop over
id.vec <- ungroup(trial.summary) %>% 
  distinct(id) %>% 
  select(id) %>%
  collect %>% .[["id"]]

# Create list for plot capture
plot_list = list()

# Make plots
for (i in 1:length(id.vec)) {
  tmp <- filter(raw.data.long, id == id.vec[i]) %>%
    ungroup() %>%
    select(-id)
  
  labels <- filter(trial.summary, id == id.vec[i]) %>%
    select(trial, rmse.V, valid.volts)
  
  p <- ggplot(data = tmp, aes(x = xvals, y = newtons)) +
    facet_grid(trial ~ ., scales = "free") +
    geom_line() +
    xlim(0, 2500) + 
    geom_hline(aes(yintercept = center.N), col = "blue") +
    geom_hline(aes(yintercept = screen.lower.N), col = "blue") +
    geom_hline(aes(yintercept = screen.upper.N), col = "blue") +
    geom_text(aes(x = 500, y = Inf, hjust = 1, vjust = 1.1,
                  label = paste0("RMSE: ", round(rmse.V, 3)),
                  group = NULL),
              data = labels) +
    geom_text(aes(x = 2000, y = Inf, hjust = 1, vjust = 1.1,
                  label = paste0("Points in view: ", round(valid.volts*100, 2), "%"),
                  group = NULL),
              data = labels) +
    ggtitle(paste("Participant: ", id.vec[i])) +
    theme_bw()
  
  plot_list[[i]] <- p
}

# Write plots to Plots folder
my.dir <- getwd()

setwd("Plots")

pdf("Trial Validation.pdf")
for (i in 1:length(id.vec)) {
  print(plot_list[[i]])
}
dev.off()

# Reset working directory
setwd(my.dir)

#### Create validation plots for trials with >10% of points in view ####
# Create list for valid trials
# Valid trials are those with > 10% of points in view. This will maximize the 
# potential of eliminating invalid trials. Further evaluation will be based on
# visual inspection of the plots.
plot_list_valid = list()
trial.vec <- vector()

# Make plots
for (i in 1:length(id.vec)) {
  tmp <- filter(raw.data.long, id == id.vec[i]) %>%
    ungroup() %>%
    select(-id)
  
  labels <- filter(trial.summary, id == id.vec[i]) %>%
    select(trial, rmse.V, valid.volts) %>%
    filter(valid.volts >= 0.1)
  
  if (nrow(labels) == 0) {
    next
  }
  
  tmp <- tmp %>%
    semi_join(labels, by = "trial")
  
  p <- ggplot(data = tmp, aes(x = xvals, y = newtons)) +
    facet_grid(trial ~ ., scales = "free") +
    geom_line() +
    xlim(0, 2500) + 
    geom_hline(aes(yintercept = center.N), col = "blue") +
    geom_hline(aes(yintercept = screen.lower.N), col = "blue") +
    geom_hline(aes(yintercept = screen.upper.N), col = "blue") +
    geom_text(aes(x = 500, y = Inf, hjust = 1, vjust = 1.1,
                  label = paste0("RMSE: ", round(rmse.V, 3)),
                  group = NULL),
              data = labels) +
    geom_text(aes(x = 2000, y = Inf, hjust = 1, vjust = 1.1,
                  label = paste0("Points in view: ", round(valid.volts*100, 2), "%"),
                  group = NULL),
              data = labels) +
    ggtitle(paste("Participant: ", id.vec[i])) +
    theme_bw()
  
  plot_list_valid[[i]] <- p
  trial.vec <- c(trial.vec, i)
}

# Write plots to Plots folder
setwd("Plots")

pdf("Trial Validation 2.pdf")
for (i in trial.vec) {
  print(plot_list_valid[[i]])
}
dev.off()

# Reset working directory
setwd(my.dir)

#### Clean up workspace ####
# Remove objects for which persistence is not required.
rm(
  list = c(
    "p", "plot_list", "plot_list_valid", "id.vec", "i", "tmp", "labels", 
    "raw.data.long", "raw.data", "my.dir", "force.valid", "trial.vec"
  )
)

