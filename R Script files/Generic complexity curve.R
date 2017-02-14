#### File Description ####
# Author: Adam Raikes
# Initial Date: 2/12/2016
# Local Depends: 
# Local Files: ./Data Files/*.csv
#
# Description: General purpose plot of complexity

#### Create clean workspace ####
rm(list = ls())

#### Source additional files ####

# Load helper functions
source("./R Script files/Helper functions.R")

# Load participant information
source("./R Script files/Participant information.R")

#### Load additional libraries ####
require(tidyverse)
require(cowplot)
#require(verification)

#### Load outcomes data ####
mmse.detrended.data <- read.csv("./Data Files/Detrended MMSE.csv",
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

rm(list = c("bad.trials"))

# to long
detrended.mmse.long <- mmse.detrended.data %>%
  gather(scale, sampen, -id, -block, -trial) %>%
  mutate_if(is.character, funs(editVars)) %>%
  mutate_if(is.character, as.numeric)

# Compute complexity
detrended.complexity <- detrended.mmse.long %>%
  group_by(id, trial) %>%
  summarise(complexity = sum(sampen)) %>%
  mutate(type = "detrended.complexity")

#### Composite data frame ####
participants <- semi_join(participants, valid.trials) %>%
  select(id, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent) %>%
  filter(sx.current != "Yes" | is.na(sx.current)) %>%
  filter(hand != "Left")

valid.participants <- select(participants, id)

trial.outcomes <- semi_join(detrended.complexity, valid.participants) %>%
  arrange(complexity)

### Plot frame
plot.frame <- detrended.mmse.long %>%
  filter(id == 845 | id == 195) %>%
  filter(trial == 10)

curve.plot <- ggplot(data = plot.frame, aes(x = scale, y = sampen, group = id)) +
  geom_point(size = 3) + 
  geom_line(size = 1) + 
  xlab(expression(paste(tau))) + 
  ylab("Sample Entropy") +
  annotate("text", x = 30, y = 0.65, label = "Complexity = 20.63", size = 6) +
  annotate("text", x = 30, y = 1.55, label = "Complexity = 54.81", size = 6) +
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 12),
    legend.text = element_text(family ="Arial", size = 24),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))

my_dir <- "C:/Users/a01382606/Dropbox/Adam/Dissertation/Images/General"
save_plot(paste(my_dir, "/Complexity curves.svg", sep = ""),
          curve.plot,
          base_width = 14,
          base_height = 6)


  
