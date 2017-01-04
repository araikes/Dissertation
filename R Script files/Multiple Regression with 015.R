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

#### Load outcomes data ####
mmse.data <- read.csv("./Data Files/MMSE_r15.csv",
                      header = TRUE, sep = ",")

mmse.detrended.data <- read.csv("./Data Files/Detrended MMSE_r15.csv",
                                header = TRUE, sep = ",")

dfa.data <- read.csv("./Data Files/DFA.csv",
                     header = TRUE, sep = ",")

avp.data <- read.csv("./Data Files/AvP.csv",
                     header = TRUE, sep = ",")

trial.summary <- read.csv("./Data Files/Trial Summary.csv",
                          header = TRUE, sep = ",")

#### Identify participants and trials to be excluded ####
# Removal is based on criteria:
# 1. Number of visible points per trial is > 20%
# 2. Trials do not include areas of 0 slope, indicating removal of the finger from the load cell.
# 3. Participants are removed if the number of valid trials < 5.
# 4. Participants are removed if there are current symptoms
bad.trials <- read.csv("./Data Files/Excluded Trials.csv",
                       header = TRUE, sep = ",")

valid.trials <- trial_inclusion(trial.summary, bad.trials)

rm(list = "bad.trials")

#### Transform MMSE dataframes to long and convert scales from chr to numeric ####
mmse.data.long <- mmse.data %>%
  gather(scale, sampen, -id, -block, -trial) %>%
  mutate_if(is.character, funs(editVars)) %>%
  mutate_if(is.character, as.numeric)

detrended.mmse.long <- mmse.detrended.data %>%
  gather(scale, sampen, -id, -block, -trial) %>%
  mutate_if(is.character, funs(editVars)) %>%
  mutate_if(is.character, as.numeric)

rm(list = c("mmse.data", "mmse.detrended.data"))

#### Get first fifty valid participants ####
first.fifty <- participants %>%
  semi_join(valid.trials) %>%
  filter(sx.current != "Yes" | is.na(sx.current)) %>%
  filter(hand != "Left") %>%
  select(id, order) %>%
  arrange(order) %>%
  filter(row_number(order) <= 50)

first.fifty.valid <- semi_join(valid.trials, first.fifty)

#### Plot MMSE curves ####
mmse.data.long <- mmse.data.long %>%
  semi_join(first.fifty.valid) %>%
  left_join(participants) %>%
  select(id, trial, scale, sampen, prior.concussion)
  
ggplot(data = mmse.data.long, aes(x = scale, y = sampen, 
                                  group = interaction(id, trial))) +
  facet_wrap(~prior.concussion) +
  geom_line()

detrended.mmse.long <- detrended.mmse.long %>%
  semi_join(first.fifty.valid) %>%
  left_join(participants) %>%
  select(id, trial, scale, sampen, prior.concussion)

ggplot(data = detrended.mmse.long, aes(x = scale, y = sampen, 
                                  group = interaction(id, trial))) +
  facet_wrap(~prior.concussion) +
  geom_line()

#### Compute complexity ####
raw.complexity <- mmse.data.long %>%
  group_by(id, trial) %>%
  summarise(complexity = sum(sampen)) %>%
  mutate(type = "raw.complexity")

detrended.complexity <- detrended.mmse.long %>%
  group_by(id, trial) %>%
  summarise(complexity = sum(sampen)) %>%
  mutate(type = "detrended.complexity")

complexity <- bind_rows(raw.complexity, detrended.complexity) %>%
  spread(type, complexity)

rm(list = c("raw.complexity", "detrended.complexity"))

#### Composite data frame ####
avp.data <- semi_join(avp.data, first.fifty.valid)

trial.summary <- semi_join(trial.summary, first.fifty.valid)

injury.time <- c("Uninjured", "Recent", "Remote")

participants <- semi_join(participants, first.fifty) %>%
  select(id, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, total.loc, total.retrograde, total.anterograde,
         diagnosed.recent, suspected.recent) %>%
  group_by(id) %>%
  mutate(tfi.min = age - max(diagnosed.recent, suspected.recent, na.rm = TRUE) - 1,
         tfi.max = age - max(diagnosed.recent, suspected.recent, na.rm = TRUE),
         tfi.min.nom = ifelse(tfi.min == Inf, "Uninjured", 
                          ifelse(tfi.min > 0.5, "Remote", "Recent")),
         tfi.max.nom = ifelse(tfi.max == Inf, "Uninjured", 
                              ifelse(tfi.max > 0.5, "Remote", "Recent")))

trial.outcomes <- left_join(complexity, avp.data) %>%
  left_join(trial.summary) %>%
  select(-valid.newtons, -valid.volts, -rmse.N)

#### Summarise individual outcomes ####
average.outcomes <- trial.outcomes %>%
  group_by(id) %>%
  summarise_each(funs(mean, cv, n()), rmse.V, raw.complexity, detrended.complexity, 
                 avp04, avp48, avp812) %>%
  ungroup() %>%
  select(id:rmse.V_n) %>%
  rename(trials = rmse.V_n) %>%
  left_join(participants) %>%
  select(id, order, block, trials, gender:tfi.max.nom, everything()) 

#### Descriptive statistics ####
table1(participants,
       height, weight, age, gender, hand, gamer, LOC, amnesia,
       as.factor(diagnosed.number), as.factor(suspected.number), as.factor(concussion.number),
       as.factor(total.loc), as.factor(total.retrograde), as.factor(total.anterograde),
       splitby = ~prior.concussion,
       test = TRUE,
       format_output = "full",
       output_type = "markdown")  

#### Boxplot outcomes ####
require(cowplot)
outcomes <- c("rmse.V_mean", "raw.complexity_mean",
              "detrended.complexity_mean", "avp04_mean", "avp48_mean", 
              "avp812_mean")
titles <- c("RMSE", "Complexity", "Detrended Complexity",
            "Average 0-4 Hz Power",
            "Average 4-8 Hz Power", "Average 8-12 Hz Power")

for (i in 1:length(outcomes)){
  a <- my_boxplot(group = "block", y = outcomes[i], data = average.outcomes)
  b <- my_boxplot(group = "prior.concussion", y = outcomes[i], data = average.outcomes)
  c <- my_boxplot(group = "gender", y = outcomes[i], data = average.outcomes)
  d <- my_boxplot(group = "gamer", y = outcomes[i], data = average.outcomes)
  
  grid <- plot_grid(a, b, c, d,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2)
  title <- ggdraw() + draw_label(titles[i], fontface = "bold")
  
  panel.plot <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1,1))
  print(panel.plot)
}

#### Scatterplots ####
desc = c("age", "diagnosed.number", "suspected.number", "concussion.number")

for (i in 1:length(outcomes)){
  cols <- c(desc, outcomes[i])
  
  scat <- my_scatterplot(cols = cols, title = titles[i], data = average.outcomes)
  
  print(scat)
}

#### Profile plots ####

ggplot(data = average.outcomes, aes(x = diagnosed.number, y = raw.complexity_mean,
                                    col = interaction(gender, LOC))) +
  geom_point(aes(shape = amnesia))

ggplot(data = average.outcomes, aes(x = suspected.number, y = raw.complexity_mean,
                                    col = interaction(gender, LOC))) +
  geom_point(aes(shape = amnesia))

### 3D scatterplot
require(scatterplot3d)
shapes = c(16,17)
shapes <- shapes[as.numeric(average.outcomes$gender)]

# 3D Scatter plot of numbers of concussions, loc/amnesia amount, complexity
with(average.outcomes, {
  # Diagnosed number----
  scatterplot3d(x = diagnosed.number,
                y = suspected.number,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = diagnosed.number,
                y = total.loc,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = diagnosed.number,
                y = total.retrograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = diagnosed.number,
                y = total.anterograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  
  # Suspected number-----
  scatterplot3d(x = suspected.number,
                y = suspected.number,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = suspected.number,
                y = total.loc,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = suspected.number,
                y = total.retrograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = suspected.number,
                y = total.anterograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  
  # Total number----
  scatterplot3d(x = concussion.number,
                y = suspected.number,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = concussion.number,
                y = total.loc,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = concussion.number,
                y = total.retrograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 100,
                type = "h")
  scatterplot3d(x = concussion.number,
                y = total.anterograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
})

with(average.outcomes, {
  scatterplot3d(x = diagnosed.number,
                y = suspected.number,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = diagnosed.number,
                y = total.loc,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = diagnosed.number,
                y = total.retrograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
  scatterplot3d(x = diagnosed.number,
                y = total.anterograde,
                z = raw.complexity_mean,
                pch = shapes,
                angle = 135,
                type = "h")
})

#-------------Models as defined in my dissertation proposal--------------------####
#### Fit linear models for average values ####
# These will fit the linear models specifically defined in the dissertation proposal.
require(gvlma)

average.outcomes <- rename(average.outcomes, total.concussions = concussion.number) %>%
  mutate(age.c = age - mean(age))

### Average raw complexity ####
mmse.lm <- lm(raw.complexity_mean ~ (diagnosed.number + suspected.number + 
                                       LOC + amnesia)^2,
              data = average.outcomes)
mmse.step <- MASS::stepAIC(mmse.lm, trace = FALSE) 

anova(mmse.step, mmse.lm)

summary(gvlma(mmse.step))
plot(gvlma(mmse.step))
summary(mmse.step)

### Average detrended complexity ####
dt.mmse.lm <- lm(detrended.complexity_mean ~ (diagnosed.number + suspected.number + 
                                                LOC + amnesia)^2,
              data = average.outcomes)
dt.mmse.step <- MASS::stepAIC(dt.mmse.lm, trace = FALSE) 

anova(dt.mmse.step, dt.mmse.lm)

summary(gvlma(dt.mmse.step))
plot(gvlma(dt.mmse.step),onepage = FALSE)
dt.mmse.step.del <- deletion.gvlma(gvlma(dt.mmse.step))
display.delstats(dt.mmse.step.del$DeltaStat4, dt.mmse.step.del$Stat4pvalue)

# Drop observation
dt.mmse.lm <- lm(detrended.complexity_mean ~ (diagnosed.number + suspected.number + 
                                                LOC + amnesia)^2,
                 data = average.outcomes[c(-15, -45),])
dt.mmse.step <- MASS::stepAIC(dt.mmse.lm, trace = FALSE) 

anova(dt.mmse.step, dt.mmse.lm)

summary(gvlma(dt.mmse.step))
plot(gvlma(dt.mmse.step), onepage = FALSE)
dt.mmse.step.del <- deletion.gvlma(gvlma(dt.mmse.step))
display.delstats(dt.mmse.step.del$DeltaStat4, dt.mmse.step.del$Stat4pvalue)


summary(dt.mmse.step)

### Average RMSE ####
rmse.lm <- lm(rmse.V_mean ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
              data = average.outcomes)
rmse.step <- MASS::stepAIC(rmse.lm, trace = FALSE) 

anova(rmse.step, rmse.lm)

summary(gvlma(rmse.lm))
plot(gvlma(rmse.step))

# Log transformation
rmse.lm <- lm(log(rmse.V_mean) ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
              data = average.outcomes)
rmse.step <- MASS::stepAIC(rmse.lm, trace = FALSE) 

anova(rmse.step, rmse.lm)

summary(gvlma(rmse.step))


### Average Power from 0-4 Hz ####
avp04.lm <- lm(avp04_mean ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
              data = average.outcomes)
avp04.step <- MASS::stepAIC(avp04.lm, trace = FALSE) 

anova(avp04.step, avp04.lm)

summary(gvlma(avp04.lm))
plot(gvlma(avp04.lm), onepage = FALSE)

# Drop outlier
avp04.lm <- lm(avp04_mean ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
               data = average.outcomes[c(-27),])
avp04.step <- MASS::stepAIC(avp04.lm, trace = FALSE) 

anova(avp04.step, avp04.lm)

summary(gvlma(avp04.step))
plot(gvlma(avp04.step), onepage = FALSE)


### Average Power from 4-8 Hz ####
avp48.lm <- lm(avp48_mean ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
               data = average.outcomes)
avp48.step <- MASS::stepAIC(avp48.lm, trace = FALSE) 

anova(avp48.step, avp48.lm)

summary(gvlma(avp48.step))
plot(gvlma(avp48.lm), onepage = FALSE)
deletion.gvlma(gvlma(avp48.lm))

# Drop one observation
avp48.lm <- lm(avp48_mean ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
               data = average.outcomes[c(-17),])
avp48.step <- MASS::stepAIC(avp48.lm, trace = FALSE) 

anova(avp48.step, avp48.lm)

summary(gvlma(avp48.step))

### Average Power from 8-12 Hz ####
avp812.lm <- lm(avp812_mean ~ (diagnosed.number + suspected.number + 
                                 LOC + amnesia)^2,
               data = average.outcomes)
avp812.step <- MASS::stepAIC(avp812.lm, trace = FALSE) 

anova(avp812.step, avp812.lm)

summary(gvlma(avp812.step))
plot(gvlma(avp812.step), onepage = FALSE)

### Log transform 
avp812.lm <- lm(log(avp812_mean) ~ (diagnosed.number + suspected.number + 
                                 LOC + amnesia)^2,
                data = average.outcomes)
avp812.step <- MASS::stepAIC(avp812.lm, trace = FALSE) 

anova(avp812.step, avp812.lm)

summary(gvlma(avp812.step))
plot(gvlma(avp812.step), onepage = FALSE)

#----------------------------------------------------------------------####
#### Fit linear models for the coefficient of variation ####

### Raw complexity CV ####
mmse.cv.lm <- lm(raw.complexity_cv ~ (diagnosed.number + suspected.number + 
                                       LOC + amnesia)^2,
              data = average.outcomes)
mmse.cv.step <- MASS::stepAIC(mmse.cv.lm, trace = FALSE) 

anova(mmse.cv.step, mmse.cv.lm)

summary(gvlma(mmse.cv.step))
plot(gvlma(mmse.cv.step), onepage = FALSE)

# Log transform
mmse.cv.lm <- lm(log(raw.complexity_cv) ~ (diagnosed.number + suspected.number + 
                                        LOC + amnesia)^2,
                 data = average.outcomes)
mmse.cv.step <- MASS::stepAIC(mmse.cv.lm, trace = FALSE) 

anova(mmse.cv.step, mmse.cv.lm)

summary(gvlma(mmse.cv.step))


### Detrended complexity CV ####
dt.mmse.cv.lm <- lm(detrended.complexity_cv ~ (diagnosed.number + suspected.number + 
                                                LOC + amnesia)^2,
                 data = average.outcomes)
dt.mmse.cv.step <- MASS::stepAIC(dt.mmse.cv.lm, trace = FALSE) 

anova(dt.mmse.cv.step, dt.mmse.cv.lm)

summary(gvlma(dt.mmse.cv.step))
plot(gvlma(dt.mmse.cv.step), onepage = FALSE)

# Log transformation
dt.mmse.cv.lm <- lm(log(detrended.complexity_cv) ~ (diagnosed.number + suspected.number + 
                                                 LOC + amnesia)^2,
                    data = average.outcomes)
dt.mmse.cv.step <- MASS::stepAIC(dt.mmse.cv.lm, trace = FALSE) 

anova(dt.mmse.cv.step, dt.mmse.cv.lm)

summary(gvlma(dt.mmse.cv.step))

### RMSE CV ####
rmse.cv.lm <- lm(rmse.V_cv ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
              data = average.outcomes)
rmse.cv.step <- MASS::stepAIC(rmse.cv.lm, trace = FALSE) 

anova(rmse.cv.step, rmse.cv.lm)

summary(gvlma(rmse.cv.lm))
plot(gvlma(rmse.cv.lm), onepage= FALSE)

# Drop outlier
rmse.cv.lm <- lm(rmse.V_cv ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
              data = average.outcomes[c(-34),])
rmse.cv.step <- MASS::stepAIC(rmse.cv.lm, trace = FALSE) 

anova(rmse.cv.step, rmse.cv.lm)

summary(gvlma(rmse.cv.step))

### Average Power from 0-4 Hz CV ####
avp04.cv.lm <- lm(avp04_cv ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
               data = average.outcomes)
avp04.cv.step <- MASS::stepAIC(avp04.cv.lm, trace = FALSE) 

anova(avp04.cv.step, avp04.cv.lm)

summary(gvlma(avp04.cv.step))


### Average Power from 4-8 Hz CV ####
avp48.cv.lm <- lm(avp48_cv ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
               data = average.outcomes)
avp48.cv.step <- MASS::stepAIC(avp48.cv.lm, trace = FALSE) 

anova(avp48.cv.step, avp48.cv.lm)

summary(gvlma(avp48.cv.step))


### Average Power from 8-12 Hz CV ####
avp812.cv.lm <- lm(avp812_cv ~ (diagnosed.number + suspected.number + 
                                 LOC + amnesia)^2,
                data = average.outcomes)
avp812.cv.step <- MASS::stepAIC(avp812.cv.lm, trace = FALSE) 

anova(avp812.cv.step, avp812.cv.lm)

summary(gvlma(avp812.cv.step))



