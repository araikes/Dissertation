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
mmse.data <- read.csv("./Data Files/MMSE.csv",
                      header = TRUE, sep = ",")

mmse.detrended.data <- read.csv("./Data Files/Detrended MMSE.csv",
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
# 3. Participants are removed if the number of valid trials < 6.
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

#rm(list = c("mmse.data", "mmse.data.long"))

#### Get first fifty valid participants ####
first.fifty <- participants %>%
  semi_join(valid.trials) %>%
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

#rm(list = c("raw.complexity", "detrended.complexity"))

#### Composite data frame ####
avp.data <- semi_join(avp.data, first.fifty.valid)

trial.summary <- semi_join(trial.summary, first.fifty.valid)

participants <- semi_join(participants, first.fifty) %>%
  select(id, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent)

injury.time <- c("Uninjured", "Recent", "Remote")

trial.outcomes <- left_join(complexity, avp.data) %>%
  left_join(trial.summary) %>%
  left_join(participants) %>%
  select(id, trial, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent,
         center.N, rmse.V, raw.complexity, detrended.complexity,
         avp04, avp48, avp812) %>%
  mutate(tfi = age - max(diagnosed.recent, suspected.recent, na.rm = TRUE) - 1,
         tfi.nom = ifelse(tfi == Inf, "Uninjured", 
                                          ifelse(tfi > 1, "Remote", "Recent")),
         tfi.nom = factor(tfi.nom, levels = injury.time))

#### Summarise individual outcomes ####
# Not ideal coding but functional
average.outcomes <- trial.outcomes %>%
  group_by(id, block, order, gender, hand, gamer, prior.concussion, LOC, amnesia, sx.current,
           age, height, weight, diagnosed.number, suspected.number, concussion.number, tfi.nom, center.N) %>%
  summarise_each(funs(mean, cv, n()), rmse.V, raw.complexity, detrended.complexity, 
                 avp04, avp48, avp812) %>%
  ungroup() %>%
  select(id:rmse.V_n) %>%
  rename(trials = rmse.V_n)

#### Descriptive statistics ####
table1(participants,
       height, weight, age, gender, hand, gamer, LOC, amnesia,
       as.factor(diagnosed.number), as.factor(suspected.number), as.factor(concussion.number),
       splitby = ~prior.concussion,
       test = TRUE,
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
desc = c("age", "center.N", "diagnosed.number", "suspected.number", "concussion.number")

for (i in 1:length(outcomes)){
  cols <- c(desc, outcomes[i])
  
  scat <- my_scatterplot(cols = cols, title = titles[i], data = average.outcomes)
  
  print(scat)
}

#-------------Models as defined in my dissertation proposal--------------------####
#### Fit linear models for average values ####
# These will fit the linear models specifically defined in the dissertation proposal.
require(MASS)
require(gvlma)

average.outcomes <- rename(average.outcomes, total.concussions = concussion.number) %>%
  mutate(age.c = age - mean(age)) %>%
  arrange(order)

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
plot(gvlma(dt.mmse.step))
dt.mmse.step.del <- deletion.gvlma(gvlma(dt.mmse.step))
display.delstats(dt.mmse.step.del$DeltaStat4, dt.mmse.step.del$Stat4pvalue)

# Dropping outlier
dt.mmse.lm <- lm(detrended.complexity_mean ~ (diagnosed.number + suspected.number + 
                                                LOC + amnesia)^2,
                 data = average.outcomes[average.outcomes$id != 918,])
dt.mmse.step <- MASS::stepAIC(dt.mmse.lm, trace = FALSE) 

anova(dt.mmse.step, dt.mmse.lm)

summary(gvlma(dt.mmse.step))
plot(gvlma(dt.mmse.step))
dt.mmse.step.del <- deletion.gvlma(gvlma(dt.mmse.step))
display.delstats(dt.mmse.step.del$DeltaStat4, dt.mmse.step.del$Stat4pvalue)


summary(dt.mmse.step)

### Average RMSE ####
rmse.lm <- lm(rmse.V_mean ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
              data = average.outcomes)
rmse.step <- MASS::stepAIC(rmse.lm, trace = FALSE) 

anova(rmse.step, rmse.lm)

summary(gvlma(rmse.step))
plot(gvlma(rmse.step))

# Log transformation
rmse.lm <- lm(log(rmse.V_mean) ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
              data = average.outcomes)
rmse.step <- MASS::stepAIC(rmse.lm, trace = FALSE) 

anova(rmse.step, rmse.lm)

summary(gvlma(rmse.step))
plot(gvlma(rmse.step))

plot(rmse.step)
summary(rmse.step)

### Average Power from 0-4 Hz ####
avp04.lm <- lm(avp04_mean ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
              data = average.outcomes)
avp04.step <- MASS::stepAIC(avp04.lm, trace = FALSE) 

anova(avp04.step, avp04.lm)

summary(gvlma(avp04.step))
plot(gvlma(avp04.step))
summary(avp04.step)

### Average Power from 4-8 Hz ####
avp48.lm <- lm(avp48_mean ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
               data = average.outcomes)
avp48.step <- MASS::stepAIC(avp48.lm, trace = FALSE) 

anova(avp48.step, avp48.lm)

summary(gvlma(avp48.step))
summary(gvlma(avp48.lm))

# Log transformation
avp48.lm <- lm(log(avp48_mean) ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
               data = average.outcomes)
avp48.step <- MASS::stepAIC(avp48.lm, trace = FALSE) 

anova(avp48.step, avp48.lm)

summary(gvlma(avp48.step))
deletion.gvlma(gvlma(avp48.step))

# Drop one observation
avp48.lm <- lm(log(avp48_mean) ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
               data = average.outcomes[c(-39),])
avp48.step <- MASS::stepAIC(avp48.lm, trace = FALSE) 

anova(avp48.step, avp48.lm)

summary(gvlma(avp48.step))
plot(gvlma(avp48.step))
summary(avp48.step)

### Average Power from 8-12 Hz ####
avp812.lm <- lm(avp812_mean ~ (diagnosed.number + suspected.number + 
                                 LOC + amnesia)^2,
               data = average.outcomes)
avp812.step <- MASS::stepAIC(avp812.lm, trace = FALSE) 

anova(avp812.step, avp812.lm)

summary(gvlma(avp812.step))
plot(gvlma(avp812.step))

### Log transform 
avp812.lm <- lm(log(avp812_mean) ~ (diagnosed.number + suspected.number + 
                                 LOC + amnesia)^2,
                data = average.outcomes)
avp812.step <- MASS::stepAIC(avp812.lm, trace = FALSE) 

anova(avp812.step, avp812.lm)

summary(gvlma(avp812.step))
plot(gvlma(avp812.step))
summary(avp812.step)

#### Calculate partial R2 for average models ####
require(relaimpo)
calc.relimp(mmse.step, type = c("lmg"), rela = FALSE)
calc.relimp(dt.mmse.step, type = c("lmg"), rela = FALSE)
calc.relimp(rmse.step, type = c("lmg"), rela = FALSE)
calc.relimp(avp04.step, type = c("lmg"), rela = FALSE)
calc.relimp(avp48.step, type = c("lmg"), rela = FALSE)
calc.relimp(avp812.step, type = c("lmg"), rela = FALSE)



#----------------------------------------------------------------------####
#### Fit linear models for average the coefficient of variation ####

### Raw complexity CV ####
mmse.cv.lm <- lm(raw.complexity_cv ~ (diagnosed.number + suspected.number + 
                                       LOC + amnesia)^2,
              data = average.outcomes)
mmse.cv.step <- MASS::stepAIC(mmse.cv.lm, trace = FALSE) 

anova(mmse.cv.step, mmse.cv.lm)

summary(gvlma(mmse.cv.step))
plot(gvlma(mmse.cv.step))

summary(mmse.cv.step)

### Detrended complexity CV ####
dt.mmse.cv.lm <- lm(detrended.complexity_cv ~ (diagnosed.number + suspected.number + 
                                                LOC + amnesia)^2,
                 data = average.outcomes)
dt.mmse.cv.step <- MASS::stepAIC(dt.mmse.cv.lm, trace = FALSE) 

anova(dt.mmse.cv.step, dt.mmse.cv.lm)

summary(gvlma(dt.mmse.cv.step))
plot(gvlma(dt.mmse.cv.step))

# Log transformation
dt.mmse.cv.lm <- lm(log(detrended.complexity_cv) ~ (diagnosed.number + suspected.number + 
                                                 LOC + amnesia)^2,
                    data = average.outcomes)
dt.mmse.cv.step <- MASS::stepAIC(dt.mmse.cv.lm, trace = FALSE) 

anova(dt.mmse.cv.step, dt.mmse.cv.lm)

summary(gvlma(dt.mmse.cv.step))
plot(gvlma(dt.mmse.cv.step))

summary(dt.mmse.cv.step)

### RMSE CV ####
rmse.cv.lm <- lm(rmse.V_cv ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
              data = average.outcomes)
rmse.cv.step <- MASS::stepAIC(rmse.cv.lm, trace = FALSE) 

anova(rmse.cv.step, rmse.cv.lm)

summary(gvlma(rmse.cv.step))
plot(gvlma(rmse.cv.step))

# Log transformation
rmse.cv.lm <- lm(log(rmse.V_cv) ~ (diagnosed.number + suspected.number + 
                                    LOC + amnesia)^2,
              data = average.outcomes)
rmse.cv.step <- MASS::stepAIC(rmse.cv.lm, trace = FALSE) 

anova(rmse.cv.step, rmse.cv.lm)

summary(gvlma(rmse.cv.step))
plot(gvlma(rmse.cv.step))

plot(rmse.cv.step)
summary(rmse.cv.step)

### Average Power from 0-4 Hz CV ####
avp04.cv.lm <- lm(avp04_cv ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
               data = average.outcomes)
avp04.cv.step <- MASS::stepAIC(avp04.cv.lm, trace = FALSE) 

anova(avp04.cv.step, avp04.cv.lm)

summary(gvlma(avp04.cv.step))
plot(gvlma(avp04.cv.step))
summary(avp04.cv.step)

### Average Power from 4-8 Hz CV ####
avp48.cv.lm <- lm(avp48_cv ~ (diagnosed.number + suspected.number + 
                               LOC + amnesia)^2,
               data = average.outcomes)
avp48.cv.step <- MASS::stepAIC(avp48.cv.lm, trace = FALSE) 

anova(avp48.cv.step, avp48.cv.lm)

summary(gvlma(avp48.cv.step))
plot(gvlma(avp48.cv.step))
summary(avp48.cv.step)

### Average Power from 8-12 Hz CV ####
avp812.cv.lm <- lm(avp812_cv ~ (diagnosed.number + suspected.number + 
                                 LOC + amnesia)^2,
                data = average.outcomes)
avp812.cv.step <- MASS::stepAIC(avp812.cv.lm, trace = FALSE) 

anova(avp812.cv.step, avp812.cv.lm)

summary(gvlma(avp812.cv.step))
plot(gvlma(avp812.cv.step))

summary(avp812.cv.step)

#### Calculate partial R2 for CV models ####
calc.relimp(mmse.cv.step, type = c("lmg"), rela = FALSE)
calc.relimp(dt.mmse.cv.step, type = c("lmg"), rela = FALSE)
calc.relimp(rmse.cv.step, type = c("lmg"), rela = FALSE)
calc.relimp(avp04.cv.step, type = c("lmg"), rela = FALSE)
calc.relimp(avp48.cv.step, type = c("lmg"), rela = FALSE)
calc.relimp(avp812.cv.step, type = c("lmg"), rela = FALSE)

#--------------------Fit alternative models-----------------------------------------------------------####
average.outcomes.dxonly <- filter(average.outcomes, 
                                diagnosed.number > 0 | prior.concussion == "No")

average.outcomes.nodx <- filter(average.outcomes,
                                diagnosed.number == 0)

### Raw complexity ---------------------------------------------------------####
## Alternative 1 ----------------------------------------------------------------
mmse.lm.dxonly <- lm(raw.complexity_mean ~ (diagnosed.number + suspected.number + 
                                       LOC + amnesia)^2,
              data = average.outcomes.dxonly)
mmse.step.dxonly <- MASS::stepAIC(mmse.lm.dxonly, trace = FALSE) 

anova(mmse.step.dxonly, mmse.lm.dxonly)

summary(gvlma(mmse.lm.dxonly))
plot(gvlma(mmse.lm.dxonly))
summary(mmse.lm.dxonly)

## Alternative 2 -----------------------------------------------------------------
mmse.lm.dxonly.2 <- lm(raw.complexity_mean ~ diagnosed.number + suspected.number,
                   data = average.outcomes.dxonly)
mmse.step.dxonly.2 <- MASS::stepAIC(mmse.lm.dxonly.2, trace = FALSE) 

anova(mmse.step.dxonly.2, mmse.lm.dxonly.2)

summary(gvlma(mmse.lm.dxonly.2))
plot(gvlma(mmse.lm.dxonly.2))
summary(mmse.lm.dxonly.2)

## Alternative 3 ------------------------------------------------------------------
mmse.lm.nodx <- lm(raw.complexity_mean ~ (suspected.number + 
                                            LOC + amnesia)^2,
                   data = average.outcomes.nodx)
mmse.step.nodx <- MASS::stepAIC(mmse.lm.nodx, trace = FALSE) 

anova(mmse.step.nodx, mmse.lm.nodx)

summary(gvlma(mmse.lm.nodx))
plot(gvlma(mmse.lm.nodx))
summary(mmse.lm.nodx)



### Detrended complexity ---------------------------------------------------------####
## Alternative 1 ----------------------------------------------------------------
# Fit with only diagnosed concussions
dt.mmse.lm.dxonly <- lm(detrended.complexity_mean ~ (diagnosed.number + suspected.number + 
                                            LOC + amnesia)^2,
                   data = average.outcomes.dxonly)
dt.mmse.step.dxonly <- MASS::stepAIC(dt.mmse.lm.dxonly, trace = FALSE) 

anova(dt.mmse.step.dxonly, dt.mmse.lm.dxonly)

summary(gvlma(dt.mmse.step.dxonly))
plot(gvlma(dt.mmse.step.dxonly))
dt.mmse.step.dxonly.del <- deletion.gvlma(gvlma(dt.mmse.step.dxonly))
display.delstats(dt.mmse.step.dxonly.del$DeltaStat4, dt.mmse.step.dxonly.del$Stat4pvalue)

#-----------------------------------------------------------------------------------------#
dt.mmse.lm.dxonly <- lm(detrended.complexity_mean ~ (diagnosed.number + suspected.number + 
                                                     LOC + amnesia)^2,
                      data = average.outcomes.dxonly[c(average.outcomes.dxonly$id != 918 &
                                                     average.outcomes.dxonly$id != 533),])
dt.mmse.step.dxonly <- MASS::stepAIC(dt.mmse.lm.dxonly, trace = FALSE) 

anova(dt.mmse.step.dxonly, dt.mmse.lm.dxonly)

summary(gvlma(dt.mmse.step.dxonly))
plot(gvlma(dt.mmse.step.dxonly))
dt.mmse.step.dxonly.del <- deletion.gvlma(gvlma(dt.mmse.step.dxonly))
display.delstats(dt.mmse.step.dxonly.del$DeltaStat4, dt.mmse.step.dxonly.del$Stat4pvalue)

summary(dt.mmse.step.dxonly)

# Fit with suspected only
dt.mmse.lm.nodx <- lm(detrended.complexity_mean ~ (suspected.number + 
                                                       LOC + amnesia)^2,
                        data = average.outcomes.nodx)
dt.mmse.step.nodx <- MASS::stepAIC(dt.mmse.lm.nodx, trace = FALSE) 

anova(dt.mmse.step.nodx, dt.mmse.lm.nodx)

summary(gvlma(dt.mmse.lm.nodx))
plot(gvlma(dt.mmse.lm.nodx))
summary(dt.mmse.lm.nodx)
dt.mmse.step.nodx.del <- deletion.gvlma(gvlma(dt.mmse.lm.nodx))
display.delstats(dt.mmse.step.nodx.del$DeltaStat4, dt.mmse.step.nodx.del$Stat4pvalue)

## Alternative 2 ------------------------------------------------------------------------------------
# Gender
dt.mmse.lm.gender <- lm(detrended.complexity_mean ~ (diagnosed.number + suspected.number + gender +  
                                                       LOC + amnesia)^2,
                        data = average.outcomes)
dt.mmse.step.gender <- MASS::stepAIC(dt.mmse.lm.gender, trace = FALSE) 

anova(dt.mmse.step.gender, dt.mmse.lm.gender)

summary(gvlma(dt.mmse.step.gender))
plot(gvlma(dt.mmse.step.gender))

# Gender and TFI
dt.mmse.lm.genderTFI <- lm(detrended.complexity_mean ~ (diagnosed.number + suspected.number + gender + tfi.nom +  
                                                       LOC + amnesia)^2,
                        data = average.outcomes)
dt.mmse.step.genderTFI <- MASS::stepAIC(dt.mmse.lm.genderTFI, trace = FALSE) 

anova(dt.mmse.step.genderTFI, dt.mmse.lm.genderTFI)

summary(gvlma(dt.mmse.step.genderTFI))
plot(gvlma(dt.mmse.step.genderTFI))

## Fit CV ---------------------------------------------------------------------------
dt.mmse.cv.lm.dxonly <- lm(log(detrended.complexity_cv) ~ (diagnosed.number + suspected.number + 
                                                       LOC + amnesia)^2,
                        data = average.outcomes.dxonly)
dt.mmse.cv.step.dxonly <- MASS::stepAIC(dt.mmse.cv.lm.dxonly, trace = FALSE)

anova(dt.mmse.cv.step.dxonly, dt.mmse.cv.lm.dxonly)

summary(gvlma(dt.mmse.cv.step.dxonly))
plot(gvlma(dt.mmse.cv.step.dxonly))


dt.mmse.cv.lm.nodx <- lm(log(detrended.complexity_cv) ~ (diagnosed.number + suspected.number + 
                                                        LOC + amnesia)^2,
                           data = average.outcomes.nodx)
dt.mmse.cv.step.nodx <- MASS::stepAIC(dt.mmse.cv.lm.nodx, trace = FALSE)

anova(dt.mmse.cv.step.nodx, dt.mmse.cv.lm.nodx)

summary(gvlma(dt.mmse.cv.step.nodx))
plot(gvlma(dt.mmse.cv.step.nodx))


# Include gender
dt.mmse.cv.lm.gender <- lm(log(detrended.complexity_cv) ~ (diagnosed.number + suspected.number +gender +
                                                             LOC + amnesia)^2,
                           data = average.outcomes)
dt.mmse.cv.step.gender <- MASS::stepAIC(dt.mmse.cv.lm.gender, trace = FALSE)

anova(dt.mmse.cv.step.gender, dt.mmse.cv.lm.gender)

summary(gvlma(dt.mmse.cv.step.gender))
plot(gvlma(dt.mmse.cv.step.gender))

summary(dt.mmse.cv.step.gender)

