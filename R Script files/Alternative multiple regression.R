#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/30/2016
# Local Depends: Multiple Regression.R
# Local Files: ./Data Files/*.csv
#
# Description: Alternative multiple regression.R runs alternative multiple regression models for my
# dissertation. This file requires the data processing conducted in Multiple Regression.R.

#### Load libraries ####
require(tidyverse)
require(gvlma)
require(stargazer)

#---------------------- Alternative multiple regression models ----------------- ####
# Reduce collinearity and confusing results by reducing outcomes to total number
# of concussions with and without LOC, with and without retrograde amnesia, and
# with/without anterograde amnesia

average.outcomes <- average.outcomes %>%
  mutate(total.without.loc = total.concussions - total.loc,
         total.without.retrograde = total.concussions - total.retrograde,
         total.without.anterograde = total.concussions - total.anterograde,
         dx.norm = ifelse(prior.concussion == "Yes", diagnosed.number/total.concussions, 0),
         susp.norm = ifelse(prior.concussion == "Yes", suspected.number/total.concussions, 0),
         loc.norm = ifelse(prior.concussion == "Yes", total.loc/total.concussions, 0),
         retro.norm = ifelse(prior.concussion == "Yes", total.retrograde/total.concussions, 0),
         antero.norm = ifelse(prior.concussion == "Yes", total.anterograde/total.concussions, 0))

fit1 <- "~ (gender + diagnosed.number + suspected.number + LOC + amnesia)^3"
fit2 <- "~ (gender + total.concussions + LOC + amnesia)^3" 
fit3 <- "~ (gender + total.concussions + total.loc + total.retrograde + total.anterograde)^3"

### Detrended complexity ####
# Alternative #1: (R2 = 0.2408) ----
dt.mmse.alt1 <- lm(as.formula(paste("detrended.complexity_mean", fit1, sep = " ")),
                   data = average.outcomes)

dt.mmse.alt1.step <- MASS::stepAIC(dt.mmse.alt1, trace = FALSE)

summary(gvlma(dt.mmse.alt1.step))

# Alternative #2: (R2 = 0.25)-----
dt.mmse.alt2 <- lm(as.formula(paste("detrended.complexity_mean", fit2, sep = " ")),
                   data = average.outcomes)

dt.mmse.alt2.step <- MASS::stepAIC(dt.mmse.alt2, trace = FALSE)

summary(gvlma(dt.mmse.alt2.step))

# Alternative #3: (R2 = 0.2461)): ----
dt.mmse.alt3 <- lm(as.formula(paste("detrended.complexity_mean", fit3, sep = " ")),
                   data = average.outcomes)

dt.mmse.alt3.step <- MASS::stepAIC(dt.mmse.alt3, trace = FALSE)
summary(gvlma(dt.mmse.alt3.step))


### Detrended complexity cv ####
# Alternative #1: (R2 = 0.2844): ----
dt.mmse.cv.alt1 <- lm(as.formula(paste("detrended.complexity_cv", fit1, sep = " ")),
                   data = average.outcomes)

dt.mmse.cv.alt1.step <- MASS::stepAIC(dt.mmse.cv.alt1, trace = FALSE)

summary(gvlma(dt.mmse.cv.alt1.step))
plot(gvlma(dt.mmse.cv.alt1.step), onepage = FALSE)

# Log transformation
dt.mmse.cv.alt1 <- lm(as.formula(paste("log(detrended.complexity_cv)", fit1, sep = " ")),
                      data = average.outcomes)

dt.mmse.cv.alt1.step <- MASS::stepAIC(dt.mmse.cv.alt1, trace = FALSE)

summary(gvlma(dt.mmse.cv.alt1.step))

# Alternative #2: (R2 = 0.236)----
dt.mmse.cv.alt2 <- lm(as.formula(paste("detrended.complexity_cv", fit2, sep = " ")),
                      data = average.outcomes)

dt.mmse.cv.alt2.step <- MASS::stepAIC(dt.mmse.cv.alt2, trace = FALSE)

summary(gvlma(dt.mmse.cv.alt2.step))

# Log transformation
dt.mmse.cv.alt2 <- lm(as.formula(paste("log(detrended.complexity_cv)", fit2, sep = " ")),
                      data = average.outcomes)

dt.mmse.cv.alt2.step <- MASS::stepAIC(dt.mmse.cv.alt2, trace = FALSE)
summary(gvlma(dt.mmse.cv.alt2.step))

# Alternative #3: (R2 = 0.2489)----
dt.mmse.cv.alt3 <- lm(as.formula(paste("detrended.complexity_cv", fit3, sep = " ")),
                      data = average.outcomes)

dt.mmse.cv.alt3.step <- MASS::stepAIC(dt.mmse.cv.alt3, trace = FALSE)

summary(gvlma(dt.mmse.cv.alt3.step))

# Log transformation
dt.mmse.cv.alt3 <- lm(as.formula(paste("log(detrended.complexity_cv)", 
                                       fit3, sep = " ")),
                      data = average.outcomes)

dt.mmse.cv.alt3.step <- MASS::stepAIC(dt.mmse.cv.alt3, trace = FALSE)

summary(gvlma(dt.mmse.cv.alt3.step))


### RMSE ####
# Alternative #1: (R2 = 0.067) ----
rmse.alt1 <- lm(as.formula(paste("rmse.V_mean", fit1, sep = " ")),
                   data = average.outcomes)

rmse.alt1.step <- MASS::stepAIC(rmse.alt1, trace = FALSE)

summary(gvlma(rmse.alt1.step))
plot(gvlma(rmse.alt1.step), onepage = FALSE)

# Log transformation
rmse.alt1 <- lm(as.formula(paste("log(rmse.V_mean)", fit1, sep = " ")),
                data = average.outcomes)

rmse.alt1.step <- MASS::stepAIC(rmse.alt1, trace = FALSE)

summary(gvlma(rmse.alt1.step))
# Alternative #2: ----
rmse.alt2 <- lm(as.formula(paste("rmse.V_mean", fit2, sep = " ")),
                data = average.outcomes)

rmse.alt2.step <- MASS::stepAIC(rmse.alt2, verbose = FALSE)

summary(gvlma(rmse.alt2.step))

# Log transfomration
rmse.alt2 <- lm(as.formula(paste("log(rmse.V_mean)", fit2, sep = " ")),
                data = average.outcomes)

rmse.alt2.step <- MASS::stepAIC(rmse.alt2, trace = FALSE)

summary(gvlma(rmse.alt2.step))

# Alternative #3: (R2 = 0.026)----
rmse.alt3 <- lm(as.formula(paste("rmse.V_mean", fit3, sep = " ")),
                data = average.outcomes)

rmse.alt3.step <- MASS::stepAIC(rmse.alt3, trace = FALSE)

summary(gvlma(rmse.alt3.step))

# Log transform
rmse.alt3 <- lm(as.formula(paste("log(rmse.V_mean)", fit3, sep = " ")),
                data = average.outcomes)

rmse.alt3.step <- MASS::stepAIC(rmse.alt3, trace = FALSE)

summary(gvlma(rmse.alt3.step))


### RMSE CV ####
# Alternative #1: (R2 = 0.1744) ----
rmse.cv.alt1 <- lm(as.formula(paste("rmse.V_cv", fit1, sep = " ")),
                data = average.outcomes)

rmse.cv.alt1.step <- MASS::stepAIC(rmse.cv.alt1, trace = FALSE)

summary(gvlma(rmse.cv.alt1.step))
plot(gvlma(rmse.cv.alt1.step), onepage = FALSE)

# Log transformation
rmse.cv.alt1 <- lm(as.formula(paste("log(rmse.V_cv)", fit1, sep = " ")),
                data = average.outcomes)

rmse.cv.alt1.step <- MASS::stepAIC(rmse.cv.alt1, trace = FALSE)

summary(gvlma(rmse.cv.alt1.step))

# Alternative #2: (R2 = 0.1557)----
rmse.cv.alt2 <- lm(as.formula(paste("rmse.V_cv", fit2, sep = " ")),
                data = average.outcomes)

rmse.cv.alt2.step <- MASS::stepAIC(rmse.cv.alt2, trace = FALSE)

summary(gvlma(rmse.cv.alt2.step))
plot(gvlma(rmse.cv.alt2), onepage = FALSE)

# Log transfomration
rmse.cv.alt2 <- lm(as.formula(paste("log(rmse.V_cv)", fit2, sep = " ")),
                data = average.outcomes)

rmse.cv.alt2.step <- MASS::stepAIC(rmse.cv.alt2, trace = FALSE)

summary(gvlma(rmse.cv.alt2.step))
# Alternative #3: (R2 = 0.1557)----
rmse.cv.alt3 <- lm(as.formula(paste("rmse.V_cv", fit3, sep = " ")),
                data = average.outcomes)

rmse.cv.alt3.step <- MASS::stepAIC(rmse.cv.alt3, trace = FALSE)

summary(gvlma(rmse.cv.alt3.step))
plot(gvlma(rmse.cv.alt3.step), onepage = FALSE)

# Log transform
rmse.cv.alt3 <- lm(as.formula(paste("log(rmse.V_cv)", fit3, sep = " ")),
                data = average.outcomes)

rmse.cv.alt3.step <- MASS::stepAIC(rmse.cv.alt3, trace = FALSE)

summary(gvlma(rmse.cv.alt3.step))



### AvP04 ####
# Alternative #1: (R2 = 0.008) ----
avp04.alt1 <- lm(as.formula(paste("avp04_mean", fit1, sep = " ")),
                data = average.outcomes)

avp04.alt1.step <- MASS::stepAIC(avp04.alt1, trace = FALSE)

summary(gvlma(avp04.alt1.step))

# Alternative #2: (R2 = -0.033)----
avp04.alt2 <- lm(as.formula(paste("avp04_mean", fit2, sep = " ")),
                data = average.outcomes)

avp04.alt2.step <- MASS::stepAIC(avp04.alt2, trace = FALSE)

summary(gvlma(avp04.alt2.step))

# Alternative #3: (R2 = -0.017)----
avp04.alt3 <- lm(as.formula(paste("avp04_mean", fit3, sep = " ")),
                data = average.outcomes)

avp04.alt3.step <- MASS::stepAIC(avp04.alt3, trace = FALSE)

summary(gvlma(avp04.alt3.step))
# Write table ----


### AvP04 CV ####
# Alternative #1: (R2 = 0.097) ----
avp04.cv.alt1 <- lm(as.formula(paste("avp04_cv", fit1, sep = " ")),
                   data = average.outcomes)

avp04.cv.alt1.step <- MASS::stepAIC(avp04.cv.alt1, trace = FALSE)
summary(gvlma(avp04.cv.alt1.step))

# Alternative #2: (R2 = 0.024)----
avp04.cv.alt2 <- lm(as.formula(paste("avp04_cv", fit2, sep = " ")),
                   data = average.outcomes)

avp04.cv.alt2.step <- MASS::stepAIC(avp04.cv.alt2, trace = FALSE)

summary(gvlma(avp04.cv.alt2.step))
plot(gvlma(avp04.cv.alt2.step), onepage = FALSE)
# Alternative #3: ----
avp04.cv.alt3 <- lm(as.formula(paste("avp04_cv", fit3, sep = " ")),
                   data = average.outcomes)

avp04.cv.alt3.step <- MASS::stepAIC(avp04.cv.alt3, trace = FALSE)

summary(gvlma(avp04.cv.alt3.step))
plot(avp04.alt3.step)

# Log transform
avp04.cv.alt3 <- lm(as.formula(paste("log(avp04_cv)", fit3, sep = " ")),
                   data = average.outcomes)

avp04.cv.alt3.step <- MASS::stepAIC(avp04.cv.alt3, trace = FALSE)

summary(gvlma(avp04.cv.alt3.step))




### AvP48 ####
# Alternative #1: (R2 = 0.2002) ----
avp48.alt1 <- lm(as.formula(paste("avp48_mean", fit1, sep = " ")),
                 data = average.outcomes)

avp48.alt1.step <- MASS::stepAIC(avp48.alt1, trace = FALSE)

summary(gvlma(avp48.alt1.step))

# Log transform
avp48.alt1 <- lm(as.formula(paste("log(avp48_mean)", fit1, sep = " ")),
                 data = average.outcomes)

avp48.alt1.step <- MASS::stepAIC(avp48.alt1, trace = FALSE)
summary(gvlma(avp48.alt1.step))


# Alternative #2: (R2 = 0.1766)----
avp48.alt2 <- lm(as.formula(paste("avp48_mean", fit2, sep = " ")),
                 data = average.outcomes)

avp48.alt2.step <- MASS::stepAIC(avp48.alt2, trace = FALSE)

summary(gvlma(avp48.alt2.step))

# Log transform
avp48.alt2 <- lm(as.formula(paste("log(avp48_mean)", fit2, sep = " ")),
                 data = average.outcomes)

avp48.alt2.step <- MASS::stepAIC(avp48.alt2, trace = FALSE)

summary(gvlma(avp48.alt2.step))

# Alternative #3: (R2 = 0.1838)----
avp48.alt3 <- lm(as.formula(paste("avp48_mean", fit3, sep = " ")),
                 data = average.outcomes)

avp48.alt3.step <- MASS::stepAIC(avp48.alt3, trace = FALSE)

summary(gvlma(avp48.alt3.step))

# Log transform
avp48.alt3 <- lm(as.formula(paste("log(avp48_mean)", fit3, sep = " ")),
                 data = average.outcomes)

avp48.alt3.step <- MASS::stepAIC(avp48.alt3, trace = FALSE)

summary(gvlma(avp48.alt3.step))

### AvP48 CV ####
# Alternative #1: (R2 = 0.2664) ----
avp48.cv.alt1 <- lm(as.formula(paste("avp48_cv", fit1, sep = " ")),
                    data = average.outcomes)

avp48.cv.alt1.step <- MASS::stepAIC(avp48.cv.alt1, trace = FALSE)
summary(gvlma(avp48.cv.alt1.step))

# Log transformation
avp48.cv.alt1 <- lm(as.formula(paste("log(avp48_cv)", fit1, sep = " ")),
                    data = average.outcomes)

avp48.cv.alt1.step <- MASS::stepAIC(avp48.cv.alt1, trace = FALSE)
summary(gvlma(avp48.cv.alt1.step))

# Alternative #2: (R2 = 0.2840)----
avp48.cv.alt2 <- lm(as.formula(paste("avp48_cv", fit2, sep = " ")),
                    data = average.outcomes)

avp48.cv.alt2.step <- MASS::stepAIC(avp48.cv.alt2, trace = FALSE)

summary(gvlma(avp48.cv.alt2.step))

# Alternative #3: (R2 = 0.3172)----
avp48.cv.alt3 <- lm(as.formula(paste("avp48_cv", fit3, sep = " ")),
                    data = average.outcomes)

avp48.cv.alt3.step <- MASS::stepAIC(avp48.cv.alt3, trace = FALSE)

summary(gvlma(avp48.cv.alt3.step))


### AvP812 ####
# Alternative #1: (R2 = 0.4366) ----
avp812.alt1 <- lm(as.formula(paste("avp812_mean", fit1, sep = " ")),
                 data = average.outcomes)

avp812.alt1.step <- MASS::stepAIC(avp812.alt1, trace = FALSE)
summary(gvlma(avp812.alt1.step))

# Log transform
avp812.alt1 <- lm(as.formula(paste("log(avp812_mean)", fit1, sep = " ")),
                 data = average.outcomes)

avp812.alt1.step <- MASS::stepAIC(avp812.alt1, trace = FALSE)
summary(gvlma(avp812.alt1.step))


# Alternative #2: (R2 = 0.3411)----
avp812.alt2 <- lm(as.formula(paste("avp812_mean", fit2, sep = " ")),
                 data = average.outcomes)

avp812.alt2.step <- MASS::stepAIC(avp812.alt2, trace = FALSE)

summary(gvlma(avp812.alt2.step))

# Log transform
avp812.alt2 <- lm(as.formula(paste("log(avp812_mean)", fit2, sep = " ")),
                 data = average.outcomes)

avp812.alt2.step <- MASS::stepAIC(avp812.alt2, trace = FALSE)

summary(gvlma(avp812.alt2.step))

# Alternative #3: (R2 = 0.3445)----
avp812.alt3 <- lm(as.formula(paste("avp812_mean", fit3, sep = " ")),
                 data = average.outcomes)

avp812.alt3.step <- MASS::stepAIC(avp812.alt3, trace = FALSE)

summary(gvlma(avp812.alt3.step))

# Log transform
avp812.alt3 <- lm(as.formula(paste("log(avp812_mean)", fit3, sep = " ")),
                 data = average.outcomes)

avp812.alt3.step <- MASS::stepAIC(avp812.alt3, trace = FALSE)

summary(gvlma(avp812.alt3.step))



### AvP812 CV ####
# Alternative #1: (R2 = 0.1006) ----
avp812.cv.alt1 <- lm(as.formula(paste("avp812_cv", fit1, sep = " ")),
                    data = average.outcomes)

avp812.cv.alt1.step <- MASS::stepAIC(avp812.cv.alt1, trace = FALSE)
summary(gvlma(avp812.cv.alt1.step))

# Alternative #2: (R2 = 0.0851)----
avp812.cv.alt2 <- lm(as.formula(paste("avp812_cv", fit2, sep = " ")),
                    data = average.outcomes)

avp812.cv.alt2.step <- MASS::stepAIC(avp812.cv.alt2, trace = FALSE)

summary(gvlma(avp812.cv.alt2.step))

# Alternative #3: (R2 = 0.0887)----
avp812.cv.alt3 <- lm(as.formula(paste("avp812_cv", fit3, sep = " ")),
                    data = average.outcomes)

avp812.cv.alt3.step <- MASS::stepAIC(avp812.cv.alt3, trace = FALSE)

summary(gvlma(avp812.cv.alt3.step))


