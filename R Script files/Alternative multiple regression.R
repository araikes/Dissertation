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

#---------------------- Alternative multiple regression models ----------------- ####
# Alternative #1: ----
# Reduce collinearity and confusing results by reducing outcomes to total number
# of concussions with and without LOC, with and without retrograde amnesia, and
# with/without anterograde amnesia

average.outcomes <- average.outcomes %>%
  mutate(total.without.loc = total.concussions - total.loc,
         total.without.retrograde = total.concussions - total.retrograde,
         total.without.anterograde = total.concussions - total.anterograde)

dt.mmse.alt1 <- lm(detrended.complexity_mean ~ diagnosed.number + suspected.number +
                     total.loc + total.retrograde + total.anterograde,
                   data = average.outcomes[average.outcomes$id != 918,])

dt.mmse.alt1.step <- MASS::stepAIC(dt.mmse.alt1, trace = FALSE)

summary(dt.mmse.alt1.step)
summary(gvlma(dt.mmse.alt1))
plot(gvlma(dt.mmse.step))
dt.mmse.alt1.step.del <- deletion.gvlma(gvlma(dt.mmse.alt1.step))
display.delstats(dt.mmse.step.del$DeltaStat4, dt.mmse.step.del$Stat4pvalue)

dt.mmse.alt1 <- lm(detrended.complexity_mean ~ diagnosed.number + suspected.number +
                     total.loc + total.retrograde + total.anterograde,
                   data = average.outcomes[average.outcomes$id != 918,])

dt.mmse.alt1.step <- MASS::stepAIC(dt.mmse.alt1, trace = FALSE)

summary(dt.mmse.alt1.step)
summary(gvlma(dt.mmse.alt1))
plot(gvlma(dt.mmse.step))

# Alternative #2: -----
dt.mmse.alt2 <- lm(detrended.complexity_mean ~ gender + tfi.nom + total.concussions + total.loc + total.retrograde +
                     total.anterograde + total.without.loc + total.without.retrograde + total.without.anterograde,
                   data = average.outcomes)

dt.mmse.alt2.step <- MASS::stepAIC(dt.mmse.alt2, trace = FALSE)

summary(dt.mmse.alt2)
summary(dt.mmse.alt2.step)
summary(gvlma(dt.mmse.alt2.step))

# Alternative #3: ----
average.outcomes.dxonly <- filter(average.outcomes, 
                                  diagnosed.number > 0 | prior.concussion == "No")

dt.mmse.alt3 <- lm(detrended.complexity_mean ~ gender + diagnosed.number + suspected.number +
                     total.loc + total.retrograde + total.anterograde,
                   data = average.outcomes.dxonly)

dt.mmse.alt3.step <- MASS::stepAIC(dt.mmse.alt3, trace = FALSE)

summary(dt.mmse.alt3)
summary(dt.mmse.alt3.step)
summary(gvlma(dt.mmse.alt3.step))
plot(gvlma(dt.mmse.alt3.step))
dt.mmse.alt3.step.del <- deletion.gvlma(gvlma(dt.mmse.alt3.step))
display.delstats(dt.mmse.step.del$DeltaStat4, dt.mmse.step.del$Stat4pvalue)

dt.mmse.alt3 <- lm(detrended.complexity_mean ~ gender + diagnosed.number + suspected.number +
                     total.loc + total.retrograde + total.anterograde,
                   data = average.outcomes.dxonly[average.outcomes.dxonly$id != 918,])

dt.mmse.alt3.step <- MASS::stepAIC(dt.mmse.alt3, trace = FALSE)

summary(dt.mmse.alt3)
summary(dt.mmse.alt3.step)
summary(gvlma(dt.mmse.alt3.step))

