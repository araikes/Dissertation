#### File Description ####
# Author: Adam Raikes
# Initial Date: 12/2/2016
# Local Depends: 
# Local Files: ./Data Files/*.csv
#
# Description: Prediction.R is the logistic regression for determining
# predictive utility of complexity and DFA.

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
#require(verification)

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

rm(list = c("bad.trials"))


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

#### Plot MMSE curves ####
mmse.data.long <- mmse.data.long %>%
  semi_join(valid.trials) %>%  
  left_join(participants) %>%
  select(id, trial, scale, sampen, prior.concussion)

ggplot(data = mmse.data.long, aes(x = scale, y = sampen, 
                                  group = interaction(id, trial))) +
  facet_wrap(~prior.concussion) +
  geom_line()

detrended.mmse.long <- detrended.mmse.long %>%
  semi_join(valid.trials) %>%
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

sample.entropy <- detrended.mmse.long %>%
  group_by(id, trial) %>%
  filter(scale == 1) %>%
  mutate(type = "sample.entropy") %>%
  rename(complexity = sampen) %>%
  select(-scale, -prior.concussion)

complexity <- bind_rows(raw.complexity, detrended.complexity, sample.entropy) %>%
  spread(type, complexity)

rm(list = c("raw.complexity", "detrended.complexity", "sample.entropy"))

#### Composite data frame ####
participants <- semi_join(participants, valid.trials) %>%
  select(id, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent) %>%
  filter(sx.current != "Yes" | is.na(sx.current)) %>%
  filter(hand != "Left")

valid.participants <- select(participants, id)

trial.outcomes <- semi_join(complexity, valid.participants) %>%
  left_join(dfa.data) %>%
  left_join(avp.data) %>%
  left_join(participants) %>%
  left_join(trial.summary) %>%
  select(id, trial, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent, 
         rmse.V, detrended.complexity, sample.entropy, avp04, avp48, avp812, alpha)

#### Summarise individual outcomes ####
# Not ideal coding but functional
average.outcomes <- trial.outcomes %>%
  group_by(id, block, order, gender, hand, gamer, prior.concussion, LOC, 
           amnesia, sx.current, age, height, weight, diagnosed.number, 
           suspected.number, concussion.number) %>%
  summarise_each(funs(mean, cv, n()), rmse.V, detrended.complexity, sample.entropy,
                 avp04, avp48, avp812, alpha) %>%
  ungroup() %>%
  select(id:rmse.V_n) %>%
  rename(trials = rmse.V_n) %>%
  mutate(dx.status = ifelse(diagnosed.number > 0 & suspected.number > 0, "Both",
                            ifelse(diagnosed.number > 0 & suspected.number == 0, "Dx.Only",
                                   ifelse(diagnosed.number == 0 & suspected.number > 0, "Susp.Only", "Uninjured"))))
average.outcomes$dx.status <- factor(average.outcomes$dx.status)

#### Descriptive statistics ####
table1(participants,
       height, weight, age, gender, hand, gamer, LOC, amnesia,
       as.factor(diagnosed.number), as.factor(suspected.number), as.factor(concussion.number),
       splitby = ~prior.concussion,
       test = TRUE,
       output_type = "markdown")  

#### Boxplot outcomes ####
require(cowplot)
outcomes <- c("raw.complexity_mean", "detrended.complexity_mean", "alpha_mean")
titles <- c("Complexity", "Detrended Complexity",
            "DFA alpha")

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

rm(list = c("a", "b", "c", "d", "panel.plot"))

#### Scatterplots ####
# Scatterplot matrices ----
desc = c("age", "diagnosed.number", "suspected.number", "concussion.number")

for (i in 1:length(outcomes)){
  cols <- c(desc, outcomes[i])
  
  scat <- my_scatterplot(cols = cols, title = titles[i], data = average.outcomes)
  
  print(scat)
}

rm(list = c("cols", "desc", "outcomes", "titles", "scat", "title"))


# Two group scatterplot ----
ggplot(data = average.outcomes, aes(x = detrended.complexity_mean,
                                    y = alpha_mean,
                                    color = prior.concussion)) +
  geom_jitter(width = 0.2, size = 2)

# Two group scatterplot with gender ----
ggplot(data = average.outcomes, aes(x = detrended.complexity_mean,
                                    y = alpha_mean,
                                    color = prior.concussion,
                                    shape = gender)) +
  geom_jitter(width = 0.2, size = 2)
# Four group scatterplot with gender ----
ggplot(data = average.outcomes, aes(x = detrended.complexity_mean,
                                    y = alpha_mean,
                                    color = dx.status,
                                    shape = gender)) +
  geom_jitter(width = 0.2, size = 2)

ggplot(data = average.outcomes, aes(x = detrended.complexity_mean,
                                    y = dx.status,
                                    color = gender)) + 
  geom_point()

#### Logistic Regression ####
# Fit a logistic regression to predict concussion status based on complexity and
# DFA. This will be a 10-fold cross-validated model using the modelr and purrr
# packages.
rm(list = c("avp.data", "complexity", "detrended.mmse.long", "dfa.data",
          "trial.outcomes", "trial.summary", "valid.participants", "valid.trials"))
          
require(modelr)
require(pROC)
set.seed(2000000)

concussion.crossval <- select(average.outcomes, id, gender, prior.concussion, rmse.V_mean:alpha_cv) %>%
  mutate(prior.concussion = ifelse(prior.concussion == "Yes", 1, 0)) %>%
  crossv_kfold(10)

actual <-  mutate(concussion.crossval, 
                  id = map(map(test, as.data.frame), "id", select),
                  actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(actual) %>%
  unnest() %>%
  collect %>% .[["actual"]]

### Singular mean models ----
mean.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(rmse.model = map(train, ~ glm(prior.concussion ~ rmse.V_mean,
                                           family = binomial,
                                           data = .)),
         dtcomplex.model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean,
                                            family = binomial,
                                            data = .)),
         sampen.model = map(train, ~glm(prior.concussion ~ sample.entropy_mean,
                                        family = binomial,
                                        data = .)),
         avp04.model = map(train, ~ glm(prior.concussion ~ avp04_mean,
                                        family = binomial,
                                        data = .)),
         avp48.model = map(train, ~ glm(prior.concussion ~ avp48_mean,
                                        family = binomial,
                                        data = .)),
         avp812.model = map(train, ~ glm(prior.concussion ~ avp812_mean,
                                        family = binomial,
                                        data = .)),
         dfa.model = map(train, ~ glm(prior.concussion ~ alpha_mean,
                                        family = binomial,
                                        data = .)),
         combo.model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean +
                                          alpha_mean,
                                        family = binomial,
                                        data = .)),
         complete.model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean + sample.entropy_mean +
                                             avp04_mean + avp48_mean + avp812_mean + alpha_mean,
                                           family = binomial,
                                           data = .)))

### Singular mean model predictions ----
mean.probs <- mean.models %>%
  mutate(rmse.pred = map2(rmse.model, test, type = "response", predict),
         dtcomplex.pred = map2(dtcomplex.model, test, type = "response", predict),
         sampen.pred = map2(sampen.model, test, type = "response", predict),
         avp04.pred = map2(avp04.model, test, type = "response", predict),
         avp48.pred = map2(avp48.model, test, type = "response", predict),
         avp812.pred = map2(avp812.model, test, type = "response", predict),
         alpha.pred = map2(dfa.model, test, type = "response", predict),
         combo.pred = map2(combo.model, test, type = "response", predict),
         complete.pred = map2(complete.model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, actual, rmse.pred:complete.pred) %>%
  unnest()

mean.thresholds <- mean.probs %>%
  summarise_at(vars(ends_with("pred")),
               function (x) coords(roc(actual, x, direction = "<"), 
                                   "best", 
                                   best.method = "youden",
                                   ret = "threshold")[1])

mean.predictions <- mean.probs %>%
  mutate(rmse.pred = ifelse(rmse.pred > mean.thresholds$rmse.pred, 1,0),
         dtcomplex.pred = ifelse(dtcomplex.pred > mean.thresholds$dtcomplex.pred, 1,0),
         sampen.pred = ifelse(sampen.pred > mean.thresholds$sampen.pred, 1,0),
         avp04.pred = ifelse(avp04.pred > mean.thresholds$avp04.pred, 1,0),
         avp48.pred = ifelse(avp48.pred > mean.thresholds$avp48.pred, 1,0),
         avp812.pred = ifelse(avp812.pred > mean.thresholds$avp812.pred, 1,0),
         alpha.pred = ifelse(alpha.pred > mean.thresholds$alpha.pred, 1,0),
         combo.pred = ifelse(combo.pred > mean.thresholds$combo.pred, 1,0),
         complete.pred = ifelse(complete.pred > mean.thresholds$complete.pred, 1, 0))

### Singular mean model performance ----
mean.perf <- mean.predictions %>%
  select(-id, -actual) %>%
  summarise_at(vars(ends_with("pred")), funs(sens, specif, ppv, npv, acc, auc, auc.p)) %>%
  gather(outcome, value) %>%
  separate(outcome, c("Outcome", "Measure"), "_") %>%
  spread(Measure, value) %>%
  arrange(auc)



### Gender mean models ----
gender.mean.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(gender.model = map(train, ~ glm(prior.concussion ~ gender,
                                                  family = binomial,
                                                  data = .)),
         rmse.model = map(train, ~ glm(prior.concussion ~ gender * rmse.V_mean,
                                           family = binomial,
                                           data = .)),
         dtcomplex.model = map(train, ~ glm(prior.concussion ~ gender * detrended.complexity_mean,
                                            family = binomial,
                                            data = .)),
         sampen.model = map(train, ~glm(prior.concussion ~ gender * sample.entropy_mean,
                                        family = binomial,
                                        data = .)),
         avp04.model = map(train, ~ glm(prior.concussion ~ gender * avp04_mean,
                                        family = binomial,
                                        data = .)),
         avp48.model = map(train, ~ glm(prior.concussion ~ gender * avp48_mean,
                                        family = binomial,
                                        data = .)),
         avp812.model = map(train, ~ glm(prior.concussion ~ gender * avp812_mean,
                                         family = binomial,
                                         data = .)),
         dfa.model = map(train, ~ glm(prior.concussion ~ gender * alpha_mean,
                                      family = binomial,
                                      data = .)),
         combo.model = map(train, ~ glm(prior.concussion ~ gender + detrended.complexity_mean +
                                          alpha_mean + gender:detrended.complexity_mean +
                                          gender:alpha_mean,
                                      family = binomial,
                                      data = .)),
         complete.model = map(train, ~ glm(prior.concussion ~ gender + detrended.complexity_mean +
                                          alpha_mean + sample.entropy_mean + avp04_mean +
                                          avp48_mean + avp812_mean + 
                                          gender:detrended.complexity_mean +
                                          gender:alpha_mean + 
                                          gender:sample.entropy_mean + 
                                          gender:avp04_mean + 
                                          gender:avp48_mean +  
                                          gender:avp812_mean,
                                        family = binomial,
                                        data = .)))

### Gender mean model predictions ----
gender.mean.probs <- gender.mean.models %>%
  mutate(gender.pred = map2(gender.model, test, type = "response", predict),
         rmse.pred = map2(rmse.model, test, type = "response", predict),
         dtcomplex.pred = map2(dtcomplex.model, test, type = "response", predict),
         sampen.pred = map2(sampen.model, test, type = "response", predict),
         avp04.pred = map2(avp04.model, test, type = "response", predict),
         avp48.pred = map2(avp48.model, test, type = "response", predict),
         avp812.pred = map2(avp812.model, test, type = "response", predict),
         alpha.pred = map2(dfa.model, test, type = "response", predict),
         combo.pred = map2(combo.model, test, type = 'response', predict),
         complete.pred = map2(complete.model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, actual, gender.pred:complete.pred) %>%
  unnest()

gender.mean.thresholds <- gender.mean.probs %>%
  summarise_at(vars(ends_with("pred")),
               function (x) coords(roc(actual, x, direction = "<"), "best", ret = "threshold")[1])

gender.mean.predictions <- gender.mean.probs %>%
  mutate(gender.pred = ifelse(gender.pred > gender.mean.thresholds$gender.pred, 1, 0),
         rmse.pred = ifelse(rmse.pred > gender.mean.thresholds$rmse.pred, 1,0),
         dtcomplex.pred = ifelse(dtcomplex.pred > gender.mean.thresholds$dtcomplex.pred, 1,0),
         sampen.pred = ifelse(sampen.pred > gender.mean.thresholds$sampen.pred, 1,0),
         avp04.pred = ifelse(avp04.pred > gender.mean.thresholds$avp04.pred, 1,0),
         avp48.pred = ifelse(avp48.pred > gender.mean.thresholds$avp48.pred, 1,0),
         avp812.pred = ifelse(avp812.pred > gender.mean.thresholds$avp812.pred, 1,0),
         alpha.pred = ifelse(alpha.pred > gender.mean.thresholds$alpha.pred, 1,0),
         combo.pred = ifelse(combo.pred > gender.mean.thresholds$combo.pred, 1, 0),
         complete.pred = ifelse(complete.pred > gender.mean.thresholds$complete.pred, 1, 0))

### Gender mean model performance ----
gender.mean.perf <- gender.mean.predictions %>%
  select(-id, -actual) %>%
  summarise_at(vars(ends_with("pred")), funs(sens, specif, ppv, npv, acc, auc, auc.p)) %>%
  gather(outcome, value) %>%
  separate(outcome, c("Outcome", "Measure"), "_") %>%
  spread(Measure, value) %>%
  arrange(auc) %>%
  select(Outcome, auc, auc.p, sens, specif, everything())






### Ensemble model ----
# Simple majority vote ----
simple.ensemble <- gender.mean.predictions %>%
  select(-gender.pred) %>%
  group_by(id, actual) %>%
  gather(Outcome, Prediction, -id, -actual) %>%
  summarise(newpred = mean(Prediction)) %>%
  mutate(newpred = ifelse(newpred > 0.5, 1, 0))

caret::confusionMatrix(simple.ensemble$newpred, 
                       simple.ensemble$actual, 
                       positive = "1")


# Weighted majority ----
weighted.ensemble <- gender.mean.probs %>%
  select(-gender.pred) %>%
  group_by(id, actual) %>%
  gather(Outcome, Prediction, -id, -actual) %>%
  summarise(newpred = mean(Prediction))

weighted.thresh <- coords(roc(weighted.ensemble$actual,
                              weighted.ensemble$newpred,
                              direction = "<"),
                          "best",
                          best.method = "youden",
                          ret = "threshold")

weighted.ensemble <- mutate(weighted.ensemble,
                            newpred = ifelse(newpred > weighted.thresh, 1, 0))

caret::confusionMatrix(weighted.ensemble$newpred,
                       weighted.ensemble$actual,
                       positive = "1")

verification::roc.area(weighted.ensemble$actual, weighted.ensemble$newpred)



### For comparison with 80% sensitivity ----
## Get thresholds ----
sens.075.thresh <- gender.mean.probs %>%
  summarise_at(vars(ends_with("pred")),
               function (x) tail(as.vector(coords(roc(actual, x,
                                                      direction = "<",
                                                      partial.auc = c(1, 0.75),
                                                      partial.auc.focus = "sensitivity"),
                                                  x = "local maximas",
                                                  ret = "threshold")),  n = 1))

sens.08.thresh <- gender.mean.probs %>%
  summarise_at(vars(ends_with("pred")),
               function (x) tail(as.vector(coords(roc(actual, x,
                                                      direction = "<",
                                                      partial.auc = c(1, 0.8),
                                                      partial.auc.focus = "sensitivity"),
                                                  x = "local maximas",
                                                  ret = "threshold")),  n = 1))

sens.085.thresh <- gender.mean.probs %>%
  summarise_at(vars(ends_with("pred")),
               function (x) tail(as.vector(coords(roc(actual, x,
                                                      direction = "<",
                                                      partial.auc = c(1, 0.85),
                                                      partial.auc.focus = "sensitivity"),
                                                  x = "local maximas",
                                                  ret = "threshold")),  n = 1))
               

# Sens = 0.75 ----
sens.075.predictions <- gender.mean.probs %>%
  mutate(rmse.pred = ifelse(rmse.pred > sens.075.thresh$rmse.pred, 1,0),
         dtcomplex.pred = ifelse(dtcomplex.pred > sens.075.thresh$dtcomplex.pred, 1,0),
         sampen.pred = ifelse(sampen.pred > sens.075.thresh$sampen.pred, 1,0),
         avp04.pred = ifelse(avp04.pred > sens.075.thresh$avp04.pred, 1,0),
         avp48.pred = ifelse(avp48.pred > sens.075.thresh$avp48.pred, 1,0),
         avp812.pred = ifelse(avp812.pred > sens.075.thresh$avp812.pred, 1,0),
         alpha.pred = ifelse(alpha.pred > sens.075.thresh$alpha.pred, 1,0),
         gender.pred = ifelse(gender.pred > sens.075.thresh$gender.pred, 1,0),
         combo.pred = ifelse(combo.pred > sens.075.thresh$combo.pred, 1,0))

# Singular mean model performance
sens.075.perf <- sens.075.predictions %>%
  select(-id, -actual) %>%
  summarise_at(vars(ends_with("pred")), funs(specif, ppv, npv, acc)) %>%
  gather(outcome, value) %>%
  separate(outcome, c("Outcome", "Measure"), "_") %>%
  spread(Measure, value)
# Sens = 0.8 ----
sens.08.predictions <- gender.mean.probs %>%
  mutate(rmse.pred = ifelse(rmse.pred > sens.08.thresh$rmse.pred, 1,0),
         dtcomplex.pred = ifelse(dtcomplex.pred > sens.08.thresh$dtcomplex.pred, 1,0),
         sampen.pred = ifelse(sampen.pred > sens.08.thresh$sampen.pred, 1,0),
         avp04.pred = ifelse(avp04.pred > sens.08.thresh$avp04.pred, 1,0),
         avp48.pred = ifelse(avp48.pred > sens.08.thresh$avp48.pred, 1,0),
         avp812.pred = ifelse(avp812.pred > sens.08.thresh$avp812.pred, 1,0),
         alpha.pred = ifelse(alpha.pred > sens.08.thresh$alpha.pred, 1,0),
         gender.pred = ifelse(gender.pred > sens.08.thresh$gender.pred, 1,0),
         combo.pred = ifelse(combo.pred > sens.08.thresh$combo.pred, 1,0))

# Singular mean model performance
sens.08.perf <- sens.08.predictions %>%
  select(-id, -actual) %>%
  summarise_at(vars(ends_with("pred")), funs(specif, ppv, npv, acc)) %>%
  gather(outcome, value) %>%
  separate(outcome, c("Outcome", "Measure"), "_") %>%
  spread(Measure, value)
# Sens = 0.85 ----
sens.085.predictions <- gender.mean.probs %>%
  mutate(rmse.pred = ifelse(rmse.pred > sens.085.thresh$rmse.pred, 1,0),
         dtcomplex.pred = ifelse(dtcomplex.pred > sens.085.thresh$dtcomplex.pred, 1,0),
         sampen.pred = ifelse(sampen.pred > sens.085.thresh$sampen.pred, 1,0),
         avp04.pred = ifelse(avp04.pred > sens.085.thresh$avp04.pred, 1,0),
         avp48.pred = ifelse(avp48.pred > sens.085.thresh$avp48.pred, 1,0),
         avp812.pred = ifelse(avp812.pred > sens.085.thresh$avp812.pred, 1,0),
         alpha.pred = ifelse(alpha.pred > sens.085.thresh$alpha.pred, 1,0),
         gender.pred = ifelse(gender.pred > sens.085.thresh$gender.pred, 1,0),
         combo.pred = ifelse(combo.pred > sens.085.thresh$combo.pred, 1,0))

# Singular mean model performance
sens.085.perf <- sens.085.predictions %>%
  select(-id, -actual) %>%
  summarise_at(vars(ends_with("pred")), funs(specif, ppv, npv, acc)) %>%
  gather(outcome, value) %>%
  separate(outcome, c("Outcome", "Measure"), "_") %>%
  spread(Measure, value)

# Weighted ensemble sens = 0.8
weighted.ensemble2 <- gender.mean.probs %>%
  select(-gender.pred) %>%
  group_by(id, actual) %>%
  gather(Outcome, Prediction, -id, -actual) %>%
  summarise(newpred = mean(Prediction))

thresh <- coords(roc(weighted.ensemble2$actual,
                     weighted.ensemble2$newpred,
                     direction = "<"),
                 "best",
                 ret = "threshold")

weighted.ensemble2 <- mutate(weighted.ensemble2,
                            newpred = ifelse(newpred > thresh, 1, 0))

caret::confusionMatrix(weighted.ensemble$newpred,
                       weighted.ensemble$actual,
                       positive = "1")

verification::roc.area(weighted.ensemble$actual, weighted.ensemble$newpred)
