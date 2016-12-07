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

rm(list = c("bad.trials", "trial.summary"))


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

complexity <- bind_rows(raw.complexity, detrended.complexity) %>%
  spread(type, complexity)

#rm(list = c("raw.complexity", "detrended.complexity"))

#### Composite data frame ####
participants <- semi_join(participants, valid.trials) %>%
  select(id, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent)

trial.outcomes <- left_join(complexity, dfa.data) %>%
  left_join(avp.data) %>%
  left_join(participants) %>%
  select(id, trial, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent, 
         raw.complexity, detrended.complexity, avp04, avp48, avp812, alpha)

#### Summarise individual outcomes ####
# Not ideal coding but functional
average.outcomes <- trial.outcomes %>%
  group_by(id, block, order, gender, hand, gamer, prior.concussion, LOC, 
           amnesia, sx.current, age, height, weight, diagnosed.number, 
           suspected.number, concussion.number) %>%
  summarise_each(funs(mean, cv, n()), raw.complexity, detrended.complexity, avp04, avp48, avp812,
                 alpha) %>%
  ungroup() %>%
  select(id:raw.complexity_n) %>%
  rename(trials = raw.complexity_n) %>%
  mutate(dx.status = ifelse(diagnosed.number > 0 & suspected.number > 0, "Both",
                            ifelse(diagnosed.number > 0 & suspected.number == 0, "Dx Only",
                                   ifelse(diagnosed.number == 0 & suspected.number > 0, "Susp Only", "Uninjured"))))

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
desc = c("age", "diagnosed.number", "suspected.number", "concussion.number")

for (i in 1:length(outcomes)){
  cols <- c(desc, outcomes[i])
  
  scat <- my_scatterplot(cols = cols, title = titles[i], data = average.outcomes)
  
  print(scat)
}

rm(list = c("cols", "desc", "outcomes", "titles", "scat", "title"))


#### Logistic Regression ####
# Fit a logistic regression to predict concussion status based on complexity and
# DFA. This will be a 10-fold cross-validated model using the modelr and purrr
# packages.


require(modelr)
set.seed(2000000)

concussion.crossval <- select(average.outcomes, id, gender, prior.concussion, raw.complexity_mean:alpha_cv) %>%
  mutate(prior.concussion = ifelse(prior.concussion == "Yes", 1, 0)) %>%
  crossv_kfold(10)

## Singular mean models
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(rcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ raw.complexity_mean,
                                           family = binomial,
                                           data = .)),
         dtcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ detrended.complexity_mean,
                                            family = binomial,
                                            data = .)),
         avp04.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp04_mean,
                                        family = binomial,
                                        data = .)),
         avp48.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp48_mean,
                                        family = binomial,
                                        data = .)),
         avp812.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp812_mean,
                                        family = binomial,
                                        data = .)),
         dfa.model = map(train, ~ glm(as.factor(prior.concussion) ~ alpha_mean,
                                        family = binomial,
                                        data = .)),
         complete.model = map(train, ~ glm(as.factor(as.factor(prior.concussion)) ~ detrended.complexity_mean + 
                                             avp04_mean + avp48_mean + avp812_mean + alpha_mean,
                                           family = binomial,
                                           data = .)))

concussion.predictions <- concussion.models %>%
  mutate(rcomplex.pred = map2(rcomplex.model, test, type = "response", predict),
         dtcomplex.pred = map2(dtcomplex.model, test, type = "response", predict),
         avp04.pred = map2(avp04.model, test, type = "response", predict),
         avp48.pred = map2(avp48.model, test, type = "response", predict),
         avp812.pred = map2(avp812.model, test, type = "response", predict),
         alpha.pred = map2(dfa.model, test, type = "response", predict),
         complete.pred = map2(complete.model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, actual, rcomplex.pred:complete.pred) %>%
  unnest() %>%
  mutate_at(vars(ends_with("pred")), funs(round_pred))

## Singular cv models
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(rcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ raw.complexity_cv,
                                           family = binomial,
                                           data = .)),
         dtcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ detrended.complexity_cv,
                                            family = binomial,
                                            data = .)),
         avp04.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp04_cv,
                                        family = binomial,
                                        data = .)),
         avp48.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp48_cv,
                                        family = binomial,
                                        data = .)),
         avp812.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp812_cv,
                                         family = binomial,
                                         data = .)),
         dfa.model = map(train, ~ glm(as.factor(prior.concussion) ~ alpha_cv,
                                      family = binomial,
                                      data = .)),
         complete.model = map(train, ~ glm(as.factor(as.factor(prior.concussion)) ~ detrended.complexity_cv + 
                                             avp04_cv + avp48_cv + avp812_cv + alpha_cv,
                                           family = binomial,
                                           data = .)))

concussion.predictions <- concussion.models %>%
  mutate(rcomplex.pred = map2(rcomplex.model, test, type = "response", predict),
         dtcomplex.pred = map2(dtcomplex.model, test, type = "response", predict),
         avp04.pred = map2(avp04.model, test, type = "response", predict),
         avp48.pred = map2(avp48.model, test, type = "response", predict),
         avp812.pred = map2(avp812.model, test, type = "response", predict),
         alpha.pred = map2(dfa.model, test, type = "response", predict),
         complete.pred = map2(complete.model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, actual, rcomplex.pred:complete.pred) %>%
  unnest() %>%
  mutate_at(vars(ends_with("pred")), funs(round_pred))





### Fit model to only those with a history of dx concussion ####
set.seed(2000000)

concussion.crossval <- select(average.outcomes, id, gender, prior.concussion, raw.complexity_mean:alpha_cv, dx.status) %>%
  filter(dx.status != "Susp Only") %>%
  crossv_kfold(10)



## Singular mean models
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(rcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ raw.complexity_mean,
                                           family = binomial,
                                           data = .)),
         dtcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ detrended.complexity_mean,
                                            family = binomial,
                                            data = .)),
         avp04.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp04_mean,
                                        family = binomial,
                                        data = .)),
         avp48.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp48_mean,
                                        family = binomial,
                                        data = .)),
         avp812.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp812_mean,
                                         family = binomial,
                                         data = .)),
         dfa.model = map(train, ~ glm(as.factor(prior.concussion) ~ alpha_mean,
                                      family = binomial,
                                      data = .)),
         complete.model = map(train, ~ glm(as.factor(as.factor(prior.concussion)) ~ detrended.complexity_mean + 
                                             avp04_mean + avp48_mean + avp812_mean + alpha_mean,
                                           family = binomial,
                                           data = .)))

concussion.predictions <- concussion.models %>%
  mutate(rcomplex.pred = map2(rcomplex.model, test, type = "response", predict),
         dtcomplex.pred = map2(dtcomplex.model, test, type = "response", predict),
         avp04.pred = map2(avp04.model, test, type = "response", predict),
         avp48.pred = map2(avp48.model, test, type = "response", predict),
         avp812.pred = map2(avp812.model, test, type = "response", predict),
         alpha.pred = map2(dfa.model, test, type = "response", predict),
         complete.pred = map2(complete.model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, actual, rcomplex.pred:complete.pred) %>%
  unnest() %>%
  mutate_at(vars(ends_with("pred")), funs(round_pred)) %>%
  mutate_at(vars(ends_with("pred")), funs(as.factor))
  summarise_each(vars(actual, ends_with("pred")), funs(predictive_ability))

## Singular cv models
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(rcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ raw.complexity_cv,
                                           family = binomial,
                                           data = .)),
         dtcomplex.model = map(train, ~ glm(as.factor(prior.concussion) ~ detrended.complexity_cv,
                                            family = binomial,
                                            data = .)),
         avp04.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp04_cv,
                                        family = binomial,
                                        data = .)),
         avp48.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp48_cv,
                                        family = binomial,
                                        data = .)),
         avp812.model = map(train, ~ glm(as.factor(prior.concussion) ~ avp812_cv,
                                         family = binomial,
                                         data = .)),
         dfa.model = map(train, ~ glm(as.factor(prior.concussion) ~ alpha_cv,
                                      family = binomial,
                                      data = .)),
         complete.model = map(train, ~ glm(as.factor(as.factor(prior.concussion)) ~ detrended.complexity_cv + 
                                             avp04_cv + avp48_cv + avp812_cv + alpha_cv,
                                           family = binomial,
                                           data = .)))

concussion.predictions <- concussion.models %>%
  mutate(rcomplex.pred = map2(rcomplex.model, test, type = "response", predict),
         dtcomplex.pred = map2(dtcomplex.model, test, type = "response", predict),
         avp04.pred = map2(avp04.model, test, type = "response", predict),
         avp48.pred = map2(avp48.model, test, type = "response", predict),
         avp812.pred = map2(avp812.model, test, type = "response", predict),
         alpha.pred = map2(dfa.model, test, type = "response", predict),
         complete.pred = map2(complete.model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, actual, rcomplex.pred:complete.pred) %>%
  unnest() %>%
  mutate_at(vars(ends_with("pred")), funs(round_pred))

## Fit model to only those with no history of dx concussion
concussion.crossval.nodx <- select(average.outcomes, id, prior.concussion, dx.status, raw.complexity_mean, raw.complexity_cv,
                                     detrended.complexity_mean, detrended.complexity_cv, alpha_mean, alpha_cv) %>%
  mutate(prior.concussion = ifelse(prior.concussion == "Yes", 1, 0)) %>%
  filter(dx.status != "Dx Only" & dx.status != "Both") %>%
  crossv_kfold(10)

concussion.models <- concussion.crossval.nodx %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean + detrended.complexity_cv + 
                                    alpha_mean + alpha_cv,
                                  family = binomial(),
                                  data = .))) 

concussion.modelfits <- concussion.models %>%
  mutate(rmse = map2_dbl(model, test, rmse),
         rsquare = map2_dbl(model, test, rsquare)) %>%
  select(.id, rmse, rsquare)

concussion.predictions <- concussion.models %>%
  mutate(predictions = map2(model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, predictions, actual) %>%
  unnest()

table(concussion.predictions$actual, round(concussion.predictions$predictions + 0.000001))

#### Include gender in regression ####
## Gender, Raw complexity, and DFA alpha
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ glm(prior.concussion ~ raw.complexity_mean + alpha_mean + gender,
                                  family = binomial(),
                                  data = .))) 

concussion.modelfits <- concussion.models %>%
  mutate(rmse = map2_dbl(model, test, rmse),
         rsquare = map2_dbl(model, test, rsquare)) %>%
  select(.id, rmse, rsquare)
print(concussion.modelfits)

concussion.predictions <- concussion.models %>%
  mutate(predictions = map2(model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, predictions, actual) %>%
  unnest()

table(concussion.predictions$actual, round(concussion.predictions$predictions + 0.000001))

## Gender, Detrended complexity, and DFA alpha
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean + alpha_mean + gender,
                                  family = binomial(),
                                  data = .))) 

concussion.modelfits <- concussion.models %>%
  mutate(rmse = map2_dbl(model, test, rmse),
         rsquare = map2_dbl(model, test, rsquare)) %>%
  select(.id, rmse, rsquare)

concussion.predictions <- concussion.models %>%
  mutate(predictions = map2(model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, predictions, actual) %>%
  unnest()

table(concussion.predictions$actual, round(concussion.predictions$predictions + 0.000001))

## Gender, Raw complexity, raw complexity CV, DFA alpha
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ glm(prior.concussion ~ raw.complexity_mean + raw.complexity_cv + alpha_mean + gender,
                                  family = binomial(),
                                  data = .))) 

concussion.modelfits <- concussion.models %>%
  mutate(rmse = map2_dbl(model, test, rmse),
         rsquare = map2_dbl(model, test, rsquare)) %>%
  select(.id, rmse, rsquare)

concussion.predictions <- concussion.models %>%
  mutate(predictions = map2(model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, predictions, actual) %>%
  unnest()

table(concussion.predictions$actual, round(concussion.predictions$predictions + 0.000001))

## Gender, Detrended complexity mean, CV, and DFA alpha
concussion.models <- concussion.crossval %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean + detrended.complexity_cv + 
                                    alpha_mean + alpha_cv + gender,
                                  family = binomial(),
                                  data = .))) 

concussion.modelfits <- concussion.models %>%
  mutate(rmse = map2_dbl(model, test, rmse),
         rsquare = map2_dbl(model, test, rsquare)) %>%
  select(.id, rmse, rsquare)

concussion.predictions <- concussion.models %>%
  mutate(predictions = map2(model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, predictions, actual) %>%
  unnest()

table(concussion.predictions$actual, round(concussion.predictions$predictions + 0.000001))

## Fit model to only those with a history of dx concussion
concussion.crossval.dxonly <- select(average.outcomes, id, gender, prior.concussion, dx.status, raw.complexity_mean, raw.complexity_cv,
                                     detrended.complexity_mean, detrended.complexity_cv, alpha_mean, alpha_cv) %>%
  mutate(prior.concussion = ifelse(prior.concussion == "Yes", 1, 0)) %>%
  filter(dx.status != "Susp Only") %>%
  crossv_kfold(10)

concussion.models <- concussion.crossval.dxonly %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean + detrended.complexity_cv + 
                                    alpha_mean + alpha_cv + gender,
                                  family = binomial(),
                                  data = .))) 

concussion.modelfits <- concussion.models %>%
  mutate(rmse = map2_dbl(model, test, rmse),
         rsquare = map2_dbl(model, test, rsquare)) %>%
  select(.id, rmse, rsquare)

concussion.predictions <- concussion.models %>%
  mutate(predictions = map2(model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, predictions, actual) %>%
  unnest()

table(concussion.predictions$actual, round(concussion.predictions$predictions + 0.000001))

## Fit model to only those with no history of dx concussion
concussion.crossval.nodx <- select(average.outcomes, id, gender, prior.concussion, dx.status, raw.complexity_mean, raw.complexity_cv,
                                   detrended.complexity_mean, detrended.complexity_cv, alpha_mean, alpha_cv) %>%
  mutate(prior.concussion = ifelse(prior.concussion == "Yes", 1, 0)) %>%
  filter(dx.status != "Dx Only" & dx.status != "Both") %>%
  crossv_kfold(10)

concussion.models <- concussion.crossval.nodx %>%
  mutate(train = map(train, as_tibble)) %>%
  mutate(model = map(train, ~ glm(prior.concussion ~ detrended.complexity_mean + detrended.complexity_cv + 
                                    alpha_mean + alpha_cv + gender,
                                  family = binomial(),
                                  data = .))) 

concussion.modelfits <- concussion.models %>%
  mutate(rmse = map2_dbl(model, test, rmse),
         rsquare = map2_dbl(model, test, rsquare)) %>%
  select(.id, rmse, rsquare)

concussion.predictions <- concussion.models %>%
  mutate(predictions = map2(model, test, type = "response", predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, predictions, actual) %>%
  unnest()

table(concussion.predictions$actual, round(concussion.predictions$predictions + 0.000001))