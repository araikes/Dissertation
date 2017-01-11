library(tidyverse)

dx.data <- filter(average.outcomes, dx.status != "Susp.Only")

dx.crossval <- select(dx.data, id, gender, prior.concussion, rmse.V_mean:alpha_cv) %>%
  mutate(prior.concussion = ifelse(prior.concussion == "Yes", 1, 0)) %>%
  crossv_kfold(10)

dx.actual <-  mutate(dx.crossval, 
                  id = map(map(test, as.data.frame), "id", select),
                  actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(actual) %>%
  unnest() %>%
  collect %>% .[["actual"]]

### Gender mean models ----
dx.mean.models <- dx.crossval %>%
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
                                        data = .)))

### Gender mean model predictions ----
dx.mean.probs <- dx.mean.models %>%
  mutate(gender.pred = map2(gender.model, test, type = "response", predict),
         rmse.pred = map2(rmse.model, test, type = "response", predict),
         dtcomplex.pred = map2(dtcomplex.model, test, type = "response", predict),
         sampen.pred = map2(sampen.model, test, type = "response", predict),
         avp04.pred = map2(avp04.model, test, type = "response", predict),
         avp48.pred = map2(avp48.model, test, type = "response", predict),
         avp812.pred = map2(avp812.model, test, type = "response", predict),
         alpha.pred = map2(dfa.model, test, type = "response", predict),
         combo.pred = map2(combo.model, test, type = 'response', predict),
         id = map(map(test, as.data.frame), "id", select),
         actual = map(map(test, as.data.frame), "prior.concussion", select)) %>%
  select(id, actual, gender.pred:combo.pred) %>%
  unnest()

dx.mean.thresholds <- dx.mean.probs %>%
  summarise_at(vars(ends_with("pred")),
               function (x) coords(roc(dx.actual, x, direction = "<"), "best", ret = "threshold")[1])

dx.mean.predictions <- dx.mean.probs %>%
  mutate(gender.pred = ifelse(gender.pred > dx.mean.thresholds$gender.pred, 1, 0),
         rmse.pred = ifelse(rmse.pred > dx.mean.thresholds$rmse.pred, 1,0),
         dtcomplex.pred = ifelse(dtcomplex.pred > dx.mean.thresholds$dtcomplex.pred, 1,0),
         sampen.pred = ifelse(sampen.pred > dx.mean.thresholds$sampen.pred, 1,0),
         avp04.pred = ifelse(avp04.pred > dx.mean.thresholds$avp04.pred, 1,0),
         avp48.pred = ifelse(avp48.pred > dx.mean.thresholds$avp48.pred, 1,0),
         avp812.pred = ifelse(avp812.pred > dx.mean.thresholds$avp812.pred, 1,0),
         alpha.pred = ifelse(alpha.pred > dx.mean.thresholds$alpha.pred, 1,0),
         combo.pred = ifelse(combo.pred > dx.mean.thresholds$combo.pred, 1, 0))

### Gender mean model performance ----
dx.mean.perf <- dx.mean.predictions %>%
  select(-id, -actual) %>%
  summarise_at(vars(ends_with("pred")), funs(sens, specif, ppv, npv, acc, auc, auc.p)) %>%
  gather(outcome, value) %>%
  separate(outcome, c("Outcome", "Measure"), "_") %>%
  spread(Measure, value) %>%
  arrange(auc) %>%
  select(Outcome, auc, auc.p, sens, specif, everything())





