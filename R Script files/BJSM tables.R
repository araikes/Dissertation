library(broom)
library(heplots)
library(stargazer)

my_dir <- "C:/Users/adamraikes/Dropbox/Manuscript Submissions/Dissertation/Chapter 3/Participant mean values/Supplements"
stargazer(dt.mmse.alt1.step, rmse.alt1.step, avp04.alt1.step, avp48.alt1.step, avp812.alt1.step,
          title = "Retained model 1 variables after AIC step-down",
          dep.var.caption = "",
          dep.var.labels = c("Complexity", "RMSE", "0-4Hz AvP", "4-8Hz AvP", "8-12Hz AvP"),
          covariate.labels = c("Males", "Diagnosed concussions", "Suspected concussions", "LOC", "Amnesia",
                               "Males x Diagnosed concussions", "Males x Suspected concussions", "Males x LOC",
                               "Males x Amnesia", "Diagnosed concussions x Suspected concussions",
                               "Diagnosed concussions x LOC", "Diagnosed concussions x Amnesia",
                               "Suspected concussions x Amnesia", "LOC x Amnesia",
                               "Males x Diagnosed x Suspected concussions",
                               "Males x LOC x Diagnosed concussions",
                               "Suspected concussions x LOC"),
          omit = "Constant",
          coef = list(dt.mmse.alt1.step$coef, exp(rmse.alt1.step$coef), avp04.alt1.step$coef,
                      exp(avp48.alt1.step$coef), exp(avp812.alt1.step$coef)),
          ci.custom = list(confint(dt.mmse.alt1.step),
                           exp(confint(rmse.alt1.step)),
                           confint(avp04.alt1.step),
                           exp(confint(avp48.alt1.step)),
                           exp(confint(avp812.alt1.step))),
          t.auto = FALSE,
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "RMSE, 4-8Hz, and 8-12Hz average power required log transformation to meet model assumptions. Those coefficients and condience intervals have been re-exponentiated. LOC = loss of consciousness",
          notes.aling = "l",
          single.row = TRUE,
          ci = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          df = TRUE,
          type = "html",
          out = paste(my_dir, "/Table 1.html", sep = ""))

stargazer(dt.mmse.alt2.step, rmse.alt2.step, avp04.alt2.step, avp48.alt2.step, avp812.alt2.step,
          title = "Retained model 2 variables after AIC step-down",
          dep.var.caption = "",
          dep.var.labels = c("Complexity", "RMSE", "0-4Hz AvP", "4-8Hz AvP", "8-12Hz AvP"),
          covariate.labels = c("Males", "Total concussions", "LOC", "Amnesia",
                               "Males x Total concussions", "Males x LOC", "Males x LOC",
                               "Total concussions x LOC", 
                               "Males x LOC x Total concussions",
                               "Total concussions x Amnesia"),
          omit = "Constant",
          coef = list(dt.mmse.alt2.step$coef, exp(rmse.alt2.step$coef), avp04.alt2.step$coef,
                      exp(avp48.alt2.step$coef), exp(avp812.alt2.step$coef)),
          ci.custom = list(confint(dt.mmse.alt2.step),
                           exp(confint(rmse.alt2.step)),
                           confint(avp04.alt2.step),
                           exp(confint(avp48.alt2.step)),
                           exp(confint(avp812.alt2.step))),
          p.auto = FALSE,
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = "RMSE, 4-8Hz, and 8-12Hz average power required log transformation to meet model assumptions. Those coefficients and condience intervals have been re-exponentiated. LOC = loss of consciousness",
          notes.aling = "l",
          single.row = TRUE,
          ci = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          df = TRUE,
          type = "html",
          out = paste(my_dir, "/Table 2.html", sep = ""))


# Table 3 ----
mod1 <- etasq(dt.mmse.alt1.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Complexity")
mod2 <- etasq(rmse.alt1.step)%>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "RMSE")
mod3 <- etasq(avp04.alt1.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "0-4Hz AvP")
mod4 <- etasq(avp48.alt1.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "4-8Hz AvP")
mod5 <- etasq(avp812.alt1.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "8-12Hz AvP")

colname <- colnames(mod1)[2]

r2table <- bind_rows(mod1, mod2, mod3, mod4, mod5) %>%
  group_by(Term) %>%
  spread(Model, "Partial eta^2") %>%
  filter(Term != "Residuals") %>%
  ungroup() %>%
  mutate_at(vars(2:6), funs(round(.,3)))  %>%
  mutate(Term = c("Amnesia", 
                  "Diagnosed concussions", 
                  "Amnesia x Diagnosed concussions",
                "LOC x Diagnosed concussions",
                "Diagnosed concussions x Suspected concussions",
                "Gender", 
                "Gender x Amnesia",
                "Gender x Diagnosed concussions",
                "Gender x LOC x Diagnosed concussions", 
                "Gender x Diagnosed x Suspected Concussion",
                "Gender x LOC",
                "Gender x Suspected concussions", 
                "LOC", 
                "LOC x Amnesia", 
                "Suspected concussions", 
                "Amensia x Suspected concussions",
                "LOC x Suspected concussions"))

order <- c("Gender", "LOC", "Amnesia", "Diagnosed concussions", "Suspected concussions", 
           "Gender x Amnesia", "Gender x LOC", "Gender x Diagnosed concussions",
           "Gender x Suspected concussions", "LOC x Amnesia", "LOC x Diagnosed concussions",
           "LOC x Suspected concussions", "Amnesia x Diagnosed concussions", "Amnesia x Suspected concussions",
           "Diagnosed concussions x Suspected concussions",
           "Gender x LOC x Diagnosed concussions", "Gender x Diagnosed x Suspected concussions")

r2table <- r2table %>%
  slice(match(order, Term))

r2table <- r2table[,c(1,5,6,2,3,4)]

stargazer(as.matrix(r2table),
          title = "Model 1 Partial R2 values",
          single.row = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          type = "html", 
          out = paste(my_dir, "/Table 3.html", sep = ""))

# Table 4 ----
mod1 <- etasq(dt.mmse.alt2.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Complexity")
mod2 <- etasq(rmse.alt2.step)%>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "RMSE")
mod3 <- etasq(avp04.alt2.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "0-4Hz AvP")
mod4 <- etasq(avp48.alt2.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "4-8Hz AvP")
mod5 <- etasq(avp812.alt2.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "8-12Hz AvP")

colname <- colnames(mod1)[2]

r2table <- bind_rows(mod1, mod2, mod3, mod4, mod5) %>%
  group_by(Term) %>%
  spread(Model, "Partial eta^2") %>%
  filter(Term != "Residuals") %>%
  ungroup() %>%
  mutate_at(vars(2:6), funs(round(.,3)))  %>%
  mutate(Term = c("Amnesia", 
                  "Gender", 
                  "Gender x Amnesia",
                  "Gender x LOC",
                  "Gender x Total concussions",
                  "Gender x LOC x Total concussions",
                  "LOC",
                  "Total concussions",
                  "Amensia x Total concussions",
                  "LOC x Total concussions"))

order <- c("Gender", "LOC", "Amnesia", "Total concussions",
           "Gender x Amnesia", "Gender x LOC", "Gender x Total concussions",
           "LOC x Total concussions", "Amnesia x Total concussions", 
           "Gender x Total x Diagnosed concussions")

r2table <- r2table %>%
  slice(match(order, Term))

r2table <- r2table[,c(1,5,6,2,3,4)]

stargazer(as.matrix(r2table),
          title = "Model 2 Partial R2 values",
          single.row = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          type = "html", 
          out = paste(my_dir, "/Table 4.html", sep = ""))

