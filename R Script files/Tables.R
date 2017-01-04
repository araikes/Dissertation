#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/30/2016
# Local Depends: Multiple Regression.R, Alternative multiple regression.R
# Local Files: ./Data Files/*.csv
#
# Description: This file is for the creation of tables for chapters 3 and 4 of
# the dissertation. This is to ease the coding density of the other files.

### Load libraries ----
library(broom)
library(heplots)
library(stargazer)


# Table 3-2 ----
stargazer(dt.mmse.step, dt.mmse.alt2.step, dt.mmse.cv.step, dt.mmse.cv.alt1.step,
          title = "Table 3-2",
          dep.var.caption = "",
          dep.var.labels = c("Trial Average", "CV"),
          column.labels = c("A priori", "Best", "A priori", "Best"),
          covariate.labels = c("Diagnosed concussions", "Suspected concussions",
                               "Males", "Total concussions", "LOC", "Amnesia",
                               "Males x Diagnosed concussions", "Males x Suspected concussions",
                               "LOC x Suspected concussions", 
                               "Diagnosed concussions x Suspected concussions",
                               "LOC x Amnesia",
                               "Males x Amnesia x Diagnosed concussions",
                               "Males x LOC x Suspected concussions",
                               "Males x LOC", "Males x Amnesia", "Amnesia x Diagnosed concussions"),
          omit = "Constant",
          notes = "CV values required log transformation to meet model assumptions. Those coefficients and confidence intervals have been re-exponentiated. LOC = loss of consciousness, RA = retrograde amnesia",
          coef = list(dt.mmse.step$coef, dt.mmse.alt2.step$coef,
                      exp(dt.mmse.cv.step$coef), exp(dt.mmse.cv.alt1.step$coef)),
          ci.custom = list(confint(dt.mmse.step), 
                           confint(dt.mmse.alt2.step),
                           exp(confint(dt.mmse.cv.step)), 
                           exp(confint(dt.mmse.cv.alt1.step))),
          notes.align = "l",
          single.row = TRUE,
          ci = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          df = FALSE,
          type = "html", 
          out = "Regression Tables/Table 3-2.html")

# Table 3-3 ----
mod1 <- etasq(dt.mmse.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori")
mod2 <- etasq(dt.mmse.alt2.step )%>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best")
mod3 <- etasq(dt.mmse.cv.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori CV")
mod4 <- etasq(dt.mmse.cv.alt1.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best CV")

colname <- colnames(mod1)[2]

r2table <- bind_rows(mod1, mod2, mod3, mod4) %>%
  group_by(Term) %>%
  spread(Model, "Partial eta^2") %>%
  filter(Term != "Residuals") %>%
  mutate_each(funs(round(.,3)), -Term) %>%
  ungroup() %>%
  mutate(Term = c("Amnesia", "Diagnosed concussions", "Amnesia x Diagnosed concussions",
                  "Diagnosed concussions x Suspected concussions",
                  "Gender", "Gender x Amnesia","Gender x Diagnosed concussions",
                  "Gender x Amnesia x Diagnosed concussions", "Gender x LOC",
                  "Gender x Suspected concussions", "Gender x LOC x Suspected concussions",
                  "LOC", "LOC x Amnesia", "Suspected concussions", "LOC x Suspected concussions",
                  "Total concussions"))

order <- c("Gender", "LOC", "Amnesia", "Diagnosed concussions", "Suspected concussions", 
           "Total concussions", "Gender x Amnesia", "Gender x LOC", "Gender x Diagnosed concussions",
           "Gender x Suspected concussions", "LOC x Amnesia",
           "LOC x Suspected concussions", "Amnesia x Diagnosed concussions",
           "Diagnosed concussions x Suspected concussions",
           "Gender x Amnesia x Diagnosed concussions", "Gender x LOC x Suspected concussions")

r2table <- r2table %>%
  slice(match(order, Term))

r2table <- r2table[,c(1,2,4,3,5)]

stargazer(as.matrix(r2table),
          title = "Table 3-3",
          single.row = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          type = "html", out = "Regression Tables/Table 3-3.html")

          




# Table 3-4 ----
stargazer(rmse.step, rmse.alt2.step, rmse.cv.step, rmse.cv.alt1.step,
          title = "Table 3-4",
          dep.var.caption = "",
          dep.var.labels = c("Trial Average", "CV", "CV"),
          column.labels = c("A priori", "Best", "A priori", "Best"),
          covariate.labels = c("Males", "Total concussions", "Diagnosed concussions", 
                               "Suspected concussions", "LOC","Amnesia", 
                               "Males x Total concussions", "Males x Diagnosed concussions",
                               "Males x LOC", "LOC x Total concussions", 
                               "Males x LOC x Total concussions",
                               "LOC x Amnesia", "Males x Amnesia",
                               "Diagnosed concussions x Suspected concussions",
                               "LOC x Diagnosed concussions", "Amnesia x Suspected concussions",
                               "Males x LOC x Diagnosed concussions"),
          omit = "Constant",
          coef = list(exp(rmse.step$coef), exp(rmse.alt2.step$coef),
                      rmse.cv.step$coef, exp(rmse.cv.alt1.step$coef)),
          ci.custom = list(exp(confint(rmse.step)), exp(confint(rmse.alt2.step)),
                           confint(rmse.cv.step), exp(confint(rmse.cv.alt1.step))),
          notes = "All models except the a priori CV model required log-transformation. Coefficents and confidence intervals for those models have been exponentiated. LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          single.row = TRUE,
          ci = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          df = FALSE,
          type = "html", 
          out = "Regression Tables/Table 3-4.html")

# Table 3-5 ----
mod1 <- etasq(rmse.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori")
mod2 <- etasq(rmse.alt2.step )%>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best")
mod3 <- etasq(rmse.cv.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori CV")
mod4 <- etasq(rmse.cv.alt1.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best CV")

colname <- colnames(mod1)[2]

r2table <- bind_rows(mod1, mod2, mod3, mod4) %>%
  group_by(Term) %>%
  filter(Term != "Residuals", Term != "(Intercept)") %>%
  spread(Model, "Partial eta^2") %>%
  mutate_each(funs(round(.,3)), -Term) %>%
  ungroup() %>%
  mutate(Term = c("Amnesia", "Diagnosed concussions", "LOC x Diagnosed concussions",
                  "Diagnosed concussions x Suspected concussions", 
                  "Gender", "Gender x Amnesia", "Gender x Diagnosed concussions",
                  "Gender x LOC x Diagnosed concussions", "Gender x LOC",
                  "Gender x Total concussions", "Gender x LOC x Total concussions",
                  "LOC", "LOC x Amnesia", "Suspected concussions", "Amnesia x Suspected concussions",
                  "Total concussions", "LOC x Total concussions"))

order <- c("Gender", "LOC", "Amnesia", "Diagnosed concussions", "Suspected concussions",
           "Total concussions", "Gender x LOC",
           "Gender x Amnesia", "LOC x Amnesia", "Gender x Diagnosed concussions",
           "Gender x Total concussions",
           "Amnesia x Suspected concussions",
           "Diagnosed concussions x Suspected concussions", "LOC x Diagnosed number",
           "LOC x Total concussions",
           "Gender x LOC x Diagnosed concussions", "Gender x LOC x Total concussions")

r2table <- r2table %>%
  slice(match(order, Term)) %>%
  mutate("A priori" = NA)

r2table <- r2table[,c(1,5,3,2,4)]

stargazer(as.matrix(r2table),
          title = "Table 3-5",
          notes = "Note: LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          single.row = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          type = "html", out = "Regression Tables/Table 3-5.html")

# Table 3-6 ----
stargazer(avp04.step, avp04.alt1.step, avp04.cv.step, avp04.cv.alt3.step,
          title = "Table 3-6",
          dep.var.caption = "",
          dep.var.labels = c("Trial Average", "CV", "CV"),
          column.labels = c("A priori", "Best", "A priori", "Best"),
          covariate.labels = c("Suspected concussions", "LOC", "Males",
                               "Diagnosed concussions", "Amnesia",
                               "LOC x Suspected concussions", "LOC x Amnesia",
                               "Males x Diagnosed concussions",
                               "Amnesia x Diagnosed concussions",
                               "Total LOC", "Total RA", "Total LOC x Total RA"),
          omit = "Constant",
          coef = list(avp04.step$coef, avp04.alt1.step$coef, 
                      avp04.cv.step$coef, exp(avp04.cv.alt3.step$coef)),
          ci.custom = list(confint(avp04.step), confint(avp04.alt1.step), 
                           confint(avp04.cv.step), exp(confint(avp04.cv.alt3.step))),
          notes = "The best alternative CV model required log-transformation. The coefficients and confidence intervals for this model have been re-exponentiated. LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          single.row = TRUE,
          ci = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          df = FALSE,
          type = "html",
          out = "Regression Tables/Table 3-6.html")

# Table 3-7 ----
mod1 <- etasq(avp04.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori")
mod2 <- etasq(avp04.alt1.step )%>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best")
mod3 <- etasq(avp04.cv.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori CV")
mod4 <- etasq(avp04.cv.alt3.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best CV")

colname <- colnames(mod1)[2]

r2table <- bind_rows(mod1, mod2, mod3, mod4) %>%
  group_by(Term) %>%
  spread(Model, "Partial eta^2") %>%
  filter(Term != "Residuals", Term != "(Intercept)") %>%
  mutate_each(funs(round(.,3)), -Term) %>%
  ungroup() %>%
  mutate(Term = c("Amnesia", "Diagnosed concussions", "Amnesia x Diagnosed concussions",
                  "Gender", "Gender x Diagnosed concussions", 
                  "LOC", "LOC x Amnesia",
                  "Suspected concussions", "LOC x Suspected concussions", "Total LOC",
                  "Total LOC x Total RA" ,"Total RA"))

order <- c("Gender", "LOC", "Amnesia", "Diagnosed concussions", "Suspected concussions",
           "Total LOC", "Total RA", "Gender x Diagnosed concussions", 
           "Amnesia x Diagnosed concussions", "LOC x Amnesia", "LOC x Suspected concussions",
           "Total LOC x Total RA")

r2table <- r2table %>%
  slice(match(order, Term))

r2table<- r2table[,c(1,2,4,3,5)]

stargazer(as.matrix(r2table),
          title = "Table 3-7",
          notes = "Note: LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          single.row = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          type = "html", out = "Regression Tables/Table 3-7.html")

# Table 3-8 ----
stargazer(avp48.step, avp48.alt1.step, avp48.cv.step, avp48.cv.alt3.step,
          title = "Table 3-8",
          dep.var.labels = c("Trial Average", "Trial Average", "CV"),
          column.labels = c("A priori", "Best", "A priori", "Best"),
          covariate.labels = c("Males", "Diagnosed concussions", "Suspected concussions",
                               "LOC", "Diagnosed concussions x Suspected concussions",
                               "LOC x Suspected concussions", "Amnesia",
                               "Males x Diagnosed concussions", "Males x LOC",
                               "LOC x Diagnosed concussions",
                               "Amnesia x Diagnosed concussions",
                               "Males x LOC x Diagnosed concussions",
                               "Total concussions", "Total LOC", "Total RA",
                               "Males x Total concussions", "Males x Total LOC",
                               "Total concussions x Total LOC", "Total LOC x Total RA"),
          omit = "Constant",
          notes = "The best model for the trial averages required log-transformation. The coefficients and confidence intervals have been re-exponentiated. LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          coef = list(avp48.step$coef, exp(avp48.alt1.step$coef),
                      avp48.cv.step$coef, avp48.cv.alt3.step$coef),
          single.row = TRUE,
          ci = TRUE,
          ci.custom = list(confint(avp48.step), exp(confint(avp48.alt1.step)),
                       confint(avp48.cv.step), confint(avp48.cv.alt3.step)),
          no.space = TRUE,
          model.numbers = FALSE,
          df = FALSE,
          type = "html",
          out = "Regression Tables/Table 3-8.html")

# Table 3-9 ----
mod1 <- etasq(avp48.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori")
mod2 <- etasq(avp48.alt1.step )%>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best")
mod3 <- etasq(avp48.cv.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori CV")
mod4 <- etasq(avp48.cv.alt3.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best CV")

colname <- colnames(mod1)[2]

r2table <- bind_rows(mod1, mod2, mod3, mod4) %>%
  group_by(Term) %>%
  spread(Model, "Partial eta^2") %>%
  filter(Term != "Residuals", Term != "(Intercept)") %>%
  mutate_each(funs(round(.,3)), -Term) %>%
  ungroup() %>%
  mutate(Term = c("Amnesia", "Diagnosed concussions", "Amnesia x Diagnosed concussions",
                  "LOC  x Diagnosed concussions", "Diagnosed concussions x Suspected concussions",
                  "Gender", "Gender x Diagnosed concussions", "Gender x LOC x Diagnosed concussions",
                  "Gender x LOC", "Gender x Total concussions", "Gender x Total LOC", "LOC", 
                  "Suspected concussions", "LOC x Suspected concussions",
                  "Total concussions", "Total concussions x Total LOC", "Total LOC",
                  "Total LOC x Total RA", "Total RA"))

order <- c("Gender", "LOC", "Amnesia", "Diagnosed concussions", "Suspected concussions",
           "Total concussions", "Total LOC", "Total RA", "Gender x LOC", "Gender x Diagnosed concussions",
           "Gender x Total concussions", "Gender x Total LOC", 
           "LOC x Diagnosed concussions", "LOC x Suspected concussions", "Amnesia x Diagnosed concussions",
           "Diagnosed concussions x Suspected concussions", "Total concussions x Total LOC", 
           "Total LOC x Total RA", "Gender x LOC x Diagnosed concussions")

r2table <- r2table %>%
  slice(match(order, Term))

r2table <- r2table[,c(1,2,4,3,5)]

stargazer(as.matrix(r2table),
          title = "Table 3-9",
          notes = "Note: LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          single.row = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          type = "html", out = "Regression Tables/Table 3-9.html")





# Table 3-10 ----
stargazer(avp812.step, avp812.alt1.step, avp812.cv.step, avp812.cv.alt1.step,
          title = "Table 3-10",
          dep.var.caption = "",
          dep.var.labels = c("Trial Average", "CV"),
          column.labels = c("A priori", "Best", "A priori", "Best"),
          covariate.labels = c("Males", "Diagnosed concussions", "Suspected concussions",
                               "LOC", "Amnesia",
                               "Males x Suspected concussions",
                               "Males x LOC",
                               "Diagnosed concussions x Suspected concussions",
                               "LOC x Diagnosed concussions", "LOC x Suspected concussions"),
          omit = "Constant",
          notes = "Mean models required log-transformation. Coefficients and confidence intervals have been re-exponentiated. LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          coef = list(exp(avp812.step$coef), exp(avp812.alt1.step$coef),
                      avp812.cv.step$coef, avp812.cv.alt1.step$coef),
          single.row = TRUE,
          ci = TRUE,
          ci.custom = list(exp(confint(avp812.step)), exp(confint(avp812.alt1.step)),
                           confint(avp812.cv.step), confint(avp812.cv.alt1.step)),
          no.space = TRUE,
          model.numbers = FALSE,
          df = FALSE,
          type = "html",
          out = "Regression Tables/Table 3-10.html")

# Table 3-11 ----
mod1 <- etasq(avp812.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori")
mod2 <- etasq(avp812.alt1.step )%>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best")
mod3 <- etasq(avp812.cv.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "A priori CV")
mod4 <- etasq(avp812.cv.alt1.step) %>%
  rownames_to_column(var = "Term") %>%
  mutate(Model = "Best CV")

colname <- colnames(mod1)[2]

r2table <- bind_rows(mod1, mod2, mod3, mod4) %>%
  group_by(Term) %>%
  spread(Model, "Partial eta^2") %>%
  filter(Term != "Residuals", Term != "(Intercept)") %>%
  mutate_each(funs(round(.,3)), -Term) %>%
  ungroup() %>%
  mutate(Term = c("Amnesia", "Diagnosed concussions", "LOC x Diagnosed concussions",
                  "Diagnosed concussions x Suspected concussions", "Gender", 
                  "Gender x LOC", "Gender x Suspected concussions","LOC", "Suspected concussions",
                  "LOC x Suspected concussions"))

order <- c("Gender", "LOC", "Amnesia", "Diagnosed concussions", "Suspected concussions",
           "Gender x LOC",
           "Gender x Suspected concussions", 
           "LOC x Diagnosed concussions", "LOC x Suspected concussions",
           "Diagnosed concussions x Suspected concussions")

r2table <- r2table %>%
  slice(match(order, Term))

r2table <- r2table[,c(1,2,4,3,5)]

stargazer(as.matrix(r2table),
          title = "Table 3-11",
          notes = "Note: LOC = loss of consciousness, RA = retrograde amnesia",
          notes.align = "l",
          single.row = TRUE,
          no.space = TRUE,
          model.numbers = FALSE,
          type = "html", out = "Regression Tables/Table 3-11.html")





