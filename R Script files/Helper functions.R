#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/14/2016
# Local Depends: none
# Local Files: none
#
# Description: This file contains various helper functions to improve the
# functional programming flow for my dissertation. These functions are intended
# to reduce redundant coding in the other .R files.
#
# Some of the functions contained herein may require the use of libraries. These
# will be loaded here as needed.
require(extrafont)

editVars <- function(x) {
  # This function drops the subject and trial identifies "S" and "tr".
  gsub("[A-z]", "", x)
}

prepSubs <- function(x) {
  # This function will call manipulation functions to edit strings to numeric or
  # factor. Broadly, the purpose of this function is to convert the variable
  # columns prepended with a letter from strings/factors to numeric.
  
  # Load dplyr if not already loaded
  require(dplyr)
  
  # Pass dataframe to editVars
  mutate_if(x, is.factor, funs(editVars)) %>%
    mutate_if(is.factor, as.numeric)
}
  
deidentify <- function(x, ids){
  # This function will replace original participant IDs with randomly generated
  # ones for deidentification purposes
  
  # Load dplyr if not already loaded
  require(dplyr)
  
  # Replace ids
  x <- x %>%
    left_join(ids) %>%
    select(-subject) %>%
    select(id, block, everything())
  
  return(x)
}

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

my_boxplot <- function(group, y, data){
  require(ggplot2)
  ggplot(data = data, aes_string(group, y)) +
    geom_boxplot(notch = TRUE) +
    geom_jitter(width = 0.2) +
    xlab(group) +
    ylab(y) +
    theme_bw()
}
  
my_scatterplot <- function(cols, title, data){
  require(ggplot2)
  require(GGally)
  ggpairs(data = data,
          columns = cols,
          lower = list(
            continuous = "smooth",
            combo = "facetdensity"),
          title = title)
}

my_qqplot <- function(y, data){
  require(ggplot2)
  
  ggplot(data, aes_string(sample = y))+
    stat_qq() +
    theme_bw()
}

boxplot_outliers <- function(y, outlier, outcome, data){
  require(ggplot2)
  ggplot(data = data, aes_string(x = 1, y)) +
    facet_wrap(reformulate(outcome), scales = "free") +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    geom_text(aes_string(label = outlier), na.rm = TRUE, hjust = -0.3) +
    xlab("") +
    ylab("") +
    theme_bw()
}

participant_inclusion <- function(x){
  # This function is not meant for broad purpose use. It is specific to this project
  require(dplyr)
  
  select(x, id, trial) %>%
    group_by(id) %>%
    summarise(count = n()) %>%
    filter(count >= 5) %>%
    select(id)
}

trial_inclusion <- function(allTrials, badTrials){
  # This function is not meant for broad purpose use. It is specific to this project
  require(dplyr)
  
  drop.low <- filter(allTrials,
                     valid.volts > 0.2)
  
  retained.ids <- participant_inclusion(drop.low)
  
  retained.trials <- semi_join(drop.low, retained.ids) %>%
    anti_join(badTrials) %>%
    select(id, trial)
  
  retained.ids <- participant_inclusion(retained.trials)
  
  retained.trials <- semi_join(retained.trials, retained.ids)
  
  return(retained.trials)
}

cv <- function(x){
  sd(x)/mean(x)
}
    
round_pred <- function(x){
  round(x + 0.00001)
}

predictive_ability <- function(real, pred){
  require(caret)
  
  confusionMatrix(pred, real)
}
    
  
  