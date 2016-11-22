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
  