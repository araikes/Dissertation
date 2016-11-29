#### File Description ####
# Author: Adam Raikes
# Initial Date: 11/22/2016
#
# Description: This is the master analytical file. The purpose of this file is
# to source various other files and functions related to the analyses contained
# in my dissertation. The expectation is for simple coding where calls are made
# either to other files or to functions contained in Helper functions.R so that
# there is a sensible workflow.
#
# Libraries will be loaded on demand by files and functions.

#### Load helper functions ####
source("./R Script files/Helper functions.R")

#### Deidentify data ####
# Run only when the participant files and Python output are updated.
source("./R Script files/Deidentification.R")

#### Trial validation ####
# Run only when the participant files and Python output are updated.
source("./R Script files/Trial Validation.R")

#### Exploratory analyses ####
# Run only when the participant files and Python output are updated.
# This will knit a PDF of the exploratory analyses. At this point, console
# command running of the knit does not produce subject-level plots. If this
# continues to be the case, then Exploratory analysis.Rmd needs to be knit from
# within the file.
rmarkdown::render(input = "./R Script files/Exploratory analysis.Rmd",
                  output_format = "pdf_document",
                  output_file = "./Plots/Exploratory Analysis.pdf")

