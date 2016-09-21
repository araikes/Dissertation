library(dplyr)
library(ggplot2)
library(tidyr)


# Load data
rawtrace <- read.csv("Raw_MVC10.csv") %>%
  select(-X) %>%
  group_by(subject, condition, trial) %>%
  gather(xvals, yvals, -subject:-center) 

rawtrace$subject <- gsub("S","", rawtrace$subject)
rawtrace$trial <- gsub("tr", "", rawtrace$trial)
rawtrace$xvals <- gsub("X", "", rawtrace$xvals)

rawtrace$subject <- as.numeric(rawtrace$subject)
rawtrace$trial <- as.numeric(rawtrace$trial)
rawtrace$xvals <- as.numeric(rawtrace$xvals)

vision <- filter(rawtrace, condition == "cond2")

ggplot(data = vision, aes(x = xvals, y = yvals)) +
  geom_line() +
  facet_grid(subject ~ trial, scale= "free") +
  geom_hline(aes(yintercept = center))
  
