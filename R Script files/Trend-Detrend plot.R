#### Load libraries ####
library(tidyverse)
library(extrafont)
library(cowplot)
my_dir <- "C:/Users/Adam/Dropbox/Adam/Dissertation/Images/General"

#### Read data ####
signal <- read.csv("Data Files/Detrended Signal.csv")
trend <- read.csv("Data Files/Trend.csv")
original <- read.csv("Data Files/Raw Data.csv") %>%
  select(-id, -block, -trial, -center) %>%
  mutate(X = row_number() -1 )

signal.long <- gather(signal, xval, dtsignal, -X)
trend.long <- gather(trend, xval, trend, -X)
original.long <- gather(original, xval, signal, -X)
original.long$xval <- gsub("[A-z]", "X", original.long$xval)

signal.data <- left_join(original.long, trend.long) %>%
  left_join(signal.long) %>%
  mutate(xval = editVars(xval)) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(xval = xval/100)


original.plot <- filter(signal.data, X == 2) %>%
  ggplot(data = ., aes(x = xval, y = signal)) + 
    geom_line() + 
    geom_line(aes(x = xval,  y = trend), 
              color = "blue",
              size = 1.1) +
    xlab("Time (s)") + 
    ylab("Volts") +
    theme_classic() + theme(
      title = element_text(family = "Arial", size = 20),
      axis.text = element_text(family = "Arial", size = 20),
      legend.text = element_text(family = "Arial", size = 20),
      axis.line.x = element_line(
        colour = "black",
        linetype = "solid",
        size = 1
      ),
      axis.line.y = element_line(
        colour = "black",
        linetype = "solid",
        size = 1
      )
    )

detrended.plot <- filter(signal.data, X == 2) %>%
  ggplot(data = ., aes(x = xval, y = dtsignal)) +
    geom_line() +
    xlab("Time (s)") +
    ylab("Volts") +
    theme_classic() +
    theme(
      title = element_text(family = "Arial", size = 20),
      axis.text = element_text(family = "Arial", size = 20),
      legend.text = element_text(family = "Arial", size = 20),
      axis.line.x = element_line(
        colour = "black",
        linetype = "solid",
        size = 1
      ),
      axis.line.y = element_line(
        colour = "black",
        linetype = "solid",
        size = 1
      )
    )

signal.plot <- plot_grid(original.plot, detrended.plot, curve.plot,
                         labels = c("A", "B", "C"),
                         label_size = 24,
                         align = "vh",
                         hjust = -2,
                         ncol = 1)

save_plot(paste(my_dir, "/Signal.svg", sep = ""),
          signal.plot,
          base_width = 14,
          base_height = 10)
