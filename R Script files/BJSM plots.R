#### File Description ####
# Author: Adam Raikes
# Initial Date: 12/28/2016
# Local Depends: Multiple Regression.R, Alternative multiple regression.R
# Local Files: 
#
# Description: Plots of regression outcomes using the original and the highest
# R2 model. These plots only include significant predictors

#### Load libraries ####
require(ggplot2)
require(cowplot)
require(extrafont)
my_dir <- "C:/Users/adamraikes/Dropbox/Manuscript Submissions/Dissertation/Chapter 3/Participant mean values/Images"

#### Complexity plots ####
# Alternative mean model ----
dt.mean.plot <- ggplot(data = average.outcomes, aes(x = total.concussions, 
                                    y = detrended.complexity_mean,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE) +
  xlab("Total number of concussions") +
  ylab("Complexity index") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) + 
  guides(colour = guide_legend(override.aes = list(linetype = c(0,0),
                                                   shape = c(16,16),
                                                   size = 4))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 24),
    legend.text = element_text(family ="Arial", size = 24),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))

save_plot(paste(my_dir, "/Complexity.svg", sep = ""),
          dt.mean.plot,
          base_width = 14,
          base_height = 6)

#### AvP 48 plots ####
# Alternative mean model ----
avp48.mean.plot <- ggplot(data = average.outcomes, aes(x = gender,
                                    y = log(avp48_mean),
                                    fill = gender)) +
  geom_boxplot() +
  xlab("Gender") +
  ylab("Log-transformed\n4-8 Hertz Average Power") +
  scale_fill_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) +
  guides(fill = FALSE) +
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 24),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))

save_plot(paste(my_dir, "/AvP48.svg", sep = ""),
          avp48.mean.plot,
          base_width = 14,
          base_height = 6)



#### AvP 812 plots ####
# Alternative mean models ----
avp812.mean.plot <- ggplot(data = average.outcomes, aes(x = total.concussions,
                                    y = log(avp812_mean), group = LOC)) +
  geom_jitter(width = 0.25, size = 2, aes(color = gender)) +
  stat_smooth(method = "lm", se = FALSE, 
              aes(linetype = LOC), color = "black") + 
  xlab("Total number of concussions concussions") +
  ylab("Log-transformed\n8-12 Hertz Average Power") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) +
  scale_linetype_manual(name = "LOC",
                        values = c(1,2),
                        labels = c("No LOC", "LOC")) +
  guides(colour = guide_legend(override.aes = list(linetype = c(0,0),
                                                   shape = c(16,16),
                                                   size = 4)),
         linetype = guide_legend(override.aes = list(linetype = c(1,2),
                                                     color = c("black", "black")))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 24),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))



save_plot(paste(my_dir, "/AvP812.svg", sep = ""),
          avp812.mean.plot,
          base_width = 14,
          base_height = 6)

