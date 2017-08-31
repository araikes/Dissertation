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
  
  


# Alternative CV model ----
dt.cv.plot <- ggplot(data = average.outcomes, aes(x = diagnosed.number,
                                    y = detrended.complexity_cv,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE, 
              aes(linetype = amnesia)) +
  xlab("Number of diagnosed concussions") +
  ylab("Complexity index\nCoefficient of Variation") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) +
  scale_linetype_manual(name = "Amnesia",
                        values = c(2,6),
                        labels = c("No Amnesia", "Amnesia")) +
  guides(colour = FALSE,
         linetype = guide_legend(override.aes = list(linetype = c(2,6),
                                                     color = c("#000000","#000000")))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 12),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))
# Panel ----
grob1 <- ggplotGrob(dt.mean.plot + theme(legend.title = element_blank(),
                                       legend.position = "bottom",
                                       legend.box = "horizontal"))$grobs

legend1 <- grob1[[which(sapply(grob1, function (x) x$name) == "guide-box")]]

grob2 <- ggplotGrob(dt.cv.plot + theme(legend.title = element_blank(),
                                        legend.position = "bottom",
                                        legend.box = "horizontal"))$grobs

legend2 <- grob2[[which(sapply(grob2, function (x) x$name) == "guide-box")]]


ci.row <- plot_grid(dt.mean.plot + theme(legend.position = "none"),
                    dt.cv.plot + theme(legend.position = "none"),
                    labels = c("A", "B"),
                    label_size = 24,
                    align = "vh",
                    hjust = -0.5,
                    nrow = 1)

leg.row <- plot_grid(legend1, legend2,
                     nrow = 1)

ci.plot <- plot_grid(ci.row, leg.row, ncol = 1, rel_heights = c(1, 0.15))



# save_plot(paste(my_dir, "/Complexity.svg", sep = ""),
#           ci.plot,
#           base_width = 14,
#           base_height = 6)

save_plot(paste(my_dir, "/Complexity.svg", sep = ""),
          dt.mean.plot,
          base_width = 14,
          base_height = 6)








#### RMSE plots ####
# Alternative mean model ----
rmse.mean.plot <- ggplot(data = average.outcomes, aes(x = total.concussions,
                                    y= rmse.V_mean,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE,
              aes(linetype = LOC)) +
  xlab("Total number of concussions") +
  ylab("RMSE") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) +
  scale_linetype_manual(name = "LOC",
                        values = c(1,2),
                        labels = c("No LOC", "LOC")) +
  guides(colour = guide_legend(override.aes = list(linetype = c(0,0),
                                                   shape = c(16,16),
                                                   size = 4)),
         linetype = FALSE) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 12),
    legend.text = element_text(family ="Arial", size = 24),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))

## Alternative CV model ----
# Plot 1 ----
rmse.cv.plot1 <- ggplot(data = average.outcomes, aes(x = diagnosed.number,
                                    y = rmse.V_cv,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE,
              aes(linetype = LOC)) +  
  xlab("Number of diagnosed concussions") +
  ylab("RMSE\nCoefficient of variation") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) +
  scale_linetype_manual(name = "LOC",
                        values = c(1,2),
                        labels = c("No LOC", "LOC")) +
  guides(colour = FALSE,
         linetype = guide_legend(override.aes = list(linetype = c(1,2),
                                                     color = c("black", "black")))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 12),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))

# Plot 2----
rmse.cv.plot2 <- ggplot(data = average.outcomes, aes(x = suspected.number,
                                    y = rmse.V_cv,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE,
              aes(linetype = amnesia)) +  
  xlab("Number of suspected concussions") +
  ylab("RMSE\nCoefficient of variation") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) +
  scale_linetype_manual(name = "Amnesia",
                        values = c(1,6),
                        labels = c("No Amnesia", "Amnesia")) +
  guides(colour = FALSE,
         linetype = guide_legend(override.aes = list(linetype = c(1,6),
                                                     color = c("black", "black")))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 12),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))


# Panel ----
grob1 <- ggplotGrob(rmse.mean.plot + theme(legend.title = element_blank(),
                                           legend.position = "bottom",
                                           legend.box = "horizontal"))$grobs

legend1 <- grob1[[which(sapply(grob1, function (x) x$name) == "guide-box")]]

grob2 <- ggplotGrob(rmse.cv.plot1 + theme(legend.title = element_blank(),
                                          legend.position = "bottom",
                                          legend.box = "horizontal"))$grobs

legend2 <- grob2[[which(sapply(grob2, function (x) x$name) == "guide-box")]]

grob3 <- ggplotGrob(rmse.cv.plot2 + theme(legend.title = element_blank(),
                                          legend.position = "bottom",
                                          legend.box = "horizontal"))$grobs

legend3 <- grob3[[which(sapply(grob3, function (x) x$name) == "guide-box")]]

leg.row <- plot_grid(legend1, legend2, legend3,
                     scale = 0.75,
                     ncol = 1)


rmse.plot <- plot_grid(rmse.mean.plot + theme(legend.position = "none"),
                       rmse.cv.plot1 + theme(legend.position = "none"),
                       rmse.cv.plot2 + theme(legend.position = "none"),
                       leg.row,
                       labels = c("A", "B", "C", ""),
                       align = "vh",
                       hjust = -0.5,
                       label_size = 24,
                       nrow = 2)

save_plot(paste(my_dir, "/RMSE.svg", sep = ""),
          rmse.plot,
          base_height = 14)





#### AvP 04 plots ####
# Alternative CV model ----
avp04.cv.plot <- ggplot(data = average.outcomes, aes(x = total.retrograde,
                                    y = avp04_cv,
                                    color = as.factor(total.loc))) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE,
              aes(linetype = as.factor(total.loc))) +
  xlab("Number of concussions resulting in retrograde amnesia") +
  ylab("0-4 Hertz Average Power\ncoefficient of variation") +
  scale_colour_manual(name = "Number of LOC",
                      values = c("#0571b0", "#ca0020", "#f4a582", "#92c5de"),
                      labels = c("No LOC",
                                 "1 LOC",
                                 "2 LOC",
                                 "3 LOC")) +
  scale_linetype_manual(values = c(1,2),
                        labels = c("No LOC",
                                   "1 LOC")) +
  guides(colour = guide_legend(override.aes = list(linetype = c(0,0,0,0),
                                                   shape = c(16,16,16,16),
                                                   size = 4)),
         linetype = guide_legend(override.aes = list(linetype = c(1,2),
                                                     color = c("black", "black")))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 18),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))









# Panel ----
grob1 <- ggplotGrob(avp04.cv.plot + theme(legend.title = element_blank(),
                                           legend.position = "bottom",
                                           legend.box = "horizontal"))$grobs

legend1 <- grob1[[which(sapply(grob1, function (x) x$name) == "guide-box")]]

avp04.row <- plot_grid(avp04.cv.plot + theme(legend.position = "none"))

avp04.plot <- plot_grid(avp04.row, legend1, ncol = 1, rel_heights = c(1, 0.15))

save_plot(paste(my_dir, "/AvP 04.svg", sep = ""),
          avp04.plot,
          base_width = 14,
          base_height = 6)


#### AvP 48 plots ####
# Alternative mean model ----
avp48.mean.plot <- ggplot(data = average.outcomes, aes(x = diagnosed.number,
                                    y = avp48_mean,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE,
              aes(linetype = LOC)) +
  xlab("Number of diagnosed concussions") +
  ylab("4-8 Hertz Average Power") +
  ylim(c(0,0.45)) +
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


                                    
# Alternative CV model ----
avp48.cv.plot <- ggplot(data = average.outcomes, aes(x = total.concussions,
                                    y = avp48_cv,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE,
              aes(linetype = as.factor(total.loc))) +
  xlab("Total number of concussions") +
  ylab("4-8 Hertz Average Power\nCoefficient of Variation") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020","#0571b0")) +
  scale_linetype_manual(name = "Number of LOC",
                        values = c(1,6),
                        labels = c("No LOC",
                                   "1 LOC")) +
  guides(colour = FALSE,
         linetype = guide_legend(override.aes = list(linetype = c(1,6),
                                                     color = c("black", "black")))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 10),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))



# Panel ----
grob1 <- ggplotGrob(avp48.mean.plot + theme(legend.title = element_blank(),
                                           legend.position = "bottom",
                                           legend.box = "horizontal"))$grobs

legend1 <- grob1[[which(sapply(grob1, function (x) x$name) == "guide-box")]]

grob2 <- ggplotGrob(avp48.cv.plot + theme(legend.title = element_blank(),
                                          legend.position = "bottom",
                                          legend.box = "horizontal"))$grobs

legend2 <- grob2[[which(sapply(grob2, function (x) x$name) == "guide-box")]]


leg.row <- plot_grid(legend1, legend2,
                     nrow = 1)


avp48.row <- plot_grid(avp48.mean.plot + theme(legend.position = "none"),
                       avp48.cv.plot + theme(legend.position = "none"),
                       labels = c("A", "B"),
                       align = "vh",
                       hjust = -1,
                       label_size = 24,
                       nrow = 1)

avp48.plot <- plot_grid(avp48.row, leg.row, ncol = 1, rel_heights = c(1, 0.15))

save_plot(paste(my_dir, "/AvP48.svg", sep = ""),
          avp48.plot,
          base_width = 18,
          base_height = 7)

save_plot(paste(my_dir, "/AvP48.svg", sep = ""),
          avp48.mean.plot,
          base_width = 14,
          base_height = 6)



#### AvP 812 plots ####
# Alternative mean models ----
avp812.mean.plot <- ggplot(data = average.outcomes, aes(x = suspected.number,
                                    y = avp812_mean,
                                    color = gender)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE, 
              aes(linetype = interaction(as.factor(diagnosed.number),
                                         LOC))) + 
  xlab("Number of suspected concussions") +
  ylab("8-12 Hertz Average Power") +
  scale_colour_manual(name = "Gender",
                      values = c("#ca0020", "#0571b0")) +
  scale_linetype_manual(name = "Diagnosed concussions and LOC",
                        values = c(1,2,3),
                        labels = c("0 concussions, no LOC",
                                   "0 concussions, LOC",
                                   "1 concussion, LOC")) +
  guides(colour = guide_legend(override.aes = list(linetype = c(0,0),
                                                   shape = c(16,16),
                                                   size = 4)),
         linetype = guide_legend(override.aes = list(linetype = c(1,2,3),
                                                     color = c("black", "black", "black")))) + 
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

# Alternative CV model ----
avp812.cv.plot <- ggplot(data = average.outcomes, aes(x = suspected.number,
                                    y = avp812_mean,
                                    color = LOC)) +
  geom_jitter(width = 0.25, size = 2) +
  stat_smooth(method = "lm", se = FALSE) +
  xlab("Number of suspected concussions") +
  ylab("8-12 Hertz Average Power\nCoefficient of Variation") +
  scale_colour_manual(name = "LOC",
                      values = c("#1f78b4", "#33a02c"),
                      labels = c("No LOC", "LOC")) +
  guides(colour = guide_legend(override.aes = list(linetype = c(0,0),
                                                   shape = c(16,16),
                                                   size = 4))) + 
  theme_classic() +
  theme(
    title = element_text(family ="Arial", size = 24),
    axis.text = element_text(family ="Arial", size = 24),
    legend.title = element_text(family = "Arial", size = 10),
    legend.text = element_text(family ="Arial", size = 24),
    legend.key.size = unit(0.5, "in"),
    axis.line.x = element_line(colour = "black", linetype = "solid",
                               size = 1),
    axis.line.y = element_line(colour = "black", linetype = "solid",
                               size = 1))


# Panel ----
grob1 <- ggplotGrob(avp812.mean.plot + 
                      theme(legend.title = element_blank(),
                                            legend.position = "bottom") +
                      guides(linetype = guide_legend(override.aes = 
                                                       list(linetype = c(1,2,3),
                                                            color = c("black", "black", "black")),
                                                     nrow = 3)))$grobs

legend1 <- grob1[[which(sapply(grob1, function (x) x$name) == "guide-box")]]

grob2 <- ggplotGrob(avp812.cv.plot + theme(legend.title = element_blank(),
                                          legend.position = "bottom",
                                          legend.box = "horizontal"))$grobs

legend2 <- grob2[[which(sapply(grob2, function (x) x$name) == "guide-box")]]


leg.row <- plot_grid(legend1, legend2,
                     ncol= 1)


avp812.row <- plot_grid(avp812.mean.plot + theme(legend.position = "none"),
                       avp812.cv.plot + theme(legend.position = "none"),
                       labels = c("A", "B"),
                       align = "vh",
                       hjust = -0.5,
                       label_size = 24,
                       ncol = 1)

avp812.plot <- plot_grid(avp812.row, leg.row, ncol = 2, rel_widths = c(0.5, 0.2))

save_plot(paste(my_dir, "/AvP812.svg", sep = ""),
          avp812.plot,
          base_height = 14)

save_plot(paste(my_dir, "/AvP812.svg", sep = ""),
          avp812.mean.plot,
          base_width = 14,
          base_height = 6)


allplot.row <- plot_grid(dt.mean.plot, avp48.mean.plot, avp812.mean.plot,
                         labels = c("A", "B", "C"),
                         align = "vh",
                         hjust = -0.5,
                         label_size = 24,
                         ncol = 1)

save_plot(paste(my_dir, "/All Plots.svg", sep = ""),
          allplot.row,
          base_width = 14,
          base_height = 6)
