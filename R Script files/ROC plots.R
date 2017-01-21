### Load pROC and tidyverse ####
library(pROC)
library(tidyverse)
library(cowplot)
require(extrafont)
my_dir <- "C:/Users/Adam/Dropbox/Adam/Dissertation/Images/Chapter 4"

### Data frame for mean ROCs ####
roc.rmse <- data.frame(Measure = "RMSE",
                       Sensitivity = roc(mean.predictions$actual, mean.predictions$rmse.pred)$sensitivities,
                       Specificity = roc(mean.predictions$actual, mean.predictions$rmse.pred)$specificities)
roc.complexity <- data.frame(Measure = "Complexity",
                       Sensitivity = roc(mean.predictions$actual, mean.predictions$dtcomplex.pred)$sensitivities,
                       Specificity = roc(mean.predictions$actual, mean.predictions$dtcomplex.pred)$specificities)
roc.sampen <- data.frame(Measure = "Sample Entropy",
                             Sensitivity = roc(mean.predictions$actual, mean.predictions$sampen.pred)$sensitivities,
                             Specificity = roc(mean.predictions$actual, mean.predictions$sampen.pred)$specificities)
roc.avp04 <- data.frame(Measure = "0-4 Hertz Average Power",
                             Sensitivity = roc(mean.predictions$actual, mean.predictions$avp04.pred)$sensitivities,
                             Specificity = roc(mean.predictions$actual, mean.predictions$avp04.pred)$specificities)
roc.avp48 <- data.frame(Measure = "4-8 Hertz Average Power",
                             Sensitivity = roc(mean.predictions$actual, mean.predictions$avp48.pred)$sensitivities,
                             Specificity = roc(mean.predictions$actual, mean.predictions$avp48.pred)$specificities)
roc.avp812 <- data.frame(Measure = "8-12 Hertz Average Power",
                             Sensitivity = roc(mean.predictions$actual, mean.predictions$avp812.pred)$sensitivities,
                             Specificity = roc(mean.predictions$actual, mean.predictions$avp812.pred)$specificities)
roc.alpha <- data.frame(Measure = "DFA alpha",
                             Sensitivity = roc(mean.predictions$actual, mean.predictions$alpha.pred)$sensitivities,
                             Specificity = roc(mean.predictions$actual, mean.predictions$alpha.pred)$specificities)
roc.combo <- data.frame(Measure = "Additive model",
                             Sensitivity = roc(mean.predictions$actual, mean.predictions$combo.pred)$sensitivities,
                             Specificity = roc(mean.predictions$actual, mean.predictions$combo.pred)$specificities)
roc.complete <- data.frame(Measure = "Full model",
                             Sensitivity = roc(mean.predictions$actual, mean.predictions$complete.pred)$sensitivities,
                             Specificity = roc(mean.predictions$actual, mean.predictions$complete.pred)$specificities)

roc.means <- bind_rows(roc.rmse, roc.complexity, roc.sampen, roc.avp04, 
                       roc.avp48, roc.avp812, roc.alpha, roc.combo, roc.complete)
### Plot mean ROCs ####
roc.means.plot <-
  ggplot(data = roc.means, aes(x = Specificity, y = Sensitivity, color = Measure)) +
  geom_line(size = 1.2) +
  geom_segment(
    aes(
      x = 0,
      y = 1,
      xend = 1,
      yend = 0
    ),
    color = "black",
    size = 1.1,
    alpha = 0.5
  ) +
  scale_x_reverse(
    name = "Specificity",
    limits = c(1, 0),
    expand = c(0.0001, 0.001)
  ) +
  scale_y_continuous(
    name = "Sensitivity",
    limits = c(0, 1),
    expand = c(0.0001, 0.01)
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "#e41a1c",
      "#377eb8",
      "#4daf4a",
      "#984ea3",
      "#ff7f00",
      "#ca0020",
      "#a65628",
      "#f781bf",
      "#999999"
    )
  ) +
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



### Data frame for gender ROCs ####
roc.rmse <- data.frame(Measure = "RMSE",
                     Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$rmse.pred)$sensitivities,
                       Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$rmse.pred)$specificities)
roc.complexity <- data.frame(Measure = "Complexity",
                             Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$dtcomplex.pred)$sensitivities,
                             Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$dtcomplex.pred)$specificities)
roc.sampen <- data.frame(Measure = "Sample Entropy",
                         Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$sampen.pred)$sensitivities,
                         Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$sampen.pred)$specificities)
roc.avp04 <- data.frame(Measure = "0-4 Hertz Average Power",
                        Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$avp04.pred)$sensitivities,
                        Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$avp04.pred)$specificities)
roc.avp48 <- data.frame(Measure = "4-8 Hertz Average Power",
                        Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$avp48.pred)$sensitivities,
                        Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$avp48.pred)$specificities)
roc.avp812 <- data.frame(Measure = "8-12 Hertz Average Power",
                         Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$avp812.pred)$sensitivities,
                         Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$avp812.pred)$specificities)
roc.alpha <- data.frame(Measure = "DFA alpha",
                        Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$alpha.pred)$sensitivities,
                        Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$alpha.pred)$specificities)
roc.combo <- data.frame(Measure = "Additive model",
                        Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$combo.pred)$sensitivities,
                        Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$combo.pred)$specificities)
roc.complete <- data.frame(Measure = "Full model",
                           Sensitivity = roc(gender.mean.predictions$actual, gender.mean.predictions$complete.pred)$sensitivities,
                           Specificity = roc(gender.mean.predictions$actual, gender.mean.predictions$complete.pred)$specificities)

roc.gender.means <- bind_rows(roc.rmse, roc.complexity, roc.sampen, roc.avp04, 
                       roc.avp48, roc.avp812, roc.alpha, roc.combo, roc.complete)
### Plot mean ROCs ####
roc.gender.plot <-
  ggplot(data = roc.gender.means, aes(x = Specificity, y = Sensitivity, color = Measure)) +
  geom_line(size = 1.2) +
  geom_segment(
    aes(
      x = 0,
      y = 1,
      xend = 1,
      yend = 0
    ),
    color = "black",
    size = 1.1,
    alpha = 0.5
  ) +
  scale_x_reverse(
    name = "Specificity",
    limits = c(1, 0),
    expand = c(0.0001, 0.001)
  ) +
  scale_y_continuous(
    name = "Sensitivity",
    limits = c(0, 1),
    expand = c(0.0001, 0.01)
  ) +
  scale_color_manual(
    name = "",
    values = c(
      "#e41a1c",
      "#377eb8",
      "#4daf4a",
      "#984ea3",
      "#ff7f00",
      "#ca0020",
      "#a65628",
      "#f781bf",
      "#999999"
    )
  ) +
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

### Panel plot ####
grob1 <- ggplotGrob(roc.means.plot + theme(legend.title = element_blank(),
                                         legend.position = "bottom",
                                         legend.box = "horizontal"))$grobs

legend1 <- grob1[[which(sapply(grob1, function (x) x$name) == "guide-box")]]

roc.row <-
  plot_grid(
    roc.means.plot + theme(legend.position = "none"),
    roc.gender.plot + theme(legend.position = "none"),
    labels = c("A", "B"),
    label_size = 24,
    align = "vh",
    hjust = -0.5,
    nrow = 1
  )

roc.plot <- plot_grid(roc.row, legend1, ncol = 1, rel_heights = c(1, 0.15))
save_plot(paste(my_dir, "/ROC.svg", sep = ""),
          roc.plot,
          base_width = 14,
          base_height = 6)

