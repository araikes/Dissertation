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
  
library(signal)
trial <- as.numeric(rawtrace[3,1:4680])
specgram(trial)

bf <- butter(5, 0.05, type = "low", plane = "z")
trial2 <- filtfilt(bf, trial)
specgram(trial2)

library(hht)
time <- seq(1:4680)

ee <- EEMD(trial, time, 250, 100, 12, "trials")
eec <- EEMDCompile("trials", 100, 12)
plot(eec$tt, eec$averaged.imfs[,12], type = "l")

emd.result <- Sig2IMF(trial, time)
PlotIMFs(emd.result, imf.list = 2, time.span = c(1, 72),
         original.signal = TRUE, residue = TRUE,
         fit.line = TRUE)

dfreq <- 1/72
hspec <- HHSpectrum(emd.result, dfreq)
HHSpecPlot(hspec, show.imfs = TRUE)

plot(trial, lwd = 3, col = "gray")
lines(c(eec$tt), emd.result$residue + apply(emd.result$imf[,2:dim(emd.result$imf)[2]], 1, sum), lwd = 2)
