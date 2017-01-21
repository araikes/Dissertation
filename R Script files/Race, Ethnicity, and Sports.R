#### Getting race, ethnicity, and athletic experience for the two studies ####

#### Create clean workspace ####
rm(list = ls())

#### Load libraries ####
library(tidyverse)
library(cowplot)
library(extrafont)

#### Load functions ####
source("./R Script files/Helper functions.R")

# Load participant information
source("./R Script files/Participant information.R")

#### Load outcomes data ####
trial.summary <- read.csv("./Data Files/Trial Summary.csv",
                          header = TRUE, sep = ",")


#### Identify participants and trials to be excluded ####
# Removal is based on criteria:
# 1. Number of visible points per trial is > 20%
# 2. Trials do not include areas of 0 slope, indicating removal of the finger from the load cell.
# 3. Participants are removed if the number of valid trials < 6.
bad.trials <- read.csv("./Data Files/Excluded Trials.csv",
                       header = TRUE, sep = ",")

valid.trials <- trial_inclusion(trial.summary, bad.trials)

rm(list = c("bad.trials"))


#### Composite data frame ####
participants <- semi_join(participants, valid.trials) %>%
  select(id, block, order, gender, hand, gamer, prior.concussion, 
         LOC, amnesia, sx.current, age, height, weight, diagnosed.number, 
         suspected.number, concussion.number, diagnosed.recent, suspected.recent) %>%
  filter(sx.current != "Yes" | is.na(sx.current)) %>%
  filter(hand != "Left")

valid.participants <- select(participants, id, order, gender, hand, gamer, prior.concussion,
                             age, height, weight)

#### Get Race/Ethnicity ####
race.eth <- read.csv("Data Files/Participant Info.csv") %>%
  select(id, hisp, hisp.nationality, race)

first.fifty <- valid.participants %>%
  left_join(race.eth) %>%
  arrange(order) %>%
  filter(row_number(order) <= 50)  %>%
  group_by(prior.concussion, race) %>%
  summarise(count = n())

full <- valid.participants %>%
  left_join(race.eth) %>%
  arrange(order) %>%
  group_by(prior.concussion, race) %>%
  summarise(count = n())

full2 <- valid.participants %>%
  left_join(race.eth) %>%
  arrange(order) %>%
  group_by(prior.concussion, hisp.nationality) %>%
  summarise(count = n())

#### Chapter 3 Athletic history ####
comp.hx <- read.csv("Data Files/Comp Athletic Hx.csv")

first.fifty.ath <- valid.participants %>%
  left_join(comp.hx) %>%
  arrange(order) %>%
  filter(row_number(order) <= 50) %>%
  select(-hand, -gamer, -order, -age, -height, -weight, -block) %>%
  gather(Sport, value, -id, -gender, -prior.concussion) %>%
  mutate(Sport = gsub("[.]", "_", Sport)) %>%
  separate(Sport, c("Sport", "Var"), "_", extra = "drop", fill = "right") %>%
  filter(!is.na(value))

first.fifty.years <- group_by(first.fifty.ath, id, prior.concussion) %>%
  summarise(years = sum(value))

first.fifty.years[nrow(first.fifty.years) + 1,] <- c(10000, "No", 0)

t.test(as.numeric(first.fifty.years$years) ~ first.fifty.years$prior.concussion)
group_by(first.fifty.years, prior.concussion) %>%
  summarise(m = mean(as.numeric(years)),
            s = sd(as.numeric(years)))

first.fifty.sports <- group_by(first.fifty.ath, gender, prior.concussion, Sport) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(gender, prior.concussion, desc(count)) %>%
  filter(Sport != "marchingband") %>%
  mutate(order = row_number(),
         Sport2 = c("Gymnastics", "Soccer", "Cross Country", "Softball",
                    "Basketball", "Cheer", "Dance", "Drill", "Lacrosse",
                    "Swimming", "Tennis", "Track", "Volleyball", "Soccer", "Basketball",
                    "Track", "Dance", "Volleyball","Cross Country", "Swimming",
                    "Gymnastics", "Lacrosse","Ultimate Frisbee", "Baseball", 
                    "Mountain Biking", "Softball", "Surfing", "Tennis", "Waterpolo",
                    "Weightlifting", "Football", "Soccer", "Baseball", "Basketball",
                    "Flag Football", "Roller Hockey", "Swimming", "Tennis", "Track", 
                    "Cross Country", "Basketball", "Football", "Soccer", "Track",
                    "Baseball", "Wrestling", "Swimming", "T-ball", "Waterpolo", 
                    "Cross Country", "Hockey", "Lacrosse", "Racquetball",
                    "Tennis", "Ultimate Frisbee")) %>%
  mutate(label = ifelse(gender == "Female" & prior.concussion == "No", "Female: No Concussion History",
                        ifelse(gender == "Female" & prior.concussion == "Yes", "Female: Previously Concussed",
                               ifelse(gender == "Male" & prior.concussion == "No", "Male: No Concussion History",
                                      "Male: Previously Concussed"))))

p1 <- ggplot(data = first.fifty.sports, aes(x = order, y = count)) +
  geom_bar(stat = "identity", fill = "#0571b0") +
  facet_wrap(~label, scales = "free") +
  theme_classic() + 
  scale_x_continuous(
    breaks = first.fifty.sports$order,
    labels = first.fifty.sports$Sport2,
    expand = c(0,0)) +
  coord_flip() +
  xlab("Sport") +
  ylab("Number of Participants")

p1 <- p1 + theme(strip.background = element_rect(colour = "white", fill = "grey"),
                 title = element_text(family ="Arial", size = 20),
                 axis.text = element_text(family ="Arial", size = 20),
                 strip.text = element_text(size = 20, family = "Arial"))

my_dir <- "C:/Users/Adam/Dropbox/Adam/Dissertation/Images/Chapter 3"
save_plot(paste(my_dir, "/Sports.svg", sep = ""),
          p1,
          base_height = 12,
          base_width = 18)

#### Chapter 4 Athletic history ####
ch4.ath <- valid.participants %>%
  left_join(comp.hx) %>%
  arrange(order) %>%
  select(-hand, -gamer, -order, -age, -height, -weight, -block) %>%
  gather(Sport, value, -id, -gender, -prior.concussion) %>%
  mutate(Sport = gsub("[.]", "_", Sport)) %>%
  separate(Sport, c("Sport", "Var"), "_", extra = "drop", fill = "right") %>%
  filter(!is.na(value))

ch4.years <- group_by(ch4.ath, id, prior.concussion) %>%
  summarise(years = sum(value))

ch4.extra = data.frame(id = c(10001, 10002, 10003, 10004, 10005, 10006, 10007,
                              10008),
                       prior.concussion = c("No", "No", "No", "No", "No",
                                            "Yes", "Yes", "Yes"),
                       years = c(0,0,0,0,0,0,0,0))

ch4.years <- bind_rows(ch4.years, ch4.extra)
t.test(ch4.years$years ~ ch4.years$prior.concussion)

ch4.years %>%
  group_by(prior.concussion) %>%
  summarise(m = mean(years),
            s = sd(years))


ch4.sports <- group_by(ch4.ath, gender, prior.concussion, Sport) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(gender, prior.concussion, desc(count)) %>%
  filter(Sport != "marchingband") %>%
  mutate(order = row_number(),
         Sport2 = c("Soccer", "Track", "Cross Country", "Dance", "Lacrosse",
                    "Volleyball", "Cheer", "Gymnastics", "Softball", "Basketball",
                    "Tennis", "Golf", "Swimming", "Baseball", "Drill", "Horseback Riding",
                    "T-ball", "Soccer", "Basketball", "Track", "Volleyball", "Dance", "Tennis",
                    "Cheer", "Gymnastics", "Swimming", "Softball", "Cross Country", "Lacrosse", "Running",
                    "Ultimate Frisbee", "Baseball", "Drill", "Flag Football", "Golf", "Horseback Riding", 
                    "Karate", "Mountain Biking", "Rock Climbing", "Surfing", "Waterpolo", "Weightlifting",
                    "Soccer", "Football", "Cross Country", "Basketball", "Track","Baseball", "Swimming", 
                    "Fencing", "Flag Football", "Golf", "Handball", "Lacrosse", "Roller Hockey", "Running", "Tennis",
                    "Football", "Basketball", "Soccer", "Baseball", "Track", "Wrestling", "Swimming",
                    "Cross Country", "Rugby", "Tennis", "Ultimate Frisbee", "Hockey", "Lacrosse", 
                    "T-ball", "Waterpolo", "Golf", "Karate", "Racquetball", "Rodeo", 
                    "Tae Kwon Do", "Volleyball", "Cross Country Skiing", "Weightlifting")) %>%
  mutate(label = ifelse(gender == "Female" & prior.concussion == "No", "Female: No Concussion History",
                        ifelse(gender == "Female" & prior.concussion == "Yes", "Female: Previously Concussed",
                               ifelse(gender == "Male" & prior.concussion == "No", "Male: No Concussion History",
                                      "Male: Previously Concussed"))))

p2 <- ggplot(data = ch4.sports, aes(x = order, y = count)) +
  geom_bar(stat = "identity", fill = "#0571b0") +
  facet_wrap(~label, scales = "free") +
  theme_classic() + 
  scale_x_continuous(
    breaks = ch4.sports$order,
    labels = ch4.sports$Sport2,
    expand = c(0,0)) +
  coord_flip() +
  xlab("Sport") +
  ylab("Number of Participants")

p2 <- p2 + theme(strip.background = element_rect(colour = "white", fill = "grey"),
                 title = element_text(family ="Arial", size = 20),
                 axis.text = element_text(family ="Arial", size = 20),
                 strip.text = element_text(size = 20, family = "Arial"))

my_dir <- "C:/Users/Adam/Dropbox/Adam/Dissertation/Images/Chapter 4"
save_plot(paste(my_dir, "/Sports.svg", sep = ""),
          p2,
          base_height = 20,
          base_width = 18)

