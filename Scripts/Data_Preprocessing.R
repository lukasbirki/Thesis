library(tidyverse)
library(tidytext)
library(SnowballC)
library(gridExtra)
library(grid)
library(ggplot2)

rm(list = ls())

#Preparation ----

## Loading Data ----

speeches <- as_tibble(read_csv("./data/speeches.csv"))

## Data Wrangling ----

speeches %>% 
  filter(electoralTerm == 19 & positionShort == 'Member of Parliament') %>% 
  mutate(count_words = sapply(strsplit(.$speechContent, " "), length)) %>% 
  filter(count_words <1000 & count_words > 100)  %>% 
  filter(factionId %in% c(0, 3, 4,6, 13,23)) %>% 
  mutate(Party = case_when(
    factionId == 0 ~ "AFD",
    factionId == 3 ~ "Greens",
    factionId == 4 ~ "CDU/CSU",
    factionId == 6 ~ "DIE LINKE.",
    factionId == 13 ~ "FDP",
    factionId == 23~ "SPD",
    TRUE ~ NA_character_
  ))  %>% mutate(Ruling_Party = case_when(
    Party == "CDU/CSU" | Party == "SPD" ~ 1,
    TRUE ~ 0
  ))-> speeches_thesis


#write.csv(speeches_thesis,"./results/dataset_speeches.csv", row.names = FALSE)
