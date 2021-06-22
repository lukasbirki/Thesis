library(tidyverse)
library(tidytext)
library(SnowballC)
library(gridExtra)
library(grid)
library(ggplot2)
library(stringi)

rm(list = ls())

#Preparation ----

## Loading Data ----

speeches <- as_tibble(read_csv("./data/speeches.csv"))
#factions <- as_tibble(read_csv("./csv/factions.csv"))
#politicans <- as_tibble(read_csv("./csv/politicians.csv"))

## Data Wrangling ----

#Filtering

speeches %>% 
  filter(electoralTerm == 19 & 
           positionShort == 'Member of Parliament' & 
           date > "2020-01-27") %>% 
  stri_replace_all_fixed(.$speechContent, pattern = c("\n", "(", ")", "{", "}"), replacement = c("")) %>% 
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

speeches_thesis[speeches_thesis$id == 1398512]

write.csv(speeches_thesis,"./results/dataset_speeches.csv", row.names = FALSE)
