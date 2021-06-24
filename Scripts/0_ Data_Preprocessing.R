library(tidyverse)
library(tidytext)
library(SnowballC)
library(gridExtra)
library(grid)
library(ggplot2)
library(haven)
library(officer)
library(stringr)
library(splitstackshape)

rm(list = ls())

source("helper_functions.R")

#Preparation ----

## Loading Data ----

#Speeches Data
speeches <- as_tibble(read_csv("./data/speeches.csv"))

#Hand-Coded Data
df_handcoded <- read_dta("./data/multilevel_nov20.dta")

# DIP_Selection
df_DIP_filter<- read_docx("data/Dip-Export.docx")

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
  ))-> df_speeches



#Filtering and Reshaping
dip_selection %>% 
  docx_summary() %>% 
  as_tibble() %>% 
  filter(style_name == "heading 1" | grepl('Datum', text)) %>% # Filter Date and 
  filter(style_name != "HeaderStandard") %>% 
  select(style_name, text, doc_index) %>% 
  pivot_wider(names_from = style_name, values_from = text) -> df_docx_wide

df_docx_wide %>% 
  mutate(index_to_merge = rep(1:(nrow(.)/2), each = 2)) %>% 
  group_by(index_to_merge) %>% 
  summarise_all(funs(trimws(paste(., collapse = '')))) %>% 
  select(-1:-2) %>% 
  rename(c('MP' = 'heading 1', 'date'='Werte')) %>% 
  mutate(date_final = str_remove_all(date, "^NADatum: ")) %>% 
  mutate(date_final = str_replace_all(date_final, '[.]','-')) %>% 
  mutate(MP = str_sub(.$MP, 1, str_length(.$MP)-2)) %>% 
  splitstackshape::cSplit(., "MP", sep = ",") %>% 
  mutate(last_name = stringi::stri_extract_last_words(MP_1)) %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(across(c(date_final), reverse_words_helper)) -> df_docx_final

speeches_thesis$date <- as.character(speeches_thesis$date)
names(df_docx_final)[6] <- "lastName"
names(speeches_thesis)[12] <- "date_final"


final_df <-  speeches_thesis %>% 
  merge(.,df_docx_final, by = c('date_final', 'lastName')) %>% 
  as_tibble() %>% 
  select(date_final, speechContent, session, MP_1, MP_2, MP_3, positionShort) %>% 
  filter(positionShort == "Member of Parliament")

#write.csv(speeches_thesis,"./results/dataset_speeches.csv", row.names = FALSE)
