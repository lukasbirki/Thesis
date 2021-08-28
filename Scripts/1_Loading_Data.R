# Part 1: Data Preprocessing

rm(list = ls())

source("Scripts/helper_functions.R") # Load Packages and helper functions

# Loading Data

as_tibble(read_csv("./data/speeches.csv")) %>% 
  filter(electoralTerm == 19) %>% 
  mutate(count_words = sapply(strsplit(.$speechContent, " "), length)) %>% #Counting words 
  filter(count_words > 100 | count_words > 1413 )  %>% 
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
    TRUE ~ 0)) %>% 
  mutate(date = as.Date(date))-> df_speeches

read_docx("data/DIP_Export.docx") -> DIP

