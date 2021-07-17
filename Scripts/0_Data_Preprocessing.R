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
require(devtools)
library(glmnet)
library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(readtext)

rm(list = ls())

source("Scripts/helper_functions.R")

#Preparation ----

## Loading Data

#Speeches Data
speeches <- as_tibble(read_csv("./data/speeches.csv"))

# DIP_Selection
df_DIP_filter<- read_docx("data/Dip-Export.docx")

# Data Wrangling ----
  
## Speeches Data Set----

### Filtering I: Basic characteristics ----

speeches %>% 
  filter(electoralTerm == 19) %>% 
  mutate(count_words = sapply(strsplit(.$speechContent, " "), length)) %>% #Counting words for the first time
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
  mutate(date = as.character(date))-> df_speeches

### Preprocessing: DIP Database ----

#Filtering and Reshaping
df_DIP_filter %>% 
  docx_summary() %>% 
  as_tibble()  %>% 
  filter(style_name == "heading 1" | grepl('Datum', text), style_name != "HeaderStandard") %>% # Filter Date and 
  select(style_name, text, doc_index) %>% 
  pivot_wider(names_from = style_name, values_from = text) %>% 
  mutate(index_to_merge = rep(1:(nrow(.)/2), each = 2)) %>% 
  group_by(index_to_merge) %>% 
  summarise_all(list(~trimws(paste(., collapse = '')))) -> r 

r %>% 
  select(-1:-2) %>% 
  rename(c('MP' = 'heading 1', 'date'='Werte' )) %>% 
  mutate(date = str_remove_all(date, "^NADatum: ")) %>% 
  mutate(date = str_replace_all(date, '[.]','-')) %>% 
  mutate(MP = str_sub(.$MP, 1, str_length(.$MP)-2)) %>% 
  splitstackshape::cSplit(., "MP", sep = ",", type.convert = F) %>% 
  mutate(last_name = word(MP_1 , -1)) %>% 
  mutate(last_name = case_when(
    last_name == "Ali" ~ "Mohamed Ali",
    T ~ last_name
  )) %>% 
  as_tibble()-> r2

r2 %>% 
  rowwise() %>% 
  mutate(across(c(date), reverse_words_helper))  %>% 
  select(-MP_1,-MP_2, -MP_4, -MP_5) %>% 
  rename(c('Party' = 'MP_3', 'lastName' = 'last_name')) %>% 
  mutate(Party = case_when(
  Party == 'SPD' ~ 'SPD',
  Party == 'CDU/CSU' ~ 'CDU/CSU',
  Party == 'BÜNDNIS 90/DIE GRÜNEN' ~ 'Greens',
  Party == 'DIE LINKE' ~ 'DIE LINKE.',
  Party == 'AfD' ~ 'AFD',
  Party == 'FDP' ~ 'FDP',
  T ~ NA_character_))  %>% 
  mutate(lastName = case_when(
    lastName == 'Kotre' ~ 'Kotré',
    T ~ lastName
  )) %>% 
  drop_na()->  df_docx_final

### Merging: Left Join Speeches with DIP into df_base ----

df_base <-  right_join(df_speeches, df_docx_final, by = c('date', 'lastName', 'Party')) %>% 
  distinct() %>% 
  filter(duplicated(.) == 'FALSE') %>% 
  as_tibble() %>% 
  select(-positionShort, -positionLong, -electoralTerm, -factionId) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(speechContent = str_remove_all(speechContent, "\\s*\\([^\\)]+\\)")) %>% 
  mutate(speechContent = str_remove_all(speechContent, "\n\n")) %>% 
  mutate(speechContent = str_remove_all(speechContent, greetings )) %>% # Filter greetings (cf. helper_functions.R)
  mutate(speechContent = str_replace_all(speechContent,'\\s+', ' ')) %>% # Remove more than one empty space
  mutate(count_words = sapply(strsplit(.$speechContent, " "), length)) %>% 
  drop_na()#count_words

## Filtering with descriptive statistics ----

df_base %>% 
  count(session, Ruling_Party) %>% 
  pivot_wider(id_cols = session,names_from = Ruling_Party, values_from =n ) %>% 
  mutate(n = `0`+`1`) %>% 
  mutate(share_government = `1`/n) %>% 
  mutate(Removed = case_when(
    (session == 195 |  session == 186 |session == 187 |session == 202) ~ "Removed",
    T ~ "Not Removed"
  ))  %>% 
  xtable::xtable() %>% 
  print(., file = "./Figures/Tables/ratio_sessions.tx")

#Dropping the following debates due to inbalanced/to few data ponits

#195 -> 81.8 % Government
#186 -> 75.0 % Government
#187 -> 37 % Government speeches
#202 -> 40 % Government

#Still Open: 173 iwth 16 speeches

df_base %>% 
  filter(session != 195 & session != 186 & session != 187 & session != 202 ) -> df_base

#df_base %>% write.csv(., file = "data/df_base.csv")

# Descriptive Statistics ----

## Latex: Number of Sessions ----

df_base %>% 
  count(session) %>% 
  right_join(., df_base,  by = 'session') %>% 
  select(date, session, n) %>% 
  mutate(date = as.character(date)) %>% 
  distinct() %>% 
  xtable::xtable() %>% 
  print(., file = "./Figures/Tables/table_sessions.tx")

## Plot: Number of Speeches per Session ----

plot_speeches_per_session <- df_base %>% 
  mutate(date = as.character(date)) %>% 
  ggplot() + geom_bar(aes(date, fill = factor(Party, 
                                              levels =  c("AFD","DIE LINKE." , "Greens","FDP" ,"SPD"  ,"CDU/CSU"))), position  = 'stack', width = 0.8) +
  scale_fill_manual(breaks = c("CDU/CSU", "DIE LINKE.", "FDP", "AFD", "Greens", "SPD"), 
                    values=c("#000000", "#BE3075", "#FFFF00",  "#009EE0",  "#64A12D","#FF0000"),
                    name = 'Party') +
  ggtitle("Total Count of Party Distribution per Session" ) +
  labs(x = "Date of Session", y = "Total Count of Speeches") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=15, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 11),
    legend.position = "top") + guides(colour = guide_legend(nrow = 1))

ggsave("./Figures/Figures/Count_per_Session.png", plot = plot_speeches_per_session, height = 9)

## Plot: Timeline ----
#https://stackoverflow.com/questions/7492274/draw-a-chronological-timeline-with-ggplot2

df_base %>% 
  count(session) %>% 
  right_join(., df_base,  by = 'session') %>% 
  select(date, session, n) %>% 
  distinct() -> df_plot

df_plot$randoms <- c(0.12, 0.22, 0.13, -0.4, 0.40, 
                     -0.3, 0.12, 0.45, -0.2, 0.4, 
                     -0.1, 0.3, 0.23, -0.3, 0.17, 
                     0.42, 0.35, 0.43, -0.29, 0.2, 
                     -0.2)
                      
 vjust = ifelse(df_plot$randoms > 0, -1, 1.5)

p2 <- df_plot %>% 
  ggplot(aes(date, randoms)) +
  ggalt::geom_lollipop(point.size = 1) +
  geom_text(aes(x = date, y = randoms, label = as.character(date)), data = df_plot,
            hjust = 0, vjust = vjust, size = 2.5) +
  expand_limits(x = c(lubridate::ymd(20200101), lubridate::ymd(20201224)), y = c(-0.5, 0.5)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 9)) +
  ggtitle("Temporal Distribution of Session from January 01 - December 24, 2020" ) +
  labs(x = "Time", y = "Event") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=15, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_text(size = 10)
  )

plot_timeline <- shift_axis(p2, lubridate::ymd(20200101), lubridate::ymd(20201224))
ggsave("./Figures/Figures/Timeline.png", plot = plot_timeline, height = 5)

## Plot: Count of Words per Speech ----

plot_speech_leght <- df_base %>% 
  ggplot(aes(count_words)) +
  geom_histogram(color = "black", fill = "gray", bins = 75) +
  ylab("Frequency") + xlab("Count of Words in a Speech") +
  ggtitle('Count of Words per Speech') +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=15, hjust = 0.5)
  ) 
ggsave("./Figures/Figures/Speech_lenght.png", plot = plot_speech_leght, height = 9)

rm(plot_speeches_per_session, df_plot, plot_timeline, plot_speech_leght, p2)


df_train <-  anti_join(df_speeches, df_docx_final, by = c('date', 'lastName', 'Party')) %>% 
  distinct() %>% 
  filter(duplicated(.) == 'FALSE') %>% 
  as_tibble() %>% 
  select(-positionShort, -positionLong, -electoralTerm, -factionId) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(speechContent = str_remove_all(speechContent, "\\s*\\([^\\)]+\\)")) %>% 
  mutate(speechContent = str_remove_all(speechContent, "\n\n")) %>% 
  mutate(speechContent = str_remove_all(speechContent, greetings )) %>% # Filter greetings (cf. helper_functions.R)
  mutate(speechContent = str_replace_all(speechContent,'\\s+', ' ')) %>% # Remove more than one empty space
  mutate(count_words = sapply(strsplit(.$speechContent, " "), length)) %>% 
  drop_na()#count_words

df_train %>% write.csv(., file = "data/df_train.csv")

rm(r, r2, df_docx_final,df_DIP_filter, speeches, df_speeches)

