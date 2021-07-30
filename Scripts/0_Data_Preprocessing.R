# DIP Data ----

# Filtering and Reshaping
DIP %>% 
  docx_summary() %>% 
  as_tibble()  %>% 
  filter(style_name == "heading 1" | grepl('Datum', text), style_name != "HeaderStandard") %>% # Filter Date and 
  select(style_name, text, doc_index) %>% 
  pivot_wider(names_from = style_name, values_from = text) %>% 
  mutate(index_to_merge = rep(1:(nrow(.)/2), each = 2)) %>% 
  group_by(index_to_merge) %>% 
  summarise_all(list(~trimws(paste(., collapse = '')))) %>% 
  select(-1:-2) -> r1

r1 %>% 
  plyr::rename(c('heading 1' = 'MP' , 'Werte' = 'date')) %>% 
  mutate(date = str_remove_all(date, "^NADatum: ")) %>% 
  mutate(date = str_replace_all(date, '[.]','-')) %>% 
  mutate(MP = str_sub(.$MP, 1, str_length(.$MP)-2)) %>% 
  mutate(MP = str_remove_all(MP, " \\s*\\([^\\)]+\\)")) %>% 
  splitstackshape::cSplit(., "MP", sep = ",", type.convert = F) %>% 
  mutate(last_name = word(MP_1 , -1)) %>% 
  mutate(last_name = case_when(
    last_name == "Ali" ~ "Mohamed Ali",
    T ~ last_name)) %>% 
  as_tibble() -> r2 

plyr::ldply(r2$date, reverse_words_helper) %>% 
  cbind(.,r2) %>% 
  as_tibble %>% 
  select(-date) %>% 
  plyr::rename(c('V1' = 'date')) %>% 
  select(-MP_1,-MP_2, -MP_4, -MP_5) %>% 
  plyr::rename(c( 'MP_3' = 'Party',  'last_name' = 'lastName')) %>% 
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
    lastName == 'Lambsdorff' ~ 'Graf Lambsdorff',
    lastName == 'Marschall' ~ 'von Marschall',
    lastName == 'Masi' ~ 'De Masi',
    lastName == 'Beeck' ~ 'in der Beek',
    T ~ lastName)) %>%  #Drops non-normal MPs
  drop_na() ->  df_docx_final

df_docx_final %>% 
  filter(duplicated(.) == 'FALSE') %>% 
  mutate(date = as.Date(date)) -> df_docx_final_without_duplicates

#Sessions where one speaker speaks more than one time
df_docx_final %>% 
  filter(duplicated(.) == 'TRUE') %>%
  mutate(date = as.Date(date))-> df_docx_final_with_duplicates


df_speeches %>% 
  select(lastName, date, Party, speechContent) -> data

data_no_duplicates <- data[!duplicated(data[,1:3]),]
data_duplicates <- data[duplicated(data[,1:3]),]

left_join(df_docx_final_without_duplicates, data_no_duplicates) -> x1
left_join(df_docx_final_with_duplicates, data_duplicates) -> x2

rbind(x1, x2) %>% 
  mutate(date = as.Date(date))-> df_base 

# Merging: Speeches Data Set + DIP Data Set ----


right_join(df_speeches, df_base, by = c('speechContent', 'Party', 'date','lastName'))  %>% 
  select(-positionShort, -positionLong, -electoralTerm, -factionId) %>% 
  mutate(speechContent = str_remove_all(speechContent, greetings )) -> df_base

# Preprocessing II: Remove Stopwords and Punctuation; Apply Wordstemming
df_base %>%
  tidytext::unnest_tokens(word, speechContent, drop = F) %>% 
  anti_join(.,sw, by = c("word" = "value")) %>% 
  filter(str_detect(word, "[a-z]")) %>% 
  mutate(token_stem = SnowballC::wordStem(.$word, language = "german")) %>% 
  group_by(id) %>% 
  summarize(speechContent_stemmed = str_c(token_stem, collapse = " ")) %>% 
  ungroup() %>% 
  right_join(df_base, y, by = 'id') %>% 
  drop_na() %>% 
  filter(count_words <= 1250 & count_words >= 250)-> df_base

# Filtering: Descriptive Statistics ----

# Total Count 
df_base %>% 
  count(Ruling_Party)

# Total Count Government/Oppossition per Session
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

# Total Count Government/Oppossition per Party
df_base %>% 
  count(session, Party) %>% 
  pivot_wider(id_cols = session,names_from = Party, values_from =n ) %>% 
  mutate(n = AFD + `CDU/CSU` + `DIE LINKE.` + FDP + Greens + SPD) %>% 
  xtable::xtable() %>% 
  print(., file = "./Figures/Tables/ratio_party_session.tx")

#Speech Median & Mean

median(df_base$count_words)
mean(df_base$count_words)
sd(df_base$count_words)

# Speech Length

plot_speech_leght <- df_base %>% 
  ggplot(aes(count_words)) +
  geom_histogram(color = "black", fill = "gray", bins = 75) +
  ylab("Frequency") + xlab("Count of Words in a Speech") +
  ggtitle('Count of Words per Speech') +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=24, hjust = 0.5),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 17),
    axis.text.y = element_text(size = 17)
    )

ggsave("./Figures/Figures/Speech_lenght.png", plot = plot_speech_leght, height = 6)

# Temporal Distribution over time

#https://stackoverflow.com/questions/7492274/draw-a-chronological-timeline-with-ggplot2

df_base %>% 
  count(session) %>% 
  right_join(., df_base,  by = 'session') %>% 
  select(date, session, n) %>% 
  distinct() -> df_plot
df_plot$randoms <- c(0.12, 0.22, 0.13, -0.4, 0.40, 
                     -0.3, 0.12, 0.45, -0.2, 0.4, 
                     -0.1, 0.3, 0.23, -0.3, 0.17)

vjust = ifelse(df_plot$randoms > 0, -1, 1.5)
p2 <- df_plot %>% 
  ggplot(aes(date, randoms)) +
  ggalt::geom_lollipop(point.size = 1) +
  geom_text(aes(x = date, y = randoms, label = as.character(date)), data = df_plot,
            hjust = 0, vjust = vjust, size = 4) +
  expand_limits(x = c(lubridate::ymd(20200201), lubridate::ymd(20200801)), y = c(-0.5, 0.5)) +
  scale_x_date(breaks = scales::pretty_breaks(n = 9)) +
  ggtitle("Timeline (February 01 - July 31, 2020)" ) +
  labs(x = "Time") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=24, hjust = 0.5),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 17),
  )
plot_timeline <- shift_axis(p2, lubridate::ymd(20200201), lubridate::ymd(20200801))

ggsave("./Figures/Figures/Timeline.png", plot = plot_timeline, height = 5)

#Number of Speeches per Session

plot_speeches_per_session <- df_base %>% 
  mutate(date = as.character(date)) %>% 
  ggplot() + geom_bar(aes(date, fill = factor(Party, 
                                              levels =  c("AFD","DIE LINKE." , "Greens","FDP" ,"SPD"  ,"CDU/CSU"))), position  = 'stack', width = 0.8) +
  scale_fill_manual(breaks = c("CDU/CSU", "DIE LINKE.", "FDP", "AFD", "Greens", "SPD"), 
                    values=c("#000000", "#BE3075", "#FFFF00",  "#009EE0",  "#64A12D","#FF0000"),
                    name = 'Party') +
  ggtitle("Total Count of Party Distribution per Debate" ) +
  labs(x = "Date", y = "Total Count") +
  hrbrthemes::theme_ipsum() +
  theme(
    plot.title = element_text(size=24, hjust = 0.5),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 15),
    axis.text.y = element_text(size = 17),
    legend.text=element_text(size=17),
    legend.title = element_text(size = 20),
    legend.position = "top") + 
  guides(colour = guide_legend(nrow = 1))

ggsave("./Figures/Figures/Count_per_Session.png", plot = plot_speeches_per_session, height = 6)

# Number of Session (Latex)

df_base %>% 
  count(session) %>% 
  right_join(., df_base,  by = 'session') %>% 
  select(date, session, n) %>% 
  mutate(date = as.character(date)) %>% 
  distinct() %>% 
  xtable::xtable() %>% 
  print(., file = "./Figures/Tables/table_sessions.tx")

#Data Preprocessing ----

df_base %>% 
  tidytext::unnest_tokens(word, speechContent, drop = F) %>% 
  select(word) %>% 
  unique()

df_base %>% 
  tidytext::unnest_tokens(word, speechContent_stemmed, drop = F) %>% 
  select(word) %>% 
  unique()

#Write test data
df_speeches %>% 
  filter(electoralTerm == 19) -> x1 

x1 %>% 
  tidytext::unnest_tokens(word, speechContent, drop = F) %>% 
  anti_join(.,sw, by = c("word" = "value")) %>% 
  filter(str_detect(word, "[a-z]")) %>% 
  mutate(token_stem = SnowballC::wordStem(.$word, language = "german")) %>% 
  group_by(id) %>% 
  summarize(speechContent_stemmed = str_c(token_stem, collapse = " ")) %>% 
  ungroup() %>% 
  right_join(., x1, by = 'id') %>% 
  anti_join(., df_base, by = 'id') %>% 
  write.csv(., file = "data/df_train_complete.csv")


rm(DIP, 
  additional_stopwords,
  df_docx_final,
  df_docx_final_with_duplicates,
  df_docx_final_without_duplicates, 
  x1, 
  r1,
  r2,
  x2,
  sw, 
  data_no_duplicates,
  data_duplicates,
  plot_speeches_per_session, 
  df_plot, 
  plot_timeline, 
  plot_speech_leght, 
  p2)
