# Data Validation ----

#Loading Data

df_handcoded  <- haven::read_dta("data/multilevel_nov20.dta")

df_gesis <- read.csv('/Users/lukas/OneDrive - University of Warwick/Dokumente/Uni/Thesis/Thesis/data/ZA7655_v4-0-0.csv', sep = ';')

##  Hand-Coded Speeches ----
#Official Dataset as in Open Science 
df_validation <- readxl::read_excel("data/Coding_Germany_for_stata.xlsx")

df_validation %>%
  filter(chair != 1 & interruption == 0) %>% 
  filter(score != 99) %>% 
  group_by(date, speaker_lastname) %>%
  summarise(validation_speech = mean(score), .groups = 'keep' ) %>% 
  drop_na()-> df_validation_scores

### Merging Sessions ----
## Individual Sentiment Scores 

df_handcoded %>% 
  splitstackshape::cSplit(., "speaker_name", sep = " ", type.convert = F) %>% 
  mutate(last_name = word(speaker_name_2 , -1)) %>% 
  mutate(last_name = case_when(
    last_name == "Mohamed" ~ "Mohamed Ali",
    T ~ last_name)) %>% 
  as_tibble() %>% 
  left_join(., df_validation_scores, by = c("date" = "date", "last_name" = "speaker_lastname" )) %>% 
  select(date, validation_speech,last_name,party_id) %>% 
  distinct() -> df_handcoded_2

df_handcoded_2 %>% 
  mutate(Party = case_when(
    party_id == 303 ~ "FDP",
    party_id == 304 ~ "Greens",
    party_id == 305 ~ "DIE LINKE.",
    party_id == 306 ~ "AFD",
    TRUE ~ NA_character_)) %>% 
  rename('lastName' = 'last_name') %>% 
  mutate(lastName = case_when(
    lastName ==  'Kotre' ~ 'Kotré',
    T ~ lastName
  )) %>% 
  right_join(., df_base, by = c('date', 'lastName', 'Party')) %>% 
  distinct()-> df_base_running

df_base_running %>% 
  aggregate(validation_speech ~ date, data = ., mean) %>% 
  rename('validation_session' = 'validation_speech') %>% 
  right_join(., df_base_running, by = 'date') %>% 
  as_tibble() %>% 
  mutate_at(vars(validation_session, validation_speech), funs(. * -1))-> df_base 

rm(df_base_running, df_handcoded_2, df_handcoded, df_validation, df_validation_scores)

## GESIS Dissatisfaction Survey ----
#Question: How do you rate the work of the federal government of CDU/CSU and SPD overall?  

df_gesis %>% 
  select(V0, datum, monat) %>% 
  mutate(satisfaction = case_when(
    V0 == 'sehr gut' ~ 1,
    V0 == 'eher gut' ~ 2,
    V0 == 'eher schlecht' ~ 3,
    V0 == 'sehr schlecht' ~ 4,
    T ~ NA_real_
  )) %>% 
  mutate(monat = case_when(
    monat == 'Januar' ~ as.Date('2020-01-01'),
    monat == 'Februar' ~ as.Date('2020-02-01'),
    monat == 'M\xe4rz' ~ as.Date('2020-03-01'),
    monat == 'April' ~ as.Date('2020-04-01'),
    monat == 'Mai' ~ as.Date('2020-05-01'),
    monat == 'Juni' ~ as.Date('2020-06-01'),
    monat == 'Juli' ~ as.Date('2020-07-01'),
    monat == 'August' ~ as.Date('2020-08-01'),
    monat == 'September' ~ as.Date('2020-09-01'),
    monat == 'Oktober' ~ as.Date('2020-10-01'),
    monat == 'November' ~ as.Date('2020-11-01'),
    monat == 'Dezember' ~ as.Date('2020-12-01'),
    T ~ NA_real_
  )) %>% 
  group_by(monat) %>% 
  summarize(dissatisfaction = mean(satisfaction, na.rm=TRUE)) %>% 
  mutate(value = scale(dissatisfaction)) -> DF_gesis_runnig



# Plots ---- 

## Plot: Validation Session over Time ----

p_validation <- df_base %>% 
  select(date, validation_session) %>% 
  distinct() %>% drop_na() %>% 
  ggplot() + geom_line(aes(x = date, y = validation_session)) + 
  geom_point(aes(x = date, y = validation_session)) +
  ggtitle("Validation Scores for Sessions (N = 6)") +
  labs(x = "Date of Session", y = "Polarisation Score (Validation)") +
  hrbrthemes::theme_ipsum(grid = 'Y') +
  theme(
    plot.title = element_text(size=15, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 11),
    legend.position = "top") + guides(colour = guide_legend(nrow = 1))

ggsave("./Figures/Figures/validation.png", plot = p_validation, height = 9)
