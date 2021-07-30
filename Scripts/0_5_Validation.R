# Data Validation ----

df_handcoded  <- haven::read_dta("data/multilevel_nov20.dta")
df_validation <- readxl::read_excel("data/Coding_Germany_for_stata.xlsx")

df_validation %>%
  filter(chair != 1 & interruption == 0) %>% 
  filter(score != 99) %>% 
  group_by(date, speaker_lastname) %>%
  summarise(validation_speech = mean(score), .groups = 'keep' ) %>% 
  drop_na()-> df_validation_scores

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
  mutate_at(vars(validation_session, validation_speech), list( ~. * -1))-> df_base # Rearrange algebraic sign

rm(df_base_running, 
   df_handcoded_2, 
   df_handcoded, 
   df_validation, 
   df_validation_scores)

# # Plots ---- 

## Plot: Validation Session over Time ----

p_validation <- df_base %>%
  select(date, validation_session) %>%
  distinct() %>% drop_na() %>% 
  ggplot() +
  geom_line(aes(x = date, y = scale(validation_session))) +
  geom_point(aes(x = date, y = scale(validation_session))) +
  ggtitle("Validation Scores for Sessions (N = 6)") +
  labs(x = "Date", y = "Polarisation (Validation)") +
  hrbrthemes::theme_ipsum(grid = 'Y') +
  theme(
    plot.title = element_text(size=24, hjust = 0.5),
    axis.text.x = element_text(size = 17, hjust = 1),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.y = element_text(size = 17),
    legend.position = "top")

ggsave("./Figures/Figures/validation.png", plot = p_validation, height = 5)
