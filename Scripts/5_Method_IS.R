###
### Ideological Scaling Approach ###
###

# Analysis ----

## Selecting Speeches and Loading into Quanteda Corpus Class ----

corpus_wordfish <- quanteda::corpus(df_base, 
                                    text_field = "speechContent_stemmed")

docnames(corpus_wordfish) <- df_base$lastName

summary(corpus_wordfish, n = 1000) %>% as_tibble() -> df_wordfish_running

#Creating Document-Feature Matrix and removing all words used less than 3 times

corpus_wordfish %>% 
  tokens() %>% 
  dfm() %>% 
  dfm_trim(.,min_termfreq = 3) %>% 
  textmodel_wordfish(., sparse = T) -> tmod_wf

# Summarising findings in dataframe

tmod_wf[c('docs','theta', 'alpha')] %>%  
  as_tibble() %>% 
  right_join(., df_wordfish_running, by = c('docs' = 'Text')) %>% 
  mutate(date = as.Date(date)) -> df_wordfish_speech

# Calculating IGO Index

df_wordfish_speech %>% 
  group_by(session) %>% 
  mutate(session_mean = mean(theta)) %>% 
  ungroup() %>% 
  group_by(session, Party) %>%
  mutate(party_mean_session = mean(theta)) %>%
  ungroup() %>%
  mutate(Party_share = case_when(
    Party == 'CDU/CSU' ~ 0.34,
    Party == 'Greens' ~ 0.094,
    Party == 'DIE LINKE.' ~ 0.097,
    Party == 'FDP' ~ 0.11,
    Party == 'AFD' ~ 0.12,
    Party == 'SPD' ~ 0.21,
  )) %>%
  group_by(session) %>% 
  mutate(IGO_session = sqrt(sum(((party_mean_session - session_mean)/6)**2))) %>%
  mutate(IGO_session_share = sqrt(sum(Party_share * ((party_mean_session - session_mean)/6)**2))) %>%
  ungroup() -> df_IGO

# Merging with df_base

df_IGO %>% 
  select(speechContent, IGO_session, IGO_session_share) %>% 
  right_join(., df_base, by = c("speechContent")) -> df_base

# Plots ----

# Polarisation over Time (Session)
df_IGO %>% 
ggplot() +
  geom_line(aes(x = date, y = IGO_session)) +
  geom_point(aes(x = date, y = IGO_session)) +
  geom_line(aes(x = date, y = IGO_session_share)) +
  geom_point(aes(x = date, y = IGO_session_share)) 

# Attention: Dimension is not good

df_IGO %>% 
  group_by(date, Ruling_Party) %>% 
  summarise(party_position = mean(theta)) %>% 
  ggplot() +
  geom_line(aes(x = date, y = party_position, color = as.character(Ruling_Party)))

df_IGO %>%
  select(Ruling_Party, theta) %>%
  group_by(Ruling_Party) %>%
  summarize(mean = mean(theta))


