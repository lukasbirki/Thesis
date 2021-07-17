library(feather)
df <- arrow::read_feather('data/topic_model_speech_values.feather')
df %>% 
  filter(electoralTerm == 19) %>%
  
  