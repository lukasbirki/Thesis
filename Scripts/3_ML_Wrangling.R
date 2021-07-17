
###
### ML Approach ###
###

df_base_ML <- as_tibble(read_csv("./data/df_base_ML.csv", col_names = ))

j <- 1
confusion_martices <- list()
for (i in sort(unique(as.character(df_base_ML$session)))){
  q <- caret::confusionMatrix(data      = factor(df_base_ML$Ruling_Party[df_base_ML$session == i]),
                       reference = factor(df_base_ML$SMV_predictions[df_base_ML$session == i]),
                       positive  = "1")$tables
  confusion_martices[[j]] <- q
  j <- j + 1
}


j <- 1
accuracy_scores <- list()
for (i in sort(unique(as.character(df_base_ML$session)))){
  q <- caret::confusionMatrix(data      = factor(df_base_ML$Ruling_Party[df_base_ML$session == i]),
                       reference = factor(df_base_ML$SMV_predictions[df_base_ML$session == i]),
                       positive  = "1")$overall
  accuracy_scores[[j]] <- q
  j <- j + 1
}

lapply(accuracy_scores,'[[',1) %>% 
  set_names(unique(df_base_ML$session)) %>% 
  as_tibble(.name_repair = "unique") %>% t() %>% 
  dplyr::as_data_frame(., rownames = "session") %>% 
  rename( "Accuracy Score" = "V1") %>% 
  mutate(session = as.numeric(session)) %>% 
  left_join(df_base, ., by = 'session') -> df_base

