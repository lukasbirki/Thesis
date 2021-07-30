source("Scripts/Loading_Data.R")
source("Scripts/0_Data_Preprocessing.R")
source("Scripts/0_5_Validation.R") 
source("Scripts/1_Dictionary_approach.R") 
source("Scripts/2_Ideological_scaling_approach.R") 
source("Scripts/3_ML_Wrangling.R") 

df_base %>% select(-session, -politicianId, -documentUrl, -count_words) -> df_analysis


#Standardizing ----
df_analysis$sentiment_Score_speech_scaled <- scale(df_base$sentiment_Score_id) %>% as.vector()
df_analysis$sentiment_Score_session_scaled <- scale(df_base$sentiment_score_session) %>% as.vector()
df_analysis$IGO_Session_scaled <- scale(df_base$IGO_session) %>% as.vector()
df_analysis$validation_speech_scaled <- scale(df_base$validation_speech) %>% as.vector()
df_analysis$validation_session_scaled <- scale(df_base$validation_session) %>% as.vector()
df_analysis$ML_accuracy_scaled <- scale(df_base$`Accuracy Score`) %>% as.vector()


# Plots ----

##Plot: Linegraph with Validation Scores ----
df_analysis %>% 
  select(date, validation_session_scaled, 
         sentiment_Score_session_scaled,
         IGO_Session_scaled) %>% 
  distinct() %>% 
  slice(1:6) %>% 
  pivot_longer(!date, names_to = "Metric", values_to = "values") %>% drop_na() %>% 
  mutate(date = as.Date(date)) %>%
  ggplot() + 
  geom_line( aes(x = date, y = values, linetype = Metric)) +
  geom_point(aes(x = date, y = values)) +
  # geom_line(data = DF_gesis_runnig, aes(x = monat, y = value[,1]))+
  # geom_point(data = DF_gesis_runnig, aes(x = monat, y = value[,1]))+
  ggtitle("Validation Scores for Debate (N = 6)") +
  labs(x = "Date of Debate", y = "Standardised Polarisation Scores") +
  scale_x_date(date_labels = "%b-%d") +
  hrbrthemes::theme_ipsum(grid = 'Y') +
  theme(
    plot.title = element_text(size=15, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 11),
    legend.position = "top") + guides(colour = guide_legend(nrow = 1)) -> plot_comparison

ggsave("./Figures/Figures/comparison_plot.png", plot = plot_comparison, height = 8)

## Correlations Validation Scores and 

df_analysis %>% 
  drop_na(validation_speech) %>%
  select(validation_speech, IGO_speech_scaled, sentiment_Score_speech_scaled) %>% 
  rename("Sentiment Dictionary"= "sentiment_Score_speech_scaled", "IGO Index" = "IGO_speech_scaled" ) %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number', tl.col = 'black', tl.cex = 0.8, type = 'lower', insig='blank')

tt[,2:4] %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number', tl.col = 'black', tl.cex = 0.8, type = 'lower', insig='blank')

ccf(tt$validation_session_scaled, tt$IGO_Session_scaled) -> correlation
