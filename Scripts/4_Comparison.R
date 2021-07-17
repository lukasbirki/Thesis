source("Scripts/0_Data_Preprocessing.R")
source("Scripts/0_5_Validation.R") 
source("Scripts/1_Dictionary_approach.R") 
source("Scripts/2_Ideological_scaling_approach.R") 
source("Scripts/3_ML_Wrangling.R") 


#Standardizing ----
df_base$sentiment_Score_speech_scaled <- scale(df_base$sentiment_Score_id) %>% as.vector()
df_base$sentiment_Score_session_scaled <- scale(df_base$sentiment_score_session) %>% as.vector()
df_base$IGO_speech_scaled <- scale(df_base$IGO_Speech) %>% as.vector()
df_base$IGO_Session_scaled <- scale(df_base$IGO_Session) %>% as.vector()
df_base$validation_speech_scaled <- scale(df_base$validation_speech) %>% as.vector()
df_base$validation_session_scaled <- scale(df_base$validation_session) %>% as.vector()
df_base$ML_accuracy_scaled <- scale(df_base$`Accuracy Score`) %>% as.vector()

df_base %>% 
  #filter(validation_session_scaled != is.na(validation_session_scaled)) %>% 
  count(date) -> sortt

# Plots ----

##Plot: Linegraph with Validation Scores ----
df_base %>% 
  select(date, validation_session_scaled, 
         sentiment_Score_session_scaled,
         IGO_Session_scaled, 
         #ML_accuracy_scaled
         ) %>% 
  distinct() %>% 
  slice(1:6) %>% 
  pivot_longer(!date, names_to = "Metric", values_to = "values") %>% drop_na()-> df_base_comparison

df_base_comparison$date <- as.Date(df_base_comparison$date)
DF_gesis_runnig$monat <- as.Date(DF_gesis_runnig$monat)

ggplot() + 
  geom_line(data = df_base_comparison, aes(x = date, y = values, linetype = Metric)) +
  geom_point(data = df_base_comparison, aes(x = date, y = values)) +
  # geom_line(data = DF_gesis_runnig, aes(x = monat, y = value[,1]))+
  # geom_point(data = DF_gesis_runnig, aes(x = monat, y = value[,1]))+
  ggtitle("Validation Scores for Sessions (N = 6)") +
  labs(x = "Date of Session", y = "Standardised Polarisation Scores") +
  scale_x_date(date_labels = "%b-%d") +
  hrbrthemes::theme_ipsum(grid = 'Y') +
  theme(
    plot.title = element_text(size=15, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 11),
    legend.position = "top") + guides(colour = guide_legend(nrow = 1)) -> plot_comparison

ggsave("./Figures/Figures/comparison_plot.png", plot = plot_comparison, height = 7)

gesis$monat
lubridate::ydm('2020-02-12 UTC')

## Correlations Validation Scores and 

df_base %>% 
  drop_na(validation_speech) %>%
  select(validation_speech, IGO_speech_scaled, sentiment_Score_speech_scaled, ML_session_scaled) %>% 
  rename("Sentiment Dictionary"= "sentiment_Score_speech_scaled", "IGO Index" = "IGO_speech_scaled" ) %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number', tl.col = 'black', tl.cex = 0.8, type = 'lower', insig='blank')

tt[,2:4] %>% 
  cor() %>% 
  corrplot::corrplot(method = 'number', tl.col = 'black', tl.cex = 0.8, type = 'lower', insig='blank')

ccf(tt$validation_session_scaled, tt$IGO_Session_scaled) -> correlation
