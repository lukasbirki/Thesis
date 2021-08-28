source("Scripts/Loading_Data.R")
source("Scripts/0_Data_Preprocessing.R")
source("Scripts/0_5_Validation.R") 
source("Scripts/1_Dictionary_approach.R") 
source("Scripts/2_Ideological_scaling_approach.R") 
source("Scripts/3_ML_Wrangling.R") 

df_base %>% select(-politicianId, -documentUrl, -count_words) %>%   mutate(date = as.Date(date)) -> df_analysis

# Z-Standardizing ----

df_analysis %>% 
  mutate(across(c(sentiment_score_session, IGO_session, validation_session, Accuracy_Score_SGD), scale)) %>% 
  mutate(across(c(sentiment_score_session, IGO_session, validation_session, Accuracy_Score_SGD), as.vector)) -> df_analysis

df_analysis %>% 
  select(date, 
         session,
         sentiment_score_session, 
         IGO_session,
         Accuracy_Score_SGD) %>% 
  plyr::rename(c('sentiment_score_session' = 'SL Score','IGO_session' = 'IS Score', 'Accuracy_Score_SGD' = 'CA Score')) %>% 
  pivot_longer(!c(date, session), names_to = "Metric", values_to = "values") %>% drop_na() %>%
  mutate(Metric = factor(Metric, levels=c('SL Score', 'IS Score', 'CA Score'))) %>% unique() -> df_temp

# Plots ----

## Facet Wrap Line ----

df_temp %>% 
  ggplot() + 
  geom_line( aes(x = as.Date(date), y = values)) +
  geom_point(aes(x = as.Date(date), y = values)) +
  geom_hline(yintercept = 0,size=0.25 )+
  geom_vline(xintercept = as.Date('2020-03-01'), linetype="dotted") +
  geom_vline(xintercept = as.Date('2020-05-01'), linetype="dotted") +
  geom_label(aes(x =as.Date('2020-03-01'), y = 2, label = 'T 1'), size = 7) +
  geom_label(aes(x =as.Date('2020-05-01'), y = 2, label = 'T 2' ), size = 7) +
  ggtitle("Polarization Scores for Debates (N = 15)") +
  labs(x = "Date of Debate", y = "Standardised Polarization Scores") +
  scale_x_date(date_labels = "%b-%d") +
  facet_wrap(~ Metric, nrow = 3) +
  hrbrthemes::theme_ipsum(grid = 'Y') +
  theme(
    plot.title = element_text(size=24, hjust = 0.5),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 20),
    legend.text=element_text(size=17),
    legend.title = element_text(size = 20),
    strip.text.x = element_text(size = 24),
    panel.spacing = unit(0.5, "lines"))+ 
  guides(colour = guide_legend(nrow = 1)) -> comparison_line
ggsave("./Figures/Figures/comparison_line.png", plot = comparison_line, height = 12)

## Correlations----

df_analysis %>% 
  select(date, 
         validation_session, 
         sentiment_score_session,
         IGO_session,
         Accuracy_Score_SGD) %>% 
  distinct() %>% 
  slice(1:6) %>%   
  plyr::rename(c("sentiment_score_session"= "SL", 
                 "IGO_session" = "IS" ,
                 "Accuracy_Score_SGD" = "CA",
                 "validation_session" = "Validation")) %>% 
  pivot_longer(!c(date, Validation), names_to = "Metric", values_to = "values") %>%
  plyr::rename(c("Metric"= "Method")) %>% 
ggplot(aes(x = Validation, y = values, color=Method)) +
  geom_point(size = 2) + 
  geom_smooth(method=lm, se = F) +
  ggtitle(" Correlation between Methof Estimates and Validation Data") +
  labs(x = "Scores Estimates", y = "Scores Validation") +
  hrbrthemes::theme_ipsum(grid = 'Y') +
  xlim(-2,2) +
  ylim(-2,2)+
  theme(
    plot.title = element_text(size=24, hjust = 0.5),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 20, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 20),
    legend.text=element_text(size=17),
    legend.position="top",
    legend.title = element_text(size = 20),
    strip.text.x = element_text(size = 24),
    panel.spacing = unit(0.5, "lines"))+ 
  guides(colour = guide_legend(nrow = 1)) -> scatterplot
ggsave("./Figures/Figures/scatterplot.png", plot = scatterplot, height = 7)


## Correlation complete

df_analysis %>% 
  select(sentiment_score_session,
         IGO_session,
         Accuracy_Score_SGD) %>% 
  unique() %>% 
  cor() %>% as_tibble()

## Correlations Validation Scores  

df_analysis %>% 
  select(validation_session, 
         sentiment_score_session,
         IGO_session,
         Accuracy_Score_SGD) %>% 
  distinct() %>% 
  slice(1:6) %>%   
  plyr::rename(c("sentiment_score_session"= "SL", 
               "IGO_session" = "IS" ,
               "Accuracy_Score_SGD" = "CA",
               "validation_session" = "Validation")) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(hc.order = TRUE, type = "lower",
                         lab = TRUE, 
                         lab_size = 12,
                         tl.cex = 24)-> p1

ggsave("./Figures/Figures/correlation.png", plot = p1, height = 8)

## Consistency Test

# Inspecting 'extreme' debates for each method

require(data.table) ## 1.9.2
df_temp <- as.data.table(df_temp)


df_max <- df_temp[df_temp[, .I[values == max(values)], by=Metric]$V1]
df_min <- df_temp[df_temp[, .I[values == min(values)], by=Metric]$V1]

df_extreme <- rbind(df_max, df_min)

df_base %>% 
  write.csv(., file = "data/validation_df_base.csv")


