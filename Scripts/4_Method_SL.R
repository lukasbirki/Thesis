
###
### Ideological Scaling Approach ###
###

# Loading Dictionary ----


read_tsv("./data/SentiWS_v2.0/SentiWS_v2.0_Negative.txt", col_names = FALSE) %>% 
  rename("Wort_POS" = "X1", "Wert" = "X2", "Inflektionen" = "X3" ) %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> neg_df

read_tsv("./data/SentiWS_v2.0/SentiWS_v2.0_Positive.txt", col_names = FALSE) %>% 
  rename("Wort_POS" = "X1", "Wert" = "X2", "Inflektionen" = "X3" ) %>% 
  mutate(Wort = str_sub(Wort_POS, 1, regexpr("\\|", .$Wort_POS)-1),
         POS = str_sub(Wort_POS, start = regexpr("\\|", .$Wort_POS)+1)) -> pos_df

bind_rows("neg" = neg_df, "pos" = pos_df, .id = "neg_pos") %>% 
  select(neg_pos, Wort, Wert, Inflektionen, -Wort_POS) %>% 
  mutate(token_stem = SnowballC::wordStem(.$Wort, language = "german")) %>% 
  mutate(Polarity = .$Wert * -1)-> sentiment_df

rm(neg_df, pos_df)

# Analysis ----

## Matching Sentiment Values ----
df_base %>% 
  tidytext::unnest_tokens(word, speechContent_stemmed, drop = F) %>% 
  rowwise() %>% 
  mutate(WordScore_token_stem = ifelse(word %in% sentiment_df$token_stem, 
                            sentiment_df$Polarity[match(word, sentiment_df$token_stem)], NA)) -> sentiment_df_final


## Statistics ----

### Percent of words with an assigned Sentiment Score ----
length(sentiment_df_final$WordScore_token_stem[!is.na(sentiment_df_final$WordScore_token_stem)])/length(sentiment_df_final$WordScore_token_stem)

### Show most frequent 100 words with a sentiment Score ----
sentiment_df_final %>% 
  filter(WordScore_token_stem != is.na(WordScore_token_stem)) %>% 
  group_by(word) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  select(word, n, WordScore_token_stem) %>% unique() %>% 
  arrange(desc(n)) %>% 
  head(n = 50) %>% 
  xtable::xtable() %>% 
  print(., file = "./Figures/Tables/with_sentiment_Score.tx")

### Show most frequent 100 words without a sentiment Score ----
sentiment_df_final %>% 
  filter(is.na(WordScore_token_stem)) %>% 
  group_by(word) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  select(word, n) %>% unique() %>% 
  arrange(desc(n)) %>% 
  head(n = 50) %>% 
  xtable::xtable() %>% 
  print(., file = "./Figures/Tables/without_sentiment_Score.tx")


## Calculating Sentiment Scores for Speeches and Sessions ----
sentiment_df_final %>%
  group_by(id) %>% 
  mutate(Score_id = mean(WordScore_token_stem, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(date) %>% 
  mutate(Score_date = mean(WordScore_token_stem, na.rm = T)) %>% 
  ungroup() -> df_sentiment

#Merging with df_base
df_sentiment %>% 
  group_by(speechContent) %>% 
  summarise(sentiment_score_session = unique(Score_date),
            sentiment_Score_id = unique(Score_id)) %>% 
  left_join(df_base, ., by = "speechContent")-> df_base

# Plots ----

## Plot: Sentiment per Session ----

plot_sentiment <- df_base %>%
  group_by(date) %>%
  distinct(date, sentiment_score_session) %>% 
  mutate(date = as.Date(date)) %>% 
  ggplot(aes(x = date, y = sentiment_score_session)) +
  geom_line(stat="identity") +
  geom_point() +
  geom_hline(yintercept = 0)+
  ggtitle("Sentiment Score per Session" ) +
  labs(y = "Sentiment (Polarity) Score per Session", x = "Time") +
  hrbrthemes::theme_ipsum(grid = "X") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")

ggsave("./Figures/Figures/sentiment_session.png", plot = plot_sentiment, height = 9)

## Plot: Sentiment per Speech ----

plot_sentiment_box <- df_base %>%
  filter(sentiment_Score_id <=0.5) %>% 
  ggplot(aes(x = as.character(date), y = sentiment_Score_id, group = as.character(date))) +
  geom_boxplot() +
  viridis::scale_fill_viridis(discrete = T, alpha=0.6) +
  hrbrthemes::theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")

ggsave("./Figures/Figures/sentiment_session.png", plot = plot_sentiment_box, height = 9)

