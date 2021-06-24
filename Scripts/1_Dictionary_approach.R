source("Scripts/Data_Preprocessing.R") #takes around 1 minute

sw <- tibble(stopwords::stopwords("de"))
sw$sw <- sw$`stopwords::stopwords("de")`

tidy_df %>% 
  #slice(1:5000) %>% 
  anti_join(.,sw, by = c("word" = "sw")) %>% #Removing Stopwords (from 9,65m to 4,73m)

# Preprocessing ----
speeches_thesis %>% 
  tidytext::unnest_tokens(word, speechContent) -> tidy_df

tidy_df %>% 
  #slice(1:5000) %>% 
  anti_join(.,sw, by = c("word" = "sw")) %>% #Removing Stopwords (from 9,65m to 4,73m)
  filter(str_detect(word, "[a-z]")) %>% #Removing numbers
  mutate(token_stem = SnowballC::wordStem(.$word, language = "german")) -> tidy_df #Word Stemming


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
  mutate(token_stem = SnowballC::wordStem(.$Wort, language = "german"))-> sentiment_df

# Analysis ----

tidy_df %>% 
  mutate(WordScore = ifelse(.$word %in% sentiment_df$Wort, sentiment_df$Wert,NA)) %>% 
  mutate(WordScore2 = ifelse(.$token_stem %in% sentiment_df$token_stem, sentiment_df$Wert,NA)) -> final

#Checking number of correct matches

summary(final$WordScore)
summary(final$WordScore2)

final %>% 
  group_by(id) %>% 
  summarise_at(vars(WordScore2),              # Specify column
               list(sentiment_id = mean), na.rm=T) -> ID_level

final_2 %>% 
  group_by(session) %>% 
  summarise_at(vars(sentiment),              # Specify column
               list(sentiment_session = mean), na.rm=T) -> Session_level

speeches_thesis %>% 
  left_join(., y = ID_level, by = c("id" = "id")) %>% 
  left_join(., y = Session_level, by = c("session" = "session")) -> final_sentiment



final_sentiment %>% 
  #filter(date > "2020-01-01") %>% 
  ggplot(data=., aes(x=date, y=sentiment_session)) +
  geom_point() +
  geom_line()

write.csv(final_sentiment,"./results/dataset_speeches_dict.csv", row.names = FALSE)
jsonlite::write_json(final_sentiment,"dataset_speeches_dict.json")

# Plots ----
## 
tidy_df %>% 
  slice(1:1000) %>% 
  dplyr::count(word, sort = TRUE) %>% 
  top_n(30) %>% 
  ggplot() +
  aes(x = reorder(word, n), y = n) +
  geom_col() + 
  labs(title = "With Stemming") +
  coord_flip() -> p1

tidy_df %>% 
  slice(1:1000) %>% 
  dplyr::count(token_stem, sort = TRUE) %>% 
  top_n(30) %>% 
  ggplot() +
  aes(x = reorder(token_stem, n), y = n) +
  geom_col() + 
  labs(title = "without Stemming") +
  coord_flip() -> p2

grid.arrange(p1, p2, ncol = 2)

tidy_df %>% 
  filter(id ==1000315) %>% 
  select(word) %>% 
  print(n = 40)

tidy_df %>% 
  filter(id ==1000315) %>% 
  select(word) %>% 
  print(n = 40)

tidy_df %>% 
  count(word, sort = T) %>% 
  print(n=40)


# Plots
## Total counts of speeches lengths
p1 <- speeches_thesis %>% 
  ggplot(aes(x = count_words)) +
  geom_histogram(binwidth=1) +
  ggtitle("Total Distribution of spech lenght") +
  theme(plot.title = element_text(hjust = 0.5) )

# Count of speeches lengths per session
# p2 <- speeches_thesis %>% 
#   group_by(session) %>% 
#   summarise(count = n()) %>%
#   ggplot(aes(x = session, y = count)) +
#   geom_col(width = 1) +
#   ggtitle("Number of speeches per session of the Bundestag")
#   
# Count of Speeches per time period

p2 <- speeches_thesis %>% 
  group_by(date) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = count)) +
  geom_col(width = 3) +
  theme_classic() 

p3 <- speeches_thesis %>% 
  group_by(Party) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(Party,-count), y = count, fill=factor(count))) +
  geom_bar(stat="identity") +
  ggtitle("Number of Speeches per Party") +
  scale_fill_manual("Party", 
                    values = c( "hotpink3","green","yellow","blue","red", "black"),
                    labels = c("Die LInke","Grüne","FDP","AFD","SPD","CDU")) +
  theme(plot.title = element_text(hjust = 0.5) )

phd_field %>% 
  group_by(broad_field,field) %>%
  summarise(n=sum(n_phds, na.rm=TRUE)) %>%
  arrange(desc(n))%>%
  head(30)%>%
  ggplot(aes(x=reorder(field,-n),y=n, fill=broad_field)) +
  xlab("field")+
  geom_col()


grid.arrange(p1, p2, p3, nrow = 3)