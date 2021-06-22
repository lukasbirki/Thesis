# Loading Dictionaries ----

load("/Users/lukas/Downloads/JITP-Replication-Final 2/1_Dictionaries/Rauh_SentDictionaryGerman_Negation.Rdata") 
load("/Users/lukas/Downloads/JITP-Replication-Final 2/1_Dictionaries/Rauh_SentDictionaryGerman.Rdata") 

#Loading and Cleaning Texts ----

### Stemming and Lowering
text_sample.senti <- speeches_thesis$speechContent[1:5]
text_sample.senti[6] <- "ich habe keinen Lust und keinen aasen "

text_sample.senti %>% 
  gsub("(\\.|\\?|!|:|,|;|-)", "", ., fixed = FALSE) %>% 
  gsub("  ", " ", ., fixed = FALSE) %>% 
  gsub("\n\n", " ", ., fixed = FALSE) %>% 
  tolower(.) -> text_sample.senti

### Replacing Negations
for (i in 1:nrow(neg.sent.dictionary)){
  text_sample.senti <- gsub(neg.sent.dictionary$pattern[i], neg.sent.dictionary$replacement[i], text_sample.senti, fixed = FALSE)
}
a


#Analysis ----

## Loop ----

text3 <- as.list(text_sample.senti)
sentiment.norm <- list()
i <- 1

for (text in text3){
  sentiment.norm[[i]] <- function_helper(text)
  i = i +1
  print(i)
}

print(sentiment.norm)

## Tidy-text Approach----

#Buildling Lexica

neg.sent.dictionary %>% 
  as_tibble() %>% 
  select(feature, sentiment) %>% 
  rbind(as_tibble(sent.dictionary),.) %>% 
  gsub(" ", "", ., fixed = TRUE) -> sent.dict.complete



text_sample.senti %>% 
  as_tibble() %>% 
  tidytext::unnest_tokens(word, value) %>% 
  mutate(WordScore = ifelse(.$word %in% sent.dict.complete$feature, sent.dict.complete$sentiment, NA)) -> final

  



final$word[1]















#Functions----

function_helper <- function(text_sample.senti){
sent.dictionary$scored <- FALSE

for (i in 1:nrow(sent.dictionary)){
  sent.dictionary$scored[i] <- ifelse(grepl(sent.dictionary$feature[i], text_sample.senti, fixed = TRUE,perl = FALSE),
                                      TRUE, sent.dictionary$scored[i])
}

neg.sent.dictionary$scored <- FALSE

for (i in 1:nrow(sent.dictionary)){
  neg.sent.dictionary$scored[i] <- ifelse(grepl(neg.sent.dictionary$feature[i], text_sample.senti, fixed = TRUE,perl = FALSE),
                                          TRUE, neg.sent.dictionary$scored[i])
}


results <- rbind(sent.dictionary[sent.dictionary$scored == TRUE, ],
                 neg.sent.dictionary[neg.sent.dictionary$scored == TRUE, c("feature", "sentiment", "scored")])

sentiment.norm <- sum(as.integer(results$sentiment)) / (nchar(gsub("[a-zäüöß]","", text_sample.senti, fixed = FALSE, perl = T))-1)
}

