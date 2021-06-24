source("Scripts/Data_Preprocessing.R") #takes around 1 minute

library(quanteda)
library(quanteda.textmodels)
library(quanteda.textplots)
library(readtext)

# Selecting Speeches and Loading into Corpus Class

data_wordfish <- speeches_thesis[1:100,]

corpus_wordfish <- quanteda::corpus(data_wordfish, 
                                    text_field = "speechContent")

docnames(corpus_wordfish) <- data_wordfish$lastName

# Tokenization and Cleaning

corpus_wordfish %>% 
  tokens(remove_punct = TRUE, remove_numbers = T)  %>% 
  tokens_remove(stopwords("de")) -> corpus_wordfish


# Creating and Trimming Document-Feature Matrix

dfm_wordfish <- dfm(corpus_wordfish)


dfm_wordfish <- dfm_trim(dfm_wordfish,
                         min_termfreq = 10)
print(dfm_wordfish)

#Running the Model

tmod_wf <- textmodel_wordfish(dfm_wordfish)
summary(tmod_wf)


textplot_scale1d(tmod_wf, groups = dfm_wordfish$Party)

data_corpus_irishbudget2010
corpus_wordfish
