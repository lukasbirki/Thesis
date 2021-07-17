###
### Ideological Scaling Approach ###
###


# Analysis ----

## Selecting Speeches and Loading into Quanteda Corpus Class ----

corpus_wordfish <- quanteda::corpus(df_base, 
                                    text_field = "speechContent")

docnames(corpus_wordfish) <- df_base$lastName

summary(corpus_wordfish, n = 700) %>% as_tibble() -> df_wordfish_running

corpus_wordfish %>% 
  tokens(remove_punct = TRUE, remove_numbers = T)  %>% 
  tokens_remove(stopwords("de"))  %>% 
  tokens_wordstem() %>% 
  dfm() %>% 
  dfm_trim(.,min_termfreq = 5) -> dfm_wordfish

## Running the Wordfish-Model ----

tmod_wf <- textmodel_wordfish(dfm_wordfish, 
                              sparse = T)

tmod_wf[c('docs','theta', 'alpha')] %>%  as_tibble() %>% 
  right_join(., df_wordfish_running, by = c('docs' = 'Text')) %>% 
  mutate(date = as.Date(date)) -> df_wordfish_base

rm(df_wordfish_running, dfm_wordfish, corpus_wordfish)

# Calculating Polarisation Scores ----

## IGO Index  ----

### Calculation IPO and Merging df_base ----
df_wordfish_base %>% 
  mutate(Mean_Theta = mean(theta)) %>% 
  mutate(IGO_Speech = sqrt((theta - Mean_Theta)**2)) %>% 
  group_by(date) %>% 
  mutate(IGO_Session = mean(IGO_Speech, na.rm = T)) %>% 
  ungroup() -> df_IGO

quanteda::corpus(df_base, text_field = "lastName") %>% 
  summary(n = 1000) %>% as_tibble %>% 
  select(count_words, speechContent, Party, politicianId, date) %>% 
  right_join(., df_IGO, by = c("count_words", "Party", "politicianId", "date")) %>% 
  select(date, lastName, Party,IGO_Speech, IGO_Session, speechContent) %>% 
  right_join(., df_base, by = c("date", "lastName", "Party", "speechContent")) -> df_base


## Goet Index ----


# Woardshoal ----

## Running the Wordshoal Model 
# - Groups: Debates
# - Authors: Individual MPs
# wordshoalfit <- wordshoal::textmodel_wordshoal(
#   dfm_wordfish, 
#   groups = docvars(dfm_wordfish, "date"), 
#   authors = docvars(dfm_wordfish, "lastName"))
# 
# author_positions <- summary(wordshoalfit)$estimated.author.positions
# author_positions$row_names <- rownames(author_positions) 
# 
# fitdf <- merge(author_positions,
#                docvars(dfm_wordfish), 
#                by.x = "row_names", by.y = "lastName") %>% as_tibble
# 
# fitdf <- subset(fitdf, !duplicated(id))
# 
# aggregate(theta ~ Party, data = fitdf, mean) %>% as.tibble %>% 
#   ggplot(aes(x = Party, y =theta)) +
#   geom_bin_2d()

# # Plots ----
# 
# 
# ## Table: Aggregated Estimates for Government / Opposition ----
# 
# df_wordfish_base %>% 
#   select(Ruling_Party, theta) %>% 
#   group_by(Ruling_Party) %>% 
#   summarize(mean = mean(theta)) 
# 
# ## Plot: Scales Textplot with highlighted Features ----
# 
# textplot_scale1d(tmod_wf, margin = "features", 
#                  highlighted = c('katastroph', 'versagen'))
# 
# ## Plot: Wordfish Estimates over Sessions ----
# 
# df_wordfish_base %>% 
#   select(Party, theta, session) %>% 
#   group_by(Party, session) %>% 
#   summarize(mean = mean(theta)) %>% 
#   ggplot(aes(x = as.character(session), y = mean, color = as.character(Party))) + 
#   geom_line() +
#   geom_point()+
#   hrbrthemes::theme_ipsum(grid = 'Y') +
#   theme(
#     plot.title = element_text(size=15, hjust = 0.5),
#     axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 11),
#     legend.position = "top") + guides(colour = guide_legend(nrow = 1)) +
#   scale_color_manual(breaks = c("CDU/CSU", "DIE LINKE.", "FDP", "AFD", "Greens", "SPD"), 
#                      values=c("#000000", "#BE3075", "#FFFF00",  "#009EE0",  "#64A12D","#FF0000"),
#                      name = 'Party')
# 
# # Polarisation across Sessions
# df_wordfish_running %>% 
#   aggregate(polarisation_wordfish ~ date, data = ., mean) %>% 
#   as_tibble %>% 
#   ggplot(aes(x = date, y =polarisation_wordfish)) +
#   geom_line() +
#   geom_point() + 
#   ggtitle('Polarisation across time') 
# 
# aggregate(theta ~ MP_3, data = tmod_wf, mean) %>% as.tibble %>% 
#   ggplot(aes(x = MP_3, y =theta)) +
#   geom_bin_2d()
# 
# textplot_scale1d(tmod_wf, 
#                  groups = dfm_wordfish$MP_3)
# 
# textplot_scale1d(tmod_wf, 
#                  margin = "features", highlighted = c('versagen'))
# 
