library(officer)
library(stringr)
library(splitstackshape)

source("Scripts/Data_Preprocessing.R") #takes around 1 minute
source("Scripts/helper_functions.R") 

# Importing Data from LINK ----
dip_selection <- read_docx("data/Dip-Export.docx")

#Filtering and Reshaping
dip_selection %>% 
  docx_summary() %>% 
  as_tibble() %>% 
  filter(style_name == "heading 1" | grepl('Datum', text)) %>% # Filter Date and 
  filter(style_name != "HeaderStandard") %>% 
  select(style_name, text, doc_index) %>% 
  pivot_wider(names_from = style_name, values_from = text) -> df_docx_wide

df_docx_wide %>% 
  mutate(index_to_merge = rep(1:(nrow(.)/2), each = 2)) %>% 
  group_by(index_to_merge) %>% 
  summarise_all(funs(trimws(paste(., collapse = '')))) %>% 
  select(-1:-2) %>% 
  rename(c('MP' = 'heading 1', 'date'='Werte')) %>% 
  mutate(date_final = str_remove_all(date, "^NADatum: ")) %>% 
  mutate(date_final = str_replace_all(date_final, '[.]','-')) %>% 
  mutate(MP = str_sub(.$MP, 1, str_length(.$MP)-2)) %>% 
  splitstackshape::cSplit(., "MP", sep = ",") %>% 
  mutate(last_name = stringi::stri_extract_last_words(MP_1)) %>% 
  as_tibble() %>% 
  rowwise() %>% 
  mutate(across(c(date_final), reverse_words_helper)) -> df_docx_final

speeches_thesis$date <- as.character(speeches_thesis$date)
names(df_docx_final)[6] <- "lastName"
names(speeches_thesis)[12] <- "date_final"


final_df <-  speeches_thesis %>% 
  merge(.,df_docx_final, by = c('date_final', 'lastName')) %>% 
  as_tibble() %>% 
  select(date_final, speechContent, session, MP_1, MP_2, MP_3, positionShort) %>% 
  filter(positionShort == "Member of Parliament")


