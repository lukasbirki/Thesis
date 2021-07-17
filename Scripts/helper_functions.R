# Defining Functions ----

# Reverse words
reverse_words_helper <- function(string)
{
  # split string by blank spaces
  string_split = strsplit(as.character(string), split = "-")
  # how many split terms?
  string_length = length(string_split[[1]])
  # decide what to do
  if (string_length == 1) {
    # one word (do nothing)
    reversed_string = string_split[[1]]
  } else {
    # more than one word (collapse them)
    reversed_split = string_split[[1]][string_length:1]
    reversed_string = paste(reversed_split, collapse = "-")
  }
  # output
  return(reversed_string)
} 


IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}

shift_axis <- function(p, xmin, xmax, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    annotate("segment", y = 0, yend = 0, x = xmin, xend = xmax, 
             arrow = arrow(length = unit(0.1, "inches"))) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}

sw <- tibble(stopwords::stopwords("de"))
sw$sw <- sw$`stopwords::stopwords("de")`
sw <- as_tibble(sw$sw)
additional_stopwords <- c('dass') %>% as.tibble()
sw <- rbind(sw,additional_stopwords )


c('Herr Präsident', 'Frau Präsidentin',
  'Herr Präsident', 'Frau Präsidentin',
  'Sehr geehrter Herr Präsident', 'Sehr geehrte Frau Präsidentin',
  'Sehr geehrte Damen und Herren', 'Meine Damen! Meine Herren',
  'Liebe Kolleginnen und Kollegen', 'Verehrte Kolleginnen und Kollegen',
  'Meine sehr verehrten Damen und Herren', 'Meine Damen und Herren',
  'Meine sehr geehrten Damen und Herren', 'Meine Damen und Herren',
  'Sehr geehrte Kolleginnen und Kollegen', 'Sehr geehrte Kollegen',
  'Werte Kollegen', ' Meine Kolleginnen und Kollegen', 'Sehr geehrte Präsidentin',
  'Liebe Kolleginnen! Liebe Kollegen', 'Sehr verehrte Kolleginnen und Kollegen',
  'Verehrte Kolleginnen', 'Verehrte Kollegen', 'Frau Präsident',
  ' Werte Kolleginnen und Kollegen', 'Liebe Zuhörerinnen und Zuhörer', 
  'Kolleginnen und Kollegen', 'Liebe Landsleute', 'Verehrte Kollegen und Zuschauer',
  'Liebe Damen und Herren', 'Meine sehr geehrten Kolleginnen und Kollegen', 'Kollegen', 
  'Meine lieben Kolleginnen und Kollegen', ' Liebe Kollegen und Kolleginnen', 'Sehr geehrte Herren Präsidenten',
  'Liebe Zuschauerinnen und Zuschauer an den Bildschirmen', 'Meine sehr verehrten',
  'Frau Bundeskanzlerin', 'Frau Bundesministerin', 'Herr Bundesminister', 'Herr Minister') %>% 
  paste(., "[,|.|!.?] ", sep = "") %>% 
  str_c(.,collapse="|") -> greetings

