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