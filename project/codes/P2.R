library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

article = read_rds("~/Desktop/Data Analysis/data/Articles/articles_details.rds")

name = article %>% select(key = indexKeywords)
name$key = as.character(name$key)

name %>% str_replace_all("[[:punct:]]"," ") %>% 
  str_split(pattern = "\\s+") %>% 
  unlist() %>% 
  table() -> words

words = words %>% as.data.frame(stringsAsFactors = F)

colnames(words) = c("word","count")

words = words  %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(nchar(word)>1) %>% 
  filter(!str_detect(word, "\\d")) %>% arrange(-count)

View(words)

wordcloud(words$word[1:100], words$count[1:100], c(2,.1), random.order = FALSE, colors = brewer.pal(8, "Dark2"))
