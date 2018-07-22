library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

article = read_rds("~/Desktop/Data Analysis/data/Articles/Articles.rds")

name = article %>% select(title)
name$title = as.character(name$title)


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
  

unuseful = c(1,2,3,4,5,6,7, 8,9,10, 12,13,17,19,24,34,41,45)
t = words[-unuseful,]

wordcloud(t$word[1:100], t$count[1:100], c(1.5,.1), random.order = FALSE, colors = brewer.pal(8, "Dark2"))
