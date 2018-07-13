library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat", delim = "$")
mname = movie %>% select(Name)

words = (mname) %>% 
  str_replace_all("[[:punct:]]"," ") %>%
  str_replace_all("[[:digit:]]","") %>% 
  str_to_lower() %>% 
  str_split(pattern = "\\s") %>% 
  unlist() %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = F)

colnames(words) = c("word","count")
words = words[-(1:250),]
View(words)
words = words  %>% 
  filter(!(word %in% stop_words$word)) %>% 
  filter(nchar(word)>1)


words = words[order(-words$count),]
wordcloud(words$word[1:20], words$count[1:20], c(2,.2), random.order = FALSE, colors = brewer.pal(8, "Dark2"))
