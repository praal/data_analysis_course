library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(highcharter)

article = read_rds("~/Desktop/Data Analysis/data/Articles/Articles.rds")
article %>% select(Language) -> lang

lang$Language = as.character(lang$Language)
lang %>% str_replace_all("[[:punct:]]"," ") %>% 
  str_split(pattern = "\\s+") %>% 
  unlist() %>% 
  table() -> words

words = words %>% as.data.frame(stringsAsFactors = F)

colnames(words) = c("word","count")
View(words)
words = words  %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(nchar(word)>1) %>% 
  filter(!str_detect(word, "\\d")) %>% arrange(-count) %>% head(10)


ggplot(words, aes(x = reorder(word,count), y = count)) + geom_col(fill = "purple") +  theme(axis.text.x = element_text(angle=-90, vjust=0.5, size = 8)) + xlab("Language")



