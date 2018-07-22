library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(highcharter)

article = read_rds("~/Desktop/Data Analysis/data/Articles/article_author_affilation.rds")

article %>% select(au_id, affilation_id) %>% unique() -> auaf
auaf %>% group_by(au_id) %>% summarise(aff = round(mean(as.integer(affilation_id),1))) -> t

aff = read_rds("~/Desktop/Data Analysis/data/affilations.rds")
aff %>% select(aff = id, name) -> aff
t = inner_join(t, aff)
t = t[-1,]

sharif = t %>% filter(aff == 366) 
View(sharif)


article = read_rds("~/Desktop/Data Analysis/data/Articles/Articles.rds")

article %>% filter(au_id %in% sharif$au_id) -> article

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
