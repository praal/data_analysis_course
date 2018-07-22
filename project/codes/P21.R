library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

article = read_rds("~/Desktop/Data Analysis/data/Articles/article_author_affilation.rds")

article %>% select(au_id, affilation_id) %>% unique() -> auaf
auaf %>% group_by(au_id) %>% summarise(aff = round(mean(as.integer(affilation_id),1))) -> t

aff = read_rds("~/Desktop/Data Analysis/data/affilations.rds")
aff %>% select(aff = id, name) -> aff
t = inner_join(t, aff)
t = t[-1,]

sharif = t %>% filter(aff == 366) 

author = read_rds("~/Desktop/Data Analysis/data/Articles/authors.rds")
author = author %>% filter(id %in% sharif$au_id)
h = na.omit(author$hindex)
mean(h)

best = author %>% select(fullname, hindex, subjAreas) %>% na.omit(.) %>%arrange(-hindex) %>% head(11) 

View(best)

best2 = author %>% select(fullname, citationbydoc, subjAreas) %>% na.omit(.) %>%arrange(-citationbydoc) %>% head(10) 
View(best2)