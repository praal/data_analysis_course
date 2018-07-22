library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(highcharter)

auth = read_rds("~/Desktop/Data Analysis/data/Articles/article_author_affilation.rds")

auth = auth %>% select(article_id, au_id) 
auth %>% group_by(au_id) %>% summarise(count = n()) %>% filter(count < 2)-> cnt

auth %>% filter(!au_id %in% cnt$au_id) -> auth
View(auth)
write.csv(auth, "~/Desktop/Data Analysis/project/authorsG.csv")
