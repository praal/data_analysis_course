library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat", delim = "$")

movie %>% mutate(year = str_sub(Name,-5,-2)) %>% filter(year > 1900) %>% filter(year < 2020)-> movie
View(movie)

movie %>% group_by(year) %>% summarise(count = n()) -> yearcnt
View(yearcnt)
yearcnt = yearcnt[order(-yearcnt$count),]

ggplot(head(yearcnt,20), aes(x = year, y = count)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
