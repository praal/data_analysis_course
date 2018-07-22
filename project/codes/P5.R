library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

article = read_rds("~/Desktop/Data Analysis/data/Articles/Articles.rds")


article %>% group_by(year) %>% summarise(count = n()) %>% filter(year > 1990) %>% filter(year != 2017) -> yr

ggplot(yr, aes(x = year, y = count)) + geom_line()
