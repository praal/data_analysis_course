library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(highcharter)

article = read_rds("~/Desktop/Data Analysis/data/Articles/Articles.rds")

article %>% select(pageStart , pageEnd) -> pgs
pgs$pageStart = as.integer(pgs$pageStart)
pgs$pageEnd = as.integer(pgs$pageEnd)
pgs %>% mutate(pageNumber = pageEnd - pageStart) %>% filter(pageNumber > 0 )-> pages

ggplot(pages, aes(x = pageNumber))+ geom_histogram(fill = "purple", binwidth = 10) 

mean(pages$pageNumber)