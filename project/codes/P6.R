library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

author = read_rds("~/Desktop/Data Analysis/data/Articles/authors.rds")

h = na.omit(author$hindex)
mean(h)

best = author %>% select(fullname, hindex, subjAreas) %>% na.omit(.) %>%arrange(-hindex) %>% head(11) 

View(best)

best2 = author %>% select(fullname, citationbydoc, subjAreas) %>% na.omit(.) %>%arrange(-citationbydoc) %>% head(10) 
View(best2)