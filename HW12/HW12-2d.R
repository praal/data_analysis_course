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


genres = read_csv("~/Desktop/Data Analysis/genres.csv")
colnames(genres) = c("ID", "Name", "Genre")

rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")

rate = rate %>% select(USER, FILM, RATE)
movrate = rate %>% group_by(FILM) %>% summarise(avg = mean(RATE, na.rm = TRUE))
movrate %>% filter(avg >= 4) -> movrate

movie %>% filter(ID %in% movrate$FILM) -> selected

selected %>% group_by(year) %>% summarise(cnt = n()) -> golden
golden = golden[order(-golden$cnt), ]
ggplot(head(golden, 20), aes(x = year, y = cnt)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
