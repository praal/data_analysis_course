library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat", delim = "$")
rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")
rate = rate %>% select(FILM, RATE, TIME)

n = nrow(movie)
genres = NULL

genres = read_csv("~/Desktop/Data Analysis/genres.csv")

rate$year = format(as.POSIXct(rate$TIME, origin = "1970-01-01"), "%Y")

rate %>% select(id = FILM, rate = RATE, year) -> rate
colnames(genres) = c("id", "genre")
genrate = full_join(rate, genres)

genrate %>% group_by(year, genre) %>% summarise(avg = mean(rate, na.rm = TRUE)) -> yeargenre

yeargenre %>% group_by(year) %>% filter(avg == max(avg)) %>% select(year, genre)-> t

t