library(readr)
library(stringr)
library(dplyr)
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat", delim = "$")
rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")

rate = rate %>% select(USER, FILM, RATE)


rate %>% group_by(FILM) %>% summarise(tot = mean(RATE)) -> avg

film = movie %>% select(FILM = ID, Name)
avg = full_join(avg, film)
avg = na.omit(avg)
avg = avg[order(-avg$tot),]


print(avg$Name[1:10])