library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat", delim = "$")
rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")
rate = rate %>% select(FILM, RATE, TIME)

n = nrow(movie)
genres = NULL

for (i in (1:n)){
  x = movie[i, ]
  g = x$Genres
  q = strsplit(g, '=')
  for(j in q[[1]]){
    genres = rbind(genres, data.frame(x$ID, j))
  }
}

rate$year = format(as.POSIXct(rate$TIME, origin = "1970-01-01"), "%Y")

rate %>% select(id = FILM, rate = RATE, year) -> rate
colnames(genres) = c("id", "genre")
genrate = full_join(rate, genres)

genrate %>% group_by(genre) %>% summarise(avg = mean(rate, na.rm = TRUE))  -> genavg
genavg = genavg[1:19,]
ggplot(genavg, aes(x = genre, y = avg)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
