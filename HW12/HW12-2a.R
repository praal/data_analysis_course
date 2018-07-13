library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat", delim = "$")
rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")

rate = rate %>% select(USER, FILM, RATE)

n = nrow(movie)
genres = NULL

for (i in (1:n)){
  x = movie[i, ]
  g = x$Genres
  q = strsplit(g, '=')
  for(j in q[[1]]){
    genres = rbind(genres, data.frame(x$ID, x$Name, j))
  }
}

write_csv(genres, "~/Desktop/Data Analysis/genres.csv")

genres %>% group_by(j) %>% summarise(count = n()) -> cnt

cnt = cnt[1:19,]

ggplot(cnt, aes(x = j, y = count)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
