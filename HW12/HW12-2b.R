library(stringr)
library(dplyr)
library(ggplot2)
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat", delim = "$")
rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")

rate = rate %>% select(USER, FILM, RATE)

n = nrow(movie)
genres = NULL

      

genrtale = data.frame(matrix(ncol= 19, nrow = 0))
gn= c("Adventure","Animation", "Children","Comedy","Fantasy", "Romance", "Drama", "Action", "Crime","Thriller", "Horror","Mystery",                                                    
                       "Sci-Fi",                                                     
                      "IMAX",                                                       
                      "Documentary",                                                
                      "War",                                                       
                       "Musical",                                                    
                      "Film-Noir",                                                  
                      "Western")
colnames(genrtale) = gn

for (i in (1:n)){
  x = movie[i, ]
  g = x$Genres
  q = strsplit(g, '=')
  h = rep(0, 19)

  for(j in q[[1]]){
    for (u in 1:(19)){
      if(gn[u] == j){
        h[u] = 1
      }
    }
  }

  
  genrtale[i, ] = h
  
}
pairs(genrtale[,1:5])
