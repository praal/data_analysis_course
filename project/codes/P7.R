library(readr)
library(dplyr)
library(ggplot2)


author = read_rds("~/Desktop/Data Analysis/data/Articles/authors.rds")
h = na.omit(author %>% select(hindex))
h %>% group_by(hindex) %>% summarise(count = n()) -> h
h = h[-1,]
View(h)

h %>% mutate(tajamo  = 1) -> h


for (i in 2:nrow(h)){
  j = nrow(h) - i + 1
  x = h[j,]
  y = h[(j+1),]
  x$tajamo = y$tajamo + x$count
  h[j,] = x
}

h %>% filter(hindex >= 30) -> t
ggplot(t, aes(x = hindex, y = tajamo)) + geom_line()

