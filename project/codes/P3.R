library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)

aff = read_rds("~/Desktop/Data Analysis/data/Articles/affilations.rds")
aff %>% select(id, afname = name, affil_address_state, affil_address_street, documents_cnt, authors_cnt) -> aff

write_csv(aff, "~/Desktop/Data Analysis/project/affilation.csv")

x = read_csv("~/Desktop/Data Analysis/project/affilation.csv")
x %>% mutate(name = " ") -> x
for (i in 1:nrow(x)){
  t = x[i,]
  s = t$affil_address_street
  s = strsplit(s, ",\\s")
  s = s[[length(s)]]
  s = s[[length(s)]]
  t$name = s
  x[i,] = t

}


write_csv(x, "~/Desktop/Data Analysis/project/affilation.csv")

cities = world.cities[world.cities$country.etc == "Iran",]

x = merge(x, cities, by = "name")

myMap <- get_map(location = "Iran", zoom = 4)

ggmap(myMap) +
  geom_point(data = x[, c("long","lat", "authors_cnt")], aes(x=long, y = lat, size = authors_cnt/2000), color = "red")


cor.test(x$authors_cnt, x$documents_cnt)