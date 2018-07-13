#uncompleted

library(readr)
library(ggplot2)

data = read_rds("~/Downloads/week_11/week_11/data/iran_earthquake.rds")

View(data)

myMap <- get_map(location = "Iran", zoom = 4)

ggmap(myMap) +
  geom_point(data = data, aes(x=Long, y = Lat)) + stat_density_2d(bins = 5)


