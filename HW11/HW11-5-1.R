library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(highcharter)
library(tools)

data = read.delim("~/Downloads/week_11/week_11/data/disaster.txt", sep = "\t", dec = ".")


data = data %>% select(LATITUDE, LONGITUDE, COUNTRY, DEATHS) 
death =data %>% group_by(COUNTRY) %>% summarise(ave = mean(DEATHS, na.rm = TRUE))

death %>% 
  rename(region = COUNTRY) -> death 


world <- map_data("world")

death$region = tolower(death$region)
world$region = tolower(world$region)

world = full_join(world, death)

ggplot() +
  geom_polygon(data=world, aes(x=long, y=lat, group = group, fill=ave),colour="white") +
  scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar") +
  labs(fill = "Deaths", title = "Deaths in World", x="", y="") +
  theme(legend.position = "top") +
  theme_bw() +
  scale_y_continuous(breaks=c()) + 
  scale_x_continuous(breaks=c()) + 
  theme(panel.border =  element_blank()) +
  theme(legend.position = "top")