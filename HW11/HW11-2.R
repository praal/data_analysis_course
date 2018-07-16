library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(highcharter)
library(ggthemes)
library(gganimate)


data = read.delim("~/Downloads/week_11/week_11/data/disaster.txt", sep = "\t", dec = ".")

data %>% filter(FLAG_TSUNAMI == "Tsu") %>% 
  rename(lat = LATITUDE,lon = LONGITUDE, z = EQ_MAG_MS,name = COUNTRY,year = YEAR) %>% 
  select(lat, lon, z, name, year) -> dis 

ggplot() +
  borders("world", colour = "gray85", fill = "gray80") +
  theme_map() +
  geom_point(aes(x = lon, y = lat),
             data = dis, colour = 'purple', alpha = .5) +
  labs(title = 'Year: {frame_time}') +
  transition_time(year) +
  ease_aes('linear')
