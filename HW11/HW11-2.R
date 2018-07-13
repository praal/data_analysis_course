library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(highcharter)

data = read.delim("~/Downloads/week_11/week_11/data/disaster.txt", sep = "\t", dec = ".")

View(data)


data %>% filter(FLAG_TSUNAMI == "Tsu") %>% 
  rename(lat = LATITUDE,lon = LONGITUDE, z = EQ_MAG_MS,name = COUNTRY,sequence = YEAR) %>% 
  select(lat, lon, z, name, sequence) -> dis 
hcmap() %>% 
  hc_add_series(data = dis, type = "mapbubble",
                minSize = 0, maxSize = 10)  %>% 
  hc_plotOptions(series = list(showInLegend = FALSE))