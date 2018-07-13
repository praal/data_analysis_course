library(readr)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)


data = read.delim("~/Downloads/week_11/week_11/data/disaster.txt", sep = "\t", dec = ".")
View(data)
data = data %>% select(lat = LATITUDE, long = LONGITUDE, country = COUNTRY, deaths = DEATHS, depth = FOCAL_DEPTH, mag = EQ_MAG_MS) 
data = na.omit(data)

fit = lm( deaths ~ lat + long + depth + mag, data = data)
summary(fit)
fit

