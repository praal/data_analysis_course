library(readr)
library(ggplot2)
library(dplyr)
mobile = read_csv("~/Downloads/mobile_data.csv")


mobile$v = mobile$dim_breadth * mobile$dim_length * mobile$dim_thickness / 1000
mobile$density = (mobile$weight) / mobile$v


mobile %>%
  filter(density <= 1) -> allW

ggplot(data = allW,aes(density)) + geom_histogram(fill = "purple", alpha= 0.7) 