library(readr)

data = read_csv("~/Downloads/week_11/week_11/data/worldwide.csv")

cor.test(data$mag, data$depth)