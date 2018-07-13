library(readr)


consum = read_csv("~/Downloads/hw_05/hw_05/data/consumption.csv")

wilcox.test(consum$A, consum$B)
