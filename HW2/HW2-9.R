library(readr)
library(ggplot2)
library(dplyr)
mobile = read_csv("~/Downloads/mobile_data.csv")

m = filter(mobile, company == "Samsung")

m %>% 
  group_by(year) %>% 
  slice(which.max(price)) -> m

p = ggplot(m, aes(x= year, y= price))
p +   geom_point(color="purple") + geom_text(aes(label=as.character(device),hjust=0,vjust=0), size = 2.5)
                                             