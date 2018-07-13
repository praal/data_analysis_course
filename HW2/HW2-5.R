library(readr)
library(ggplot2)
library(dplyr)
mobile = read_csv("~/Downloads/mobile_data.csv")

mobile$ppi = mobile$px_col * mobile$px_row / mobile$display_size

p = ggplot(data = mobile, aes(x = ppi)) 
p + geom_histogram(fill = "purple")


mobile %>% 
  group_by(year) %>% 
  summarise(avg_ppi = mean(ppi, na.rm = TRUE)) -> pp

p = ggplot(data = pp, aes(x = year, y = avg_ppi))
p + geom_line(color = "purple")

c = head(arrange(mobile, -ppi),1)
c
