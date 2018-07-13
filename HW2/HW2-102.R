library(readr)
library(ggplot2)
library(dplyr)
mobile = read_csv("~/Downloads/mobile_data.csv")

mobile %>% filter(company == "Apple" | (company == "Samsung") | (company == "LG") | (company == "Sony")) -> mobile

mobile %>% group_by(company, year)  %>% 
  summarise(avg_cam = mean(cam_px, na.rm = TRUE)) -> alls
alls

ggplot(alls) + geom_line(aes(x = year, y = avg_cam, colour = company))