library(WDI)
library(readr)
library(dplyr)
library(reshape)
library(stats)
library(tidyr)
library(ggplot2)
data = read_csv("~/Downloads/WDI_csv/WDIData.csv")

data %>% filter(`Indicator Code` == "SP.DYN.LE00.IN") -> life
life = life[,-3]
life = life[,-3]
life = life[,-2]
life = life[, 1:58]
life <- life %>% gather(Year, Exp, 2:58)
life %>% filter(`Country Name` %in% c("Rwanda")) -> rw
ggplot(mapping = aes(x = Year, y = Exp)) + geom_boxplot(data = life) + geom_point(data = rw, color = "purple", size = 5) + theme(axis.text.x = element_text(angle=-90, vjust=0.5))
