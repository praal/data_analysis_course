library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)


fdb = as.tbl(spain)

fdb = arrange(fdb, Date)

fdb %>% filter(Season == 2012) -> d

d %>% select(home, visitor, FT) -> d
ggplot(d, aes(home, visitor, fill=FT)) + geom_tile(aes(width=.95, height=.95)) + geom_text(aes(label=FT), size=2) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
