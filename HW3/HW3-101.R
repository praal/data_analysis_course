library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)


fdb = as.tbl(spain)
fdb = arrange(fdb, Date)


fdb %>% group_by(Season, team = home) %>% summarise(w1 = sum(hgoal > vgoal), dr1 = sum(hgoal == vgoal), g1 = sum(hgoal), t1 = sum(vgoal)) -> d1
fdb %>% group_by(Season, team = visitor) %>% summarise(w2 = sum(vgoal > hgoal), dr2 = sum(hgoal == vgoal), g2 = sum(vgoal), t2 = sum(hgoal)) -> d2


d3 = full_join(d1,d2)
d3 %>% group_by(team) %>% summarise(whome = sum(w1), wvisit = sum(w2), gh = sum(g1), gv = sum(g2)) -> d3
d3 %>% mutate(rate = whome / wvisit, grate = gh/gv) -> d3
d3 = arrange(d3, -whome - wvisit)
d3 = head(d3, 20)

ggplot(d3, aes(x = team, y = rate)) + geom_col(fill= "purple") +theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 