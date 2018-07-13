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
d3 %>% mutate(wins = w1 + w2, draws = dr1 + dr2, goals = g1 + g2, opgoals = t1 + t2) -> d3

d3 %>% mutate(diff = goals - opgoals) -> d3
d3 %>% mutate(score = 3 * wins + draws) -> d3

d3 = arrange(d3, -score,-diff, -goals, Season)

d3 %>% group_by(Season) %>% slice(which.max(score)) -> d4
View(d3)
d4 %>% group_by(team) %>% summarise(num = sum(Season > 0)) ->d5
View(d4)
ggplot(data = d5, aes(x = team, y = num)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5))

d5 %>% hchart(type = "bar", hcaes(x = team , y = num))
