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
d3 %>% mutate(diffwithone = 0) ->d3


for (i in 1:nrow(d3)){
  x = d3[i,]
  name =  d4$team[d4$Season == x$Season]
  score =  d4$score[d4$Season == x$Season]
  
  if(x$team == name)
    d3$diffwithone[d3$Season == x$Season & d3$team == x$team] = -1
  else
    d3$diffwithone[d3$Season == x$Season & d3$team == x$team] = score - x$score
  
}

d3 %>% filter(diffwithone > -1) -> d5
d5 %>% group_by(Season) %>% slice(which.min(diffwithone)) -> d5
d5 = arrange(d5, -diffwithone)
d5 %>% select(Season, second = team, diff = diffwithone) -> d5
d4 %>% select(Season, first = team) -> d4
d6 = full_join(d4, d5)

d6 = arrange(d6 , -diff)
d6 = head(d6, 10)

ggplot(d6, aes(x = interaction(Season, first), y = diff)) + geom_col(fill = "purple" , alpha = 0.5) +theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
d6 %>% hchart(type = "bar", hcaes(x = interaction(Season, first), y = diff), name = "difference with second")
