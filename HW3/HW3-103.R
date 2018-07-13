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

d4 %>% group_by(team) %>% summarise(num = sum(Season > 0)) ->d5


d5 = arrange(d5, -num)
d5 = head(d5, 20)
d5 %>% select(team0 = team) %>% unique() -> teams
teams %>% select(team1 = team0) -> teams1

alls = merge(teams, teams1)

alls %>% mutate(wins = 0) -> alls

for (i in 1:nrow(fdb)){
  x = fdb[i,]
  if(x$hgoal > x$vgoal)
    alls$wins[alls$team0 == x$home & alls$team1 == x$visitor] =   alls$wins[alls$team0 == x$home & alls$team1 == x$visitor] + 1
  
  if(x$hgoal < x$vgoal)
    alls$wins[alls$team0 == x$visitor & alls$team1 == x$home] =   alls$wins[alls$team0 == x$visitor & alls$team1 == x$home] + 1
}
ggplot(alls, aes(team0, team1, fill="red")) + geom_tile(aes(width=.95, height=.95)) + geom_text(aes(label=wins), size=2) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
