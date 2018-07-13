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

d4 %>% select(Season, team) -> d4


fdb %>% filter((home == "FC Barcelona" & visitor == "Real Madrid") | (visitor == "FC Barcelona" & home == "Real Madrid")) -> darbi

darbi %>% filter(home == "FC Barcelona") -> hb
darbi %>% filter(home != "FC Barcelona") -> hm

hb %>% group_by(Season) %>% summarise(bcw0 = sum(hgoal > vgoal), rmw0 = sum(hgoal < vgoal), d0 = sum(hgoal == vgoal)) ->hb
hm %>% group_by(Season) %>% summarise(rmw1 = sum(hgoal > vgoal), bcw1 = sum(hgoal < vgoal), d1 = sum(hgoal == vgoal)) ->hm

tot = full_join(hb, hm)
tot %>% mutate(rmw = rmw0 + rmw1, bcw = bcw0 + bcw1, d = d0 + d1)->tot


tot %>% filter(d == 0 & (bcw == 0 | rmw == 0)) -> tot
tot %>% select(Season, bcw, rmw) -> tot
tot %>% mutate(champion = FALSE) -> tot

for (i in 1:nrow(tot)){
  x = tot[i,]
  if(x$bcw > x$rmw){
    ch = d4$team[d4$Season == x$Season]
    if(ch == "FC Barcelona")
      tot$champion[tot$Season == x$Season] = TRUE
  }
  
  if(x$bcw < x$rmw){
    ch = d4$team[d4$Season == x$Season]
    if(ch == "Real Madrid")
      tot$champion[tot$Season == x$Season] = TRUE
  }
}

tot = na.omit(tot)
tot %>% summarise(rate = 100 * sum(champion) / n() )