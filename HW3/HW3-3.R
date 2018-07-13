library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)

data(package = "engsoccerdata")

fdb = as.tbl(spain)

fdb = arrange(fdb, Date)

fdb %>% select(name = home) %>% unique() -> teams

fdb %>% group_by(Season) %>% summarise(count = n()) -> seas

seas %>% mutate(half = "Empty", total = "empty") -> seas

teams %>% mutate(score = 0, goals = 0, diff = 0) -> teams


prev = 0
c = 0

for (i in 1:nrow(fdb)){
  x = fdb[i,]
  now = x$Season
  if(now != prev){
    c = 0
    teams$score = 0
    teams$goals = 0
    teams$diff = 0
  }
  prev = now
  
  homescore = teams$score[teams$name == x$home]
  visitorscore = teams$score[teams$name == x$visitor]
  
  homegoals = teams$goals[teams$name == x$home]
  visitorgoals = teams$goals[teams$name == x$visitor]
  
  homediff = teams$diff[teams$name == x$home]
  visitordiff = teams$diff[teams$name == x$visitor]
  
  if(x$hgoal == x$vgoal){
    teams$score[teams$name == x$home] = homescore + 1
    teams$score[teams$name == x$visitor] = visitorscore + 1
    
  }
  else if(x$hgoal > x$vgoal)
    teams$score[teams$name == x$home] = homescore + 3
  else
    teams$score[teams$name == x$visitor] = visitorscore + 3
  
  
  
  teams$goals[teams$name == x$home] = homegoals + x$hgoal
  teams$goals[teams$name == x$visitor] = visitorgoals + x$vgoal
  
  
  teams$diff[teams$name == x$home] = homediff + x$hgoal - x$vgoal
  teams$diff[teams$name == x$visitor] = visitordiff + x$vgoal - x$hgoal
  
  c = c+1
  tot = seas$count[seas$Season == x$Season]
  if(c * 2 == tot){
    teams = arrange(teams, -score, -diff, -goals)
    halfw = head(teams, 1)
    seas$half[seas$Season == x$Season] = halfw$name
  }
  else if(c == tot){
    
    teams = arrange(teams, -score, -diff, -goals)
    totw = head(teams, 1)
    seas$total[seas$Season == x$Season] = totw$name
  }
}

View(seas)
seas %>% summarise(both = sum(half == total), tot = n()) -> final
final %>% mutate(rate = both/ tot)
