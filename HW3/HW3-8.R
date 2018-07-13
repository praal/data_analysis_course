library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)

fdb = as.tbl(spain)
fdb = arrange(fdb, Date)
fdb %>% mutate(day = substr(Date, 6, 10)) -> games
games %>% filter(Season == 1998) -> games


games %>% select(name = home) %>% unique() -> teams
games %>% select(Date) %>% unique()-> dates

dates = merge(dates, teams)
dates %>% mutate(rank = 0) -> dates

teams %>% mutate(score = 0 , goals = 0 , diff = 0) -> teams

for (i in 1:nrow(games)){
  x = games[i,]
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
  
  
  teams = arrange(teams, -score, -diff , -goals)
  for (j in 1:nrow(teams)){
    y = teams[j,]
    dates$rank[dates$Date == x$Date & dates$name == y$name] = j
  }
}
ggplot(dates, aes(x = Date, y = rank , color = name)) + geom_line()
