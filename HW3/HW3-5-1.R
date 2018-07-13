library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)


fdb = as.tbl(spain)
fdb = arrange(fdb, Date)
fdb %>% group_by(Season) %>% summarise(end = max(Date), result = 0, winner = "empty") -> seas

View(seas)
fdb %>% group_by(Season, name = home) %>% summarise(c1 = n()) -> d1
fdb %>% group_by(Season, name = visitor) %>% summarise(c2 = n()) -> d2
d3 = full_join(d1, d2)
d3 %>% mutate(count = c1 + c2) -> teams
teams %>% select(Season, name , count) -> teams
teams %>% mutate(score = 0 , future = 0) -> teams
View(teams)

for (i in 1:nrow(fdb)){

  x = fdb[i,]

  
  homescore = teams$score[x$Season == teams$Season & teams$name == x$home]
  visitorscore = teams$score[x$Season == teams$Season & teams$name == x$visitor]

  
  if(x$hgoal == x$vgoal){
    teams$score[x$Season == teams$Season & teams$name == x$home] = homescore + 1
    teams$score[x$Season == teams$Season & teams$name == x$visitor] = visitorscore + 1
    
  }
  else if(x$hgoal > x$vgoal)
    teams$score[x$Season == teams$Season & teams$name == x$home] = homescore + 3
  else
    teams$score[x$Season == teams$Season & teams$name == x$visitor] = visitorscore + 3
  
  teams$count[teams$Season == x$Season & teams$name == x$home] = teams$count[teams$Season == x$Season & teams$name == x$home] - 1
  teams$count[teams$Season == x$Season & teams$name == x$visitor] = teams$count[teams$Season == x$Season & teams$name == x$visitor] - 1


  
  teams %>% filter(Season == x$Season) -> t0
  t0 = arrange(t0 , -score)
  q = t0[1,]
  w = t0[2,]
  ss = w$score + teams$count[teams$Season == x$Season & teams$name == w$name] * 3
  if(ss < q$score){
    seas$result[seas$Season == x$Season] = max(seas$result[seas$Season == x$Season],seas$end[seas$Season == x$Season] - x$Date)
    seas$winner[seas$Season == x$Season] = q$name
  }
  print(x$Season)
}

View(seas)

seas = arrange(seas, -result)
seas = head(seas, 10)

seas %>% hchart("bar", hcaes(x = interaction(Season, winner), y = result), name = "days remaining")