library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)

fdb = as.tbl(spain)
fdb = arrange(fdb, Date)

fdb %>% select(name = home) %>% unique()-> teams
teams %>% mutate(maxWin = 0 , maxLose = 0 , nowWin = 0 , nowLose = 0, nowDraw = 0 , maxDraw = 0) -> teams

for (i in 1:nrow(fdb)) {
  x = fdb[i,]
  home = x$home
  visitor = x$visitor
  
  teams$maxWin[teams$name == home] = max(teams$maxWin[teams$name == home], teams$nowWin[teams$name == home])
  teams$maxWin[teams$name == visitor] = max(teams$maxWin[teams$name == visitor], teams$nowWin[teams$name == visitor])
  
  teams$maxLose[teams$name == visitor] = max(teams$maxLose[teams$name == visitor], teams$nowLose[teams$name == visitor])
  teams$maxLose[teams$name == home] = max(teams$maxLose[teams$name == home], teams$nowLose[teams$name == home])
  
  teams$maxDraw[teams$name == visitor] = max(teams$maxDraw[teams$name == visitor], teams$nowDraw[teams$name == visitor])
  teams$maxDraw[teams$name == home] = max(teams$maxDraw[teams$name == home], teams$nowDraw[teams$name == home])
  
  
  if(x$hgoal > x$vgoal){
    teams$nowWin[teams$name == home] = teams$nowWin[teams$name == home] + 1
    teams$nowLose[teams$name == home] = 0
    
    
    teams$nowLose[teams$name == visitor] = teams$nowLose[teams$name == visitor] + 1
    teams$nowWin[teams$name == visitor] = 0
    
    
    teams$nowDraw[teams$name == visitor] = 0
    teams$nowDraw[teams$name == home] = 0
    
    
  }
  else if(x$hgoal < x$vgoal){
    teams$nowWin[teams$name == visitor] = teams$nowWin[teams$name == visitor] + 1
    teams$nowLose[teams$name == visitor] = 0
    
    teams$nowLose[teams$name == home] = teams$nowLose[teams$name == home] + 1
    teams$nowWin[teams$name == home] = 0
    
    
    teams$nowDraw[teams$name == visitor] = 0
    teams$nowDraw[teams$name == home] = 0
    
  }
  else{
    teams$nowWin[teams$name == home] = 0
    teams$nowWin[teams$name == visitor] = 0
   
    teams$nowLose[teams$name == visitor] = 0
    teams$nowLose[teams$name == home] = 0
    
    
    teams$nowDraw[teams$name == visitor] = teams$nowDraw[teams$name == visitor] + 1
    teams$nowDraw[teams$name == home] = teams$nowDraw[teams$name == home] + 1
    
  }
  
  teams$maxWin[teams$name == home] = max(teams$maxWin[teams$name == home], teams$nowWin[teams$name == home])
  teams$maxWin[teams$name == visitor] = max(teams$maxWin[teams$name == visitor], teams$nowWin[teams$name == visitor])
  
  teams$maxLose[teams$name == visitor] = max(teams$maxLose[teams$name == visitor], teams$nowLose[teams$name == visitor])
  teams$maxLose[teams$name == home] = max(teams$maxLose[teams$name == home], teams$nowLose[teams$name == home])

  
}


maxwins = arrange(teams, -maxWin)
maxwins = head(maxwins, 5)
maxloses = arrange(teams, -maxLose)
maxloses = head(maxloses , 5)
maxdraws = arrange(teams, -maxDraw)
maxdraws = head(maxdraws , 5)


maxwins %>% hchart(type = "bar", hcaes(x = name , y = maxWin), name = "number of wins")
maxloses %>% hchart(type = "bar", hcaes(x = name , y = maxLose), name = "number of loses")
maxdraws %>% hchart(type = "bar", hcaes(x = name , y = maxDraw), name = "number of draws")

