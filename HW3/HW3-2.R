library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)


fdb = as.tbl(spain)
fdb = arrange(fdb, Date)

fdb %>% group_by(team = home) %>% summarise(dr1 = sum(hgoal == vgoal), z1 = sum(hgoal == 0), dz1 = sum(hgoal == 0 & vgoal == 0)) -> d1
fdb %>% group_by(team = visitor) %>% summarise( dr2 = sum(hgoal == vgoal),z2 = sum(vgoal == 0), dz2 = sum(hgoal == 0 & vgoal == 0)) -> d2


d3 = full_join(d1,d2)

d3 %>% mutate(draws = dr1 + dr2, zerodraws = dz1 + dz2 , zerogames = z1 + z2) -> d3

d3 %>% mutate(score = zerodraws + 2 * zerogames) -> d3
d3 = arrange(d3, -score)
d3 = head(d3, 10)

ggplot(data = d3, aes(x = team, y = score)) + geom_col(fill = "purple")


d3 %>% hchart(type = "bar", hcaes(x = team , y = score), name = "boringness")



#season

fdb %>% group_by(Season) %>% summarise(goals = sum(hgoal + vgoal), draws = sum(hgoal == vgoal), zdraws = sum(hgoal == vgoal & hgoal == 0)) -> d7
d7 %>% mutate(score = zdraws * 2 + draws ) -> d7
d7= arrange(d7, -score, goals)
d7 = head(d7, 10)

ggplot(data = d7, aes(x = Season, y = score)) + geom_col(fill = "purple")


d7 %>% hchart(type = "bar", hcaes(x = Season , y = score), name = "boringness")






