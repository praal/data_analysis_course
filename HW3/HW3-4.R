library(devtools)
library(engsoccerdata)
library(dplyr)
library(highcharter)
library(ggplot2)

data(package = "engsoccerdata")

fdb = as.tbl(spain)
fdb = arrange(fdb, Date)

fdb %>% filter(Season > 2000 & Season <= 2010) -> d
d %>% filter(home == "Real Madrid" | home == "FC Barcelona" | home == "Atletico Madrid" | visitor == "Real Madrid" | visitor == "FC Barcelona" | visitor == "Atletico Madrid") -> d
d %>% filter(home == "Real Madrid") -> dreal1
d %>% filter(visitor == "Real Madrid") -> dreal2

dreal1 %>% group_by(team = visitor) %>% summarise(wins1 = sum(vgoal > hgoal), tot1 = n()) -> dreal1
dreal2 %>% group_by(team = home) %>% summarise(wins2 = sum(vgoal > hgoal), tot2 = n()) -> dreal2

dreal = full_join(dreal1, dreal2)
dreal %>% mutate(wins = wins1 + wins2, tot = tot1 + tot2) -> dreal
dreal %>% filter(team != "FC Barcelona" & team != "Atletico Madrid") -> dreal
dreal = arrange(dreal , -wins)
dreal = head(dreal , 4)

ggplot(dreal , aes(x = team , y = wins)) + geom_col(fill = "purple" , alpha = 0.6) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 



d %>% filter(home == "FC Barcelona") -> dbarca1
d %>% filter(visitor == "FC Barcelona") -> dbarca2

dbarca1 %>% group_by(team = visitor) %>% summarise(wins1 = sum(vgoal > hgoal), tot1 = n()) -> dbarca1
dbarca2 %>% group_by(team = home) %>% summarise(wins2 = sum(vgoal > hgoal), tot2 = n()) -> dbarca2

dbarca = full_join(dbarca1, dbarca2)
dbarca %>% mutate(wins = wins1 + wins2, tot = tot1 + tot2) -> dbarca
dbarca %>% filter(team != "Real Madrid" & team != "Atletico Madrid") -> dbarca
dbarca = arrange(dbarca , -wins)
dbarca = head(dbarca , 4)

ggplot(dbarca , aes(x = team , y = wins)) + geom_col(fill = "purple" , alpha = 0.6) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 


d %>% filter(home == "Atletico Madrid") -> dmad1
d %>% filter(visitor == "Atletico Madrid") -> dmad2

dmad1 %>% group_by(team = visitor) %>% summarise(wins1 = sum(vgoal > hgoal), tot1 = n()) -> dmad1
dmad2 %>% group_by(team = home) %>% summarise(wins2 = sum(vgoal > hgoal), tot2 = n()) ->dmad2

dmad= full_join(dmad1, dmad2)
dmad %>% mutate(wins = wins1 + wins2, tot = tot1 + tot2) -> dmad
dmad %>% filter(team != "Real Madrid" & team != "FC Barcelona") -> dmad
dmad = arrange(dmad , -wins)
dmad= head(dmad , 4)

ggplot(dmad , aes(x = team , y = wins)) + geom_col(fill = "purple" , alpha = 0.6) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
