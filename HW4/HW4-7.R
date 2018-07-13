library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

bsg= read_rds("~/Downloads/bsg.rds") 



bsg %>% select(idcntry, idschool,bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05, bsbg12) -> scores

scores %>% mutate(nomre = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0)) -> scores


bcg = read_rds("~/Downloads/bcg.rds")
bcg %>% select(idcntry, idschool, bcbg06a, bcbg06b) -> school
school$id = paste(school$idcntry, school$idschool)
school %>% mutate(food2 = 6 - bcbg06a - bcbg06b) -> school
school %>% select(id, food2) -> school
scores$id = paste(scores$idcntry, scores$idschool)
scores %>% mutate(food1 = 4 - bsbg12) -> scores
scores %>% select(id , nomre, food1) -> scores

scores %>% group_by(id) %>% summarise(totfood = mean(food1), totScore = mean(nomre)) -> scores
both = full_join(school, scores)
both %>% mutate(food = round((food2 + 10 * totfood)/25, 0)) -> both
both = na.omit(both)

ggplot(data = both) + geom_density( aes(x = as.numeric(totScore), fill = as.character(food)), alpha = 0.3)

both %>% filter(food == 0) -> yek
both %>% filter(food == 1) -> dow
t.test(yek$totScore, dow$totScore, alt = "less") 

hchart(density(yek$totScore), type = "area", name = "less food") %>% hc_add_series(density(dow$totScore), type = "area", name = "more food")