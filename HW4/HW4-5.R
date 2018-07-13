library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

btm= read_rds("~/Downloads/btm.rds") 

btm %>% select(idcntry, idschool, btbg01, btbg04) -> teachers

teachers$id = paste(teachers$idcntry, teachers$idschool)

bsg= read_rds("~/Downloads/bsg.rds") 

bsg %>% select(idcntry, idschool,bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05) -> scores

scores %>% mutate(score = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0)) -> scores
scores$id = paste(scores$idcntry, scores$idschool)

teachers %>% mutate(eduexpr = btbg01 + 3 * btbg04) -> teachers
teachers %>% select(id, eduexpr) -> teachers
teachers %>% group_by(id) %>% summarise(toteduexpr = mean(eduexpr)) -> teachers

scores %>% select(id, score) -> scores

both = full_join(teachers, scores)
both = na.omit(both)
both %>% mutate(educAndExpr = round(toteduexpr/20, 0)) -> both

ggplot(data = both) + geom_density( aes(x = as.numeric(score), fill = as.character(educAndExpr)), alpha = 0.3)

aov(score ~ educAndExpr , data = both) -> fit
summary(fit)

both %>% mutate(eduAndExpr = round(toteduexpr/ 50, 0)) -> both

both %>% filter(eduAndExpr == 0) -> yek
both %>% filter(eduAndExpr == 1) -> dow

t.test(dow$score, yek$score, alt = "greater") 

hchart(density(yek$score), type = "area", name = "low education and experience") %>% hc_add_series(density(dow$score), type = "area", name = "high education and experience")

