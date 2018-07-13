library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

btm= read_rds("~/Downloads/btm.rds") 

btm %>% select(idcntry, idschool, btbg10a, btbg10b, btbg10c, btbg10d, btbg10e, btbg10f, btbg10g, btbg11a, btbg11b, btbg11c, btbg11d, btbg11e, btbg11f, btbg11g, btbg11h, btdgtjs) -> teachers

teachers$id = paste(teachers$idcntry, teachers$idschool)

bsg= read_rds("~/Downloads/bsg.rds") 

bsg %>% select(idcntry, idschool,bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05) -> scores

scores %>% mutate(score = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0)) -> scores
scores$id = paste(scores$idcntry, scores$idschool)

#teachers %>% mutate(satis1 = 28 - (btbg10a + btbg10b + btbg10c + btbg10d + btbg10e + btbg10f + btbg10g)) -> teachers
#teachers %>% mutate(satis2 = (btbg11a + btbg11b + btbg11c + btbg11d + btbg11e + btbg11f + btbg11g + btbg11h)) -> teachers
#teachers %>% mutate(happiness = satis1 + satis2) -> teachers
teachers %>% mutate(satis = btdgtjs) -> teachers
teachers %>% select(id, satis) -> teachers

teachers %>% group_by(id) %>% summarise( happiness = round(mean(satis), 0)) -> teachers
scores %>% select(id, score) -> scores

both = full_join(teachers, scores)
both = na.omit(both)

ggplot(data = both) + geom_density( aes(x = as.numeric(score), fill = as.character(happiness)), alpha = 0.3)

aov(score ~ happiness , data = both) -> fit
summary(fit)

both %>% filter(happiness == 1 | happiness == 3) -> hyp
t.test(score ~ happiness, data = hyp) 

both %>% filter(happiness == 1) -> yek
both %>% filter(happiness == 2) -> dow
both %>% filter(happiness == 3) -> se

hchart(density(yek$score), type = "area", name = "low satsification") %>% hc_add_series(density(dow$score), type = "area", name = "medium satsification") %>% hc_add_series(density(se$score), type = "area", name = "high satsification")
