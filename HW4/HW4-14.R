library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

btm= read_rds("~/Downloads/btm.rds") 

btm %>% select(idcntry, idschool, btbm24a , btbm24b, btbm24c, btbm24d, btbm24e, btbm24f, btbm24g, btbm25) -> teachers

teachers$id = paste(teachers$idcntry, teachers$idschool)

bsg= read_rds("~/Downloads/bsg.rds") 

bsg %>% select(idcntry, idschool,bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05) -> scores

scores %>% mutate(score = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0)) -> scores
scores$id = paste(scores$idcntry, scores$idschool)

teachers %>% mutate(prepare= 14 - (btbm24a +  btbm24b +  btbm24c + btbm24d + btbm24e +  btbm24f+ btbm24g) + 2 * btbm25) -> teachers

teachers %>% select(id, prepare) -> teachers

teachers %>% group_by(id) %>% summarise(preparation = round(mean(prepare/12), 0)) -> teachers
scores %>% select(id, score) -> scores

alls = full_join(scores, teachers)

alls = na.omit(alls)

ggplot(data = alls) + geom_density( aes(x = as.numeric(score), fill = as.character(preparation)), alpha = 0.3)

t.test(score ~ preparation, data = alls, alt = "less")
