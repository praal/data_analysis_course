library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

bsg= read_rds("~/Downloads/bsg.rds") 

bsg %>% select(bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05, bsbm19a, bsbm19b, bsbm19c, bsbm19d, bsbm19e, bsbm19f, bsbm19g, bsbm19h, bsbm19i) -> scores

scores %>% mutate(score = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0), strength = round((bsbm19a - bsbm19b - bsbm19c + bsbm19d - bsbm19e + bsbm19f + bsbm19g - bsbm19h - bsbm19i)/10 * (-1), 0)) -> scores
scores= na.omit(scores)
ggplot(data = scores) + geom_density( aes(x = as.numeric(score), fill = as.character(strength)), alpha = 0.3)


aov(score ~ strength , data = scores) -> fit
summary(fit)

scores %>% mutate(totalStrength = round((strength-1)/2 ,0)) -> scores
t.test(score ~ totalStrength, data = scores, alt = "less") 

scores %>% filter(totalStrength == -1) -> yek
scores %>% filter(totalStrength == 0) -> dow


hchart(density(yek$score), type = "area", name = "low strength") %>% hc_add_series(density(dow$score), type = "area", name = "high strength")