library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

bsg= read_rds("~/Downloads/bsg.rds") 

bsg %>% select(bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05, bsbm17a, bsbm17b, bsbm17c, bsbm17d, bsbm17e, bsbm17f, bsbm17g, bsbm17h, bsbm17i) -> scores

scores %>% mutate(score = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0), love = round((bsbm17a - bsbm17b + bsbm17c + bsbm17d + bsbm17e + bsbm17f + bsbm17g + bsbm17h + bsbm17i)/10, 0)) -> scores
scores= na.omit(scores)
ggplot(data = scores) + geom_density( aes(x = as.numeric(score), fill = as.character(love)), alpha = 0.3)


aov(score ~ love , data = scores) -> fit
summary(fit)

scores %>% mutate(totalLove = round((love-1)/ 2 ,0)) -> hyp
hyp %>% filter(totalLove == 0) -> yek
hyp %>% filter(totalLove == 1) -> dow

t.test(yek$score, dow$score, alt = "greater") 

hchart(density(r$correct_ratio_per_question), type = "area", name = "reasoning") %>% hc_add_series(density(ap$correct_ratio_per_question), type = "area", name = "applying")