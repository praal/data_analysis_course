library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

bsg= read_rds("~/Downloads/bsg.rds") 



bsg %>% select(bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05, bsbg07a, bsbg07b) -> scores

scores %>% mutate(nomre = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0)) -> scores

scores %>% filter(bsbg07a != 8 & bsbg07b != 8) -> scores

scores %>% mutate(edu = round((bsbg07a + bsbg07b)/5, 0)) -> fives



fives %>% select(nomre, edu) -> p
p = na.omit(p)
p %>% mutate(education = as.character(edu)) -> p
ggplot(data = p) + geom_density( aes(x = as.numeric(nomre), fill = education), alpha = 0.3)

aov(nomre ~ edu , data = p) -> fit
summary(fit)

scores %>% mutate(edu = round((bsbg07a + bsbg07b)/10, 0)) -> two
two %>% filter(edu == 0) -> yek
two %>% filter(edu == 1) -> dow
hchart(density(yek$nomre), type = "area", name = "low educated") %>% hc_add_series(density(dow$nomre), type = "area", name = "high educated") 


t.test(dow$nomre, yek$nomre, alt = "greater")