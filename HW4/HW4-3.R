library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

bsg= read_rds("~/Downloads/bsg.rds") 

bsg %>% select(idcntry, idschool, idclass, idstud, bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05, bsbg06a, bsbg06b, bsbg06c, bsbg06d, bsbg06e, bsbg06f, bsbg06g, bsbg06h, bsbg06i, bsbg06j, bsbg06k) -> scores

scores %>% mutate(nomre = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0)) -> scores

scores %>% mutate(facilities= 22 - (bsbg06a + bsbg06b + bsbg06c + bsbg06d + bsbg06e + bsbg06f + bsbg06g + bsbg06h + bsbg06i + bsbg06j + bsbg06k)) -> ziad


ziad %>% select(nomre, facilities) -> p
p = na.omit(p)

ggplot(data = p) + geom_density( aes(x = as.numeric(nomre), fill = as.character(facilities)), alpha = 0.3)

aov(nomre ~ facilities , data = p) -> fit
summary(fit)

p%>% mutate(totalFacilities = round(facilities / 8,0)) -> hyp


hyp %>% filter(totalFacilities == 0) -> yek
hyp %>% filter(totalFacilities == 1) -> dow
t.test(yek$nomre, dow$nomre, alt = "less")
hchart(density(yek$nomre), type = "area", name = "low facilities") %>% hc_add_series(density(dow$nomre), type = "area", name = "high facilities")
