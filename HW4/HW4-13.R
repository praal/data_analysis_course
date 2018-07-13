library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

bsg= read_rds("~/Downloads/bsg.rds") 

bsg %>% select(bsmmat01, bsmmat02, bsmmat03, bsmmat04, bsmmat05, bsbm25ba) -> scores

scores %>% mutate(score = round((bsmmat01 + bsmmat02 + bsmmat03 + bsmmat04 + bsmmat05) / 5, 0),   homework= bsbm25ba) -> scores

scores= na.omit(scores)
ggplot(data = scores) + geom_density( aes(x = as.numeric(score), fill = as.character(homework)), alpha = 0.3)


aov(score ~ homework , data = scores) -> fit
summary(fit)
