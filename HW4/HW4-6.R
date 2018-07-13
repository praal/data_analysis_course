library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

timss = read_rds("~/Downloads/timss_2015.rds") 
timss %>% filter(cognitive_domain == "Applying" & content_domain == "Geometry") -> timss

timss %>% select(question, correct_ratio_per_question_female,correct_ratio_per_question_male) -> d
d %>% mutate(gender = "male") -> males
males %>% select(correct = correct_ratio_per_question_male, gender) -> males

d %>% mutate(gender = "female") -> females
females %>% select(correct = correct_ratio_per_question_female, gender) -> females

alls = rbind(males, females)

ggplot(data = alls) + geom_density( aes(x = as.numeric(correct), fill = as.character(gender)), alpha = 0.3)

t.test(correct ~ gender, data = alls)

hchart(density(females$correct), type = "area", name = "female") %>% hc_add_series(density(males$correct), type = "area", name = "male")