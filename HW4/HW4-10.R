library(readr)
library(dplyr)
library(ggplot2)
library(highcharter)

timss = read_rds("~/Downloads/timss_2015.rds") 
timss %>% filter(Country == "Iran, Islamic Rep. of") -> timss
timss %>% filter(cognitive_domain == "Reasoning" | cognitive_domain == "Applying") -> timss
timss %>% select(cognitive_domain, correct_ratio_per_question) -> d


ggplot(data = d) + geom_density( aes(x = as.numeric(correct_ratio_per_question), fill = as.character(cognitive_domain)), alpha = 0.3)


d %>% filter(cognitive_domain == "Reasoning") %>%  select(correct_ratio_per_question)-> r
d %>% filter(cognitive_domain != "Reasoning") %>%  select(correct_ratio_per_question)-> ap
t.test(x = r, y = ap , data = d, alt = "less")

hchart(density(r$correct_ratio_per_question), type = "area", name = "reasoning") %>% hc_add_series(density(ap$correct_ratio_per_question), type = "area", name = "applying")
