library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)
library(highcharter)

article = read_rds("~/Desktop/Data Analysis/data/Articles/article_author_affilation.rds")

article %>% select(au_id, affilation_id) %>% unique() -> auaf
auaf %>% group_by(au_id) %>% summarise(aff = round(mean(as.integer(affilation_id),1))) -> t

aff = read_rds("~/Desktop/Data Analysis/data/affilations.rds")
aff %>% select(aff = id, name) -> aff
t = inner_join(t, aff)
t = t[-1,]
g = read_csv("~/Desktop/Data Analysis/project/authorsgraph.csv")
colnames(g) = c("auth1", "auth2")
colnames(t) = c("auth1", "aff1")
g = inner_join(g, t)

colnames(t) = c("auth2", "aff2")
g = inner_join(g, t)
g %>% select(auth1, aff1, auth2, aff2) -> g
write_csv(g, "~/Desktop/Data Analysis/project/allhomophily.csv")
