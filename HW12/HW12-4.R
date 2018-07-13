library(readr)
library(arules)
library(arulesViz)
library(colorspace)
library(dplyr)

rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat",  delim="$")

rate = rate %>% select(USER, ID = FILM, RATE) %>% filter(RATE >= 4)

trans <- rate %>% inner_join(movie) %>% group_by(USER) %>% summarise(names= list(c(Name)))

trans <- as(trans$names, "transactions")
allrules <- apriori(trans, parameter = list(support = 0.01,confidence = 0.25, minlen = 2))

rules = subset(allrules, lhs %in% c("Castle in the Sky (Tenkû no shiro Rapyuta) (1986)", 
                                              "Cast Away (2000)", 
                                              "No Country for Old Men (2007)",
                                              "Memento (2000)"))
inspect(rules_subset) %>% View()