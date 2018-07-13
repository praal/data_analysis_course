library(readr)
library(arules)
library(arulesViz)
library(colorspace)
library(dplyr)

rate = read_delim("~/Downloads/ml-10m/ml-10M100K/ratings.dat", delim = "::")
movie = read_delim("~/Downloads/ml-10m/ml-10M100K/movies.dat",  delim="$")

rate = rate %>% select(USER, ID = FILM, RATE) %>% filter(RATE >= 3)

trans <- rate %>% inner_join(movie) %>% group_by(USER) %>% summarise(names= list(c(Name)))

trans <- as(trans$names, "transactions")

allrules <- apriori(trans, parameter = list(support = 0.001,confidence = 0.25, minlen = 2), appearance = list(lhs=c("Castle in the Sky (Tenkû no shiro Rapyuta) (1986)", 
                                                                                                                   "No Country for Old Men (2007)",
                                                                                                                   "Cast Away (2000)",
                                                                                                                   "Memento (2000)")))



rules_info <-
  data.frame(
    LHS = labels(lhs(allrules)), 
    RHS = labels(rhs(allrules)),          
    quality(allrules)
  )
alls = rules_info[5485:5584,]
alls = alls[order[-alls$lift],]
head(alls, 10)$RHS
