library(readr)
library(dplyr)


g= read_csv("~/Desktop/Data Analysis/project/authorsgraph.csv")

View(g)
colnames(g) = c("v1", "v2")
g %>% group_by(v1, v2) %>% summarise(count = n()) -> g


mean(g$count)


r = g %>% filter(count < 100)
ggplot(r, aes(x = count)) + geom_histogram(binwidth = 1, fill = "purple")
