library(readr)
library(ggplot)

graph = read_csv("~/Desktop/Data Analysis/project/authorsgraph.csv")

View(graph)
colnames(graph) = c("x", "y")
graph %>% rowwise() %>% mutate(v1 = min(x,y), v2 = max(x,y)) -> graph
graph %>% select(v1, v2) -> graph
graph %>% group_by(v1, v2) %>% summarise(cnt = n()) -> cnt

ggplot(cnt, aes(x = cnt)) + geom_histogram(color = "purple" , binwidth = 3) + xlab("multiplexity")
mean(cnt$cnt)