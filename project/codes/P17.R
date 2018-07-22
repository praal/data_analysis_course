library(readr)
library(dplyr)

library(ggplot2)
g= read_rds("~/Desktop/Data Analysis/data/Articles/Articles.rds")

View(g)

g %>% group_by(source_id) %>% summarise(count = n()) %>% arrange(-count) -> source
View(source)

source %>% select(id = source_id, count) -> source


s= read_rds("~/Desktop/Data Analysis/data/Articles/articlesource.rds")


s %>% select(id, title) -> s
f = full_join(s, source)

f = arrange(f, -count)

ggplot(head(f, 10), aes(x = reorder(title,count), y = count)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, size = 8)) + xlab("Source")

