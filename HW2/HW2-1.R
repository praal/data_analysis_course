library(readr)
library(ggplot2)

mobile = read_csv("~/Downloads/mobile_data.csv")


library(dplyr)



mobile %>%
  group_by(company) %>%
  summarise(count = n(), na.rm = TRUE) -> stat



newstat = stat[order(-stat$count), ]
newstat = head(newstat,20)
newstat
p = ggplot(data = newstat, aes(x = reorder(company, count), y = count))
p = p + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 
p + geom_col(color = "purple", fill = "purple", alpha = 0.7) + xlab('company') + ylab('count') + ggtitle("20 Max Companies")





