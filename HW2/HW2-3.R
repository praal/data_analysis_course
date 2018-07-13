library(readr)
library(ggplot2)

mobile = read_csv("~/Downloads/mobile_data.csv")


library(dplyr)


m0 = mobile[which(mobile$LTE == "Yes"),]
m1 = mobile[which(mobile$LTE != "Yes"),]

m0 %>% 
  group_by(sim_no) %>%
  summarise(avg = mean(price, na.rm = TRUE)) -> fig0


m1 %>% 
  group_by(sim_no) %>%
  summarise(avg = mean(price, na.rm = TRUE)) -> fig1

fig0$name <- "LTE"
fig1$name <- "No LTE"
d = rbind(fig0, fig1)
p = ggplot(d, aes(sim_no, avg, fill = name)) + geom_bar(position = "dodge", stat = "identity") + xlab('Number of Simcards') + ylab('Average Price') 
p + geom_smooth(method="lm")
p


