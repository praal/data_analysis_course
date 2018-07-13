library(readr)
library(ggplot2)
library(dplyr)
mobile = read_csv("~/Downloads/mobile_data.csv")

p = ggplot(data = mobile, aes(x = battery_mah, y = weight)) + xlab("Battery") + ylab("Weight")
p + geom_point(color = "purple")

c = data_frame(mobile$battery_mah, mobile$weight)
ans = cor(c, use="complete.obs")
ans = head(ans,1)
ans[2]
