library(readr)
library(ggplot2)

mobile = read_csv("~/Downloads/mobile_data.csv")

p = ggplot(data = mobile, aes(x = year, y = cam_px)) 
p = p + geom_point(size = 2, alpha = 0.6, color = "purple") + xlab('Year') + ylab('Camera') + ggtitle("Camera")
p + geom_smooth(method="lm", se = FALSE)





