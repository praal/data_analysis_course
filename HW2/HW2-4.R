library(readr)
library(ggplot2)

mobile = read_csv("~/Downloads/mobile_data.csv")


theme = theme_set(theme_minimal())

p = ggplot(data = subset(mobile, !is.na(audio_jack)), aes(x = audio_jack, y = dim_thickness)) + xlab("Audio Jack") + ylab("Thickness")
p + geom_boxplot()
