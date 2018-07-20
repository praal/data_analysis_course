library(WDI)
library(readr)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")


data %>% filter(`Indicator Code` == "SI.POV.LMIC") ->pov

pov$mean <- rowMeans(subset(pov, select = c("2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

pov %>% filter(mean != "NaN") -> pov
pov[rev(order(pov$mean)),] -> pov
head(pov$`Country Name`, 20)