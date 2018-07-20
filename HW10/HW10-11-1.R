library(WDI)
library(readr)
library(dplyr)
library(tidyr)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")

p = WDIsearch(string = "gdp", cache = NULL)

data %>% filter(`Country Name` %in% c("Russian Federation", "United Kingdom", "United States")) -> tri
tri %>% filter(`Indicator Code` == "NE.TRD.GNFS.ZS") -> ss
ss = ss[,-2]
ss = ss[,-2]
ss = ss[,-2]
ss<- ss %>% gather(Year, gdp, 2:58)

ss = ss[, c(1, 4, 5)]
ss %>% select(country = `Country Name`, Year, gdp) -> ss
ggplot(ss) + geom_point(mapping = aes(x = Year, y = gdp,  color = country))
                