library(WDI)
library(readr)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")

p = WDIsearch(string = "health", cache = NULL)
data %>% filter(`Indicator Code` == "SP.DYN.LE00.IN") ->life

data %>% filter(`Indicator Code` == "SH.XPD.CHEX.PP.CD") ->exp

life$mean <- rowMeans(subset(life, select = c("2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)
exp$mean <- rowMeans(subset(exp, select = c("2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

life %>% filter(mean != "NaN") -> life
exp %>% filter(mean != "NaN") -> exp

life %>% select(`Country Name`, life = mean) -> life
exp %>% select(`Country Name`, exp = mean)-> exp
fin = inner_join(life, exp)
ggplot(fin , aes(x = exp, y = life)) + geom_point(color = "purple")