library(WDI)
library(readr)
library(dplyr)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")

p = WDIsearch(string = "life", cache = NULL)
data %>% filter(`Indicator Code` == "SP.DYN.LE00.IN") ->pov
pov$mean <- rowMeans(subset(pov, select = c("2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

pov %>% filter(mean != "NaN") -> pov
names = c("Congo, Dem. Rep.", "Madagascar", "Burundi", "Malawi","Guinea-Bissau" , "Mozambique","Rwanda" ,"Sierra Leone", "Niger","Tanzania")
pov %>% filter(`Country Name` %in% names) %>% select(`Country Name`, mean)->pov
pov
