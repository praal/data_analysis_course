library(WDI)
library(readr)
library(dplyr)
data = read_csv("~/Downloads/WDI_csv/WDIData.csv")


data%>% filter(`Indicator Code`=="SI.SPR.PCAP") -> income

income$mean <- rowMeans(subset(income, select = c("2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

income %>% filter(mean != "NaN") -> income
names = c("Congo, Dem. Rep.", "Madagascar", "Burundi", "Malawi","Guinea-Bissau" , "Mozambique","Rwanda" ,"Sierra Leone", "Niger","Tanzania","Lesotho","Zambia","Benin","Burkina Faso","Low income","Togo" ,"Liberia" ,"Timor-Leste", "Guinea" ,"Sao Tome and Principe")
income %>% filter(`Country Name` %in% names) %>% select(`Country Name`, mean)->income
income