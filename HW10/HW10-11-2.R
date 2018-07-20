library(WDI)
library(readr)
library(dplyr)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")

data %>% filter(`Indicator Code` == "EG.USE.ELEC.KH.PC") -> elec
elec %>% select(`Country Name`, `2014`) %>% na.omit()-> elec
elec = elec[order(elec$`2014`),]
elec = tail(elec, 100)
ggplot(elec) + geom_col( aes(x = reorder(`Country Name`, `2014`), y = `2014`),  fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5))