library(WDI)
library(readr)
library(dplyr)
library(tidyr)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")



data %>% filter(`Indicator Code` == "GC.XPN.TOTL.GD.ZS") ->gdp

gdp %>% filter(`Country Code` == "IRN") -> irn
irn = irn[17:53]
pr = irn[,54]
irn = t(irn)
irn = as.data.frame(irn)
irn <- add_rownames(irn, "year")
fit = lm(data = irn, V1 ~ year)
fit
