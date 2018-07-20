library(WDI)
library(readr)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")

p = WDIsearch(string = "consumption", cache = NULL)

data %>% filter(`Indicator Code` == "NE.CON.PRVT.CD") ->h

h %>% filter(`Country Name` == "Iran, Islamic Rep.") -> h


h %>% select_if(~sum(!is.na(.)) > 0) ->h


chisq.test(h[5:60])
h = h[5:60]
h = t(h)
h = as.data.frame(h)
h <- add_rownames(h, "year")
colnames(h) = c("year", "value")
ggplot(h, aes(x = year, y = value)) + geom_point() +  theme(axis.text.x = element_text(angle=-90, vjust=0.5))