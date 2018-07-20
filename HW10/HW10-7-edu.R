
library(WDI)
library(readr)
library(dplyr)
library(reshape)


data = read_csv("~/Downloads/WDI_csv/WDIData.csv")
indic = list("SE.XPD.TOTL.GD.ZS", "SE.XPD.PRIM.PC.ZS", "SE.XPD.SECO.PC.ZS", "SL.TLF.TOTL.IN", "SE.ADT.LITR.ZS", "SE.ADT.1524.LT.ZS", "SE.ADT.1524.LT.MA.ZS", "SE.ADT.LITR.MA.ZS", "SE.PRM.PRSL.ZS", "SE.PRM.PRSL.MA.ZS", "SE.PRM.CMPT.ZS", "SE.SEC.PROG.FE.ZS", "SE.TER.ENRL.TC.ZS", "SE.PRM.REPT.ZS", "SE.PRM.NENR", "SE.ENR.TERT.FM.ZS", "SE.ENR.SECO.FM.ZS", "SL.UEM.TOTL.FE.ZS", "SE.PRM.UNER.ZS", "SE.PRM.CMPT.ZS")
data %>% filter(`Indicator Code` %in% indic) -> alls

se= c("Country Code","Indicator Code", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")
alls = alls[,se]
alls$mean <- rowMeans(subset(alls, select = c("1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

bads = alls  %>% filter(mean == "NaN") %>% select(`Country Code`) %>% unique()
alls = alls %>% filter(!(`Country Code` %in% bads$`Country Code`))
alls %>% select(`Country Code`, `Indicator Code`, mean) -> alls

tuna<-melt(alls,id.vars=c("Country Code","Indicator Code"))
tuna %>% select(country= `Country Code`, indicator = `Indicator Code`, variable, value) -> tuna
final = cast(tuna,country~indicator~variable)
final = as.data.frame(final)
#final <- add_rownames(final, "Country")
kcl = kmeans(final, centers = 3)

pca = prcomp(final[,-1], center=T, scale.=T)
chosencomp = pca$rotation[,1:2]
compact = t(chosencomp) %*% t(final[,-1])
compact = t(compact)
vis = cbind(compact, kcl$cluster)
vis = as.data.frame(vis)
ggplot(vis) + geom_point( aes(x = PC1, y = PC2, color = V3)) +scale_color_gradientn(colours = rainbow(5))
