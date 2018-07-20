library(WDI)
library(readr)
library(dplyr)
library(reshape)


data = read_csv("~/Downloads/WDI_csv/WDIData.csv")
indic = list("SP.ADO.TFRT", "SP.DYN.CBRT.IN", "SH.DTH.COMM.ZS", "SH.DTH.NCOM.ZS", "SP.REG.DTHS.ZS", "SP.POP.DPND.YG", "SH.STA.BRTC.ZS", "SH.DTH.INJR.ZS", "SP.REG.BRTH.ZS", "SP.DYN.CDRT.IN", "SP.DYN.TFRT.IN", "SH.IMM.IDPT", "SH.TBS.INCD", "SM.POP.TOTL", "SP.DYN.LE00.IN", "SH.STA.TRAF.P5", "SH.STA.ANVC.ZS", "SH.DYN.AIDS.ZS", "SN.ITK.DEFC.ZS", "SH.MED.BEDS.ZS")
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
data %>% select(`Country Name`, code = `Country Code`) -> names
nam = as.data.frame(kcl$cluster)
nam <- add_rownames(nam, "code")
names = full_join(names, nam)
names = names %>% select(`Country Name`, cluster = `kcl$cluster`) %>% na.omit() %>% unique()
names