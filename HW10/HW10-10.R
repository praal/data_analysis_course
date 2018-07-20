



library(WDI)
library(readr)
library(dplyr)
library(reshape)
library(stats)
data = read_csv("~/Downloads/WDI_csv/WDIData.csv")
indic = list("SE.XPD.TOTL.GD.ZS", "SE.XPD.PRIM.PC.ZS", "SE.XPD.SECO.PC.ZS", "SL.TLF.TOTL.IN", "SE.ADT.LITR.ZS", "SE.ADT.1524.LT.ZS", "SE.ADT.1524.LT.MA.ZS", "SE.ADT.LITR.MA.ZS", "SE.PRM.PRSL.ZS", "SE.PRM.PRSL.MA.ZS", "SE.PRM.CMPT.ZS", "SE.SEC.PROG.FE.ZS", "SE.TER.ENRL.TC.ZS", "SE.PRM.REPT.ZS", "SE.PRM.NENR", "SE.ENR.TERT.FM.ZS", "SE.ENR.SECO.FM.ZS", "SL.UEM.TOTL.FE.ZS", "SE.PRM.UNER.ZS", "SE.PRM.CMPT.ZS")
data %>% filter(`Indicator Code` %in% indic) -> alls

se= c("Country Code","Indicator Code", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")
alls = alls[,se]
alls$mean <- rowMeans(subset(alls, select = c("1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

bads = alls  %>% filter(mean == "NaN") %>% select(`Country Code`) %>% unique()
alls = alls %>% filter(!(`Country Code` %in% bads$`Country Code`))
#alls[is.na(alls)] = 0
alls %>% select(`Country Code`, `Indicator Code`, mean) -> alls

tuna<-melt(alls,id.vars=c("Country Code","Indicator Code"))
tuna %>% select(country= `Country Code`, indicator = `Indicator Code`, variable, value) -> tuna
final = cast(tuna,country~indicator~variable)
final = as.data.frame(final)
final[is.na(final)] = 0

colnames(final) <- paste("edu", colnames(final), sep = "_")
final <- add_rownames(final, "Country")
finaledu = final



indic = list("SP.ADO.TFRT", "SP.DYN.CBRT.IN", "SH.DTH.COMM.ZS", "SH.DTH.NCOM.ZS", "SP.REG.DTHS.ZS", "SP.POP.DPND.YG", "SH.STA.BRTC.ZS", "SH.DTH.INJR.ZS", "SP.REG.BRTH.ZS", "SP.DYN.CDRT.IN", "SP.DYN.TFRT.IN", "SH.IMM.IDPT", "SH.TBS.INCD", "SM.POP.TOTL", "SP.DYN.LE00.IN", "SH.STA.TRAF.P5", "SH.STA.ANVC.ZS", "SH.DYN.AIDS.ZS", "SN.ITK.DEFC.ZS", "SH.MED.BEDS.ZS")
data %>% filter(`Indicator Code` %in% indic) -> alls

se= c("Country Code","Indicator Code", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")
alls = alls[,se]
alls$mean <- rowMeans(subset(alls, select = c("1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

bads = alls  %>% filter(mean == "NaN") %>% select(`Country Code`) %>% unique()
alls = alls %>% filter(!(`Country Code` %in% bads$`Country Code`))
#alls[is.na(alls)] = 0
alls %>% select(`Country Code`, `Indicator Code`, mean) -> alls

tuna<-melt(alls,id.vars=c("Country Code","Indicator Code"))
tuna %>% select(country= `Country Code`, indicator = `Indicator Code`, variable, value) -> tuna
final = cast(tuna,country~indicator~variable)
final = as.data.frame(final)
final[is.na(final)] = 0

colnames(final) <- paste("health", colnames(final), sep = "_")
final <- add_rownames(final, "Country")

finalhealth = final



indic = list("NY.GDP.MKTP.KN","GC.XPN.TOTL.GD.ZS", "GC.REV.XGRT.GD.ZS", "NE.IMP.GNFS.ZS", "NV.AGR.TOTL.ZS" , "DT.DOD.DECT.GN.ZS", "NE.EXP.GNFS.ZS", "NE.CON.GOVT.KD.ZG", "NY.GNP.PCAP.CD", "NE.EXP.GNFS.KD.ZG")
data %>% filter(`Indicator Code` %in% indic) -> alls

se= c("Country Code","Indicator Code", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")
alls = alls[,se]
alls$mean <- rowMeans(subset(alls, select = c("1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")), na.rm = TRUE)

bads = alls  %>% filter(mean == "NaN") %>% select(`Country Code`) %>% unique()
alls = alls %>% filter(!(`Country Code` %in% bads$`Country Code`))
#alls[is.na(alls)] = 0
alls %>% select(`Country Code`, `Indicator Code`, mean) -> alls

tuna<-melt(alls,id.vars=c("Country Code","Indicator Code"))
tuna %>% select(country= `Country Code`, indicator = `Indicator Code`, variable, value) -> tuna
final = cast(tuna,country~indicator~variable)
final = as.data.frame(final)
final[is.na(final)] = 0



colnames(final) <- paste("eco", colnames(final), sep = "_")
final <- add_rownames(final, "Country")

finaleco = final


final60 = inner_join(finaleco, finaledu)
final60 = inner_join(final60, finalhealth)


t = final60[,-1]
rownames(t) = final60$Country

kcl = kmeans(t, centers = 3)
kclnm = kcl$cluster
kclnm = as.data.frame(kclnm)
kclnm <- add_rownames(kclnm, "Country")
names = data %>% select(`Country Name`, Country = `Country Code`)

names = inner_join(kclnm, names)

names %>% group_by(`Country Name`) %>% summarize(group = mean(kclnm)) -> names

dist = stats::dist(t,method = "euclidean")
dist %>% as.matrix() %>% image()
clus = hclust(dist,method = "complete")
plot(clus)