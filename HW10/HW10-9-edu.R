library(WDI)
library(readr)
library(dplyr)
library(ggplot2)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")

indic = list("SE.XPD.TOTL.GD.ZS", "SE.XPD.PRIM.PC.ZS", "SE.XPD.SECO.PC.ZS", "SL.TLF.TOTL.IN", "SE.ADT.LITR.ZS", "SE.ADT.1524.LT.ZS", "SE.ADT.1524.LT.MA.ZS", "SE.ADT.LITR.MA.ZS", "SE.PRM.PRSL.ZS", "SE.PRM.PRSL.MA.ZS", "SE.PRM.CMPT.ZS", "SE.SEC.PROG.FE.ZS", "SE.TER.ENRL.TC.ZS", "SE.PRM.REPT.ZS", "SE.PRM.NENR", "SE.ENR.TERT.FM.ZS", "SE.ENR.SECO.FM.ZS", "SL.UEM.TOTL.FE.ZS", "SE.PRM.UNER.ZS", "SE.PRM.CMPT.ZS")
alls = NULL
for (ind in 1:20){
  
  ind = 0
  ind = ind + 1
  
  str = indic[ind]
  data %>% filter(`Indicator Code` == str) ->gdp
  name = gdp[1,3]
  name = name[[1]]
  se= c("Country Code","1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009","2010", "2011","2012","2013","2014","2015", "2016")
  gdp = gdp[,se]
  
  gdp %>% filter(`Country Code` == "IRN") -> irn
  gdp %>% filter(`Country Code` != "IRN") -> rest
  restmean = colMeans(rest[,-1], na.rm = TRUE)
  irnmean = colMeans(irn[,-1], na.rm = TRUE)
  fac1 = cbind(irnmean, restmean)
  fac1 = as.data.frame(fac1)
  fac1 <- add_rownames(fac1, "year")
  fac1gg = data.frame(year = 0, value = 0 ,country =0, type = 0)
  s = 1
  for (i in 1:(nrow(fac1))){
    x = fac1[i,]
    fac1gg[s,] = c(x$year, x$irnmean, "iran", name)
    s = s+1
    fac1gg[s,] = c(x$year, x$restmean, "rest", name)
    s = s + 1
    
  }
  fac1gg %>% filter(value != "NaN") -> fac1gg
  alls = rbind(alls, fac1gg)
  
  mypath <- file.path("~/Desktop/Data Analysis/HW10/images/",paste("edumyplot_", ind, ".jpg", sep = ""))
  jpeg(file=mypath)
  


  ggplot(fac1gg, aes(x= year , y = value , color = country)) + geom_point() + ggtitle(name) +  theme(axis.text.x = element_text(angle=-90, vjust=0.5))
  dev.off() 
}



