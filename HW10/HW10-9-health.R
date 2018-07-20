library(WDI)
library(readr)
library(dplyr)
library(ggplot2)

data = read_csv("~/Downloads/WDI_csv/WDIData.csv")


indic = list("SP.ADO.TFRT", "SP.DYN.CBRT.IN", "SH.DTH.COMM.ZS", "SH.DTH.NCOM.ZS", "SP.REG.DTHS.ZS", "SP.POP.DPND.YG", "SH.STA.BRTC.ZS", "SH.DTH.INJR.ZS", "SP.REG.BRTH.ZS", "SP.DYN.CDRT.IN", "SP.DYN.TFRT.IN", "SH.IMM.IDPT", "SH.TBS.INCD", "SM.POP.TOTL", "SP.DYN.LE00.IN", "SH.STA.TRAF.P5", "SH.STA.ANVC.ZS", "SH.DYN.AIDS.ZS", "SN.ITK.DEFC.ZS", "SH.MED.BEDS.ZS")
alls = NULL
for (ind in 1:20){

  ind =1
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
  
  
  mypath <- file.path("~/Desktop/Data Analysis/HW10/images/",paste("hmyplot_", ind, ".jpg", sep = ""))
  jpeg(file=mypath)
  

  ggplot(fac1gg, aes(x= year , y = value , color = country)) + geom_point() + ggtitle(name) +  theme(axis.text.x = element_text(angle=-90, vjust=0.5))
  dev.off() 
}


