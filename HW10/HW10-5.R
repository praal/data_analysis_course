library(WDI)
library(readr)
library(jpeg)
data = read_csv("~/Downloads/WDI_csv/WDIData.csv")


indic = list("NY.GDP.MKTP.KN","GC.XPN.TOTL.GD.ZS", "GC.REV.XGRT.GD.ZS", "NE.IMP.GNFS.ZS", "NV.AGR.TOTL.ZS" , "DT.DOD.DECT.GN.ZS", "NE.EXP.GNFS.ZS", "NE.CON.GOVT.KD.ZG", "NY.GNP.PCAP.CD", "NE.EXP.GNFS.KD.ZG", "NY.GNS.ICTR.ZS")

alls = NULL
for (ind in 1:11){

  ind = 0
  ind = ind + 1
  print(ind)
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
  
  mypath <- file.path("~/Desktop/Data Analysis/HW10/images/",paste("myplot_", ind, ".jpg", sep = ""))
  jpeg(file=mypath)
 
  ggplot(fac1gg, aes(x= year , y = value , color = country)) + geom_point() + ggtitle(name) +  theme(axis.text.x = element_text(angle=-90, vjust=0.5))
  
  dev.off() 
  
}

for (i in 1:20){
  s = paste("<div align=\"center\">
    <img  src=\"images/hmyplot_", i, sep = "")
  s = paste(s, ".jpg\"  align = 'center'> 
</div>")
  print(s)
}
