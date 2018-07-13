library(readr)
library(car)
library(dplyr)
library(reshape2)
library(ggplot2)

d = read_csv("~/Desktop/Data Analysis/murder_suicide.csv")




d %>% select(Id, ResidentStatus, EducationReportingFlag, MonthOfDeath, Sex, Age, PlaceOfDeathAndDecedentsStatus, MaritalStatus, DayOfWeekOfDeath, InjuryAtWork, MethodOfDisposition, Autopsy, PlaceOfInjury, Race, MannerOfDeath) -> d

d$Sex = as.factor(d$Sex)
d$MaritalStatus = as.factor(d$MaritalStatus)
d$InjuryAtWork = as.factor(d$InjuryAtWork)
d$MethodOfDisposition= as.factor(d$MethodOfDisposition)
d$Autopsy = as.factor(d$Autopsy)
d$MannerOfDeath = d$MannerOfDeath - 2
             
d %>% mutate(t = as.integer(Id / 20)) -> less
less %>% filter(t * 20 == Id) -> less             

d %>% select(ResidentStatus, EducationReportingFlag, MonthOfDeath, Sex, Age, PlaceOfDeathAndDecedentsStatus, MaritalStatus, DayOfWeekOfDeath, InjuryAtWork, MethodOfDisposition, Autopsy, PlaceOfInjury, Race, MannerOfDeath) -> d

must_convert<-sapply(d,is.factor)  
M2<-sapply(d[,must_convert],unclass)  

cleand<-cbind(d[,!must_convert],M2) 




res = cor(cleand)
res = round(res, 2)

melted_cormat <- melt(res)
#melted_cormat = na.omit(melted_cormat)

ggplot(melted_cormat, aes(Var1, Var2, fill=value)) + geom_tile(aes(width=1.1, height=1.1)) + geom_text(aes(label=value), size=2) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 

less %>% select(ResidentStatus, EducationReportingFlag, MonthOfDeath, Sex, Age, MaritalStatus, DayOfWeekOfDeath, MethodOfDisposition,  PlaceOfInjury, Race, MannerOfDeath) -> less


#scatterplotMatrix(less)
