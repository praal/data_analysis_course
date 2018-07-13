library(readr)
library(h2o)
d = read_csv("~/Desktop/Data Analysis/murder_suicide.csv")

d$Sex = as.factor(d$Sex)
d$MaritalStatus = as.factor(d$MaritalStatus)
d$InjuryAtWork = as.factor(d$InjuryAtWork)
d$MethodOfDisposition= as.factor(d$MethodOfDisposition)
d$Autopsy = as.factor(d$Autopsy)
d$MannerOfDeath = d$MannerOfDeath - 2

d %>%  select(Sex, Race, EducationReportingFlag, MethodOfDisposition, Age, MaritalStatus, DayOfWeekOfDeath, ResidentStatus, MannerOfDeath) -> d
h2o.init()
h = as.h2o(d)

chglm = h2o.glm(y = "MannerOfDeath", x= c("Sex", "Race", "EducationReportingFlag", "MethodOfDisposition", "Age", "MaritalStatus", "DayOfWeekOfDeath", "ResidentStatus"), training_frame = h, family="binomial",nfolds = 5)

chglm
