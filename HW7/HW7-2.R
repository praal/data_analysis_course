library(readr)

d = read_csv("~/Desktop/Data Analysis/murder_suicide.csv")



t.test(MannerOfDeath ~ Sex, data = d)
summary(aov(MannerOfDeath ~ Race, data = d))
summary(aov(MannerOfDeath ~  EducationReportingFlag, data = d))
summary(aov(MannerOfDeath ~  MethodOfDisposition, data = d))
summary(aov(MannerOfDeath ~  Age, data = d))

