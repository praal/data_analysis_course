library(readr)
library(boot)
library(ResourceSelection)

d = read_csv("~/Desktop/Data Analysis/murder_suicide.csv")


d = read_csv("~/Desktop/Data Analysis/murder_suicide.csv")

d$MannerOfDeath = d$MannerOfDeath - 2

d %>%  select(Sex, Race, EducationReportingFlag, MethodOfDisposition, Age, MaritalStatus, DayOfWeekOfDeath, ResidentStatus, MannerOfDeath) -> d
g = glm(MannerOfDeath ~ factor(Sex) + factor(Race) + factor(EducationReportingFlag) + factor(MethodOfDisposition) + Age + factor(MaritalStatus) + DayOfWeekOfDeath + factor(ResidentStatus), data = d, family = "binomial")

summary(g)

hoslem.test(d$MannerOfDeath, fitted(g))
#glm.diag.plots(g, glmdiag = glm.diag(g))
