library(readr)

d = read_csv("~/Desktop/Data Analysis/murder_suicide.csv")


d = read_csv("~/Desktop/Data Analysis/murder_suicide.csv")

d$MannerOfDeath = d$MannerOfDeath - 2

d %>%  select(Sex, Race, EducationReportingFlag, MethodOfDisposition, Age, MaritalStatus, DayOfWeekOfDeath, ResidentStatus, MannerOfDeath) -> d


random = d[sample(nrow(d)),]


n = nrow(d)
trainnum = as.integer(4 * n / 5)
testnum = n - trainnum
train = head(random, trainnum)
test= tail(random, testnum)



g = glm(MannerOfDeath ~ factor(Sex) + factor(Race) + factor(EducationReportingFlag) + factor(MethodOfDisposition) + Age + factor(MaritalStatus) + DayOfWeekOfDeath + factor(ResidentStatus), data = train, family = "binomial")

test$predict = predict(g, test)
test %>% mutate(answer = as.integer(predict > 0.5)) -> test

test %>% summarise(p = sum(answer == 1)) -> p
test %>% summarise(n = sum(answer == 0)) -> n

test %>% filter(answer == 1) %>% filter(MannerOfDeath == 1) %>%  summarise(tp = n()) -> tp
test %>% filter(answer == 0) %>% filter(MannerOfDeath == 0) %>%  summarise(tn = n()) -> tn

test %>% filter(answer == 1) %>% filter(MannerOfDeath == 0) %>%  summarise(fp= n()) -> fp
test %>% filter(answer == 0) %>% filter(MannerOfDeath == 1) %>%  summarise(fn = n()) -> fn

test %>% mutate(type = ifelse(answer == 1, ifelse(MannerOfDeath == 1, "TP", "FP"), ifelse(MannerOfDeath == 1, "FN", "TN"))) -> test
tp = as.numeric(tp)
tn = as.numeric(tn)
n = as.numeric(n)
p = as.numeric(p)
fp = as.numeric(fp)
fn = as.numeric(fn)
acc = (tp + tn) / (p + n)
print("Accuracy:")
acc

fpr = 1 - (tn/n)
print("FPR:")
fpr

tpr = tp/p
print("TPR:")
tpr


ggplot(data = test, aes(x = MannerOfDeath, y = predict, color = type)) + geom_violin(fill = "white", color = NA) + geom_jitter(shape = 1) 

