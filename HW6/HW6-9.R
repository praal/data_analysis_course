library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)

house = read.csv("~/Downloads/house/train.csv")

house = house %>% select_if(is.numeric)
house %>% select(Id, SalePrice, OverallQual, GrLivArea, GarageCars, BsmtFinSF1, TotalBsmtSF, X1stFlrSF, WoodDeckSF, OverallCond, YearBuilt, YearRemodAdd) -> data

random = data[sample(nrow(data)),]


n = nrow(data)
trainnum = as.integer(4 * n / 5)
testnum = n - trainnum
train = head(random, trainnum)
test= tail(random, testnum)

fit = lm(SalePrice ~  poly(OverallQual,3) + poly(GrLivArea,3) + GarageCars + poly(BsmtFinSF1,2) + TotalBsmtSF  + poly(X1stFlrSF,3)+ WoodDeckSF + OverallCond+ poly(YearBuilt,3) + poly(YearRemodAdd,2)  , data = train)
summary(fit)


wtest = predict(fit, test[3:12])

test$predict = wtest
test %>% mutate(dif = (predict - SalePrice)*(predict - SalePrice)) -> test
test %>% summarise(sum = sqrt(sum(dif)/testnum)) -> ss
ss
