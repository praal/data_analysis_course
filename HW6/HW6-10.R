library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(car)

house = read.csv("~/Downloads/house/train.csv")

house = house %>% select_if(is.numeric)
house %>% select(SalePrice, OverallQual, GrLivArea, GarageCars, BsmtFinSF1, TotalBsmtSF, X1stFlrSF, WoodDeckSF, OverallCond, YearBuilt, YearRemodAdd) -> train

fit = lm(SalePrice ~  poly(OverallQual,3) + poly(GrLivArea,3) + GarageCars + poly(BsmtFinSF1,2) + TotalBsmtSF  + poly(X1stFlrSF,3)+ WoodDeckSF + OverallCond+ poly(YearBuilt,3) + poly(YearRemodAdd,2)  , data = train)
summary(fit)

test = read.csv("~/Downloads/house/test.csv")
test %>% select(Id, OverallQual, GrLivArea, GarageCars, BsmtFinSF1, TotalBsmtSF, X1stFlrSF, WoodDeckSF, OverallCond, YearBuilt, YearRemodAdd) -> td

td[is.na(td)] <- 0
w = predict(fit , td[2:11])
td$SalePrice = w
View(td)

td %>% select(Id, SalePrice) -> fina
View(fina)
write.csv(fina, file = "~/Desktop/foo.csv", row.names = FALSE)



