library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(car)

house = read.csv("~/Downloads/house/train.csv")



house = house %>% select_if(is.numeric)
house %>% select(SalePrice, OverallQual, GrLivArea, GarageCars, BsmtFinSF1, TotalBsmtSF, X1stFlrSF, WoodDeckSF, OverallCond, YearBuilt, YearRemodAdd) -> data

fit = lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + BsmtFinSF1 + TotalBsmtSF  + X1stFlrSF+ WoodDeckSF + OverallCond+ YearBuilt + YearRemodAdd , data = data)
summary(fit)

data$resi = fit$residuals
data %>% mutate(dif = (resi)*(resi)) -> data
data %>% summarise(RSE = sqrt(mean((sum(dif))))) -> ss

ss


