library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(car)

house = read.csv("~/Downloads/house/train.csv")


house = house %>% select_if(is.numeric)
house %>% select(SalePrice, OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, X1stFlrSF, FullBath, TotRmsAbvGrd, YearBuilt, YearRemodAdd) -> data

fit = lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF  + X1stFlrSF+ FullBath + TotRmsAbvGrd+ YearBuilt + YearRemodAdd , data = data)

summ = summary(fit)

print("R Squared")
summ$r.squared
print("adjusted R Squared")
summ$adj.r.squared

print("F Statistic")
summ$fstatistic