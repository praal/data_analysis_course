library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(car)

house = read.csv("~/Downloads/house/train.csv")


house = house %>% select_if(is.numeric)
house %>% select(Id,SalePrice, OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, X1stFlrSF, FullBath, TotRmsAbvGrd, YearBuilt, YearRemodAdd) -> data

fit = lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + GarageArea + TotalBsmtSF  + X1stFlrSF+ FullBath + TotRmsAbvGrd+ YearBuilt + YearRemodAdd , data = data)


w = predict(fit, data[3:12])
data$resi = fit$residuals
data$predict = w
data %>% mutate(dif = resi*resi) -> data
data %>% summarise(RSE = sqrt(mean((sum(dif))))) -> ss

summary(fit)
ss

ggplot() +
  # blue plot
  geom_point(data=data, aes(x=Id, y=SalePrice, color = "blue")) + 

  # red plot
  geom_point(data=data, aes(x=Id, y=predict, color = "red")) 



