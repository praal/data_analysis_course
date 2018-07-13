library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(car)

house = read.csv("~/Downloads/house/train.csv")

house = house %>% select_if(is.numeric)
house %>% select(SalePrice, OverallQual, GrLivArea, GarageCars, GarageArea, TotalBsmtSF, X1stFlrSF, FullBath, TotRmsAbvGrd, YearBuilt, YearRemodAdd) -> data
scatterplotMatrix(data)

ggplot(data, aes(x = OverallQual, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x =  GrLivArea, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = GarageCars, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = GarageArea, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = TotalBsmtSF, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = X1stFlrSF, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = FullBath, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = TotRmsAbvGrd, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = YearBuilt, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")
ggplot(data, aes(x = YearRemodAdd, y = SalePrice)) + geom_point()+geom_smooth(method="loess", formula=y~x, colour = "blue")


