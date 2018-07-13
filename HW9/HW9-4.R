library(readr)
library(stringr)
library(dplyr)

apple = read_csv("~/Downloads/class_data/stock_dfs/AAPL.csv")

n = nrow(apple)
tn = 4 * n/5 
testn = n - tn
train = apple[1:tn, ]

ans = list()
for (k in 1:20){
  
test = apple[(tn-k + 1):(n - k),]
valid = apple[(tn + 1):(n),]

fit = lm(Open ~ High + Low + Close + Volume , data = train)
valid$y = predict(fit, test)
valid %>% mutate(dif = 100 * abs(y - Open)/Open) -> valid

s = mean(valid$dif)
ans[[k]] = s
}
ans[[1]]

