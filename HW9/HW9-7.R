library(readr)
library(stringr)
library(dplyr)

apple = read_csv("~/Downloads/class_data/stock_dfs/AAPL.csv")

pca = prcomp(apple[,-1],center = T,  scale. = T)
x = t((summary(pca)$importance[3,]))
x = t(x)

x = as.data.frame(x)


d = pca$x

n = nrow(d)
tn = 4 * n/5 
testn = n - tn
train = d[1:tn, ]
train = as.data.frame(train)
trainopen = apple[1:tn, ]

temp = cbind(trainopen$Open, train$PC1)
colnames(temp) = c("Open", "PC1")


res = list()
for (k in 1:20){

  print(k)
  test = d[(tn-k + 1):(n - k),]
  test = as.data.frame(test)
  valid = apple[(tn + 1):(n),]
  ans = cbind(valid$Open, test$PC1)
  colnames(ans) = c("Open", "PC1")
  ans = as.data.frame(ans)
  temp = as.data.frame(temp)
  fit = lm(Open ~ PC1, data = temp)
  ans$y = predict(fit, ans)
  ans %>% mutate(dif = 100 * abs(y - Open)/Open) -> ans
  
  s = mean(ans$dif)
  res[[k]] = s
}
res[[1]]

