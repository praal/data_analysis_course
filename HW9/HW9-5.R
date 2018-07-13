library(readr)

files = list.files("~/Downloads/class_data/stock_dfs")


n = length(files)


s = files[1]
t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
x = read_csv(t)
x %>% select(Date, Close) -> x
x$Close = as.numeric(x$Close)
name = paste("n", 1, sep = "")
colnames(x) = c("date", name)

tot = x

for (i in 2:n){
  s = files[i]
  t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
  x = read_csv(t)
  x %>% select(Date, Close) -> x
  x$Close = as.numeric(x$Close)
  name = paste("n", i, sep = "")
  colnames(x) = c("date", name)
  tot = merge(tot, x)
}

pca = prcomp(tot[,-1],center = T,  scale. = T)
x = t((summary(pca)$importance[3,]))
x = t(x)

x = as.data.frame(x)

plot(summary(pca)$importance[3,], type="l",
     ylab="variance", xlab="number of components ")
abline(h=0.99,col="red")
abline(v = 32,col="red",lty=3)

head(x,3)


