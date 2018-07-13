library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

files = list.files("~/Downloads/class_data/stock_dfs")


n = length(files)

s = files[2]
name = strsplit(s, "\\.")
t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
x = read_csv(t)
x %>% select(Date, Close) -> x
a <- x$Date
x$day = substring(a,9,10)

x$Close = as.integer(x$Close)
x %>% group_by(day) %>% summarise(benefit = mean(Close)) -> x
siz = x %>% filter(day == 13)
oth = x %>% filter(day != 13)
siz = mean(siz$benefit)
oth = mean(oth$benefit)
num = c(siz, oth)
ruz = c(13, 0)
t = data.frame("day" = ruz, "benefit" = num)

tot = t

for (i in 1:n){
  s = files[i]
  name = strsplit(s, "\\.")
  t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
  x = read_csv(t)
  x %>% select(Date, Close) -> x
  a <- x$Date
  x$day = substring(a,9,10)
  
  x$Close = as.integer(x$Close)
  x %>% group_by(day) %>% summarise(benefit = mean(Close)) -> x
  siz = x %>% filter(day == 13)
  oth = x %>% filter(day != 13)
  siz = mean(siz$benefit)
  oth = mean(oth$benefit)
  num = c(siz, oth)
  num
  ruz = c(13, 0)
  t = data.frame("day" = ruz, "benefit" = num)
  
  tot = rbind(tot, t)
  gc()
}


kruskal.test(benefit ~ day, data = tot)
w = tot %>% filter(benefit < 200)
w1 = w %>% filter(day == 13)
w2 = w %>% filter(day != 13)

ggplot(data = w1, aes(x = benefit, color = "red")) + geom_histogram(binwidth = 1)
ggplot(data = w1, aes(x = benefit, color = "red")) + geom_histogram(binwidth = 1)