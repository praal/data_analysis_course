library(readr)
library(stringr)
library(dplyr)

files = list.files("~/Downloads/class_data/stock_dfs")

n = length(files)

s = files[1]
name = strsplit(s, "\\.")
t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
x = read_csv(t)
x %>% select(Date,Close, Open,  Volume) -> x

x$Volume = as.integer(x$Volume)

x %>% mutate(trade = abs(Close - Open) * Volume) %>% select(Date, trade) -> x

tot = x

for (i in 1:n){
  s = files[i]
  name = strsplit(s, "\\.")
  t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
  x = read_csv(t)
  x %>% select(Date,Close, Open,  Volume) -> x
  
  x$Volume = as.integer(x$Volume)
  
  x %>% mutate(trade = abs(Close - Open) * Volume) %>% select(Date, trade) -> x
  
  tot = rbind(tot, x)
  gc()
}


r = tot %>% group_by(Date) %>% summarise(total = sum(trade)) %>% arrange(-total)
head(r, 1)$Date

