library(readr)
library(stringr)
library(dplyr)
library(ggplot2)

files = list.files("~/Downloads/class_data/stock_dfs")
industry = read_csv("~/Downloads/class_data/constituents.csv")
n = length(files)

s = files[1]
name = strsplit(s, "\\.")
t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
x = read_csv(t)
x %>% select(Date, Close, Open) -> x
a <- x$Date
x$year = substring(a,1,4)

x$Close = as.integer(x$Close)
x$Open = as.integer(x$Open)
x %>% group_by(year) %>% summarise(benefit = sum(Close - Open)) -> x
x %>% mutate(comp = name[[1]][1]) -> x

tot = x

for (i in 1:n){
  s = files[i]
  name = strsplit(s, "\\.")
  t = paste("~/Downloads/class_data/stock_dfs/" , s, sep = "")
  x = read_csv(t)
  x %>% select(Date, Close, Open) -> x
  a <- x$Date
  x$year = substring(a,1,4)
  
  x$Close = as.integer(x$Close)
  x$Open = as.integer(x$Open)
  x %>% group_by(year) %>% summarise(benefit = sum(Close - Open)) -> x
  x %>% mutate(comp = name[[1]][1]) -> x
  
  tot = rbind(tot, x)
  gc()
}

tot %>% select(year, benefit, Symbol = comp) -> ans
ans = full_join(ans, industry)
ans = na.omit(ans)
one = ans %>% group_by(Sector) %>% slice(which.max(benefit)) 

oneall = ans %>% arrange(-benefit) %>% head(10)
ggplot(one , aes(x = paste( Sector, "-", Name, year), y = benefit)) +  geom_col(fill = "purple", alpha = 0.8) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("Companies") + ggtitle("Benefits in one year in different Sectors")
ggplot(oneall , aes(x = paste( Sector, "-", Name, year), y = benefit)) +  geom_col(fill = "purple", alpha = 0.8) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("Companies") + ggtitle("10 companies with largest benefit in one year")


ans2 = ans
ans2 = arrange(ans2, Symbol, year)

for (i in 1:(nrow(ans2) - 1)){
  v = ans2[i,]
  c = ans2[i + 1, ]
  ans2[i,2] = -1000
  if(as.integer(c$year) == as.integer(v$year) + 1){
    ans2[i,2] = c$benefit + v$benefit
  }

}
two = ans2 %>% group_by(Sector) %>% slice(which.max(benefit)) 

twoall = ans2 %>% arrange(-benefit) %>% head(10)
ggplot(two , aes(x = paste( Sector, "-", Name, year), y = benefit)) +  geom_col(fill = "purple", alpha = 0.8) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("Companies") + ggtitle("Benefits in two years in different Sectors")
ggplot(twoall , aes(x = paste( Sector, "-", Name, year), y = benefit)) +  geom_col(fill = "purple", alpha = 0.8) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("Companies") + ggtitle("10 companies with largest benefit in two years")


ans3 = ans
ans3 = arrange(ans3, Symbol, year)

for (i in 1:(nrow(ans3) - 5)){
  v = ans3[i,]
  c1 = ans3[i + 1, ]
  c2 = ans3[i + 2, ]
  c3 = ans3[i + 3, ]
  c4 = ans3[i + 4, ]
  c5 = ans3[i + 5, ]

  ans3[i,2] = -1000
  yy= as.integer(c5$year)
  yyy = as.integer(v$year)
  yyy = yyy + 5
  if(yy == yyy){

    if(c5$Symbol == v$Symbol)
      ans3[i,2] = c1$benefit + c2$benefit + c3$benefit + c4$benefit + c5$benefit + v$benefit
  }
  
}
five = ans3 %>% group_by(Sector) %>% slice(which.max(benefit)) 

fiveall = ans3 %>% arrange(-benefit) %>% head(10)
ggplot(five , aes(x = paste( Sector, "-", Name, year), y = benefit)) +  geom_col(fill = "purple", alpha = 0.8) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("Companies") + ggtitle("Benefits in 5 years in different Sectors")
ggplot(fiveall , aes(x = paste( Sector, "-", Name, year), y = benefit)) +  geom_col(fill = "purple", alpha = 0.8) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("Companies") + ggtitle("10 companies with largest benefit in 5 years")




