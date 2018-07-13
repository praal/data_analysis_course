library(readr)
library(dplyr)
library(reshape2)
library(ggplot2)

house = read.csv("~/Downloads/house/train.csv")

house = house %>% select_if(is.numeric)
res = cor(house)
res = round(res, 2)

melted_cormat <- melt(res)
melted_cormat = na.omit(melted_cormat)
melted_cormat %>% filter( Var1 != "Id") -> d
d %>% filter(Var2 != "Id") -> d
d %>% filter(Var1 != Var2) -> d

ggplot(d, aes(Var1, Var2, fill=value)) + geom_tile(aes(width=1.1, height=1.1)) + geom_text(aes(label=value), size=2) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 



n = length(house)
df <- data.frame(Var1 = paste("", 1:(n*n)), Var2 = "", P.value = "")
pvalues = numeric(n * n)
Var1 = character(n * n)
Var2 = character(n * n)

x = 1
for (i in 1:n){
  for (j in 1:n){
    t = cor.test(house[,i], house[,j])
    t = t$p.value
    t = round(t,2)
    Var1[x] = colnames(house)[i]
    Var2[x] = colnames(house)[j]
    pvalues[x] = t
    x = x + 1
    
  }
}
df$Var1 = Var1
df$Var2 = Var2
df$P.value = pvalues


ans = full_join(x = d , y = df)

ans = na.omit(ans)
ggplot(ans, aes(Var1, Var2, fill=P.value)) + geom_tile(aes(width=1.1, height=1.1)) + geom_text(aes(label=P.value), size=2) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 

ans %>% filter(Var1 == "SalePrice") -> s
s %>% mutate(pos = abs(value)) -> s
s= arrange(s, -pos)

fin = head(s, 35)
fin$Var2