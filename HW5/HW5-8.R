
#m = 301 + 353 + 558
#f = 502 + 155 + 153
m = 1
f = 1
male = c(301/m, 353/m, 558/m)
female = c(502/f, 155/f, 153/f)

x = rbind(male, female)
x = as.table(x)
chisq.test(x)

