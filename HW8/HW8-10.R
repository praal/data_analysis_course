
dickens = backup
jane = janedf
jane$X7 = 1

select1 = jane[1,]
select2 = dickens[1,]
select3 = dickens[2,]
jane = jane[-1,]
dickens = dickens[-1,]

dickens = dickens[-1, ]
all = rbind(dickens, jane)
View(all)

fit = glm(X7 ~ X1 + X2 + X3 + X4 + X5 + X6 , family = "binomial", data = all)
summary(fit)
predict(fit,select1[,1:6]) # -> -13
predict(fit,select2[,1:6]) # -> -69
predict(fit,select3[,1:6]) # -> 30
predict(fit, dickens[,1:6])
# -> negative : dickens positibve = jane austin