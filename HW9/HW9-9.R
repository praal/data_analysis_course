library(EBImage)
library(dplyr)

pic = flip(readImage("~/Desktop/stock.jpg"))
red.weigth = .2989; green.weigth = .587; blue.weight = 0.114;

img = red.weigth * imageData(pic)[,,1] + green.weigth * imageData(pic)[,,2] + blue.weight * imageData(pic)[,,3]
image(img, col = grey(seq(0,1,length = 256)))

pca.img = prcomp(img, scale = TRUE)


x = t((summary(pca.img)$importance[3,]))
x = t(x)

x = as.data.frame(x)

names <- rownames(x)
rownames(x) <- NULL
x <- cbind(names,x)
colnames(x) = c("id", "var")
x %>% filter(var >= 0.99) %>% arrange(var) %>% head(1)

plot(summary(pca.img)$importance[3,], type="l",
     ylab="variance", xlab="number of components ")
abline(h=0.99,col="red")
abline(v = 32,col="red",lty=3)



chosen.components = 1:109
feautre.vector = pca.img$rotation[, chosen.components]

compact.data = t(feautre.vector) %*% t(img)
approx.img = t(feautre.vector %*% compact.data)
image(approx.img, col = grey(seq(0,1,length = 256)))
