

f = 1
sec = 1
th = 1
fo = 1
white = c(510/f, 720/sec, 930/th, 754/fo)
blue = c(925/f,735/sec,753/th,685/fo)
red = c(730/f, 745/sec, 875/th, 610/fo)

x = rbind(white, blue, red)

chisq.test(x)

#n = 5
#d <- matrix(c(white, blue, red), nrow = n,  byrow = TRUE,
#            dimnames =list(1:n, c("White", "Blue", "Red")))
#d

#kruskal.test(d)
#friedman.test(d)

