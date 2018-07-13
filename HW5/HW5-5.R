library(readr)


tv = read_csv("~/Downloads/hw_05/hw_05/data/tv.csv")
tm = data.matrix(tv)
chisq.test(tv$Jun, tv$April)

#n = 15
#res1 <- matrix(nrow = n, ncol = 2, colnames("month", "value"));
#for (i in 1:n) {
#  res1[i,1] = "march"
#  res1[i, 2] <- (tv$March[i]);
#}


#res2 <- matrix(nrow = n, ncol = 2, colnames("month", "value"));
#for (i in 1:n) {
#  res2[i,1] = "april"
#  res2[i, 2] <- (tv$April[i]);
#}

#res3 <- matrix(nrow = n, ncol = 2, colnames("month", "value"));
#for (i in 1:n) {
#  res3[i,1] = "may"
#  res3[i, 2] <- (tv$May[i]);
#}


#res4 <- matrix(nrow = n, ncol = 2, colnames("month", "value"));
#for (i in 1:n) {
#  res4[i,1] = "jun"
#  res4[i, 2] <- (tv$Jun[i]);
#}

