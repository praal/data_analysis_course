
first = c(509, 517, 502, 629, 830, 911, 847, 803, 727, 853, 757, 730, 774, 718, 904)
sec = c(517, 508, 523, 730, 821, 940, 818, 821, 842, 842, 709, 688, 787, 780, 901)

n = 100
res1 <- matrix(nrow = n, ncol = 1);
for (i in 1:n) {
  sample        <- sample(first, replace = TRUE);
  res1[i, 1] <- mean(sample);
}

res2 <- matrix(nrow = n, ncol = 1);
for (i in 1:n) {
  sample        <- sample(sec, replace = TRUE);
  res2[i, 1] <- mean(sample);
}

wilcox.test(res1, res2)