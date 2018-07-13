

first = c(50, 50, 60, 70, 75, 80, 90, 85)
sec = c(55, 75, 80, 90, 105, 65)

n = 1000
res1 <- matrix(nrow = n, ncol = 1);
for (i in 1:n) {
  sample        <- sample(first, replace = TRUE, size = 4);
  res1[i, 1] <- mean(sample);
}

res2 <- matrix(nrow = n, ncol = 1);
for (i in 1:n) {
  sample        <- sample(sec, replace = TRUE, size = 4);
  res2[i, 1] <- mean(sample);
}

t.test(res1, res2)