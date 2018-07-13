

small = c(151,252,603)
medium = c(802,603,405)
large = c(753,55,408)

x = rbind(small, medium, large)
x = as.table(x)
x
chisq.test(x)
