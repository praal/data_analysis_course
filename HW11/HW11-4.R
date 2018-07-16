library(readr)
library(ggplot2)

year = c(856,2013, 1721, 1042, 1990, 1978, 1968, 2017, 1997, 1909, 1947, 1929, 1930, 1981, 1962, 1957, 1957)
year = sort(year)
last = list()
for (i in 1:(length(year) - 1)){
  last[i] = year[i+1] - year[i] 
}

# A|B = AB / B B = > 1 A = < 6
ab = 0
b = 0
for (i in 1:(length(last))){
  x = last[i]
  if(x >= 1)
    b = b + 1
  if ((x >= 1) && (x <= 6))
    ab = ab + 1
}

print(ab/b)