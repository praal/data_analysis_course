library(readr)

x = read_csv("~/Downloads/week_11/week_11/data/worldwide.csv")

x %>% select(mag, time, place) -> x
x %>% mutate(country = "", year = 0) -> x

for (i in 1:nrow(x)){
  t = x[i,]
  s = t$place
  s = strsplit(s, ",\\s")
  s = s[[length(s)]]
  s = s[[length(s)]]
  t$country = s
  x[i,] = t
  
  t = x[i,]
  s = t$time
  s = substring(s, 1, 4)
  t$year = s
  
  x[i,] = t
  
  
  
}

View(x)
x %>% select(mag, year, country) -> x 
x %>% group_by(year,country) %>% summarise(average = mean(mag)) -> ave
View(ave)

countries = unique(ave$country)
View(countries)

p = list()
for (i in 1:length(countries)){
  t = countries[i]
  
  ave %>% filter(country == t) -> s
  if(nrow(s) == 1){
    p[[i]] = 1
  }
  else{
    z = chisq.test(s$average)
    p[[i]] = z$p.value
  }
  if(p[[i]] < 0.1)
    print(t)
}

