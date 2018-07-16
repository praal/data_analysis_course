library(readr)

#data = read_rds("~/Downloads/week_11/week_11/data/iran_earthquake.rds")
#data %>% select(OriginTime, Mag, Long, Lat) -> d
#d %>% mutate(Lo = round(Long), La = round(Lat)) -> d


#pish = data.frame(0, 0, 0)
#siz = 0

#d = d[order(d$OriginTime),]
#d %>% select(OriginTime, La, Lo, Mag) ->d


#for (i in 3:(nrow(d))){
#  x = d[i,]
#  st = max(1, i-10)
#  for (j in (i-1):st){
#    y = d[j,]
#    if(((x$La) == (y$La)) & ((x$Lo) == (y$Lo))){
#      t = x$OriginTime - y$OriginTime
#      if(t < 120){
#        if((y$Mag < 4) && (x$Mag > 5)){
#         siz = siz+1
#          pish[siz, ] = c(i, x$Mag, y$Mag) 
#       }
#      }
#      
#    }
#  }
#}

#colnames(pish) = c("id", "main", "pre")
#write_csv(pish, "~/Desktop/Data Analysis/pish.csv")
pish = read_csv("~/Desktop/Data Analysis/pish.csv")
pish %>% group_by(id, main) %>% summarise(avg = mean(pre), cnt = n()) -> avg

cor.test(avg$main, avg$avg)
cor.test(avg$main, avg$cnt)