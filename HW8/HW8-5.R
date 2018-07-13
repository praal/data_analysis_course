library(gutenbergr)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
library(ggplot2)


miser_raw  = gutenberg_download(135)




a = miser_raw %>% 
  str_replace_all("\"","") %>% 
  str_replace_all("[[:punct:]]","") %>% 
  str_split(pattern = "\\s")

a = a[[2]]


miserlist = list()
n = length(a)
m = as.integer(n / 200)
l = 1

for (i in 1:199){
  e = l + m
  p = a[l:e]
  l = l + m + 1
  miserlist[[i]]= p
}

miserlist[[200]] =a[l:n]
cleanlist = list()

senti = na.omit(sentiments %>% select(word, sentiment) %>% filter(sentiment == "negative" | sentiment == "positive") %>% unique())


allsent = data.frame(1,0,0)
names(allsent)<-c("part", "negative","positive")

for (i in 2:200){

miser = miserlist[[i]] %>% 
  unlist() %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = F)

colnames(miser) = c("word","count")

miser = miser  %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(nchar(word)>1) %>% 
  filter(!str_detect(word, "\\d")) %>% 
  mutate(proper = !word %in% str_to_lower(word)) %>%
  filter(proper != TRUE) %>%
  arrange(-count)

  miser = full_join(miser, senti)
  miser = na.omit(miser)
  cleanlist[[i]] = miser

  cleanlist[[i]] %>% group_by(sentiment) %>% summarise(t = sum(count)) -> v

  
  allsent[nrow(allsent) + 1,] = c(i, v[1,2], v[2,2])

}


ggplot(allsent) + geom_line(aes(x = part, y = negative, color = "blue")) + geom_line(aes(x = part, y = positive, color = "red")) + ylab("count")


