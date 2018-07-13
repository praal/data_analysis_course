

library(gutenbergr)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)

janeids = c(105, 121, 141, 158, 161, 1212, 1342)




jane = list()
janebooks = list()

for (i in 1:length(janeids)){
  
  x = ids[i]
  b  = gutenberg_download(x, meta_fields = "title")
  janebooks[[i]] = b
}


janecleand = list()
for (i in 1:length(janeids)){
  
  book  = janebooks[[i]]
  f = book
  for (j in 1:nrow(book)){
    x = book[j, 2]
    x = x %>% str_replace_all("[[:punct:]]"," ") %>%  str_replace_all("[^[:alpha:]]", " ")
    f[j,2] = x
  }
  janecleand[[i]] = f
  
}



janedf = data.frame(1,2,3,4,5,6,7)
tries = list()
bis = list()
uns = list()

for (i in 1:length(janeids)){
  
  res = janecleand[[i]]
  
  res %>%  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>% 
    count(ngram, sort = TRUE)  %>% 
    separate(ngram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word) %>% 
    filter(!str_detect(word1, "^\\d+")) %>% 
    filter(!str_detect(word2, "^\\d+")) %>% 
    arrange(-n) -> bigram
  bigram$index<-seq(1:nrow(bigram))
  b<-glm(log(n) ~ log(index), family="gaussian", data=bigram)
  
  
  res %>%  unnest_tokens(ngram, text, token = "ngrams", n = 1) %>% 
    count(ngram, sort = TRUE)  %>% 
    separate(ngram, c("word1"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>% 
    filter(!str_detect(word1, "^\\d+")) %>% 
    arrange(-n) -> unigram
  unigram$index<-seq(1:nrow(unigram))
  u<-glm(log(n) ~ log(index), family="gaussian", data=unigram)
  
  
  res %>%  unnest_tokens(ngram, text, token = "ngrams", n = 3) %>% 
    count(ngram, sort = TRUE)  %>% 
    separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word) %>%
    filter(!word3 %in% stop_words$word) %>% 
    filter(!str_detect(word1, "^\\d+")) %>% 
    filter(!str_detect(word2, "^\\d+")) %>%
    filter(!str_detect(word3, "^\\d+")) %>% 
    arrange(-n) -> trigram
  trigram$index<-seq(1:nrow(trigram))
  t<-glm(log(n) ~ log(index), family="gaussian", data=trigram)
  
  
  janedf[nrow(janedf) + 1,] = c(u$coefficients, b$coefficients, t$coefficients, 0)
  tries[[i]] = trigram
  bis[[i]] = bigram
  uns[[i]]= unigram
}  

View(janedf)
janedf = janedf[-1,]

df = janedf

df$X1 = abs(df$X1)
df$X2 = abs(df$X2)
df$X3 = abs(df$X3)
df$X4 = abs(df$X4)
df$X5 = abs(df$X5)
df$X6 = abs(df$X6)
df$X7 = abs(df$X7)

chisq.test(df[,1:6])

i = 0

i = i + 1
g1 = uns[[i]] %>% select(n) 
g2 = bis[[i]] %>% select(n) 
g3 = tries[[i]] %>% select(n) 

ggplot(data = g1, aes(x = n)) + geom_histogram(binwidth  = 1, fill = "purple", alpha = 0.7) + xlim(0,50) + ggtitle(paste("1-gram histogram of",janebooks[[i]]$title) )

ggplot(data = g2, aes(x = n)) + geom_histogram(binwidth  = 1, fill = "blue", alpha = 0.7) + xlim(0,20) + ggtitle(paste("2-gram histogram of",janebooks[[i]]$title) )

ggplot(data = g3, aes(x = n)) + geom_histogram(binwidth  = 1, fill = "red", alpha = 0.7) + xlim(0,10) + ggtitle(paste("3-gram histogram of",janebooks[[i]]$title) )

