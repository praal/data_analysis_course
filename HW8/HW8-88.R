library(gutenbergr)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)

ids = c(580, 730, 967, 700, 917, 968, 821, 766, 1023, 786, 963, 98, 1400, 883, 564)



dickens = list()
dickensbooks = list()

for (i in 1:length(ids)){
  
  x = ids[i]
  b  = gutenberg_download(x, meta_fields = "title")
  dickensbooks[[i]] = b
}


cleand = list()
for (i in 1:length(ids)){
  
  book  = dickensbooks[[i]]
  f = book
  for (j in 1:nrow(book)){
    x = book[j, 2]
    x = x %>% str_replace_all("[[:punct:]]"," ") %>%  str_replace_all("[^[:alpha:]]", " ")
    f[j,2] = x
  }
  cleand[[i]] = f
  
}



df = data.frame(1,2,3,4,5,6,7)
tries = list()
bis = list()
uns = list()

for (i in 1:length(ids)){

  res = cleand[[i]]

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

    
    df[nrow(df) + 1,] = c(u$coefficients, b$coefficients, t$coefficients, 0)
    tries[[i]] = trigram
    bis[[i]] = bigram
    uns[[i]]= unigram
}  

View(df)
df = df[-1,]
backup = df
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

ggplot(data = g1, aes(x = n)) + geom_histogram(binwidth  = 1, fill = "purple", alpha = 0.7) + xlim(0,50) + ggtitle(paste("1-gram histogram of",dickensbooks[[i]]$title) )

ggplot(data = g2, aes(x = n)) + geom_histogram(binwidth  = 1, fill = "blue", alpha = 0.7) + xlim(0,20) + ggtitle(paste("2-gram histogram of",dickensbooks[[i]]$title) )

ggplot(data = g3, aes(x = n)) + geom_histogram(binwidth  = 1, fill = "red", alpha = 0.7) + xlim(0,10) + ggtitle(paste("3-gram histogram of",dickensbooks[[i]]$title) )

