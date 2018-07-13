
library(gutenbergr)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)

#miser_raw  = gutenberg_download(135)

miser = miser_raw

for (i in 1:nrow(miser_raw)){
  x = miser_raw[i, 2]
  x = x %>% str_replace_all("[[:punct:]]"," ") 
  miser[i,2] = x
}

stopss = stop_words %>% filter(word != "he") %>% filter(word != "she") 

miser %>%  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>% 
  count(ngram, sort = TRUE)  %>% 
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stopss$word) %>% 
  filter(!word2 %in% stopss$word) %>% 
  filter(!str_detect(word1, "^\\d+")) %>% 
  filter(!str_detect(word2, "^\\d+")) %>% 
  arrange(-n) -> words

words %>% filter(word1 != "chapter") %>% filter(word1 != "Chapter") -> words
fr = stopwords(kind = "fr")

words %>% filter(!word1 %in% fr) %>% filter(!word2 %in% fr) -> words

words %>% filter(word1 == "she") %>% head(20)-> women
words %>% filter(word1 == "he") %>% head(20) -> men

ggplot(women, aes(x =word2, y = n)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("verbs") + ylab("count")+ ggtitle("women")
ggplot(men, aes(x =word2, y = n)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("verbs") + ylab("count")+ ggtitle("men")


