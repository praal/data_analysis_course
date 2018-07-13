
library(gutenbergr)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)


miser_raw  = gutenberg_download(135)
typeof(miser_raw)

miser = miser_raw

for (i in 1:nrow(miser_raw)){
  x = miser_raw[i, 2]
  x = x %>% str_replace_all("[[:punct:]]"," ") 
  miser[i,2] = x
}
View(miser)

miser %>%  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>% 
  count(ngram, sort = TRUE)  %>% 
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!str_detect(word1, "^\\d+")) %>% 
  filter(!str_detect(word2, "^\\d+")) %>% 
  arrange(-n) -> miser


words = miser
words %>% filter(word1 != "chapter") %>% filter(word1 != "Chapter") -> words
fr = stopwords(kind = "fr")

words %>% filter(!word1 %in% fr) %>% filter(!word2 %in% fr) -> words

best = head(words, 30)
View(words)
ggplot(best, aes(x = reorder(paste(word1 , word2),n), y = n)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 


