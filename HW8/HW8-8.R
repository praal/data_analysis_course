library(gutenbergr)
library(dplyr)
library(tm)
library(wordcloud)
library(stringr)
library(tidytext)
library(ggplot2)

ids = c(580, 730, 967, 700, 917, 968, 821, 766, 1023, 786, 963, 98, 1400, 883, 564)



dickens = list()
dickensbooks = list()

for (i in 1:length(ids)){
  
  x = ids[i]
  b  = gutenberg_download(x)
  dickensbooks[[i]] = b
}


book = dickensbooks[[1]]
typeof(book)
text = paste(book$text, collapse = " ")
text = text %>% 
  str_replace_all("[[:punct:]]"," ") %>% 
  str_replace_all("[^[:alpha:]]", " ")

View(text)

chapters <- strsplit( text, "[Cc][Hh][Aa][Pp][Tt][Ee][Rr]")[[1]]


chapters = as.data.frame(chapters)
chapters$text = as.character(chapters$text)
colnames(chapters) = c("text")
chapters = chapters %>% filter(nchar(text) > 200)


q = chapters
q = as.data.frame(chapters[100,])
colnames(q) = c("text")
View(q)
q %>%  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>% 
  count(ngram, sort = TRUE)  %>% 
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!str_detect(word1, "^\\d+")) %>% 
  filter(!str_detect(word2, "^\\d+")) %>% 
  arrange(-n) -> toks

View(toks)


q = q %>% 
  str_split(pattern = "\\s") %>% 
  unlist() %>% 
  table() %>% 
  as.data.frame(stringsAsFactors = F)

View(q)
colnames(book) = c("word","count")

book = book  %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(nchar(word)>1) %>% 
  filter(!str_detect(word, "\\d")) %>% 
  mutate(proper = !word %in% str_to_lower(word)) %>%
  mutate(book_no = x)




