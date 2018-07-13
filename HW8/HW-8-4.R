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
  b  = gutenberg_download(x, meta_fields = "title")
  dickensbooks[[i]] = b
}

for (i in 1:length(ids)){
  
  book  = dickensbooks[[i]]
  book = book %>% 
    str_replace_all("\"","") %>% 
    str_replace_all("[[:punct:]]","") %>% 
    str_split(pattern = "\\s") %>% 
    unlist() %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F)
  
  colnames(book) = c("word","count")
  
  book = book  %>% 
    filter(!str_to_lower(word) %in% stop_words$word) %>% 
    filter(nchar(word)>1) %>% 
    filter(!str_detect(word, "\\d")) %>% 
    mutate(proper = !word %in% str_to_lower(word)) %>%
    mutate(book_no = x) %>% 
    filter(proper != TRUE) %>% arrange(-count)
  
  dickens[[i]] = book 
}

senti = na.omit(sentiments %>% select(word, sentiment) %>% filter(sentiment == "negative" | sentiment == "positive") %>% unique())

for (i in 1:length(ids)){
  v = dickens[[i]]
  v = full_join(v, senti)
  v = na.omit(v)
  v = arrange(v, -count)
  dickens[[i]] = v
}


  i = 0
  i = i + 1
  v = dickens[[i]]
  vpos = v %>% filter(sentiment == "positive") %>% head(20)
  vneg = v %>% filter(sentiment == "negative") %>% head(20)
  v = rbind(vpos, vneg)
  ggplot(v, aes(x = reorder(word, count), count, fill = sentiment)) + geom_col() + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) + xlab("Words") + ggtitle(dickensbooks[[i]]$title) 

