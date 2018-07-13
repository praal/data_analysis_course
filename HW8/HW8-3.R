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
    mutate(book_no = i) %>% filter(proper == TRUE) %>% arrange(-count)%>%
    filter(word != "Miss") %>% filter(word != "Sir") %>% 
    mutate(freq = 100 * count / sum(count))
  
  
  
  dickens[[i]] = head(book,5) 
}

alls = bind_rows(dickens)

book  = dickens[[i]]
  ggplot(book, aes(x = reorder(word, count), y = freq)) + geom_col( fill = "purple", alpha = 0.5) + xlab("Names") + ggtitle(dickensbooks[[i]]$title)
  

