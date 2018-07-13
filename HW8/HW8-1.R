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
    str_replace_all("[[:punct:]]"," ") %>% 
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
    mutate(book_no = x)
  
  dickens[[i]] = book 
}

alls = bind_rows(dickens)

alls %>% filter(proper != TRUE) -> oth

oth %>% group_by(word) %>% summarise(totalCount = sum(count)) -> merged
merged = arrange(merged, -totalCount)


best = head(merged, 20)

ggplot(best, aes(x = reorder(word, totalCount) , y = totalCount)) + geom_col(fill = "purple", alpha = 0.8) + theme(axis.text.x = element_text(angle=-90, vjust=0.5)) 


