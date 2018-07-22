library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

author = read_rds("~/Desktop/Data Analysis/data/Articles/authors.rds")
subj = author %>% select(subjAreas)
View(author)
subj %>% str_replace_all("~", " ") %>% 
  str_replace_all("[[:punct:]]"," ") %>% 
  str_split(pattern = "\\s+") %>% 
  unlist() %>% 
  table() -> words

words = words %>% as.data.frame(stringsAsFactors = F)

colnames(words) = c("word","count")

words = words  %>% 
  filter(!str_to_lower(word) %in% stop_words$word) %>% 
  filter(nchar(word)>1) %>% 
  filter(!str_detect(word, "\\d")) %>% arrange(-count)


words = words[-4,]
words = arrange(words, -count)
words = head(words, 44)
ggplot(words, aes(x = reorder(word, -count), y = count)) + geom_col(fill = "purple") + theme(axis.text.x = element_text(angle=-90, vjust=0.5, size = 8))+ xlab("Field Name")




