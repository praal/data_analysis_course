library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(wordcloud)

article = read_rds("~/Desktop/Data Analysis/data/Articles/article_author_affilation.rds")

article %>% select(au_id, affilation_id) %>% unique() -> auaf
auaf %>% group_by(au_id) %>% summarise(aff = round(mean(as.integer(affilation_id),1))) -> t

aff = read_rds("~/Desktop/Data Analysis/data/affilations.rds")
aff %>% select(aff = id, name) -> aff
t = inner_join(t, aff)
t = t[-1,]

sharif = t %>% filter(aff == 366) 



author = read_rds("~/Desktop/Data Analysis/data/Articles/authors.rds")

author = author %>% filter(id %in% sharif$au_id)
subj = author %>% select(subjAreas)

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




