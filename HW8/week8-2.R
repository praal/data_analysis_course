library(gutenbergr)
library(dplyr)


m = gutenberg_metadata
m %>% filter(author == "Dickens, Charles" ) %>% filter(language == "en")-> ch
View(ch)


oliver = gutenberg_download(730)