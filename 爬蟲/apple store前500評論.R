library('rvest')
library('dplyr')
library('jsonlite')

appID <- '1326734526'
name <- NULL
score <- NULL
title <- NULL
content <- NULL


for(page in 1:10){
  
  url <- paste0('https://itunes.apple.com/rss/customerreviews/page=', page,'/id=', appID, '/sortby=mostrecent/json?l=en&&cc=tw')
  doc <- fromJSON(url)
  
  name <- c(name, doc$feed$entry$author$name$label)
  score <- c(score, doc$feed$entry$`im:rating`$label)
  title <- c(title, doc$feed$entry$title$label)
  content <- c(content, doc$feed$entry$content$label)

}

comments <- cbind(name = name, score = score, title = title, content = content) %>% as.data.frame()
