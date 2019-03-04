#第一次貼的

require(rvest)
require(dplyr)

letter <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

urlList <- letter %>% lapply(.,function(t){
  sprintf('http://www.517ming.com/%s-kaitou-danci.html',t)
}) %>% unlist()

docList <- urlList %>% lapply(.,function(t){
  Sys.sleep(2)
  doc <- read_html(t)
  print(t)
  print('successful')
  return(doc)
})

#可以t <- docList
wordList <- docList %>% lapply(.,function(t){
  word <- t %>% html_nodes('tr td:nth-child(2)') %>% html_text() %>% .[-1] %>%  .[-length(.)]
  return(word)
})

wordList %>% .[[1]] %>% head()

#第二次貼

require(rvest)
require(dplyr)

letter <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

urlList <- letter %>% lapply(.,function(t){
  sprintf('http://www.517ming.com/%s-kaitou-danci.html',t)
}) %>% unlist()

docList <- urlList %>% lapply(.,function(t){
  Sys.sleep(2)
  doc <- read_html(t)
  print(t)
  print('successful')
  return(doc)
})

wordList <- docList %>% lapply(.,function(t){
  word <- t %>% html_nodes('tr td:nth-child(2)') %>% html_text() %>% .[-c(1,length(.))]
  return(word)
})

wordTable <- wordList %>% unlist() %>% data.frame(word = .,stringsAsFactors = F)

#----------------

wordTable <- wordList %>% lapply(., function(t){
  return(t[nchar(t) >= 5] %>% head(5))
}) %>% unlist() %>% data.frame(word = ., stringsAsFactors = F)

wordTable #看只有五個以上

wordTable$partOfspeech <- wordTable$word %>% lapply(., function(t){
  url <-sprintf('http://cdict.info/query/%s', t)
  Sys.sleep(2)
  doc <- read_html(url)
  resultBox <- doc %>% html_nodes('.resultbox') %>% html_text()
  resultBox <- resultBox %>% strsplit(., split = '【') %>% unlist() %>% .[2]
  resultBox <- resultBox %>% strsplit(., split = '】') %>% unlist() %>% .[1]
  return(resultBox)
}) %>% unlist()

wordTable$partOfspeech[is.na(wordTable$partOfspeech)] <- '名詞'

wordTable %>% group_by(.,partOfspeech) %>% summarise(count=n())

writeLines(toJSON(wordTable,auto_unbox = T, dataframe = 'rows',useBytes=T),'C:/Users/Student/Desktop/wordTable.json')
