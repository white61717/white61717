require(jsonlite)
require(dplyr)
require(rvest)

# 欲回爬日數
days <- 180

i <- 0
repeat{
  
  if(i == days){
    break
  }
  datetime <- (Sys.Date() - i) %>% as.character() %>% gsub('-','',.)
  url <- 'http://www.twse.com.tw/exchangeReport/MI_INDEX?response=json&date=20181210&type=ALL'
  Sys.sleep(runif(1,min = 3,max = 5))
  jsonFile <- fromJSON(url)
  closeAllConnections()
  i <- i + 1
  if(jsonFile$stat == '很抱歉，沒有符合條件的資料!'){
    print('未開市')
    next
  } else {
    stockTable <- jsonFile$data5 %>% data.frame(.,stringsAsFactors = F)
    colnames(stockTable) <- jsonFile$fields5
    stockTable$`漲跌(+/-)` <- stockTable$`漲跌(+/-)` %>% lapply(.,function(t){
      if(nchar(t) <= 2){
        result <- t
      } else {
        result <- read_html(t) %>% html_nodes('p') %>% html_text()
      }
      
      return(result)
    }) %>% unlist()
    
    writeLines(toJSON(stockTable,auto_unbox = T,
               dataframe = 'rows'),
               useBytes = T,
               sprintf('C:/Users/tacoi5pc/Desktop/twstock/%s',datetime))
    
    print(sprintf('C:/Users/tacoi5pc/Desktop/twstock/%s %s',datetime,'created'))
  }

}

