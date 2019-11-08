library(tidyverse)
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
library(parallel)
library(xml2)
library(httr)

#====================================================中國時報============================================================
print('ChinaTimes')

chinatimes <- 'https://www.chinatimes.com'
category <- '/world/total?page='
chinatimesUrl <- paste0(chinatimes, category)
pageST <- 1
pageED <- 10

article.list <- NULL
for(page in pageST:pageED){
  url <- paste0(chinatimesUrl, page)
  doc <- GET(url) %>% content
  article.list <- c(article.list, doc %>% html_nodes('h3.title a') %>% html_attr('href') %>% paste0(chinatimes,.))
  # article.list <- c(article.list, article.list.temp)
}
article.list <- article.list[article.list != 'https://www.chinatimes.com']


cl <- makeCluster(4)

clusterEvalQ(cl,{
  require(dplyr)
  library(tidyverse)
  library(rvest)
  library(stringr)
  library(httr)
})
clusterExport(cl,'article.list')


temp <- NULL

print("crawling......")
temp <- parLapply(cl, article.list, function(article){

  temp.html <- article %>% GET() %>% content()
  title <- temp.html %>% html_node('header > h1') %>% html_text()
  dateTime <- temp.html %>% html_node('div.meta-info > time >span:nth-child(2)') %>% html_text()
  content <- temp.html %>% html_nodes('div.col-xl-11 div.article-body > p') %>% html_text() %>%
    paste(., collapse = ' ') %>%
    trimws(.) %>%
    gsub(' ','',.)
  temp <- data.frame(title = title, dateTime = dateTime, content = content, stringsAsFactors = F)
  return(temp)
  
}) %>% bind_rows(.)


stopCluster(cl)
gc()


#去掉NA的新聞(影片新聞)
all <- temp %>% 
  mutate(
    dateTime = dateTime %>% parse_datetime("%Y/%m/%d") %>% as.Date(),
    month = format(dateTime, '%m'),
    day = format(dateTime, '%d')
  ) %>%
  filter(.,
         content != ""
  )


#斷詞
print("NLPing......")
jieba.worker <- worker()


article.date <- all %>%
  group_by(dateTime) %>% # 以每⽇做分組
  do((function(input) {
    freq(segment(input$content, jieba.worker)) %>% # 斷詞後計算詞頻   # 分詞segment(文章,結巴引擎) output= 'X' 'A' 'B'...
      filter(
        !(char %in% toTrad(stopwordsCN())), # 過濾 stopword (的、吧、阿...)  # toTrad把簡體轉繁體
        !str_detect(char, "[A-z0-9]"), # 過濾英⽂數字
        nchar(char) > 1 # 過濾單個字
      ) %>%
      arrange(desc(freq)) %>% # 以詞頻排序
      slice(1:100) %>% # 取前 100
      return
  })(.)) %>%
  ungroup

article.date.words <- freq(article.date$char) %>%
  rename(freq.all = freq)

article.everyday <- article.date %>%
  left_join( # ⽐對全部詞
    article.date.words,
    by = 'char'
  ) %>%
  group_by(dateTime) %>% # 以每⽇做分組
  arrange(freq.all) %>% # 每組的詞頻做排序由⼩到⼤
  slice(1:5) %>% # 取每組前 5
  summarise( # 合併詞並對詞頻加總
    char = str_c(char, collapse = ", "),
    freq = sum(freq)
  ) %>%
  ungroup



print("saving......")
today <- Sys.Date() %>% as.character()
filepath <- paste0("D:\\crawl\\", today, "_chinatimes_international.csv" )
write.csv(all,file = filepath, row.names = F)
#====================================================中國時報============================================================
