library(jiebaR)
library(tmcn)
library(parallel)
library(xml2)
library(httr)

#====================================================蘋果日報============================================================
print('Apple')

apple <- 'https://tw.news.appledaily.com'
category <- '/international/realtime/'
appleUrl <- paste0(apple, category)
pageST <- 1
pageED <- 7

article.list <- NULL
for(page in pageST:pageED){
  url <- paste0(appleUrl, page)
  doc <- GET(url) %>% content
  article.list <- c(article.list, doc %>% html_nodes('#maincontent > div.thoracis > div.abdominis.rlby.clearmen > ul > li > a') %>% html_attr('href') %>% paste0(apple,.))
  # article.list <- c(article.list, article.list.temp)
}


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
    title <- temp.html %>% html_node('#article > div.wrapper > div > main > article > hgroup > h1') %>% html_text()
    dateTime <- temp.html %>% html_node('#article > div.wrapper > div > main > article > hgroup > div.ndArticle_creat') %>% html_text()
    content <- temp.html %>% html_nodes(xpath = '//*[@id="article"]/div[1]/div/main/article/div[2]/div[2]/article/div[1]/p[1]/text()') %>% 
      html_text() %>%
      paste(., collapse = ' ') %>%
      gsub('\n','',.) %>%
      gsub('\U00A0','',.) %>%
      gsub('\U63D1','',.)
    temp <- data.frame(title = title, dateTime = dateTime, content = content, stringsAsFactors = F)
    return(temp)
    
  }) %>% bind_rows(.)
  

# stopCluster(cl)
# gc()


#去掉NA的新聞(影片新聞)
all <- temp %>% 
  mutate(
    dateTime = dateTime %>% substr(.,start = 6, stop = 21) %>% parse_datetime("%Y/%m/%d %H:%M") %>% as.Date(),
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
filepath <- paste0("D:\\crawl\\", today, "_apple_international.csv" )
write.csv(all,file = filepath, row.names = F)
#====================================================蘋果日報============================================================
