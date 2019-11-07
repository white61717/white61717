library(tidyverse)
library(RSelenium)
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
library(parallel)
library(xml2)
library(httr)
#====================================================自由時報============================================================

ltn <- 'https://news.ltn.com.tw/list/breakingnews/'
category <- 'world'
ltnUrl <- paste0(ltn, category)


remDr <- remoteDriver$new(
  remoteServerAddr = "localhost",
  port = 4444,
  browserName = "chrome",
  )
remDr$open()
remDr$navigate(ltnUrl)

repeat {
  last_height = 0
  new_height = remDr$executeScript("return document.body.scrollHeight")
  remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
  Sys.sleep(2)
  new2_height = remDr$executeScript("return document.body.scrollHeight")
  if(unlist(new_height) == unlist(new2_height)){
    break
  }
}

article.links <- remDr$findElements(using = 'css',value = '#ltnRWD > div.content > section > div.whitecon.boxTitle > ul > li > a.tit')

article.list <- NULL
for(i in 1:length(article.links)){
  article.list <- c(article.list, article.links[[i]]$getElementAttribute("href"))
} 
article.list <- article.list %>% unlist()

remDr$quit()

cl <- makeCluster(4)
clusterEvalQ(cl,{
  require(dplyr)
  library(tidyverse)
  library(rvest)
  library(stringr)
  library(httr)
})
clusterExport(cl,'article.list')

print("crawling......")
temp <- parLapply(cl,article.list, function(article){

  temp.html <- article %>% GET() %>% content()
  title <- temp.html %>% html_node('.whitecon h1') %>% html_text()
  dateTime <- temp.html %>% html_node('.whitecon span.time') %>% html_text() %>% parse_datetime("%Y-%m-%d %H:%M:%S") %>% as.Date()
  content <- temp.html %>% html_nodes('.whitecon p:not(.before_ir):not(.appE1121)') %>%
    html_text() %>% 
    paste(.,collapse = ' ') %>%
    trimws(.)
  temp <- data.frame(title = title, dateTime = dateTime, content = content, stringsAsFactors = F)
  return(temp)
  
}) %>% bind_rows(.)

all <- temp %>% 
  mutate(
    month = format(dateTime, '%m'),
    day = format(dateTime, '%d')
  ) %>%
  filter(.,
         content != ""
  )


#斷詞
print("NLPing......")
jieba.worker <- worker()

# 
# article.date <- all %>%
#   group_by(dateTime) %>% # 以每⽇做分組
#   do((function(input) {
#     freq(segment(input$content, jieba.worker)) %>% # 斷詞後計算詞頻   # 分詞segment(文章,結巴引擎) output= 'X' 'A' 'B'...
#       filter(
#         !(char %in% toTrad(stopwordsCN())), # 過濾 stopword (的、吧、阿...)  # toTrad把簡體轉繁體
#         !(char %in% c('報導','綜合','即時新聞')), # 過濾 stopword (的、吧、阿...)  # toTrad把簡體轉繁體
#         !str_detect(char, "[A-z0-9]"), # 過濾英⽂數字
#         nchar(char) > 1 # 過濾單個字
#       ) %>%
#       arrange(desc(freq)) %>% # 以詞頻排序
#       slice(1:100) %>% # 取前 100
#       return
#   })(.)) %>%
#   ungroup
# 
# article.date.words <- freq(article.date$char) %>%
#   rename(freq.all = freq)
# 
# 
# article.everyday <- article.date %>%
#   left_join( # ⽐對全部詞
#     article.date.words,
#     by = 'char'
#   ) %>%
#   group_by(dateTime) %>% # 以每⽇做分組
#   arrange(freq.all) %>% # 每組的詞頻做排序由⼩到⼤
#   slice(1:5) %>% # 取每組前 5
#   summarise( # 合併詞並對詞頻加總
#     char = str_c(char, collapse = ", "),
#     freq = sum(freq)
#   ) %>%
#   ungroup


print("saving......")
today <- Sys.Date() %>% as.character()
filepath <- paste0("D:\\crawl\\", today, "_ltn_international.csv" )
write.csv(all,file = filepath, row.names = F)

#====================================================自由時報============================================================
