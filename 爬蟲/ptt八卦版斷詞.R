library(tidyverse)
library(rvest)
library(stringr)
library(jiebaR)
library(tmcn)
library(parallel)
library(xml2)
library(httr)

ptt.url <- "https://www.ptt.cc"
gossiping.url <- str_c(ptt.url, "/bbs/Gossiping")
gossiping.session <- html_session(url = gossiping.url)

gossiping.form <- gossiping.session %>% html_node("form") %>% html_form()
gossiping.form

gossiping <- submit_form(
  session = gossiping.session,
  form = gossiping.form,
  submit = "yes"
)
gossiping


page <- gossiping %>% html_node(.,'#action-bar-container > div > div.btn-group.btn-group-paging > a:nth-child(2)') %>% 
          html_attr(.,'href') %>% strsplit(.,'index') %>% unlist() %>% .[2] %>% strsplit(.,'.html') %>% unlist() %>% as.numeric() + 1

links.article <- NULL
page.length <- 500

for(page.index in (page):(page-page.length)){
  links.article <- c(links.article, paste0('https://www.ptt.cc/bbs/Gossiping/index', page.index, '.html'))
}

links.article <- unique(links.article)

#抓各文章網址
article.list <- NULL
for(article.url in links.article){
  
  temp.html <- gossiping %>% jump_to(article.url) 
  article.list <- c(article.list,temp.html %>% html_nodes('#main-container > div.r-list-container.action-bar-margin.bbs-screen > div > div.title > a') %>% 
                      html_attr('href') %>% paste0(ptt.url,.) )
  
}

gc()

all <- data.frame()

# for(article in article.list){
# 
#   temp.html <- gossiping %>% jump_to(article)
#   author <- temp.html %>% html_node('#main-content > div:nth-child(1) > span.article-meta-value') %>% html_text()
#   title <- temp.html %>% html_node('#main-content > div:nth-child(3) > span.article-meta-value') %>% html_text()
#   dateTime <- temp.html %>% html_node('#main-content > div:nth-child(4) > span.article-meta-value') %>% html_text()
#   content <- temp.html %>% html_nodes(.,xpath = '//*[@id="main-content"]/text()') %>% html_text() %>% gsub('\n','',.) %>% paste(., collapse = ' ') %>% as.character()
#   message <- paste0(temp.html %>% html_nodes(.,'#main-content > div > span.f1.push-tag') %>% html_text(),
#                     temp.html %>% html_nodes(.,'#main-content > div > span.f3.push-userid') %>% html_text(), 
#                     temp.html %>% html_nodes(.,'#main-content > div > span.f3.push-content') %>% html_text()) %>% as.list
#   # cbind的輸入型態為向量時，輸出為matrix，在matrix內只能容納一個class，若有非數字向量，他就會轉成factor，故要用dataframE而非rbind
#   temp <- data.frame(author = author, title = title, dateTime = dateTime, content = content, message = paste(message, collapse = ' \\ '),stringsAsFactors = F)
#   all <- rbind(all, temp, stringsAsFactors = F)
#   
# }
# 
# 
# 
# #==================================================================================================================
# # lapply
# 
# 
# 
# all <- lapply(article.list,function(article){
#   
#   temp.html <- gossiping %>% jump_to(article)
#   author <- temp.html %>% html_node('#main-content > div:nth-child(1) > span.article-meta-value') %>% html_text()
#   title <- temp.html %>% html_node('#main-content > div:nth-child(3) > span.article-meta-value') %>% html_text()
#   dateTime <- temp.html %>% html_node('#main-content > div:nth-child(4) > span.article-meta-value') %>% html_text()
#   content <- temp.html %>% html_nodes(.,xpath = '//*[@id="main-content"]/text()') %>% html_text() %>% gsub('\n','',.) %>% paste(., collapse = ' ') %>% as.character()
#   message <- paste0(temp.html %>% html_nodes(.,'#main-content > div > span.f1.push-tag') %>% html_text(),
#                     temp.html %>% html_nodes(.,'#main-content > div > span.f3.push-userid') %>% html_text(), 
#                     temp.html %>% html_nodes(.,'#main-content > div > span.f3.push-content') %>% html_text()) %>% as.list
#   # cbind的輸入型態為向量時，輸出為matrix，在matrix內只能容納一個class，若有非數字向量，他就會轉成factor，故要用dataframE而非rbind
#   temp <- data.frame(author = author, title = title, dateTime = dateTime, content = content, message = paste(message, collapse = ' \\ '),stringsAsFactors = F)
#   
#   }) %>% bind_rows()
  
#==================================================================================================================
#parlappy

cl <- makeCluster(4)

clusterEvalQ(cl,{
  require(dplyr)
  library(tidyverse)
  library(rvest)
  library(stringr)
  library(httr)
})
clusterExport(cl,'article.list')


all <- parLapply(cl,article.list,function(article,gossiping){
  
  temp.html <- article %>% GET(set_cookies('over18' = 1)) %>% content
  # temp.html <- gossiping %>% jump_to(article)
  author <- temp.html %>% html_node('#main-content > div:nth-child(1) > span.article-meta-value') %>% html_text()
  title <- temp.html %>% html_node('#main-content > div:nth-child(3) > span.article-meta-value') %>% html_text()
  dateTime <- temp.html %>% html_node('#main-content > div:nth-child(4) > span.article-meta-value') %>% html_text()
  content <- temp.html %>% html_nodes(.,xpath = '//*[@id="main-content"]/text()') %>% html_text() %>% gsub('\n','',.) %>% paste(., collapse = ' ') %>% as.character()
  message <- paste0(temp.html %>% html_nodes(.,'#main-content > div > span.f1.push-tag') %>% html_text(),
                    temp.html %>% html_nodes(.,'#main-content > div > span.f3.push-userid') %>% html_text(), 
                    temp.html %>% html_nodes(.,'#main-content > div > span.f3.push-content') %>% html_text()) %>% as.list
  # cbind的輸入型態為向量時，輸出為matrix，在matrix內只能容納一個class，若有非數字向量，他就會轉成factor，故要用dataframE而非rbind
  temp <- data.frame(author = author, title = title, dateTime = dateTime, content = content, message = paste(message, collapse = ' \\ '),stringsAsFactors = F)
  return(temp)
}, gossiping) %>% bind_rows()


#==================================================================================================================

rm(gossiping, gossiping.form, gossiping.session, gossiping.url, article, article.list, links.article, 
   page, page.index, page.length, temp, temp.html, message, author, title, dateTime, content)
gc()
all <- all2

all <- all %>% # 格式整理清除 NA
  mutate(
    dateTime = str_sub(dateTime, 5) %>% parse_datetime("%b %d %H:%M:%S %Y") %>% as.Date(),
    month = format(dateTime, "%m"),
    day = format(dateTime, "%d")
)%>%
  filter(
    .,!(grepl('.{1}公告',.$title)|grepl('.{1}協尋',.$title)|is.na(.$title))
  )


#斷詞

jieba.worker <- worker()


article.date <- all %>%
  group_by(dateTime) %>% # 以每⽇做分組
  do((function(input) {
    freq(segment(input$content, jieba.worker)) %>% # 斷詞後計算詞頻
      filter(
        !(char %in% toTrad(stopwordsCN())), # 過濾 stopword
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


article.everyday <- article.everyday[1:2,]


article.everyday %>%
  mutate( # 計算⽉⽇和頻率排名
    month = str_c(format(dateTime, "%m"), "⽉"),
    day = format(dateTime, "%d") %>% parse_number(),
    freq.rank = rank(freq)
  ) %>%
  ggplot() +
  geom_text(
    aes(
      x = 1,
      y = day,
      label = char,
      color = freq.rank
    ),
    hjust = 1,
    size = 3
  ) +
  geom_text(
    aes(
      x = 0,
      y = day,
      label = format(dateTime, "%d")
    ),
    hjust = 0,
    size = 3,
    alpha = 0.4
  ) +
  scale_color_continuous(low = "#03A9F4", high = "#EF5350") +
  scale_y_reverse() +
  facet_grid( ~ month) +
  theme_void()
