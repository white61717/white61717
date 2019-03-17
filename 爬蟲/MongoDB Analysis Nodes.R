require(mongolite)
require(dplyr)
require(rvest)
#dburl資料庫位置
dbUrl <- 'mongodb://xnet:23222635@127.0.0.1:27017'
conn <- mongo(db = 'pool',collection = 'dbdomain',url = dbUrl)

#查表
tb <- conn$find()

tb

#collect 資料表
#
conn <- mongo(db = 'culturelaunch',collection = 'articleSource',url = dbUrl)
#query 查詢條件
#field 用json的格式下sql指令
articleSource <- conn$find(fields = '{"_id":1,"bodyContent":1}')



allInfo <- NULL

for(i in 1:nrow(articleSource)){
  
  doc <- articleSource$bodyContent %>% .[i] %>% read_html()
  title <- doc %>% html_nodes(.,"#outer-wrap > div:nth-child(2) > div.container > div > div.col-md-8.col-sm-12.col-xs-12.main-content > div > article > div.post-header > div.post-information-RL-15 > h1") %>% html_text()
  content <- doc %>% html_nodes(.,"#outer-wrap > div:nth-child(2) > div.container > div > div.col-md-8.col-sm-12.col-xs-12.main-content > div > article > div.post-content") %>% html_text() %>% trimws(.) %>% gsub("　","",.) %>% gsub(" ","",.) %>% gsub("\n","",.)
  tag <- doc %>% html_nodes(.,"#outer-wrap > div:nth-child(2) > div.container > div > div.col-md-8.col-sm-12.col-xs-12.main-content > div > article > div.center-block.text-center > div > a") %>% html_text() %>% strsplit(.,split = " ")
  author <- doc %>% html_nodes(.,"#outer-wrap > div:nth-child(2) > div.container > div > div.col-md-8.col-sm-12.col-xs-12.main-content > div > article > div.post-header > div.post-information-RL-15 > div > div.pull-rigt.text-right > div.post-author > a") %>% html_text()
  post <- doc %>% html_nodes(.,"#outer-wrap > div:nth-child(2) > div.container > div > div.col-md-8.col-sm-12.col-xs-12.main-content > div > article > div.post-header > div.post-information-RL-15 > div > div.pull-rigt.text-right > span > time") %>% html_text()
  cate <- doc %>% html_nodes(.,"#outer-wrap > div:nth-child(2) > div.container > div > div.col-md-8.col-sm-12.col-xs-12.main-content > div > article > div.post-header > div.post-information-RL-15 > span > a") %>% html_text() %>% strsplit(.,split = " ") %>% as.array()
  
  all <- list("title" = title,"content"=content,"tag"=tag,"author"=author,"post"=post,"cate"=cate) %>% t() %>% as.data.frame()
  allInfo <- rbind(allInfo,all)

}
allInfo[allInfo$title == "character(0)",] <- NULL
