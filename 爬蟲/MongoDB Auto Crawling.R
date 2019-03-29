require(mongolite)
require(dplyr)
require(rvest)
require(parallel)
require(httr)

dbUrl <- 'mongodb://xnet:23222635@127.0.0.1:27017'
connArticle <- mongo(db = 'businesstoday',collection = 'article',url = dbUrl)
article <- connArticle$find(fields = '{"_id":1,"title":1,"keyword":1}')
article <- article[grepl(pattern = "^/article/",article$`_id`),]

connCrawlRule <- mongo(db = 'businesstoday',collection = 'crawlRule',url = dbUrl)
crawlRule <- connCrawlRule$find(fields = '{"_id":1,"selector":1,"method":1,"attr":1,"return":1}')

  
crawlfunc <- function(rule,doc){
    
    if(rule[["method"]] == "text"){
      if(rule[["return"]] == "single"){
        output <- doc %>% html_nodes(rule[["selector"]]) %>% html_text() %>% paste(.,collapse = "")
      }else if(rule[["return"]] == "multi"){
        output <- doc %>% html_nodes(rule[["selector"]]) %>% html_text()
      }
    }else if(rule[["method"]] == "attr"){
      if(rule[["return"]] == "single"){
        output <- doc %>% html_nodes(rule[["selector"]]) %>% html_attr(.,rule[["attr"]])
      }else if(rule[["return"]] == "multi"){
        output <- doc %>% html_nodes(rule[["selector"]]) %>% html_attr(.,rule[["attr"]])
      }
    }
  #從article裡面用grep抓出所有文章，當成網址，再從crawlRule內抓規則
  return(output)
}

cl <- makeCluster(4)
clusterEvalQ(cl,{
  require(mongolite)
  require(dplyr)
  require(rvest)
  require(httr)
  
  articleInfo <- mongo(db = 'test',collection = 'test',url = 'mongodb://xnet:23222635@r.xnet.world:27017')
  })
#當parLapply內要用到外面的參數，要先給到cluster內
clusterExport(cl,"crawlfunc",envir = environment())
parLapply(cl, article$`_id`, function(pageid,crawlRule){
  
  url <- paste0("https://www.businesstoday.com.tw",pageid)
  doc <- url %>% GET() %>% content()

  test <- apply(crawlRule,1,crawlfunc,doc)
  
},crawlRule)
