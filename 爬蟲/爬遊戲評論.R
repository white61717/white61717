{

  library('RSelenium')
  library('rvest')
  library('parallel')
  library('dplyr')
  
  url <- 'https://play.google.com/store/apps/details?id=com.a.one.gh&hl=zh_TW&tdsourcetag=s_pctim_aiomsg&showAllReviews=true'
  
  remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4444,
    browserName = "chrome")
  
  remDr$open()
  
  remDr$navigate(url)


  #持續滾動卷軸直到最下面
  repeat {
    
    tryCatch({
      
      last_height = 0
      remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
      new_height = remDr$executeScript("return document.body.scrollHeight")
      Sys.sleep(2)
      new2_height = remDr$executeScript("return document.body.scrollHeight")
      
      if(unlist(new_height) == unlist(new2_height)){
        more <- remDr$findElement(using = 'css',value = '#fcxH9b > div.WpDbMd > c-wiz > div > div.ZfcPIb > div > div.JNury.Ekdcne > div > div > div.W4P4ne > div:nth-child(2) > div.PFAhAf > div')
        more$sendKeysToElement(list(key = "enter"))
      }
    }, error = function(e){
      print('error')
    })
  }
}  






{
  #抓名字
  cat('crawling name data...\n')
  name <- remDr$findElements(using = 'css',value = '#fcxH9b > div.WpDbMd > c-wiz > div > div.ZfcPIb > div > div.JNury.Ekdcne > div > div > div.W4P4ne > div:nth-child(2) > div > div > div > div.d15Mdf.bAhLNe > div.xKpxId.zc7KVe > div.bAhLNe.kx8XBd > span')
  
  cl <- makeCluster(detectCores()-1)
  clusterEvalQ(cl,{
    library('RSelenium')
  })
  clusterExport(cl,'name')
  allName <- parLapply(cl,name,function(x){
    x$getElementText()[[1]]
  }) %>% unlist()
  stopCluster(cl)
  rm(name)
  gc()
  
  
  #抓評論
  cat('crawling comment data...\n')
  comment <- remDr$findElements(using = 'css',value = '#fcxH9b > div.WpDbMd > c-wiz > div > div.ZfcPIb > div > div.JNury.Ekdcne > div > div > div.W4P4ne > div:nth-child(2) > div > div > div > div.d15Mdf.bAhLNe > div.UD7Dzf > span:nth-child(1)')
  
  cl <- makeCluster(detectCores()-1)
  clusterEvalQ(cl,{
    library('RSelenium')
  })
  clusterExport(cl,'comment')
  allComment <- parLapply(cl,comment,function(x){
    x$getElementText()[[1]]
  }) %>% unlist()
  stopCluster(cl)
  rm(comment)
  gc()
  
  
  #抓評分
  cat('crawling score data...\n')
  score <- remDr$findElements(using = 'css',value = '#fcxH9b > div.WpDbMd > c-wiz > div > div.ZfcPIb > div > div.JNury.Ekdcne > div > div > div.W4P4ne > div:nth-child(2) > div > div > div > div.d15Mdf.bAhLNe > div.xKpxId.zc7KVe > div.bAhLNe.kx8XBd > div > span.nt2C1d > div > div')
  
  cl <- makeCluster(detectCores()-1)
  clusterEvalQ(cl,{
    library('RSelenium')
  })
  clusterExport(cl,'score')
  allScore <- parLapply(cl,score,function(x){
    x$getElementAttribute("aria-label")[[1]]
  }) %>% unlist() %>% substr(.,start = 5,stop = 6)
  stopCluster(cl)
  rm(score)
  gc()
  
  
  
  tb <- cbind(allName,allScore, allComment)
  colnames(tb) <- c('Name', 'Score', 'Comment')
  
  
}

