  {
  library('RSelenium')
  library('rvest')
  library('stringr')
  library('parallel')
  library('dplyr')
  
  url <- 'https://www.facebook.com/nsfwstudio/'
  
  remDr <- remoteDriver(
    remoteServerAddr = "localhost",
    port = 4444,
    browserName = "chrome")
  
  remDr$open()
  
  remDr$navigate(url)
  
  photo <- remDr$findElement(using = 'css', value = '#u_0_g > div:nth-child(5) > a')
  photo$sendKeysToElement(list(key = "enter"))
  
  #持續滾動卷軸直到最下面
  repeat {   
    last_height = 0
    remDr$executeScript("window.scrollTo(0,document.body.scrollHeight);")
    new_height = remDr$executeScript("return document.body.scrollHeight")
    Sys.sleep(3)
    new2_height = remDr$executeScript("return document.body.scrollHeight")
    if(unlist(new_height) == unlist(new2_height)){
      break
    }
  }
  #點掉ads
  ads <- remDr$findElement(using = 'css',value = '#expanding_cta_close_button')
  ads$sendKeysToElement(list(key = "enter"))

  #抓圖片資料
  cat('crawling pic data...\n')
  imgListUrl <- remDr$findElements(using = 'css',value = '._2eec > ._2eea > a')
  
  if(10000 < length(imgListUrl)){
    cat('too large!!\n')
    break
  }else if( 10000 >= length(imgListUrl) && length(imgListUrl) > 7500){
    
    cat('4 cluster\n')
    imgListUrl1 <- list()
    imgListUrl2 <- list()
    imgListUrl3 <- list()
    imgListUrl4 <- list()
    for(i in 1:floor(length(imgListUrl)/4)){
      imgListUrl1 <- c(imgListUrl1,imgListUrl[[i]])
    }
    for(i in ceiling(length(imgListUrl)/4):floor(length(imgListUrl)*2/4)){
      imgListUrl2 <- c(imgListUrl2,imgListUrl[[i]])
    }
    for(i in ceiling(length(imgListUrl)*2/4):floor(length(imgListUrl)*3/4)){
      imgListUrl3 <- c(imgListUrl3,imgListUrl[[i]])
    }
    for(i in ceiling(length(imgListUrl)*3/4):length(imgListUrl)){
      imgListUrl4 <- c(imgListUrl4,imgListUrl[[i]])
    }
    rm(imgListUrl,ads)
    gc()
    
    #起cluster1
    cat('cluster1\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl1')
    #解開成圖片網址
    imgList1 <- parLapply(cl,imgListUrl1,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl1)
    gc()
    
    #起cluster2
    cat('cluster2\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl2')
    #解開成圖片網址
    imgList2 <- parLapply(cl,imgListUrl2,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl2)
    gc()
    
    #起cluster3
    cat('cluster3\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl3')
    #解開成圖片網址
    imgList3 <- parLapply(cl,imgListUrl3,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl3)
    gc()
    
    #起cluster4
    cat('cluster4\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl4')
    #解開成圖片網址
    imgList4 <- parLapply(cl,imgListUrl4,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl4)
    gc()
    
    imgList <- c(imgList1,imgList2,imgList3,imgList4)
    rm(imgList1,imgList2,imgList3,imgList4)
    gc()
    
  }else if(7500 >= length(imgListUrl) && length(imgListUrl) > 4000){
    
    cat('3 cluster\n')
    imgListUrl1 <- list()
    imgListUrl2 <- list()
    imgListUrl3 <- list()
    for(i in 1:floor(length(imgListUrl)/3)){
      imgListUrl1 <- c(imgListUrl1,imgListUrl[[i]])
    }
    for(i in ceiling(length(imgListUrl)/3):floor(length(imgListUrl)*2/3)){
      imgListUrl2 <- c(imgListUrl2,imgListUrl[[i]])
    }
    for(i in ceiling(length(imgListUrl)*2/3):length(imgListUrl)){
      imgListUrl3 <- c(imgListUrl3,imgListUrl[[i]])
    }
    rm(imgListUrl,ads)
    gc()
    
    #起cluster1
    cat('cluster1\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl1')
    #解開成圖片網址
    imgList1 <- parLapply(cl,imgListUrl1,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl1)
    gc()
    
    #起cluster2
    cat('cluster2\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl2')
    #解開成圖片網址
    imgList2 <- parLapply(cl,imgListUrl2,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl2)
    gc()
    
    #起cluster3
    cat('cluster3\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl3')
    #解開成圖片網址
    imgList3 <- parLapply(cl,imgListUrl3,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl3)
    gc()
    
    imgList <- c(imgList1,imgList2,imgList3)
    rm(imgList1,imgList2,imgList3)
    gc()
    
  }else if(4000 >= length(imgListUrl) && length(imgListUrl) > 2000){
    
    cat('2 cluster\n')
    imgListUrl1 <- list()
    imgListUrl2 <- list()
    for(i in 1:floor(length(imgListUrl)/2)){
      imgListUrl1 <- c(imgListUrl1,imgListUrl[[i]])
    }
    for(i in ceiling(length(imgListUrl)/2):length(imgListUrl)){
      imgListUrl2 <- c(imgListUrl2,imgListUrl[[i]])
    }
    rm(imgListUrl,ads)
    gc()
    
    #起cluster1
    cat('cluster1\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl1')
    #解開成圖片網址
    imgList1 <- parLapply(cl,imgListUrl1,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl1)
    gc()
    
    #起cluster2
    cat('cluster2\n')
    cl <- makeCluster(7)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl2')
    #解開成圖片網址
    imgList2 <- parLapply(cl,imgListUrl2,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    stopCluster(cl)
    rm(imgListUrl2)
    gc()
    
    
    imgList <- c(imgList1,imgList2)
    rm(imgList1,imgList2)
    gc()
  }else{
    
    cat('1 cluster\n')
    #起cluster
    cat('cluster\n')
    cl <- makeCluster(5)
    clusterEvalQ(cl,{
      library('RSelenium')
    })
    clusterExport(cl,'imgListUrl')
    
    #解開成圖片網址
    imgList <- parLapply(cl,imgListUrl,function(x){
      x$getElementAttribute("href")
    }) %>% unlist() %>% paste0(.,'&theater')
    rm(imgListUrl,ads)
    stopCluster(cl)
    gc()
  }
  #可能大太導致記憶體不足 分開
  
  
  

  Sys.sleep(10)
  remDr$executeScript("return document.readyState")
  # 
  # #抓讚數資料
  # likesList <- remDr$findElements(using = 'css',value = '.rfloat ._4crj > a:nth-child(1)')
  # #解開讚數資料
  # lapply(likesList,function(x){
  #   x$getElementText()
  # }) %>% unlist()

  
  #抓讚數資料
  cat('crawling likes data...\n')
  doc <- remDr$getPageSource()[[1]] %>% read_html()
  # likesList <- doc %>% html_nodes(".rfloat ._4crj > a:nth-child(1)") %>% html_text()
  likesList <- doc %>% html_nodes(".rfloat ._4crj") %>% html_text()
  cat('transforming likes data...\n')
  likesList <- lapply(likesList,function(x){
                  
                    if(grepl('萬',x) == T){
                      temp <- strsplit(x,split = '\u00A0',fixed=T) %>% unlist() %>% .[1] %>% as.numeric() * 10000
                      temp %>% as.character()
                    }else{
                      y <- gsub(",",replacement = "",x)
                      strsplit(y,split = ' · ',fixed=T) %>% unlist() %>% .[1]
                      }
                  
                }) %>% unlist() %>% as.numeric()
  
  gc()
  #最後table
  final <- data.frame(img = imgList,likes = likesList,stringsAsFactors = F) %>% arrange(.,desc(.$likes))
  }
  
  
  #看前20張
  {
  headpage <- final %>% head(20) %>% .[,1]
  cl <- makeCluster(7)
  clusterEvalQ(cl,{
    library('RSelenium')
  })
  clusterExport(cl,'headpage')
  parLapply(cl,headpage,function(url){
    remDr <- remoteDriver(
      remoteServerAddr = "localhost",
      port = 4444,
      browserName = "chrome")
    remDr$open()
    remDr$navigate(url)
  })
  }
  
  #抓粉絲頁名稱
  pageName <- doc %>% html_nodes('#seo_h1_tag > a > span') %>% html_text()
  filePath <- paste0('E:\\Desktop\\', pageName ,'.csv')
  write.csv(final,file = filePath,row.names = F)

