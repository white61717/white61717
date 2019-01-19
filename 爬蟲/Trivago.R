library(readr)
library(rvest)
library(dplyr)
library(ggplot2)
library(tmcn)
library(httr)
library(RSelenium)
rD <- rsDriver() # runs a chrome browser, wait for necessary files to download
remDr <- rD$client
#no need for remDr$open() browser should already be open
hotelinfo <- NULL
loop <- 1:20
remDr$navigate("https://www.trivago.com.tw/?aDateRange%5Barr%5D=2018-10-25&aDateRange%5Bdep%5D=2018-10-26&aPriceRange%5Bfrom%5D=0&aPriceRange%5Bto%5D=0&iPathId=92368&aGeoCode%5Blat%5D=25.085405&aGeoCode%5Blng%5D=121.561501&iGeoDistanceItem=0&aCategoryRange=0%2C1%2C2%2C3%2C4%2C5&aOverallLiking=1%2C2%2C3%2C4%2C5&sOrderBy=relevance%20desc&bTopDealsOnly=false&iRoomType=7&cpt=9236803&iIncludeAll=0&iViewType=0&bIsSeoPage=false&bIsSitemap=false&")
for(i in loop){
  tryCatch({
    print(i)
    Sys.sleep(5)
    house <- remDr$findElements(using = "class name", value = "item__details")
    house <- unlist(lapply(house, function(t) { t$getElementText() }))
    house <- strsplit(house,split="\n")
    house <- house %>% lapply(.,function(t){t %>% .[!(grepl(pattern='星級',.)|grepl(pattern='人氣',.)|.=='新')]})
    price <- remDr$findElements(using = "class name", value = "item__best-price")
    price <- unlist(lapply(price, function(t) { t$getElementText() }))
    #price <- strsplit(price,split=" ")
    name <- house %>% lapply(.,function(t){t[1]}) %>% unlist()
    category <- house %>% lapply(.,function(t){t[2]}) %>% unlist()
    location <- house %>% lapply(.,function(t){t[3] %>% strsplit(.,',') %>% unlist() %>% .[2] %>% trimws()}) %>% unlist()
    score <- house %>% lapply(.,function(t){t[4]}) %>% unlist()
    tmp <- data.frame(name,category,location,score,price,stringsAsFactors = F)
    hotelinfo <- hotelinfo %>% rbind(.,tmp)
    #remDr$refresh()
    webElem <- remDr$findElement("css", "body")
    webElem$sendKeysToElement(list(key = "end")) #rolling to the bottom
    webElem <- remDr$findElement("css selector", ".btn.btn--pagination.btn--small.btn--page-arrow.btn--next")
    webElem$clickElement()
  },error=function(e){
    print("Error")
  })
}


remDr$close()
# stop the selenium server
rD[["server"]]$stop() 
# if user forgets to stop server it will be garbage collected.
rm(rD)


