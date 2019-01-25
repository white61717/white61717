library(jsonlite)
library(dplyr)
all <- fromJSON("E:/Desktop/全部的人瀏覽網頁記錄統計.json")
exchange <- fromJSON("E:/Desktop/買外匯的人瀏覽網頁記錄統計.json")
card <- fromJSON("E:/Desktop/買信用卡的人瀏覽網頁記錄統計.json")
trust <- fromJSON("E:/Desktop/買信託的人瀏覽網頁記錄統計.json")
credit <- fromJSON("E:/Desktop/買信貸的人瀏覽網頁記錄統計.json")


cutstr <- function(x){
  strlist <- strsplit(x,split="/",fixed=T,)
}
test <- lapply(exchange$PAGE,cutstr)
i<- 1
b<- list()
for (i in c(1:length(test))){
  a <- paste0("/",test[[i]][[1]][2],"/",test[[i]][[1]][3])
  b <- c(b,a)
}
exchange$PAGE <- b %>% unlist()
exchange$PAGE_NO <- NULL
exchange <- aggregate(Freq ~ PAGE,exchange,sum) 
exchange <- exchange[order(exchange$Freq, decreasing = T),]


cutstr <- function(x){
  strlist <- strsplit(x,split="/",fixed=T,)
}
test <- lapply(card$PAGE,cutstr)
i<- 1
b<- list()
for (i in c(1:length(test))){
  a <- paste0("/",test[[i]][[1]][2],"/",test[[i]][[1]][3])
  b <- c(b,a)
}
card$PAGE <- b %>% unlist()
card$PAGE_NO <- NULL
card <- aggregate(Freq ~ PAGE,card,sum)
card <- card[order(card$Freq, decreasing = T),]


cutstr <- function(x){
  strlist <- strsplit(x,split="/",fixed=T,)
}
test <- lapply(trust$PAGE,cutstr)
i<- 1
b<- list()
for (i in c(1:length(test))){
  a <- paste0("/",test[[i]][[1]][2],"/",test[[i]][[1]][3])
  b <- c(b,a)
}
trust$PAGE <- b %>% unlist()
trust$PAGE_NO <- NULL
trust <- aggregate(Freq ~ PAGE,trust,sum)
trust <- trust[order(trust$Freq, decreasing = T),]


cutstr <- function(x){
  strlist <- strsplit(x,split="/",fixed=T,)
}
test <- lapply(credit$PAGE,cutstr)
i<- 1
b<- list()
for (i in c(1:length(test))){
a <- paste0("/",test[[i]][[1]][2],"/",test[[i]][[1]][3])
b <- c(b,a)
}
credit$PAGE <- b %>% unlist()
credit$PAGE_NO <- NULL
credit <- aggregate(Freq ~ PAGE,credit,sum)
credit <- credit[order(credit$Freq, decreasing = T),]


