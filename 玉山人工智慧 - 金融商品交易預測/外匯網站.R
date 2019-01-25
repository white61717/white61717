require(dplyr)
require(parallel)
require(jiebaR)

behavior <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CUST_BEHAVIOR.csv')
exchange <- read.csv('C:/Users/Student/Desktop/dataset/TBN_FX_TXN.csv')
customerTable <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CIF.csv')
customerTable
# 算外匯交易次數和外匯交易總金額
exchange_count <- aggregate(TXN_DT ~ CUST_NO, exchange , length)
exchange_total <- aggregate(FX_TXN_AMT ~ CUST_NO, exchange , sum)
# join
custom_exchange <- left_join(customerTable,exchange_count)
custom_exchange <- left_join(custom_exchange,exchange_total)
# 沒有值代表沒有投資 補0
custom_exchange$TXN_DT[is.na(custom_exchange$TXN_DT)] <- 0
custom_exchange$FX_TXN_AMT[is.na(custom_exchange$FX_TXN_AMT)] <- 0

#買過外匯的客戶
custom_do_exchange <- custom_exchange[custom_exchange$FX_TXN_AMT != 0,]



cl <- makeCluster(detectCores() - 1)
# 將http改為https
behavior$PAGE <- behavior$PAGE %>% parLapply(cl,.,function(t){
  gsub('http:','https:',t)
})  %>% unlist()

# 去除重複部分 https://www.esunbank.com.tw
behavior$PAGE <- behavior$PAGE %>% parLapply(cl,.,function(t){
  gsub('https://www.esunbank.com.tw','',t)
})  %>% unlist()

# 建立頁面編號對照表
pageNoTable <- behavior$PAGE %>% unique() %>% data.frame(PAGE = .,stringsAsFactors = F)
pageNoTable$PAGE_NO <- 1:nrow(pageNoTable)

log_unique <- behavior$PAGE %>% unique() %>% as.data.frame()

#買過外匯的客戶
custom_do_exchange <- custom_exchange[custom_exchange$FX_TXN_AMT != 0,]

b <- left_join(behavior,pageNoTable,by='PAGE')[,-c(2)]





customerTable <- fromJSON('C:/Users/Student/Desktop/wtf.json')

# ===========================================
clusterExport(cl,'b')
customerTable$webHistory <- customerTable$CUST_NO %>% parLapply(cl,.,function(t){unique(b[b$CUST_NO == t , 'PAGE_NO'])})


# require(jsonlite)
# writeLines(toJSON(customerTable,auto_unbox = T , data.frame = 'rows'),useBytes = T,'C:/Users/Student/Desktop/wtf.json')

#=====================================================================

#==================================我成功拉！！！！！！！！！！！！！！！！！！！！===================================
exchangeTable <- customerTable[customerTable$CUST_NO %in% custom_do_exchange$CUST_NO,]
writeLines(toJSON(exchangeTable,auto_unbox = T , data.frame = 'rows'),useBytes = T,'C:/Users/Student/Desktop/買外匯的人瀏覽網頁記錄.json')
#==================================我成功拉！！！！！！！！！！！！！！！！！！！！===================================
#所有買過外匯的人瀏覽過的網站次數
exchange_log_list <- table(unlist(exchangeTable$webHistory, use.names=F)) %>% as.data.frame()
exchange_log_list <- exchange_log_list[order(exchange_log_list$Freq,decreasing = T),]
exchange_log_list$PAGE_NO <- exchange_log_list$PAGE_NO %>% as.integer()
colnames(exchange_log_list)=c('PAGE_NO','Freq')
exchange_log_list_www <- left_join(exchange_log_list,pageNoTable,"PAGE_NO")

#exchange_log_list_www$PAGE %>% strsplit(.,split = '/') %>% .[1]

writeLines(toJSON(exchange_log_list_www,auto_unbox = T , data.frame = 'rows'),useBytes = T,'C:/Users/Student/Desktop/買外匯的人瀏覽網頁記錄統計.json')
