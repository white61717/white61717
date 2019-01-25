require(dplyr)
require(parallel)
require(jiebaR)
require(jsonlite)
behavior <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CUST_BEHAVIOR.csv')
exchange <- read.csv('C:/Users/Student/Desktop/dataset/TBN_FX_TXN.csv')
custom <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CIF.csv')
customerTable <- fromJSON('C:/Users/Student/Desktop/wtf.json')
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
#============================================================================================

# 算外匯交易次數和外匯交易總金額
exchange_count <- aggregate(TXN_DT ~ CUST_NO, exchange , length)
exchange_total <- aggregate(FX_TXN_AMT ~ CUST_NO, exchange , sum)
# join
custom_exchange <- left_join(custom,exchange_count)
custom_exchange <- left_join(custom_exchange,exchange_total)
# 沒有值代表沒有投資 補0
custom_exchange$TXN_DT[is.na(custom_exchange$TXN_DT)] <- 0

custom_exchange$FX_TXN_AMT[is.na(custom_exchange$FX_TXN_AMT)] <- 0
#買過外匯的客戶
custom_do_exchange <- custom_exchange[custom_exchange$FX_TXN_AMT != 0,]

exchangeTable <- customerTable[customerTable$CUST_NO %in% custom_do_exchange$CUST_NO,]

exchange_log_list <- table(unlist(exchangeTable$webHistory, use.names=F)) %>% as.data.frame()
exchange_log_list <- exchange_log_list[order(exchange_log_list$Freq,decreasing = T),]
colnames(exchange_log_list)=c('PAGE_NO','Freq')
exchange_log_list$PAGE_NO <- exchange_log_list$PAGE_NO %>% as.integer()
exchange_log_list_www <- left_join(exchange_log_list,pageNoTable,"PAGE_NO")


#信用卡＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝
custom <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CIF.csv')
card <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CC_APPLY.csv')
library(dplyr)

card_count <- aggregate(TXN_DT ~ CUST_NO, card , length)
# join
custom_card <- left_join(custom,card_count)
# 沒有值代表沒有投資 補0
custom_card$TXN_DT[is.na(custom_card$TXN_DT)] <- 0
#買過外匯的客戶
custom_do_card <- custom_card[custom_card$TXN_DT != 0,]

cardTable <- customerTable[customerTable$CUST_NO %in% custom_do_card$CUST_NO,]

card_log_list <- table(unlist(cardTable$webHistory, use.names=F)) %>% as.data.frame()
card_log_list <- card_log_list[order(card_log_list$Freq,decreasing = T),]
colnames(card_log_list)=c('PAGE_NO','Freq')
card_log_list$PAGE_NO <- card_log_list$PAGE_NO %>% as.integer()
card_log_list_www <- left_join(card_log_list,pageNoTable,"PAGE_NO")
writeLines(toJSON(card_log_list_www,auto_unbox = T , data.frame = 'rows'),useBytes = T,'C:/Users/Student/Desktop/買信用卡的人瀏覽網頁記錄統計.json')



#信貸＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝
credit <- read.csv('C:/Users/Student/Desktop/dataset/TBN_LN_APPLY.csv')


credit_count <- aggregate(TXN_DT ~ CUST_NO, credit , length)
# join
custom_credit <- left_join(custom,credit_count)
# 沒有值代表沒有投資 補0
custom_credit$TXN_DT[is.na(custom_credit$TXN_DT)] <- 0
#買過外匯的客戶
custom_do_credit <- custom_credit[custom_credit$TXN_DT != 0,]

creditTable <- customerTable[customerTable$CUST_NO %in% custom_do_credit$CUST_NO,]

credit_log_list <- table(unlist(creditTable$webHistory, use.names=F)) %>% as.data.frame()
credit_log_list <- credit_log_list[order(credit_log_list$Freq,decreasing = T),]
colnames(credit_log_list)=c('PAGE_NO','Freq')
credit_log_list$PAGE_NO <- credit_log_list$PAGE_NO %>% as.integer()
credit_log_list_www <- left_join(credit_log_list,pageNoTable,"PAGE_NO")
writeLines(toJSON(credit_log_list_www,auto_unbox = T , data.frame = 'rows'),useBytes = T,'C:/Users/Student/Desktop/買信貸的人瀏覽網頁記錄統計.json')



#信託＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝
trust <- read.csv('C:/Users/Student/Desktop/dataset/TBN_WM_TXN.csv')
trust_count <- aggregate(TXN_DT~ CUST_NO, trust , length)

# join
custom_trust <- left_join(custom,trust_count)
# 沒有值代表沒有投資 補0
custom_trust$TXN_DT[is.na(custom_trust$TXN_DT)] <- 0
#買過外匯的客戶
custom_do_trust <- custom_trust[custom_trust$TXN_DT != 0,]

trustTable <- customerTable[customerTable$CUST_NO %in% custom_do_trust$CUST_NO,]

trust_log_list <- table(unlist(trustTable$webHistory, use.names=F)) %>% as.data.frame()
trust_log_list <- trust_log_list[order(trust_log_list$Freq,decreasing = T),]
colnames(trust_log_list)=c('PAGE_NO','Freq')
trust_log_list$PAGE_NO <- trust_log_list$PAGE_NO %>% as.integer()
trust_log_list_www <- left_join(trust_log_list,pageNoTable,"PAGE_NO")
writeLines(toJSON(trust_log_list_www,auto_unbox = T , data.frame = 'rows'),useBytes = T,'C:/Users/Student/Desktop/買信託的人瀏覽網頁記錄統計.json')


