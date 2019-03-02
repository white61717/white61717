require(dplyr)
require(data.table)
library(tidyr)

train <- fread('C:\\Users\\Student\\Desktop\\厄瓜多data\\train.csv', encoding="UTF-8")
test <- fread('C:\\Users\\Student\\Desktop\\厄瓜多data\\test.csv', encoding="UTF-8")
train <- train[train$date > "2017-02-01",]

train_backup <- train


# unit_sales取log攤平
train$unit_sales[train$unit_sales < 0] <- 0
train$unit_sales[train$unit_sales >= 0] <- log1p(train$unit_sales[train$unit_sales >= 0])

df <- train[,-c(1,6)]

data_sale <- df %>% 
                unite(header,'date') %>% 
                count(store_nbr, item_nbr,unit_sales, header) %>% 
                spread(header,unit_sales, fill = 0)

data_sale$n <- NULL
# data_sale$item_nbr[data_sale$store_nbr == 5] %>% unique() %>% length()

#onpromotion攤平

df2 <-  train[,-c(1,5)]

data_onpromotion <- df2 %>% 
                        unite(header,'date') %>% 
                        count(store_nbr, item_nbr,onpromotion, header) %>% 
                        spread(header,onpromotion, fill = 0)

data_onpromotion$n <- NULL


#gc
rm(df,df2)
gc()

#sale移動平均
meanSale = data.frame(1:nrow(data_sale))
meanSaleAll = data.frame(meanSale3=0,meanSale7=0,meanSale14=0,
                         meanSale30=0,meanSale60=0,meanSale140=0)
for( j in 5:50){
  for (i in c(3,7,14,30,60,140)){
  a <- data_sale[tail(seq_along(data_sale)-j,i)]
  rowMean <- rowMeans(a)
  meanSale <- cbind(meanSale,data.frame(rowMean))
  }
meanSale$X1.nrow.data_sale. <- NULL
colnames(meanSale) <- c("meanSale3","meanSale7","meanSale14","meanSale30","meanSale60","meanSale140")
meanSaleAll <- rbind(meanSaleAll,meanSale)
meanSale <- data.frame(1:nrow(data_sale))
}
meanSaleAll <- meanSaleAll[-1,]


gc()


#onpromotion移動平均
meanOnpro = data.frame(1:nrow(data_onpromotion))
meanOnproAll = data.frame(meanOnpro3=0,meanOnpro7=0,meanOnpro14=0,
                         meanOnpro30=0,meanOnpro60=0,meanOnpro140=0)
for( j in 5:50){
  for (i in c(3,7,14,30,60,140)){
    a <- data_onpromotion[tail(seq_along(data_onpromotion)-j,i)]
    rowMean <- rowSums(a)
    meanOnpro <- cbind(meanOnpro,data.frame(rowMean))
  }
  meanOnpro$X1.nrow.data_onpromotion. <- NULL
  colnames(meanOnpro) <- c("meanOnpro3","meanOnpro7","meanOnpro14","meanOnpro30","meanOnpro60","meanOnpro140")
  meanOnproAll <- rbind(meanOnproAll,meanOnpro)
  meanOnpro <- data.frame(1:nrow(data_onpromotion))
}
meanOnproAll <- meanOnproAll[-1,]

gc()

#cbind sale跟onpromotion
All <-  cbind(meanSaleAll,meanOnproAll)
