library(dplyr)
library(parallel)
library(ggplot2)
library(catboost)

setwd('E:/Desktop/台灣不動產預測/')

train <- read.csv('train.csv')
test <- read.csv('test.csv')

factorColumnsName <- c('building_material','city','town','village','building_type','building_use',
                       'parking_way','I_index_50','I_index_500','I_index_1000','I_index_5000','I_index_10000',
                       'II_index_50','II_index_500','II_index_1000','II_index_5000','II_index_10000',
                       'III_index_50','III_index_500','III_index_1000','III_index_5000','III_index_10000',
                       'IV_index_50','IV_index_500','IV_index_1000','IV_index_5000','IV_index_10000',
                       'V_index_50','V_index_500','V_index_1000','V_index_5000','V_index_10000',
                       'VI_index_50','VI_index_500','VI_index_1000','VI_index_5000','VI_index_10000',
                       'VII_index_50','VII_index_500','VII_index_1000','VII_index_5000','VII_index_10000',
                       'VIII_index_50','VIII_index_500','VIII_index_1000','VIII_index_5000','VIII_index_10000',
                       'IX_index_50','IX_index_500','IX_index_1000','IX_index_5000','IX_index_10000',
                       'X_index_50','X_index_500','X_index_1000','X_index_5000','X_index_10000',
                       'XI_index_50','XI_index_500','XI_index_1000','XI_index_5000','XI_index_10000',
                       'XII_index_50','XII_index_500','XII_index_1000','XII_index_5000','XII_index_10000',
                       'XIII_index_50','XIII_index_500','XIII_index_1000','XIII_index_5000','XIII_index_10000',
                       'XIV_index_50','XIV_index_500','XIV_index_1000','XIV_index_5000','XIV_index_10000')

for(i in factorColumnsName){
  train[,i] <- train[,i] %>% as.factor()
}

for(i in factorColumnsName){
  test[,i] <- test[,i] %>% as.factor()
}
#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  壓縮離群值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  

plus3Std <- ( mean(train$total_price) + 3 * sd(train$total_price) ) %>% round()
train$total_price[train$total_price > plus3Std] <- plus3Std

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  衍生欄位  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  


#交易樓層 / 總樓層
train$floorPercentage <- train$txn_floor / train$total_floor
test$floorPercentage <- test$txn_floor / test$total_floor

#交易日期 - 建物完成日期
train$buildingAge <- train$txn_dt - train$building_complete_dt
test$buildingAge <- test$txn_dt - test$building_complete_dt

train$city <-NULL
test$city <-NULL
train$lat <-NULL
test$lat <-NULL
train$lon <-NULL
test$lon <-NULL


train$parking_area <-NULL
test$parking_area <-NULL

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 train 的 floorPercentage 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  

train2 <- train[,-c(1,which(colnames(train) %in% c('total_price','txn_floor')))]
train.New <- train2[!is.na(train2$floorPercentage),]
test.New <- train2[is.na(train2$floorPercentage),]

train.New.x <- train.New[,-which(colnames(train.New) == 'floorPercentage')]
train.New.y <- train.New[,'floorPercentage']

test.New.x <- test.New[,-which(colnames(test.New) == 'floorPercentage')]

train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
test_pool <- catboost.load_pool(test.New.x)

fit_params <- list(iterations = 500,
                   # loss_function = 'MultiClass',
                   task_type = 'GPU',
                   learning_rate = 0.01,
                   l2_leaf_reg = 1,
                   l1_leaf_reg = 0.1,
                   depth = 6)

model <- catboost.train(train_pool, params = fit_params)

prediction <- catboost.predict(model, 
                               test_pool, 
                               prediction_type = 'RawFormulaVal'
                               )


prediction


#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 train 的 floorPercentage 和 txn_floor  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  

train$floorPercentage[is.na(train$floorPercentage)] <- prediction

train$txn_floor <- round(train$total_floor * train$floorPercentage)

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 test 的 floorPercentage 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  

test2 <- test[,-c(1,which(colnames(test) %in% c('total_price','txn_floor')))]
train.New <- test2[!is.na(test2$floorPercentage),]
test.New <- test2[is.na(test2$floorPercentage),]

train.New.x <- train.New[,-which(colnames(train.New) == 'floorPercentage')]
train.New.y <- train.New[,'floorPercentage']

test.New.x <- test.New[,-which(colnames(test.New) == 'floorPercentage')]

train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
test_pool <- catboost.load_pool(test.New.x)

prediction <- catboost.predict(model, 
                               test_pool, 
                               prediction_type = 'RawFormulaVal'
)

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 test 的 floorPercentage 和 txn_floor  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  

test$floorPercentage[is.na(test$floorPercentage)] <- prediction

test$txn_floor <- round(test$total_floor * test$floorPercentage)
