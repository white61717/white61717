library(dplyr)
library(parallel)
library(ggplot2)
library(catboost)

setwd('E:/Desktop/台灣不動產預測/')

train <- read.csv('train.csv')
test <- read.csv('test.csv')
test_backup <- test
train$parking_price
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
                     'XIV_index_50','XIV_index_500','XIV_index_1000','XIV_index_5000','XIV_index_10000','N_50',
                     'N_500','N_1000','N_5000','N_10000')

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

train$buildingAge <- train$buildingAge / 365
test$buildingAge <- test$buildingAge / 365
train$txn_dt <- train$txn_dt / 365
test$txn_dt <- test$txn_dt / 365
train$building_complete_dt <- train$building_complete_dt / 365
test$building_complete_dt <- test$building_complete_dt / 365

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  衍生欄位2  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 


train$lat <-NULL
test$lat <-NULL
train$lon <-NULL
test$lon <-NULL

train$parking_area <-NULL
test$parking_area <-NULL

train$highEducation <- train$doc_rate + train$master_rate + train$bachelor_rate + train$jobschool_rate
test$highEducation <- test$doc_rate + test$master_rate + test$bachelor_rate + test$jobschool_rate

train$lowEducation <- train$junior_rate + train$elementary_rate
test$lowEducation <- test$junior_rate + test$elementary_rate

train$doc_rate <-NULL
test$doc_rate <-NULL
train$master_rate <-NULL
test$master_rate <-NULL
train$bachelor_rate <-NULL
test$bachelor_rate <-NULL
train$junior_rate <-NULL
test$junior_rate <-NULL
train$elementary_rate <-NULL
test$elementary_rate <-NULL
train$jobschool_rate <-NULL
test$jobschool_rate <-NULL


# #train 的 N_Total
# temp <- train[,which(colnames(train) %in% c('I_MIN','II_MIN','III_MIN','IV_MIN','V_MIN','VI_MIN','VII_MIN','VIII_MIN','IX_MIN',
#                                             'X_MIN','XI_MIN','XII_MIN','XIII_MIN','XIV_MIN'))]
# 
# temp <- lapply(temp , function(x){
#           x = (x - min(x)) / (max(x) - min(x))
#           return(x)
#         })  %>% as.data.frame()
# 
# train$N_Total <- temp$I_MIN + temp$II_MIN + temp$III_MIN + temp$IV_MIN + temp$V_MIN +
#                  temp$VI_MIN + temp$VII_MIN + temp$VIII_MIN + temp$IX_MIN + temp$X_MIN +
#                  temp$XI_MIN + temp$XII_MIN + temp$XIII_MIN + temp$XIV_MIN
# 
# train <- train[,-which(colnames(train) %in% c('I_MIN','II_MIN','III_MIN','IV_MIN','V_MIN','VI_MIN','VII_MIN','VIII_MIN','IX_MIN',
#                                             'X_MIN','XI_MIN','XII_MIN','XIII_MIN','XIV_MIN'))]
# 
# #test 的 N_Total
# temp <- test[,which(colnames(test) %in% c('I_MIN','II_MIN','III_MIN','IV_MIN','V_MIN','VI_MIN','VII_MIN','VIII_MIN','IX_MIN',
#                                           'X_MIN','XI_MIN','XII_MIN','XIII_MIN','XIV_MIN'))]
# 
# temp <- lapply(temp , function(x){
#           x = (x - min(x)) / (max(x) - min(x))
#           return(x)
#         })  %>% as.data.frame()
# 
# test$N_Total <- temp$I_MIN + temp$II_MIN + temp$III_MIN + temp$IV_MIN + temp$V_MIN +
#                  temp$VI_MIN + temp$VII_MIN + temp$VIII_MIN + temp$IX_MIN + temp$X_MIN +
#                  temp$XI_MIN + temp$XII_MIN + temp$XIII_MIN + temp$XIV_MIN
# 
# test <- test[,-which(colnames(test) %in% c('I_MIN','II_MIN','III_MIN','IV_MIN','V_MIN','VI_MIN','VII_MIN','VIII_MIN','IX_MIN',
#                                            'X_MIN','XI_MIN','XII_MIN','XIII_MIN','XIV_MIN'))]

#train 部分取log1p
temp <-  train[colnames(train) %in% c('XIV_MIN','XIV_10000','XIV_5000',
                                      'XIII_10000','XIII_5000','XII_10000','XII_5000',
                                      'XI_10000','XI_5000','X_10000','X_5000',
                                      'IX_10000','IX_5000','VIII_10000','VIII_5000',
                                      'VII_10000','VII_5000','VI_10000','VI_5000',
                                      'V_10000','V_5000','IV_10000','IV_5000',
                                      'III_10000','III_5000','II_10000','II_5000',
                                      'I_10000','I_5000')]

temp <- lapply(temp,log1p) %>% as.data.frame()
train[,colnames(train) %in% colnames(temp)] <- temp

#test 部分取log1p
temp <-  test[colnames(test) %in% c('XIV_MIN','XIV_10000','XIV_5000',
                                    'XIII_10000','XIII_5000','XII_10000','XII_5000',
                                    'XI_10000','XI_5000','X_10000','X_5000',
                                    'IX_10000','IX_5000','VIII_10000','VIII_5000',
                                    'VII_10000','VII_5000','VI_10000','VI_5000',
                                    'V_10000','V_5000','IV_10000','IV_5000',
                                    'III_10000','III_5000','II_10000','II_5000',
                                    'I_10000','I_5000')]

temp <- lapply(temp,log1p) %>% as.data.frame()
test[,colnames(test) %in% colnames(temp)] <- temp


# #刪所有index的欄位
# 
# train <- train[,-grep(colnames(train),pattern = 'index')]
# test <- test[,-grep(colnames(test),pattern = 'index')]


#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 補  txn_floor 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  

train$txn_floor[is.na(train$txn_floor)] <- 0
test$txn_floor[is.na(test$txn_floor)] <- 0

train$txn_floor <- train$txn_floor %>% as.factor()
test$txn_floor <- test$txn_floor %>% as.factor()
train$total_floor <- train$total_floor %>% as.factor()
test$total_floor <- test$total_floor %>% as.factor()

# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 train 的 floorPercentage 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  
# 
# train2 <- train[,-c(1,which(colnames(train) %in% c('total_price','txn_floor')))]
# train.New <- train2[!is.na(train2$floorPercentage),]
# test.New <- train2[is.na(train2$floorPercentage),]
# 
# train.New.x <- train.New[,-which(colnames(train.New) == 'floorPercentage')]
# train.New.y <- train.New[,'floorPercentage']
# 
# test.New.x <- test.New[,-which(colnames(test.New) == 'floorPercentage')]
# 
# train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
# test_pool <- catboost.load_pool(test.New.x)
# 
# fit_params <- list(iterations = 500,
#                    # loss_function = 'MultiClass',
#                    task_type = 'GPU',
#                    learning_rate = 0.01,
#                    l2_leaf_reg = 1,
#                    l1_leaf_reg = 0.1,
#                    depth = 6)
# 
# model <- catboost.train(train_pool, params = fit_params)
# 
# prediction <- catboost.predict(model, 
#                                test_pool, 
#                                prediction_type = 'RawFormulaVal'
# )
# 
# 
# prediction
# 
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 train 的 floorPercentage 和 txn_floor  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  
# 
# train$floorPercentage[is.na(train$floorPercentage)] <- prediction
# 
# train$txn_floor <- round(train$total_floor * train$floorPercentage)
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 test 的 floorPercentage 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  
# 
# test2 <- test[,-c(1,which(colnames(test) %in% c('total_price','txn_floor')))]
# train.New <- test2[!is.na(test2$floorPercentage),]
# test.New <- test2[is.na(test2$floorPercentage),]
# 
# train.New.x <- train.New[,-which(colnames(train.New) == 'floorPercentage')]
# train.New.y <- train.New[,'floorPercentage']
# 
# test.New.x <- test.New[,-which(colnames(test.New) == 'floorPercentage')]
# 
# train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
# test_pool <- catboost.load_pool(test.New.x)
# 
# prediction <- catboost.predict(model, 
#                                test_pool, 
#                                prediction_type = 'RawFormulaVal'
# )
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 test 的 floorPercentage 和 txn_floor  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  
# 
# test$floorPercentage[is.na(test$floorPercentage)] <- prediction
# 
# test$txn_floor <- round(test$total_floor * test$floorPercentage)



#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 train 的 village_income_median 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

train2 <- train[,-c(1,which(colnames(train) == 'total_price'))]


train.New <- train2[!is.na(train2$village_income_median),]
test.New <- train2[is.na(train2$village_income_median),]

train.New.x <- train.New[,-which(colnames(train.New) == 'village_income_median')]
train.New.y <- train.New[,'village_income_median']

test.New.x <- test.New[,-which(colnames(test.New) == 'village_income_median')]


train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
test_pool <- catboost.load_pool(test.New.x)

fit_params <- list(iterations = 1500,
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


#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 train 的 village_income_median  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

train$village_income_median[is.na(train$village_income_median)] <- prediction

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 test 的 village_income_median 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

test2 <- test[,-c(1,which(colnames(test) == 'total_price'))]
train.New <- test2[!is.na(test2$village_income_median),]
test.New <- test2[is.na(test2$village_income_median),]

train.New.x <- train.New[,-which(colnames(train.New) == 'village_income_median')]
train.New.y <- train.New[,'village_income_median']

test.New.x <- test.New[,-which(colnames(test.New) == 'village_income_median')]


train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
test_pool <- catboost.load_pool(test.New.x)


prediction <- catboost.predict(model,
                               test_pool,
                               prediction_type = 'RawFormulaVal'
)

rm(train2, test2, test.New, test.New.x, train.New, train.New.x, train.New.y, i, plus3Std)
gc()

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 test 的 village_income_median  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

test$village_income_median[is.na(test$village_income_median)] <- prediction

# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 train 的 parking_price 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# train$parking_price <- log1p(train$parking_price)
# test$parking_price <- log1p(test$parking_price)
# 
# train2 <- train[,-c(1,which(colnames(train) == 'total_price'))]
# 
# 
# train.New <- train2[!is.na(train2$parking_price),]
# test.New <- train2[is.na(train2$parking_price),]
# 
# train.New.x <- train.New[,-which(colnames(train.New) == 'parking_price')]
# train.New.y <- train.New[,'parking_price']
# 
# test.New.x <- test.New[,-which(colnames(test.New) == 'parking_price')]
# 
# # train.New.y <- log10(train.New.y)
# # train.New.y[train.New.y < 0] <- 0
# 
# train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
# test_pool <- catboost.load_pool(test.New.x)
# 
# fit_params <- list(iterations = 1000,
#                    task_type = 'GPU',
#                    learning_rate = 0.01,
#                    l2_leaf_reg = 1,
#                    l1_leaf_reg = 0.1,
#                    depth = 6)
# 
# model <- catboost.train(train_pool, params = fit_params)
# 
# prediction <- catboost.predict(model,
#                                test_pool,
#                                prediction_type = 'RawFormulaVal'
# )
# # prediction <- 10^prediction
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 train 的 parking_price  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# train$parking_price[is.na(train$parking_price)] <- prediction
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost 補 test 的 parking_price 空值  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# test2 <- test[,-c(1,which(colnames(test) == 'total_price'))]
# train.New <- test2[!is.na(test2$parking_price),]
# test.New <- test2[is.na(test2$parking_price),]
# 
# train.New.x <- train.New[,-which(colnames(train.New) == 'parking_price')]
# train.New.y <- train.New[,'parking_price']
# 
# test.New.x <- test.New[,-which(colnames(test.New) == 'parking_price')]
# 
# # train.New.y <- log10(train.New.y)
# # train.New.y[train.New.y < 0] <- 0
# 
# train_pool <- catboost.load_pool(data = train.New.x, label = train.New.y)
# test_pool <- catboost.load_pool(test.New.x)
# 
# 
# prediction <- catboost.predict(model,
#                                test_pool,
#                                prediction_type = 'RawFormulaVal'
# )
# 
# # prediction <- 10^prediction
# 
# rm(train2, test2, test.New, test.New.x, train.New, train.New.x, train.New.y, i, plus3Std)
# gc()
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補回去 test 的 parking_price  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# test$parking_price[is.na(test$parking_price)] <- prediction
# 
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  補 parking_price  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# # train$parking_price <- log(train$parking_price)
# # test$parking_price <- log(test$parking_price)
# 
# train$parking_price[is.na(train$parking_price)] <- 0
# test$parking_price[is.na(test$parking_price)] <- 0
# 
# train$total_price <- train$total_price - train$parking_price



#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  total_price 取log10  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

#total_price 部分取log10
train$total_price <- log10(train$total_price)

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  Factor型態數據品質表  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  
factorQuality <- function(data){

  factorDataQuality <- lapply(data %>% select_if(is.factor),function(x){
      factorData <- data.frame(matrix(,nrow=6, ncol=0))
      uniqueCount <- x %>% unique() %>% length()
      NAcount <- sum( is.na(x) ) 
      NApercentage <- ( sum( is.na(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
      mostAppear <- names( which.max( table(x) ) )
      mostAppearNum <- max( table(x) )
      unbalence <- ( max( table(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
      columnsQuality <- rbind(uniqueCount, NAcount, NApercentage, mostAppear, mostAppearNum, unbalence)
      factorData <- cbind(factorData, columnsQuality)
      return(factorData)
  }) %>% bind_cols() 
  
  row.names(factorDataQuality) = c('不同值個數','空值個數','空值比例','次數最多的值','最多出現次數','不平衡度')
  columnsName <- data %>% select_if(is.factor) %>% names
  colnames(factorDataQuality) <- columnsName
  return(factorDataQuality)
}

factortb <- factorQuality(test)


#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  數字型態數據品質表  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  
numsQuality <- function(data){
  
  cl <- makeCluster(detectCores() - 2)
  clusterEvalQ(cl,{
    require(dplyr)
    require(parallel)
    })
  clusterExport(cl,'data')
  
  numsDataQuality <- parLapply(cl,data %>% select_if(function(x){is.numeric(x) || is.integer(x)}),function(x){
    numsData <- data.frame(matrix(,nrow=6, ncol=0))
    uniqueCount <- x %>% unique() %>% length()
    NAcount <- sum( is.na(x) ) 
    NApercentage <- ( sum( is.na(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
    mostAppear <- names( which.max( table(x) ) )
    mostAppearNum <- max( table(x) )
    unbalence <- ( max( table(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
    minNum <- min(x, na.rm = T) %>% round()
    maxNum <- max(x, na.rm = T) %>% round()
    meanNum <- mean(x, na.rm = T) %>% round()
    stdev <- sd(x, na.rm = T) %>% round()
    minus3Std <- ( meanNum - 3 * stdev ) %>% round()
    plus3Std <- ( meanNum + 3 * stdev ) %>% round()

    columnsQuality <- rbind(uniqueCount, NAcount, NApercentage, mostAppear, mostAppearNum, unbalence,
                            minNum,maxNum,meanNum,stdev,minus3Std,plus3Std)
    numsData <- cbind(numsData, columnsQuality)
    return(numsData)
  }) %>% bind_cols() 
  
  row.names(numsDataQuality) = c('不同值個數','空值個數','空值比例','次數最多的值','最多出現次數','不平衡度',
                                 '最小值','最大值','平均值','標準差','M-3','M+3')
  columnsName <- data %>% select_if(function(x){is.numeric(x) || is.integer(x)}) %>% names
  colnames(numsDataQuality) <- columnsName
  stopCluster(cl)
  return(numsDataQuality)
}

numstb <- numsQuality(test)

# write.csv(factortb %>% t %>% as.data.frame(),'E:/Desktop/testFactorQuality.csv')
# write.csv(numstb %>% t %>% as.data.frame(),'E:/Desktop/testnumQuality.csv')


#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  批次存ggplot  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

Names <- train %>% select_if(function(x){is.numeric(x) || is.integer(x)}) %>% names
Names <- train %>% select_if(function(x){is.factor(x)}) %>% names


  

lapply(Names,function(t){
  filename <- paste0('E:/Desktop/pic2/', t, '.png')
  ggsave(filename , plot = ggplot(data=train, aes(x = train[,t] , y = total_price , color = city)) + geom_point() + xlab(t))
})

lapply(Names,function(t){
  filename <- paste0('E:/Desktop/pic3/', t, '.png')
  ggsave(filename , plot = ggplot(data=train, aes(x = train[,t] , y = total_price , color = building_type)) + geom_point() + xlab(t))
})

lapply(Names,function(t){
  filename <- paste0('E:/Desktop/pic4/', t, '.png')
  ggsave(filename , plot = ggplot(data=train, aes(x = train[,t] , y = total_price , color = building_use)) + geom_point() + xlab(t))
})
  
lapply(Names,function(t){
  filename <- paste0('E:/Desktop/pic5/', t, '.png')
  ggsave(filename , plot = ggplot(data=train, aes(x = train[,t] , y = total_price , color = parking_way)) + geom_point() + xlab(t))
})

train2 <- train[train$parking_price != 0,]
lapply(Names,function(t){
  filename <- paste0('E:/Desktop/pic6/', t, '.png')
  ggsave(filename , plot = ggplot(data=train2, aes(x = train2[,t] , y = total_price , color = parking_way)) + geom_point() + xlab(t))
})

train
ggplot(data=train_not2, aes(x=city, y=total_price, color = parking_way)) + geom_point()


#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
save.image('E:/Desktop/天數除365 catboost補log(parking_price) 目標log(total_price).RData')
load('E:/Desktop/天數除365 catboost補log(parking_price) 目標log(total_price).RData')

final <- data.frame()
finalTrain <- data.frame()
for(i in c(3, 5, 6, 7, 9, 10, 12, 13, 14, 17, 21)){
  print(i)

  train.temp <- train[train$city == i,]
  test.temp <- test[test$city == i,]

  train.temp <- rbind(train,train.temp,train.temp) %>% as.data.frame()


  train.temp$city <- NULL
  test.temp$city <- NULL
  # train.temp$parking_price <- NULL
  # test.temp$parking_price <- NULL
  
  train_x <- train.temp[,-c(1,which(colnames(train.temp) == 'total_price'))]
  train_y <- train.temp[,'total_price']
  test_x <- test.temp[,-1]
  
  
  
  train_pool <- catboost.load_pool(data = train_x, label = train_y)
  test_pool <- catboost.load_pool(test_x)
  
  
  fit_params <- list(iterations = 50000,
                     # loss_function = 'MultiClass',
                     task_type = 'GPU',
                     learning_rate = 0.05,
                     l2_leaf_reg = 1,
                     l1_leaf_reg = 0.1,
                     depth = 6)
  
  
  model <- catboost.train(train_pool, params = fit_params)
  
  prediction <- catboost.predict(model, 
                                 test_pool, 
                                 prediction_type = 'RawFormulaVal')
  
  prediction <- 10^prediction
  
  final.temp <- cbind(test.temp$building_id %>% as.character(),prediction) %>% as.data.frame()
  final <- rbind(final,final.temp) %>% as.data.frame()
  gc()
  
  
  #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  train_score  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  train_x_score <- train[train$city == i,-c(1,which(colnames(train) == 'total_price'))]
  train_x_score$city <- NULL
  train_pool_score <- catboost.load_pool(train_x_score)
  train_score <- catboost.predict(model,
                                  train_pool_score,
                                  prediction_type = 'RawFormulaVal')
  
  train_score <- 10^train_score
  final.score <- cbind(train[train$city == i,'building_id'] %>% as.character(),train_score) %>% as.data.frame(.,stringsAsFactors = F)
  finalTrain <- rbind(finalTrain,final.score) %>% as.data.frame()
  gc()
}

colnames(final) <- c('building_id','total_price') 
finalbackup <- final

# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  train_score  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# colnames(finalTrain) <- c('building_id','train_score')
# building_id <- train$building_id %>% as.character() %>% as.data.frame()
# colnames(building_id) <- 'building_id'
# trainScore <- left_join(building_id, finalTrain)
# 
# #可能有錯
# # score <- function(x){
# #   train_score <- x$train_score %>% as.numeric()
# #   y <- train[,'total_price']
# #   hit_rate <- abs( (train_score - 10^y)/10^y ) <= 0.1
# #   score <- ( hit_rate[hit_rate == T] %>% length ) / 10000
# #   MAPE <- sum(abs( (train_score - 10^y)/10^y )) / 10000
# #   finalScore <- score * 10^4 + 1 - MAPE
# #   return(finalScore)
# # }
# # 
# # score(trainScore)


#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  feature_importance  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

importance <- catboost.get_feature_importance(model) %>% as.data.frame() %>% .[,1]
importanceName <- colnames(train_x)
feature_importance <- cbind(importanceName,importance) %>% as.data.frame()
colnames(feature_importance) <- c('Name','Importance')
feature_importance$Importance <- feature_importance$Importance %>% as.character() %>% as.numeric() 
feature_importance <- feature_importance %>% arrange(desc(round(Importance,digits = 5)))
write.csv(feature_importance,'E:/Desktop/feature_importance31.csv',fileEncoding = 'UTF-8',row.names = F)

write.csv(final,'E:/Desktop/catboost-submission31.csv',fileEncoding = 'UTF-8',row.names = F)


# # #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  catboost  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# # save.image('E:/Desktop/14的資料.RData')
# # 
# train_x <- train[,-c(1,which(colnames(train) == 'total_price'))]
# train_y <- train[,'total_price']
# test_x <- test[,-1]
# 
# 
# train_x$parking_price <- NULL
# test_x$parking_price <- NULL
# 
# # validation_set.x <- train[ sample(1:nrow(train),nrow(train)*0.2),]
# 
# train_pool <- catboost.load_pool(data = train_x, label = train_y)
# test_pool <- catboost.load_pool(test_x)
# # validation_pool <- catboost.load_pool(validation_set.x)
# #75000-40000
# fit_params <- list(iterations = 15000,
#                    # loss_function = 'MultiClass',
#                    task_type = 'GPU',
#                    learning_rate = 0.05,
#                    l2_leaf_reg = 1,
#                    l1_leaf_reg = 0.1,
#                    depth = 6)
# 
# 
# model <- catboost.train(train_pool, params = fit_params)
# 
# prediction <- catboost.predict(model,
#                                test_pool,
#                                prediction_type = 'RawFormulaVal')
#   
# prediction <- 10^prediction + test$parking_price
# 
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  train_score  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# 
# train_score <- catboost.predict(model,
#                                train_pool,
#                                prediction_type = 'RawFormulaVal')
# 
# train_score <- 10^train_score
# 
# score <- function(train_score){
#   hit_rate <- abs( (train_score - 10^train_y)/10^train_y ) <= 0.1
#   score <- ( hit_rate[hit_rate == T] %>% length ) / 10000
#   MAPE <- sum(abs( (train_score - 10^train_y)/10^train_y )) / 10000
#   finalScore <- score * 10^4 + 1 - MAPE
#   return(finalScore)
# }
# 
# score(train_score)
# 
# #  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  feature_importance  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#   importance <- catboost.get_feature_importance(model) %>% as.data.frame() %>% .[,1]
#   importanceName <- colnames(train_x)
#   feature_importance <- cbind(importanceName,importance) %>% as.data.frame()
#   colnames(feature_importance) <- c('Name','Importance')
#   feature_importance$Importance <- feature_importance$Importance %>% as.character() %>% as.numeric()
#   feature_importance <- feature_importance %>% arrange(desc(round(Importance,digits = 5)))
#   write.csv(feature_importance,'E:/Desktop/feature_importance31.csv',fileEncoding = 'UTF-8',row.names = F)
# 
#   final <- cbind(test$building_id %>% as.character(),prediction) %>% as.data.frame()
#   colnames(final) <- c('building_id','total_price')
# 
#   write.csv(final,'E:/Desktop/catboost-submission31.csv',fileEncoding = 'UTF-8',row.names = F)
# 
