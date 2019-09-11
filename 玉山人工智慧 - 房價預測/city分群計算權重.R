library(dplyr)
library(parallel)
library(ggplot2)
library(catboost)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
load('E:/Desktop/log(all-parkingPrice).RData')

final <- data.frame()

# for(i in c(3, 5, 6, 7, 9, 10, 12, 13, 14, 17, 21)){

  i = 21
  trainOneCity <- train[train$city == i,]

  set.seed(108)
  select <- sample(1:nrow(trainOneCity), nrow(trainOneCity)*0.8)
  train.temp <- trainOneCity[select,]
  test.temp <- trainOneCity[-select,]
  parkingPrice <- test.temp$parking_price
  
  
  train.temp <- train %>% as.data.frame()

    
  train.temp$city <- NULL
  test.temp$city <- NULL
  train.temp$parking_price <- NULL
  test.temp$parking_price <- NULL
  
  train_x <- train.temp[,-c(1,which(colnames(train.temp) == 'total_price'))]
  train_y <- train.temp[,'total_price']
  test_x <- test.temp[,-1]
  
  
  
  train_pool <- catboost.load_pool(data = train_x, label = train_y)
  test_pool <- catboost.load_pool(test_x)
  
  
  fit_params <- list(iterations = 15000,
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
  
  prediction <- 10^prediction + parkingPrice
  
  final <- cbind(test.temp$building_id %>% as.character(),prediction) %>% as.data.frame()
  rm(model,temp,test,test_backup,test_x,test.temp,train_x,factorColumnsName)
  gc()
  
  
# }

colnames(final) <- c('building_id','total_price') 

#  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=  train_score  =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

score <- function(x){
  train_score <- x$total_price %>% as.character() %>% as.numeric()
  y <- trainOneCity[-select,'total_price']
  hit_rate <- abs( (train_score - (10^y + parkingPrice))/(10^y + parkingPrice) ) <= 0.1
  score <- ( hit_rate[hit_rate == T] %>% length ) / length(y)
  MAPE <- sum(abs( (train_score - (10^y + parkingPrice))/(10^y + parkingPrice) )) / length(y)
  finalScore <- score * length(y) + 1 - MAPE
  return(finalScore)
}

a1 <- score(final)
a1


