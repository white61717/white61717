library(dplyr)
library(parallel)
library(ggplot2)
library(catboost)

# save.image('E:/Desktop/分parking_way訓練前的資料.RData')
load('E:/Desktop/分parking_way訓練前的資料.RData')
final <- data.frame()
finalTrain <- data.frame()

  
train_2 <- train[train$parking_way == 2,]
test_2 <- test[test$parking_way == 2,]

train_2 <- rbind(train,train_2) %>% as.data.frame()

train_2$parking_price <- NULL
test_2$parking_price <- NULL




train_x <- train_2[,-c(1,which(colnames(train_2) == 'total_price'))]
train_y <- train_2[,'total_price']
test_x <- test_2[,-1]


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

final.temp <- cbind(test_2$building_id %>% as.character(),prediction) %>% as.data.frame()
final <- rbind(final,final.temp) %>% as.data.frame()
gc()

#==========================



train_not2 <- train[train$parking_way != 2,]
test_not2 <- test[test$parking_way != 2,]

train_not2 <- rbind(train,train_not2) %>% as.data.frame()

train_not2$parking_price <- log1p(train_not2$parking_price)
test_not2$parking_price <- log1p(test_not2$parking_price)


train_x <- train_not2[,-c(1,which(colnames(train_not2) == 'total_price'))]
train_y <- train_not2[,'total_price']
test_x <- test_not2[,-1]


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
prediction2 <- catboost.predict(model, 
                               test_pool, 
                               prediction_type = 'RawFormulaVal')

prediction2 <- 10^prediction2

final.temp2 <- cbind(test_not2$building_id %>% as.character(),prediction2) %>% as.data.frame()
colnames(final.temp2) = c('V1','prediction2')
final <- rbind(final,final.temp2) %>% as.data.frame()
gc()

colnames(final) <- c('building_id','total_price') 
write.csv(final,'E:/Desktop/只有parking_price = 2(50000).csv',fileEncoding = 'UTF-8',row.names = F)
