library(dplyr)
library(data.table)
library(parallel)
library(xgboost)
library(nnet)
library(zoo)
library(gpuR)
library(catboost)


setwd("E:/Desktop/R code/期末專題 - 厄瓜多市場預測")
train <- fread("train_weekday.csv")[,-c(1)]
test <- fread("test.csv")[,-c(1)]
item <- fread("items.csv")[,c(1,2)]
holidays <- fread("holidays.csv")[,c(2,8)]
oil <- fread("oil.csv")

#train <- train[80000000:103839389,]

# Test內加上星期幾
cl <- makeCluster(detectCores() - 1 )
parL_date <- test$date
clusterExport(cl,"parL_date")
test$weekday <- parLapply(cl,parL_date,function(x){
  a <- weekdays(as.Date(x))
  a <- sub("星期一",0,a)
  a <- sub("星期二",1,a)
  a <- sub("星期三",2,a)
  a <- sub("星期四",3,a)
  a <- sub("星期五",4,a)
  a <- sub("星期六",5,a)
  a <- sub("星期日",6,a)
  return(a)
}) %>% unlist()
test$weekday <- test$weekday %>% as.integer()
#不要的欄位先刪掉省資源
rm(cl,parL_date)
gc()




#item 分類
item$family <- item$family %>% as.factor()
# 把test join到train,test內
train <- left_join(train,item)
test <- left_join(test,item)



#轉類別,數值
train$onpromotion <- train$onpromotion %>% as.integer()
test$onpromotion <- test$onpromotion %>% as.integer()
train$onpromotion <- train$onpromotion %>% as.factor()
test$onpromotion <- test$onpromotion %>% as.factor()


#轉store_nbr 首都or非首都
train$store_nbr <- train$store_nbr %>% as.factor()
test$store_nbr <- test$store_nbr %>% as.factor()



#增加是否會腐壞
item <- fread("items.csv")[,c(1,4)]
train <- left_join(train,item)
test <- left_join(test,item)
gc()




train.x <- train[,-c(3,4)]
train.x <- train.x %>% mutate_if(is.integer,as.factor)
train.x <- train.x %>% mutate_if(is.numeric,as.factor)
train.y <- train[,c(1,4)]


train_set.x <- train.x[train.x$date < "2017-07-16",-1 ]
train_set.y <- train.y[train.y$date < '2017-07-16', -1]
validation_set.x <- train.x[train.x$date >= '2017-07-16' & train.x$date < '2017-08-01',-1]
validation_set.y <- train.y[train.y$date >= '2017-07-16' & train.y$date < '2017-08-01', -1]



rm(holidays,item,oil,train,train.x,train.y)
gc()



train_pool <- catboost.load_pool(data = train_set.x, label = train_set.y)
validation_pool <- catboost.load_pool(validation_set.x)

load("E:/Desktop/R code/期末專題 - 厄瓜多市場預測/model data.RData")
train_set.x <- train_set.x[50000,]
train_set.y <- train_set.y[]

#設參數

#for(i in c(0.01,0.02,0.03,0.04,0.05)){
#  for(j in 1:3){
#    for(k in 4:7){
      
      fit_params <- list(iterations = 130,
                         loss_function = 'Logloss',
                         task_type = 'GPU',
                         learning_rate = 0.01)
      
      
      #建模,預測
      model <- catboost.train(train_pool, params = fit_params)
      prediction <- catboost.predict(model, 
                                     validation_pool, 
                                     prediction_type = 'RawFormulaVal')
      
      #計算誤差
      prediction <- prediction %>% round(.)
      prediction %>% unique
      sst2 = sum((validation_set.y - mean(validation_set.y))^2)
      ssr2 = sum((prediction- mean(validation_set.y))^2)
      r22 = ssr2/sst2
      #all <- c(all,r22)
      r22
      gc()
#    }
#  }
#}




#計算R平方 評估訓練模型準確度
sst = sum((train_set.y - mean(train_set.y))^2)
ssr = sum((xgb.Prediction - mean(train_set.y))^2)
r2 = ssr/sst
r2




#開始預測啦!!!!!!!!!!!!!!
test <- test[,-c(1,3)]

test <- test %>% mutate_if(is.integer,as.factor)
test <- test %>% mutate_if(is.numeric,as.factor)
test_set.x <- test
gc()




test_pool <- catboost.load_pool(test_set.x)
prediction <- catboost.predict(model, 
                               test_pool, 
                               prediction_type = 'RawFormulaVal')


prediction<- prediction %>% round(.)
prediction %>% unique()




prediction[prediction <0] <- 0
final_1 <- cbind(fread("test.csv")[,c(1)],unit_sales = prediction)
final_1$unit_sales %>% unique()
write.csv(final_1,fileEncoding = "UTF-8",row.names = F,"E:/Desktop/catboost-2.csv")
