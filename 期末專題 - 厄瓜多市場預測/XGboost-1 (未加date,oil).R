library(dplyr)
library(data.table)
library(parallel)
library(xgboost)
library(nnet)

setwd("E:/Desktop")
train <- fread("train_weekday.csv")[,-c(1,2)]
test <- fread("test.csv")[,-c(1)]
item <- fread("items.csv")[,c(1,2)]

test <- test [1:100000,]
train <- train[90000000:100000000,]

#item 分類
item$family[item$family %in% c("BOOKS","BABY CARE","MAGAZINES","PET SUPPLIES","SCHOOL AND OFFICE SUPPLIES",
                 "LAWN AND GARDEN","CELEBRATION","GROCERY II","HARDWARE","HOME APPLIANCES","PLAYERS AND ELECTRONICS",
                 "AUTOMOTIVE","LINGERIE","LADIESWEAR","BEAUTY","HOME AND KITCHEN II","HOME AND KITCHEN I")] <- 1
item$family[item$family %in% c("PERSONAL CARE","HOME CARE")] <- 2
item$family[item$family %in% c("FROZEN FOODS","DELI","EGGS","PREPARED FOODS","SEAFOOD",
                               "MEATS","POULTRY","LIQUOR,WINE,BEER","BREAD/BAKERY")] <- 3
item$family[item$family == "PRODUCE"] <- 4
item$family[item$family == "GROCERY I"] <- 5
item$family[item$family == "DAIRY"] <- 6
item$family[item$family == "CLEANING"] <- 7
item$family[item$family == "BEVERAGES"] <- 8
item$family <- item$family %>% as.factor()
# 把test join到train,test內
train <- left_join(train,item)
test <- left_join(test,item)


# Test內加上星期幾
cl <- makeCluster(detectCores() - 1 )
parL_date <- test$date
clusterExport(cl,"parL_date")
test$weekday <- parLapply(cl,parL_date,function(x){
  a <- weekdays(as.Date(x))
  a <- sub("星期一",1,a)
  a <- sub("星期二",2,a)
  a <- sub("星期三",3,a)
  a <- sub("星期四",4,a)
  a <- sub("星期五",5,a)
  a <- sub("星期六",6,a)
  a <- sub("星期日",7,a)
  return(a)
}) %>% unlist() %>% as.factor()
#不要的欄位先刪掉省資源
test$date <- NULL
rm(cl,parL_date)
gc()

#轉類別,數值
train$onpromotion <- train$onpromotion %>% as.integer()
test$onpromotion <- test$onpromotion %>% as.integer()
train$weekday <- train$weekday %>% as.factor()
#train$date<- train$date %>% as.Date()
#test$date <- test$date %>% as.Date()

#攤平日期,商品類別
train.dummy = as.data.frame(class.ind(train$weekday))
names(train.dummy) <- c("Mon", "Tue", "Wed", "Thu","Fri","Sat","Sun")
train.dummy2 = as.data.frame(class.ind(train$family))
names(train.dummy2) <- c("a", "b", "c", "d","e","f","g","h")

train.x <- train[,-c(2,3,5,6)]
train.y <- train[,c(3)]
set.seed(102)
select <- sample(1:nrow(train),nrow(train)*0.8)

train_set.x <- cbind(train.x[select,], train.dummy[select,], train.dummy2[select,])
train_set.y <- train.y[select]
validation_set.x <- cbind(train.x[-select,], train.dummy[-select,], train.dummy2[-select,])
validation_set.y <- train.y[-select]
rm(item,train,train.x,train.y,train.dummy,train.dummy2,select)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(train_set.x), label =  as.matrix(train_set.y))
dvalidation <- xgb.DMatrix(data = as.matrix(validation_set.x))

xgb.params = list(
  colsample_bytree = 0.5,                    
  subsample = 0.5,
  booster = "gbtree",
  max_depth = 2,
  eta = 0.02,
  eval_metric = "rmse",
  objective = "reg:linear",
  gamma = 0)

cv.model = xgb.cv(
  params = xgb.params,
  data = dtrain,
  nfold = 5,
  nrounds=200,
  early_stopping_rounds = 30, 
  print_every_n = 20
)

tmp <- cv.model$evaluation_log

plot(x=1:nrow(tmp), y= tmp$train_rmse_mean, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 
points(x=1:nrow(tmp), y= tmp$test_rmse_mean, col='blue') 
legend("topright", pch=1, col = c("red", "blue"), 
       legend = c("Train", "Validation") )

best.nrounds = cv.model$best_iteration 
best.nrounds

#xgb.Model <- xgb.train(paras = xgb.params, data = dtrain, nrounds = best.nrounds) 

# Make Predictions for Training Data
xgb.Prediction <- predict(xgb.Model, dtrain)

#計算R平方 評估訓練模型準確度
sst = sum((train_set.y - mean(train_set.y))^2)
ssr = sum((xgb.Prediction - mean(train_set.y))^2)
r2 = ssr/sst
r2


# Make Predictions for validation Data
xgb.Prediction <- predict(xgb.Model, dvalidation)

#計算R平方 評估調參模型準確度
sst2 = sum((validation_set.y - mean(validation_set.y))^2)
ssr2 = sum((xgb.Prediction - mean(validation_set.y))^2)
r22 = ssr2/sst2
r22





#開始預測
test.dummy = as.data.frame(class.ind(test$weekday))
names(test.dummy) <- c("Mon", "Tue", "Wed", "Thu","Fri","Sat","Sun")
test.dummy2 = as.data.frame(class.ind(test$family))
names(test.dummy2) <- c("a", "b", "c", "d","e","f","g","h")
test <- test[,-c(2,4,5)]
test_set.x <- cbind(test,test.dummy,test.dummy2)
rm(test.dummy,test.dummy2)
gc()
dtest <- xgb.DMatrix(data = as.matrix(test_set.x))
xgb.Prediction <- predict(object = xgb.Model,newdata = dtest,missing = NA)
finalPrediction <- round(xgb.Prediction)
