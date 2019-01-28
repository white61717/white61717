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
  #col的抽样比例，越高表示每棵树使用的col越多，会增加每棵小树的复杂度
  colsample_bytree = 0.5,                    
  # row的抽样比例，越高表示每棵树使用的col越多，会增加每棵小树的复杂度
  subsample = 0.5,                      
  booster = "gbtree",
  # 树的最大深度，越高表示模型可以长得越深，模型复杂度越高
  max_depth = 2,           
  # boosting会增加被分错的数据权重，而此参数是让权重不会增加的那么快，因此越大会让模型愈保守
  eta = 0.03,
  # 或用'mae'也可以
  eval_metric = "rmse",                      
  objective = "reg:linear",
  # 越大，模型会越保守，相对的模型复杂度比较低
  gamma = 0)

cv.model = xgb.cv(
  params = xgb.params,
  data = dtrain,
  nfold = 5,     # 5-fold cv
  nrounds=200,   # 测试1-100，各个树总数下的模型
  # 如果当nrounds < 30 时，就已经有overfitting情况发生，那表示不用继续tune下去了，可以提早停止                
  early_stopping_rounds = 30, 
  print_every_n = 20 # 每20个单位才显示一次结果，
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


# Make Predictions for Test Data
xgb.Prediction <- predict(xgb.Model, dvalidation)

#計算R平方 評估調參模型準確度
sst2 = sum((validation_set.y - mean(validation_set.y))^2)
ssr2 = sum((xgb.Prediction - mean(validation_set.y))^2)
r22 = ssr2/sst2
r22





#開始預測啦!!!!!!!!!!!!!!
test.dummy = as.data.frame(class.ind(test$weekday))
names(test.dummy) <- c("Mon", "Tue", "Wed", "Thu","Fri","Sat","Sun")
test.dummy2 = as.data.frame(class.ind(test$family))
names(test.dummy2) <- c("a", "b", "c", "d","e","f","g","h")
#names(test.dummy) <- "V2"  #注意這裡的欄位名稱
test <- test[,-c(2,4,5)]
test_set.x <- cbind(test,test.dummy,test.dummy2)
rm(test.dummy,test.dummy2)
gc()
dtest <- xgb.DMatrix(data = as.matrix(test_set.x))
xgb.Prediction <- predict(object = xgb.Model,newdata = dtest,missing = NA)
finalPrediction <- round(xgb.Prediction)