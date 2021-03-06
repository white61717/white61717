library(dplyr)
library(data.table)
library(parallel)
library(xgboost)
library(nnet)
library(gpuR)
library(zoo)


setwd("C:/Users/Student/Desktop/厄瓜多市場預測")
train <- fread("train_weekday.csv")[,-c(1)]
test <- fread("test.csv")[,-c(1)]
item <- fread("items.csv")[,c(1,2)]
holidays <- fread("holidays.csv")[,c(2,8)]
oil <- fread("oil.csv")

test <- test [1:100000,]
train <- train[9000000:10000000,]

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

#轉類別,數值
train$onpromotion <- train$onpromotion %>% as.integer()
test$onpromotion <- test$onpromotion %>% as.integer()
#增加假日欄位
train$holiday <- train$weekday
test$holiday <- test$weekday
train$holiday[!train$holiday %in% c(5,6)] <- 0
train$holiday[train$holiday %in% c(5,6)] <- 1
test$holiday[!test$holiday %in% c(5,6)] <- 0
test$holiday[test$holiday %in% c(5,6)] <- 1

offdays <- holidays$date[holidays$duty_off == 1]
ondays <- holidays$date[holidays$duty_off == 0]
train$holiday[train$date %in% offdays] <- 1
train$holiday[train$date %in% ondays] <- 0
test$holiday[test$date %in% offdays] <- 1
test$holiday[test$date %in% ondays] <- 0
rm(offdays,ondays)
gc()



#轉store_nbr 首都or非首都
train$store_nbr[train$store_nbr %in% c(1,2,3,4,6,7,8,9,10,17,18,20,44,45,46,47,48,49)] <- 1
train$store_nbr[!train$store_nbr %in% c(1,2,3,4,6,7,8,9,10,17,18,20,44,45,46,47,48,49)] <- 0
test$store_nbr[test$store_nbr %in% c(1,2,3,4,6,7,8,9,10,17,18,20,44,45,46,47,48,49)] <- 1
test$store_nbr[!test$store_nbr %in% c(1,2,3,4,6,7,8,9,10,17,18,20,44,45,46,47,48,49)] <- 0


#增加石油欄位
oil$dcoilwtico <- na.locf(oil$dcoilwtico, fromLast = TRUE)
a <- vector()
for (i in 2:1218){
  if(oil$dcoilwtico[i]-oil$dcoilwtico[i-1] > 0){
    a <- c(a,1)
  }
  else if(oil$dcoilwtico[i]-oil$dcoilwtico[i-1] < 0){
    a <- c(a,0)
  }else{
    a <- c(a,NA)
  }
}
a <- c(NA,a)
oil$trend <- a
oil$trend <- na.locf(oil$trend, fromLast = TRUE)
rm(a)
gc()
oil <- oil[,-2]
train <- left_join(train,oil)
test <- left_join(test,oil)
train$trend <- na.locf(train$trend, fromLast = TRUE)
test$trend <- na.locf(test$trend, fromLast = TRUE)
#train$date<- train$date %>% as.Date()
#test$date <- test$date %>% as.Date()

#攤平日期,商品類別
train$weekday <- train$weekday %>% as.factor()
test$weekday <- test$weekday %>% as.factor()
train.dummy = as.data.frame(class.ind(train$weekday))
names(train.dummy) <- c("Mon", "Tue", "Wed", "Thu","Fri","Sat","Sun")
train.dummy2 = as.data.frame(class.ind(train$family))
names(train.dummy2) <- c("a", "b", "c", "d","e","f","g","h")

train.x <- train[,-c(1,3,4,6,7)]
train.y <- train[,c(4)]
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
  eta = 0.015,
  # 或用'mae'也可以
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

save(cv.model,file = "C:/Users/Student/Desktop/厄瓜多市場預測/model.RData")


xgb.Model <- xgb.train(paras = xgb.params, data = dtrain, nrounds = best.nrounds,print_every_n = 20) 
save(xgb.Model,file = "C:/Users/Student/Desktop/厄瓜多市場預測/xgbmodel.RData")
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
test <- test[,-c(1,3,5,6)]
test_set.x <- cbind(test,test.dummy,test.dummy2)
rm(test.dummy,test.dummy2)
gc()
dtest <- xgb.DMatrix(data = as.matrix(test_set.x))
xgb.Prediction <- predict(object = xgb.Model,newdata = dtest,missing = NA)
finalPrediction <- round(xgb.Prediction)




finalPrediction[finalPrediction <0] <- 0
final_1 <- cbind(fread("test.csv")[,c(1)],unit_sales = finalPrediction)
final_1$unit_sales %>% unique()
write.csv(final_1,fileEncoding = "UTF-8",row.names = F,"C:/Users/Student/Desktop/final_5.csv")

sqrt_finalPrediction <- sqrt(finalPrediction) %>% round(.)
final_2 <- cbind(fread("test.csv")[,c(1)],unit_sales = sqrt_finalPrediction)
final_2$unit_sales %>% unique()
write.csv(final_2,fileEncoding = "UTF-8",row.names = F,"C:/Users/Student/Desktop/final_6.csv")
