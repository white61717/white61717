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

#test <- test [1:1000000,]
train <- train[85000000:103839389,]

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



#轉類別,數值
train$onpromotion <- train$onpromotion %>% as.integer()
test$onpromotion <- test$onpromotion %>% as.integer()
train$onpromotion <- train$onpromotion %>% as.factor()
test$onpromotion <- test$onpromotion %>% as.factor()

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



#增加是否會腐壞
item <- fread("items.csv")[,c(1,4)]
train <- left_join(train,item)
test <- left_join(test,item)





train.x <- train[,-c(1,3,4)]
train.x <- train.x %>% mutate_if(is.integer,as.factor)
train.x <- train.x %>% mutate_if(is.numeric,as.factor)
train.y <- train[,c(4)]
set.seed(102)
select <- sample(1:nrow(train),nrow(train)*0.8)



train_set.x <- train.x[select,]
train_set.y <- train.y[select]
validation_set.x <- train.x[-select,]
validation_set.y <- train.y[-select]
rm(item,train,train.x,train.y,select)
gc()



train_pool <- catboost.load_pool(data = train_set.x, label = train_set.y)
validation_pool <- catboost.load_pool(validation_set.x)


#設參數

for(i in c(0.01,0.02,0.03,0.04,0.05)){
  for(j in 1:3){
    for(k in 4:7){
  
fit_params <- list(iterations = 150,
                   loss_function = 'Logloss',
                   task_type = 'GPU',
                   learning_rate = 0.05,
                   l2_leaf_reg = 2,
                   depth = 4)


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
all <- c(all,r22)
r22
gc()
    }
  }
}




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



prediction <- prediction-5
prediction[prediction <0] <- 0
final_1 <- cbind(fread("test.csv")[,c(1)],unit_sales = prediction)
final_1$unit_sales %>% unique()
write.csv(final_1,fileEncoding = "UTF-8",row.names = F,"E:/Desktop/catboost-1(learning_rate = 0.05,l2_leaf_reg = 2,depth = 4,-4).csv")

prediction <- prediction-5
prediction[prediction <0] <- 0
final_1 <- cbind(fread("test.csv")[,c(1)],unit_sales = prediction)
final_1$unit_sales %>% unique()
write.csv(final_1,fileEncoding = "UTF-8",row.names = F,"E:/Desktop/catboost-2(learning_rate = 0.05,l2_leaf_reg = 2,depth = 4,-5).csv")

prediction <- prediction-9
prediction[prediction <0] <- 0
final_1 <- cbind(fread("test.csv")[,c(1)],unit_sales = prediction)
final_1$unit_sales %>% unique()
write.csv(final_1,fileEncoding = "UTF-8",row.names = F,"E:/Desktop/catboost-3(learning_rate = 0.05,l2_leaf_reg = 2,depth = 4,-9).csv")



final_1 <- cbind(fread("test.csv")[,c(1)],unit_sales = prediction)
final_1$unit_sales %>% unique()
write.csv(final_1,fileEncoding = "UTF-8",row.names = F,"E:/Desktop/catboost-4(learning_rate = 0.05,l2_leaf_reg = 2,depth = 4).csv")



prediction <- sqrt(prediction) %>% round(.)
final_2 <- cbind(fread("test.csv")[,c(1)],unit_sales = prediction)
final_2$unit_sales %>% unique()
write.csv(final_2,fileEncoding = "UTF-8",row.names = F,"E:/Desktop/catboost-4(learning_rate = 0.05,l2_leaf_reg = 2,depth = 4,sqrt).csv")