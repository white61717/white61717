require(dplyr)
require(parallel)
require(data.table)
library(xgboost)
library(nnet)

setwd('E:/Desktop/玉山 預測/dataset')
inputdata <- fread("imputedData.csv")
pre_custom <- fread("TBN_Y_ZERO.csv")
card1 <- fread('TBN_CC_APPLY.csv')
custom <- fread('TBN_CIF.csv')
behavior <- fread('TBN_CUST_BEHAVIOR.csv')




#信用卡
custom$GENDER_CODE <- custom$GENDER_CODE %>% as.character()
custom$GENDER_CODE[custom$GENDER_CODE=="M"] <- 1
custom$GENDER_CODE[custom$GENDER_CODE=="F"] <- 0
custom$GENDER_CODE[custom$GENDER_CODE==""] <- NA
custom$GENDER_CODE <- custom$GENDER_CODE %>% as.numeric()
#有逛信用卡網站的人
card_behavior <- grep(behavior$PAGE,pattern = "edrn/deoxt") %>% behavior[.,]
card_behavior$VISITDATE<-cut(card_behavior$VISITDATE,breaks = 4,labels = c("0-30", "31-60", "61-90", "91-120"),ordered_result = T)
#攤平
dummy.c <- as.data.frame(class.ind(card_behavior$VISITDATE))
names(dummy.c) <- c("0-30", "31-60", "61-90", "91-120")
card_behavior <- cbind(CUST_NO=card_behavior$CUST_NO,dummy.c)
a <- aggregate(`0-30` ~ CUST_NO, card_behavior , sum)
b <- aggregate(`31-60` ~ CUST_NO, card_behavior , sum)
c <- aggregate(`61-90` ~ CUST_NO, card_behavior , sum)
d <- aggregate(`91-120` ~ CUST_NO, card_behavior , sum)
card_behavior <- cbind(a,`31-60`=b$`31-60`,`61-90`=c$`61-90`,`91-120`=d$`91-120`)
card_behavior
knn_card <- left_join(custom,card_behavior)
knn_card <- knn_card[-c(3,4,9)]
knn_card$`31-60`[is.na(knn_card$`31-60`)] <- 0
knn_card$`61-90`[is.na(knn_card$`91-120`)] <- 0
knn_card$`91-120`[is.na(knn_card$`91-120`)] <- 0
card1 <- aggregate(TXN_DT ~ CUST_NO, card1 , length)
knn_card <- left_join(knn_card,card1)
knn_card$TXN_DT[is.na(knn_card$TXN_DT)] <- 0
knn_card$INCOME_RANGE_CODE[is.na(knn_card$INCOME_RANGE_CODE)] <- 0
knn_card$CUST_NO <- knn_card$CUST_NO %>% as.factor()

knn_card <-knn_card_backup
#knn_card$AGE[knn_card$AGE==1] <-'0'
#knn_card$AGE[knn_card$AGE==2] <-'0.33'
#knn_card$AGE[knn_card$AGE==3] <-'0.66'
#knn_card$AGE[knn_card$AGE==4] <-'1'
#knn_card$AGE <- knn_card$AGE %>% as.double()

#knn_card$INCOME_RANGE_CODE[knn_card$INCOME_RANGE_CODE==1] <-'0'
#knn_card$INCOME_RANGE_CODE[knn_card$INCOME_RANGE_CODE==2] <-'0.33'
#knn_card$INCOME_RANGE_CODE[knn_card$INCOME_RANGE_CODE==3] <-'0.66'
#knn_card$INCOME_RANGE_CODE[knn_card$INCOME_RANGE_CODE==4] <-'1'
#knn_card$INCOME_RANGE_CODE <- knn_card$INCOME_RANGE_CODE %>% as.double()

dummy.edu <- as.data.frame(class.ind(knn_card$EDU_CODE))
names(dummy.edu) <- c("edu1", "edu2", "edu3", "edu4")
knn_card$EDU_CODE <- NULL
knn_card <- cbind(knn_card, dummy.edu)
knn_card_target <- knn_card[9]
knn_card <- knn_card[-c(1,9)]

pre_custom <- knn_card[pre_custom$CUST_NO,]

# Build xgboosting Model


dtrain <- xgb.DMatrix(data = as.matrix(knn_card), label =  as.matrix(knn_card_target))
dtest <- xgb.DMatrix(data = as.matrix(pre_custom))

xgb.params = list(
  colsample_bytree = 0.5,                    
  subsample = 0.5,                      
  booster = "gbtree",
  max_depth = 2,           
  eta = 0.03,
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
#最佳次數
best.nrounds = cv.model$best_iteration 
best.nrounds

xgb.Model <- xgb.train(paras = xgb.params, data = dtrain, nrounds = best.nrounds) 

# Make Predictions for Training Data
xgb.Prediction <- predict(xgb.Model, dtrain)
xgb.Prediction <- ifelse(xgb.Prediction > 0.2, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==knn_card_target)/length(xgb.Prediction)
accuracy.xgb

#table(xgb.Prediction, train_set.y)

# Make Predictions for Test Data
xgb.Prediction <- predict(xgb.Model, dtest)
xgb.Prediction <- ifelse(xgb.Prediction > 0.2, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==test_set.y)/length(xgb.Prediction)
accuracy.xgb
xgb.Prediction %>% sum()

#table(xgb.Prediction, test_set.y)
xgboost_card <- xgb.Prediction %>% as.data.frame()



#外匯

pre_custom <- fread("TBN_Y_ZERO.csv")
exchange1 <- fread('TBN_FX_TXN.csv')

exchange_behavior <- grep(behavior$PAGE,pattern = "edrn/deoxt") %>% behavior[.,]
exchange_behavior$VISITDATE<-cut(exchange_behavior$VISITDATE,breaks = 4,labels = c("0-30", "31-60", "61-90", "91-120"),ordered_result = T)
library("nnet")
dummy.c <- as.data.frame(class.ind(exchange_behavior$VISITDATE))
names(dummy.c) <- c("0-30", "31-60", "61-90", "91-120")
exchange_behavior <- cbind(CUST_NO=exchange_behavior$CUST_NO,dummy.c)
a <- aggregate(`0-30` ~ CUST_NO, exchange_behavior , sum)
b <- aggregate(`31-60` ~ CUST_NO, exchange_behavior , sum)
c <- aggregate(`61-90` ~ CUST_NO, exchange_behavior , sum)
d <- aggregate(`91-120` ~ CUST_NO, exchange_behavior , sum)
exchange_behavior <- cbind(a,`31-60`=b$`31-60`,`61-90`=c$`61-90`,`91-120`=d$`91-120`)
exchange_behavior
knn_exchange <- left_join(custom,exchange_behavior)
knn_exchange <- knn_exchange[-c(3,4,9)]
knn_exchange$`31-60`[is.na(knn_exchange$`31-60`)] <- 0
knn_exchange$`61-90`[is.na(knn_exchange$`91-120`)] <- 0
knn_exchange$`91-120`[is.na(knn_exchange$`91-120`)] <- 0
exchange1 <- aggregate(TXN_DT ~ CUST_NO, exchange1 , length)
knn_exchange <- left_join(knn_exchange,exchange1)
knn_exchange$TXN_DT[is.na(knn_exchange$TXN_DT)] <- 0
knn_exchange$INCOME_RANGE_CODE[is.na(knn_exchange$INCOME_RANGE_CODE)] <- 0
knn_exchange$CUST_NO <- knn_exchange$CUST_NO %>% as.factor()

knn_exchange <-knn_exchange_backup
#knn_exchange$AGE[knn_exchange$AGE==1] <-'0'
#knn_exchange$AGE[knn_exchange$AGE==2] <-'0.33'
#knn_exchange$AGE[knn_exchange$AGE==3] <-'0.66'
#knn_exchange$AGE[knn_exchange$AGE==4] <-'1'
#knn_exchange$AGE <- knn_exchange$AGE %>% as.double()

#knn_exchange$INCOME_RANGE_CODE[knn_exchange$INCOME_RANGE_CODE==1] <-'0'
#knn_exchange$INCOME_RANGE_CODE[knn_exchange$INCOME_RANGE_CODE==2] <-'0.33'
#knn_exchange$INCOME_RANGE_CODE[knn_exchange$INCOME_RANGE_CODE==3] <-'0.66'
#knn_exchange$INCOME_RANGE_CODE[knn_exchange$INCOME_RANGE_CODE==4] <-'1'
#knn_exchange$INCOME_RANGE_CODE <- knn_exchange$INCOME_RANGE_CODE %>% as.double()

dummy.edu <- as.data.frame(class.ind(knn_exchange$EDU_CODE))
names(dummy.edu) <- c("edu1", "edu2", "edu3", "edu4")
knn_exchange$EDU_CODE <- NULL
knn_exchange <- cbind(knn_exchange, dummy.edu)
knn_exchange_target <- knn_exchange[9]
knn_exchange <- knn_exchange[-c(1,9)]


pre_custom <- knn_exchange[pre_custom$CUST_NO,]

dtrain <- xgb.DMatrix(data = as.matrix(knn_exchange), label =  as.matrix(knn_exchange_target))
dtest <- xgb.DMatrix(data = as.matrix(pre_custom))

xgb.params = list(
  colsample_bytree = 0.5,                    
  subsample = 0.5,                      
  booster = "gbtree",
  max_depth = 2,           
  eta = 0.03,
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

xgb.Model <- xgb.train(paras = xgb.params, data = dtrain, nrounds = best.nrounds) 

# Make Predictions for Training Data
xgb.Prediction <- predict(xgb.Model, dtrain)
xgb.Prediction <- ifelse(xgb.Prediction > 2, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==knn_exchange_target)/length(xgb.Prediction)
accuracy.xgb

#table(xgb.Prediction, train_set.y)

# Make Predictions for Test Data
xgb.Prediction <- predict(xgb.Model, dtest)
xgb.Prediction <- ifelse(xgb.Prediction > 2, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==test_set.y)/length(xgb.Prediction)
accuracy.xgb
xgb.Prediction %>% sum()

#table(xgb.Prediction, test_set.y)
xgboost_exchange <- xgb.Prediction %>% as.data.frame()



#信託
pre_custom <- fread("TBN_Y_ZERO.csv")
trust1 <- fread('TBN_WM_TXN.csv')

trust_behavior <- grep(behavior$PAGE,pattern = "edrn/deoxt") %>% behavior[.,]
trust_behavior$VISITDATE<-cut(trust_behavior$VISITDATE,breaks = 4,labels = c("0-30", "31-60", "61-90", "91-120"),ordered_result = T)
library("nnet")
dummy.c <- as.data.frame(class.ind(trust_behavior$VISITDATE))
names(dummy.c) <- c("0-30", "31-60", "61-90", "91-120")
trust_behavior <- cbind(CUST_NO=trust_behavior$CUST_NO,dummy.c)
a <- aggregate(`0-30` ~ CUST_NO, trust_behavior , sum)
b <- aggregate(`31-60` ~ CUST_NO, trust_behavior , sum)
c <- aggregate(`61-90` ~ CUST_NO, trust_behavior , sum)
d <- aggregate(`91-120` ~ CUST_NO, trust_behavior , sum)
trust_behavior <- cbind(a,`31-60`=b$`31-60`,`61-90`=c$`61-90`,`91-120`=d$`91-120`)
trust_behavior
knn_trust <- left_join(custom,trust_behavior)
knn_trust <- knn_trust[-c(3,4,9)]
knn_trust$`31-60`[is.na(knn_trust$`31-60`)] <- 0
knn_trust$`61-90`[is.na(knn_trust$`91-120`)] <- 0
knn_trust$`91-120`[is.na(knn_trust$`91-120`)] <- 0
trust1 <- aggregate(TXN_DT ~ CUST_NO, trust1 , length)
knn_trust <- left_join(knn_trust,trust1)
knn_trust$TXN_DT[is.na(knn_trust$TXN_DT)] <- 0
knn_trust$INCOME_RANGE_CODE[is.na(knn_trust$INCOME_RANGE_CODE)] <- 0
knn_trust$CUST_NO <- knn_trust$CUST_NO %>% as.factor()

knn_trust <-knn_trust_backup
#knn_trust$AGE[knn_trust$AGE==1] <-'0'
#knn_trust$AGE[knn_trust$AGE==2] <-'0.33'
#knn_trust$AGE[knn_trust$AGE==3] <-'0.66'
#knn_trust$AGE[knn_trust$AGE==4] <-'1'
#knn_trust$AGE <- knn_trust$AGE %>% as.double()

#knn_trust$INCOME_RANGE_CODE[knn_trust$INCOME_RANGE_CODE==1] <-'0'
#knn_trust$INCOME_RANGE_CODE[knn_trust$INCOME_RANGE_CODE==2] <-'0.33'
#knn_trust$INCOME_RANGE_CODE[knn_trust$INCOME_RANGE_CODE==3] <-'0.66'
#knn_trust$INCOME_RANGE_CODE[knn_trust$INCOME_RANGE_CODE==4] <-'1'
#knn_trust$INCOME_RANGE_CODE <- knn_trust$INCOME_RANGE_CODE %>% as.double()

dummy.edu <- as.data.frame(class.ind(knn_trust$EDU_CODE))
names(dummy.edu) <- c("edu1", "edu2", "edu3", "edu4")
knn_trust$EDU_CODE <- NULL
knn_trust <- cbind(knn_trust, dummy.edu)
knn_trust_target <- knn_trust[9]
knn_trust <- knn_trust[-c(1,9)]


pre_custom <- knn_trust[pre_custom$CUST_NO,]

# Build xgboosting Model
library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(knn_trust), label =  as.matrix(knn_trust_target))
dtest <- xgb.DMatrix(data = as.matrix(pre_custom))

xgb.params = list(
  colsample_bytree = 0.5,                    
  subsample = 0.5,                      
  booster = "gbtree",
  max_depth = 2,           
  eta = 0.03,
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

xgb.Model <- xgb.train(paras = xgb.params, data = dtrain, nrounds = best.nrounds) 

# Make Predictions for Training Data
xgb.Prediction <- predict(xgb.Model, dtrain)
xgb.Prediction <- ifelse(xgb.Prediction > 0.1, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==knn_trust_target)/length(xgb.Prediction)
accuracy.xgb

#table(xgb.Prediction, train_set.y)

# Make Predictions for Test Data
xgb.Prediction <- predict(xgb.Model, dtest)
xgb.Prediction <- ifelse(xgb.Prediction > 0.1, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==test_set.y)/length(xgb.Prediction)
accuracy.xgb
xgb.Prediction %>% sum()

#table(xgb.Prediction, test_set.y)
xgboost_trust <- xgb.Prediction %>% as.data.frame()




#信貸
pre_custom <- fread("TBN_Y_ZERO.csv")
credit1 <- fread('TBN_LN_APPLY.csv')

credit_behavior <- grep(behavior$PAGE,pattern = "edrn/deoxt") %>% behavior[.,]
credit_behavior$VISITDATE<-cut(credit_behavior$VISITDATE,breaks = 4,labels = c("0-30", "31-60", "61-90", "91-120"),ordered_result = T)
library("nnet")
dummy.c <- as.data.frame(class.ind(credit_behavior$VISITDATE))
names(dummy.c) <- c("0-30", "31-60", "61-90", "91-120")
credit_behavior <- cbind(CUST_NO=credit_behavior$CUST_NO,dummy.c)
a <- aggregate(`0-30` ~ CUST_NO, credit_behavior , sum)
b <- aggregate(`31-60` ~ CUST_NO, credit_behavior , sum)
c <- aggregate(`61-90` ~ CUST_NO, credit_behavior , sum)
d <- aggregate(`91-120` ~ CUST_NO, credit_behavior , sum)
credit_behavior <- cbind(a,`31-60`=b$`31-60`,`61-90`=c$`61-90`,`91-120`=d$`91-120`)
credit_behavior
knn_credit <- left_join(custom,credit_behavior)
knn_credit <- knn_credit[-c(3,4,9)]
knn_credit$`31-60`[is.na(knn_credit$`31-60`)] <- 0
knn_credit$`61-90`[is.na(knn_credit$`91-120`)] <- 0
knn_credit$`91-120`[is.na(knn_credit$`91-120`)] <- 0
credit1 <- aggregate(TXN_DT ~ CUST_NO, credit1 , length)
knn_credit <- left_join(knn_credit,credit1)
knn_credit$TXN_DT[is.na(knn_credit$TXN_DT)] <- 0
knn_credit$INCOME_RANGE_CODE[is.na(knn_credit$INCOME_RANGE_CODE)] <- 0
knn_credit$CUST_NO <- knn_credit$CUST_NO %>% as.factor()

knn_credit <-knn_credit_backup
#knn_credit$AGE[knn_credit$AGE==1] <-'0'
#knn_credit$AGE[knn_credit$AGE==2] <-'0.33'
#knn_credit$AGE[knn_credit$AGE==3] <-'0.66'
#knn_credit$AGE[knn_credit$AGE==4] <-'1'
#knn_credit$AGE <- knn_credit$AGE %>% as.double()

#knn_credit$INCOME_RANGE_CODE[knn_credit$INCOME_RANGE_CODE==1] <-'0'
#knn_credit$INCOME_RANGE_CODE[knn_credit$INCOME_RANGE_CODE==2] <-'0.33'
#knn_credit$INCOME_RANGE_CODE[knn_credit$INCOME_RANGE_CODE==3] <-'0.66'
#knn_credit$INCOME_RANGE_CODE[knn_credit$INCOME_RANGE_CODE==4] <-'1'
#knn_credit$INCOME_RANGE_CODE <- knn_credit$INCOME_RANGE_CODE %>% as.double()

dummy.edu <- as.data.frame(class.ind(knn_credit$EDU_CODE))
names(dummy.edu) <- c("edu1", "edu2", "edu3", "edu4")
knn_credit$EDU_CODE <- NULL
knn_credit <- cbind(knn_credit, dummy.edu)
knn_credit_target <- knn_credit[9]
knn_credit <- knn_credit[-c(1,9)]


pre_custom <- knn_credit[pre_custom$CUST_NO,]

# $TXN_DT[knn_credit$TXN_DT > 1] <- 1

# Build xgboosting Model
library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(knn_credit), label =  as.matrix(knn_credit_target))
dtest <- xgb.DMatrix(data = as.matrix(pre_custom))

xgb.params = list(
  colsample_bytree = 0.5,
  subsample = 0.5,                      
  booster = "gbtree",
  max_depth = 2,           
  eta = 0.03,
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

xgb.Model <- xgb.train(paras = xgb.params, data = dtrain, nrounds = best.nrounds) 

# Make Predictions for Training Data
xgb.Prediction <- predict(xgb.Model, dtrain)
xgb.Prediction <- ifelse(xgb.Prediction > 0.02, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==knn_credit_target)/length(xgb.Prediction)
accuracy.xgb

#table(xgb.Prediction, train_set.y)

# Make Predictions for Test Data
xgb.Prediction <- predict(xgb.Model, dtest)
xgb.Prediction <- ifelse(xgb.Prediction > 0.02, 1, 0)

accuracy.xgb <- sum(xgb.Prediction==test_set.y)/length(xgb.Prediction)
accuracy.xgb
xgb.Prediction %>% sum()

#table(xgb.Prediction, test_set.y)
xgboost_credit <- xgb.Prediction %>% as.data.frame()



#匯總
predict_target <- fread("TBN_Y_ZERO.csv")


final <- cbind(CUST_NO = predict_target[,1],CC_IND=xgboost_card,FX_IND=xgboost_exchange,LN_IND=xgboost_credit,WM_IND=xgboost_trust)
colnames(final) = c("CUST_NO","CC_IND","FX_IND","LN_IND","WM_IND")
write.csv(final,row.names = F,fileEncoding = "UTF-8","E:/Desktop/final.csv")
