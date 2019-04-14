library('dplyr')
library('data.table')
library('xgboost')
library('readr')

data <- fread('E:\\Desktop\\prudential-life-insurance-assessment\\train.csv')
test <- fread('E:\\Desktop\\prudential-life-insurance-assessment\\test.csv')

data <- data[,-c(1)]
data$Product_Info_2 <- data$Product_Info_2 %>% as.factor()
test$Product_Info_2 <- test$Product_Info_2 %>% as.factor()

set.seed(102)
select <- sample(1:nrow(data),nrow(data)*0.8)

train_x <- data[select,-127]
train_y <- data$Response[select]
vibration_x <- data[-select,-'Response']
vibration_y <- data[-select,'Response']

xgb <- xgboost(data=data.matrix(train_x),
              label = train_y,
              eta = 0.1,
              depth = 6,
              nround = 500,
              objective   = "reg:linear",
              eval_metric = "rmse"
              )
#畫圖
tmp <- xgb$evaluation_log
plot(x=1:nrow(tmp), y= tmp$train_rmse, col='red', xlab="nround", ylab="rmse", main="Avg.Performance in CV") 

#vib正確率
{
vibSubmission <- predict(xgb,data.matrix(vibration_x)) %>% round()
vibSubmission[vibSubmission<1] <- 1
vibSubmission[vibSubmission>8] <- 8
vibRate <- sum(vibSubmission == vibration_y) / length(vibSubmission)
}

#train正確率
{
trainSubmission <- predict(xgb,data.matrix(train_x)) %>% round()
trainSubmission[trainSubmission<1] <- 1
trainSubmission[trainSubmission>8] <- 8
trainRate <- sum(trainSubmission == train_y) / length(trainSubmission)
}

cat('vib正確率',vibRate,'\ntrain正確率',trainRate)




xgb.importance(model = xgb)

id <- data.frame(Id=test$Id)
submission <- predict(xgb, data.matrix(test[,-1])) %>% round()
submission[submission<1] <- 1
submission[submission>8] <- 8
final <- cbind(id,submission)
names(final) <- c('Id','Response')
write_csv(final, 'xgboost-2.csv')

