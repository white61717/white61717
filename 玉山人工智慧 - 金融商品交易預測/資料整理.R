alltarget <- cbind(card = custom_card$TXN_DT ,credit =  custom_credit$TXN_DT ,trust = custom_trust$TXN_DT , exchange = custom_exchange$TXN_DT) %>% as.data.frame()


alltarget$card[alltarget$card != 0] <- 1
alltarget$exchange[alltarget$exchange != 0] <- 1
alltarget$credit[alltarget$credit != 0] <- 1
alltarget$trust[alltarget$trust != 0] <- 1
train_targer

write.csv(alltarget, fileEncoding = 'UTF-8','E:/Desktop/train_target.csv')



library(RWeka)

Model.tree <- M5P(PERF ~ ., data=train)
Model.tree
plot(Model.tree)

# Make Predictions
test_set.y_hat <- predict(Model.tree, newdata=test)
test_set.y_hat

# Model Evaluation
mae = sum(abs(test_set.y_hat-test$PERF)) / length(test$PERF)
mae

customtest <- read.csv('E:/Desktop/大數據/TBN_CIF.csv')
customtest