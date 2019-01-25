inputdata <- read.csv("E:/Desktop/玉山 預測/dataset/imputedData.csv")
pre_custom <- read.csv("E:/Desktop/玉山 預測/dataset/TBN_Y_ZERO.csv")
card1 <- read.csv('E:/Desktop/玉山 預測/dataset/TBN_CC_APPLY.csv')


replace <- function(x){
  x = sub("1","0",x)
  x = sub("2","0.33",x)
  x = sub("3","0.66",x)
  x = sub("4","1",x)
  
  x = as.double(x)
  return(x)
}

inputdata$AGE <- inputdata$AGE %>% as.factor()

inputdata$EDU_CODE <- inputdata$EDU_CODE %>% as.factor()
inputdata$INCOME_RANGE_CODE <- inputdata$INCOME_RANGE_CODE %>% as.factor()
inputdata$GENDER_CODE <-inputdata$GENDER_CODE %>% as.factor()





knn_card <- left_join(inputdata,card_behavior)
knn_card <- knn_card[-c(4,9)]
knn_card$`31-60`[is.na(knn_card$`31-60`)] <- 0
knn_card$`61-90`[is.na(knn_card$`91-120`)] <- 0
knn_card$`91-120`[is.na(knn_card$`91-120`)] <- 0
card1 <- aggregate(TXN_DT ~ CUST_NO, card1 , length)
knn_card <- left_join(knn_card,card1)
knn_card$TXN_DT[is.na(knn_card$TXN_DT)] <- 0
knn_card$CUST_NO <- knn_card$CUST_NO %>% as.factor()
pre_custom <- knn_card[pre_custom$CUST_NO,]
pre_custom <- pre_custom[-c(11)]
knn_card <- knn_card[-c(1)]

knn_card$TXN_DT <- knn_card$TXN_DT %>% as.factor()




SVM.Model <- svm(formula = TXN_DT ~ ., kernel = "radia", data = knn_card, 
                 probability=TRUE)
summary(SVM.Model)

# Make Predictions
SVM.Prediction <- predict(SVM.Model, pre_custom, probability=TRUE)



Model.tree.credit <- M5P(TXN_DT ~ ., data=knn_card)
test_set.credit <- predict(Model.tree.credit, newdata=pre_custom)
