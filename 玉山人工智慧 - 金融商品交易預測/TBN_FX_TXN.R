exchange <- read.csv('C:/Users/Student/Desktop/dataset/TBN_FX_TXN.csv')
custom <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CIF.csv')
library(dplyr)
# 算外匯交易次數和外匯交易總金額
exchange_count <- aggregate(TXN_DT ~ CUST_NO, exchange , length)
exchange_total <- aggregate(FX_TXN_AMT ~ CUST_NO, exchange , sum)
# join
custom_exchange <- left_join(custom,exchange_count)
custom_exchange <- left_join(custom_exchange,exchange_total)
# 沒有值代表沒有投資 補0
custom_exchange$TXN_DT[is.na(custom_exchange$TXN_DT)] <- 0
custom_exchange$FX_TXN_AMT[is.na(custom_exchange$FX_TXN_AMT)] <- 0
# 去極端值
custom_exchange <- custom_exchange[custom_exchange$TXN_DT<=14,]


custom_exchange$AGE <- custom_exchange$AGE %>% as.factor(.)
custom_exchange$EDU_CODE <- custom_exchange$EDU_CODE %>% as.factor(.)
custom_exchange$INCOME_RANGE_CODE <- custom_exchange$INCOME_RANGE_CODE %>% as.factor(.)
custom_exchange$WORK_MTHS <- custom_exchange$WORK_MTHS %>% as.factor(.)
custom_exchange$TXN_DT <- custom_exchange$TXN_DT %>% as.factor(.)

# 把外匯次數為0的刪去
# custom_exchange_no0 <- custom_exchange[custom_exchange$TXN_DT != '0',]


# 外匯次數比例
ggplot(custom_exchange)+ geom_bar(mapping = aes(x=TXN_DT))
# 教育程度對外匯次數作圖
ggplot(custom_exchange, aes(EDU_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 年齡對外匯次數
ggplot(custom_exchange, aes(AGE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 收入對外匯次數
ggplot(custom_exchange, aes(INCOME_RANGE_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 小孩對外匯次數
ggplot(custom_exchange, aes(CHILDREN_CNT)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 性別對外匯次數
ggplot(custom_exchange, aes(GENDER_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 工作對外匯次數
ggplot(custom_exchange, aes(WORK_MTHS)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# =========================================================================================================

# 備份
custom_exchange2 <- custom_exchange
# 刪去外匯金額極端值
custom_exchange2$FX_TXN_AMT %>% summary()
mean(custom_exchange2$FX_TXN_AMT, na.rm = TRUE) + 3 * sd(custom_exchange2$FX_TXN_AMT, na.rm = TRUE)
custom_exchange2 <- custom_exchange2[custom_exchange$FX_TXN_AMT<=244198.8,]
custom_exchange2$FX_TXN_AMT %>% summary()

# 外匯金額轉階層
total <- custom_exchange2$FX_TXN_AMT 
clean_exchange_total <- function(x){
  if(x<=30000){
    return('1')
  }else if(30000<x & x<=60000){
    return('2')
  }else if(60000<x & x<=90000){
    return('3')
  }else if(90000<x & x<=120000){
    return('4')
  }else if(120000<x & x<=150000){
    return('5')
  }else if(150000<x){
    return('6')
  }
}
total <- lapply(total,clean_exchange_total)
total <- total %>% as.numeric()
custom_exchange2 <- cbind(custom_exchange2[-c(10)],total)
custom_exchange2$total <- custom_exchange2$total %>% as.factor(.)

# 教育程度對外匯金額作圖
library(ggplot2)
ggplot(custom_exchange2, aes(EDU_CODE)) + 
  geom_bar(aes(fill=total), position = position_fill(reverse = F))

# 年齡對外匯金額
ggplot(custom_exchange2, aes(AGE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 收入對外匯金額
ggplot(custom_exchange2, aes(INCOME_RANGE_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 小孩對外匯金額
ggplot(custom_exchange2, aes(CHILDREN_CNT)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 性別對外匯金額
ggplot(custom_exchange2, aes(GENDER_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 工作對外匯金額
ggplot(custom_exchange2, aes(WORK_MTHS)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# ============================================================================
# 只選male
custom_exchange_male <- custom_exchange2[custom_exchange2$GENDER_CODE == 'M',]

# 教育程度對外匯金額作圖
library(ggplot2)
ggplot(custom_exchange_male, aes(EDU_CODE)) + 
  geom_bar(aes(fill=total), position = position_fill(reverse = F))

# 年齡對外匯金額
ggplot(custom_exchange_male, aes(AGE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 收入對外匯金額
ggplot(custom_exchange_male, aes(INCOME_RANGE_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 小孩對外匯金額
ggplot(custom_exchange_male, aes(CHILDREN_CNT)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 工作對外匯金額
ggplot(custom_exchange_male, aes(WORK_MTHS)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# ============================================================================
# 只選female
custom_exchange_female <- custom_exchange2[custom_exchange2$GENDER_CODE == 'F',]

# 教育程度對外匯金額作圖
library(ggplot2)
ggplot(custom_exchange_female, aes(EDU_CODE)) + 
  geom_bar(aes(fill=total), position = position_fill(reverse = F))

# 年齡對外匯金額
ggplot(custom_exchange_female, aes(AGE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 收入對外匯金額
ggplot(custom_exchange_female, aes(INCOME_RANGE_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 小孩對外匯金額
ggplot(custom_exchange_female, aes(CHILDREN_CNT)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 工作對外匯金額
ggplot(custom_exchange_female, aes(WORK_MTHS)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))
