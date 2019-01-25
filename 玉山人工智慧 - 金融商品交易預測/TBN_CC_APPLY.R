custom <- read.csv('E:/Desktop/tbrain/TBN_CIF.csv')
card <- read.csv('E:/Desktop/tbrain/TBN_CC_APPLY.csv')
library(dplyr)

# 算信用卡個數
card_count <- aggregate(TXN_DT~ CUST_NO, card , length)
# join
custom_card <- left_join(custom,card_count)
# NA = 沒辦信用卡, 補0
custom_card$TXN_DT[is.na(custom_card$TXN_DT)] <- 0

custom_card$AGE <- custom_card$AGE %>% as.factor(.)
custom_card$EDU_CODE <- custom_card$EDU_CODE %>% as.factor(.)
custom_card$INCOME_RANGE_CODE <- custom_card$INCOME_RANGE_CODE %>% as.factor(.)
custom_card$WORK_MTHS <- custom_card$WORK_MTHS %>% as.factor(.)
custom_card$TXN_DT <- custom_card$TXN_DT %>% as.factor(.)

# 把信用卡申辦次數為0的刪去
# custom_card_no0 <- custom_card[custom_card$TXN_DT != '0',]

library(ggplot2)
# 教育程度對信用卡作圖
ggplot(custom_card, aes(EDU_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 年齡對信用卡
ggplot(custom_card, aes(AGE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 收入對信用卡
ggplot(custom_card, aes(INCOME_RANGE_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 小孩對信用卡
ggplot(custom_card, aes(CHILDREN_CNT)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 性別對信用卡
ggplot(custom_card, aes(GENDER_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 工作對信用卡
ggplot(custom_card, aes(WORK_MTHS)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

#====================================================================
# 只選male
custom_card_male <- custom_card[custom_card$GENDER_CODE == 'M',]

# 教育程度對信用卡
ggplot(custom_card_male, aes(EDU_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 年齡對信用卡
ggplot(custom_card_male, aes(AGE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 收入對信用卡
ggplot(custom_card_male, aes(INCOME_RANGE_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 小孩對信用卡
ggplot(custom_card_male, aes(CHILDREN_CNT)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 工作對信用卡
ggplot(custom_card_male, aes(WORK_MTHS)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

#====================================================================
# 只選female
custom_card_female <- custom_card[custom_card$GENDER_CODE == 'M',]

# 教育程度對信用卡
ggplot(custom_card_female, aes(EDU_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 年齡對信用卡
ggplot(custom_card_female, aes(AGE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 收入對信用卡 
ggplot(custom_card_female, aes(INCOME_RANGE_CODE)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 小孩對信用卡
ggplot(custom_card_female, aes(CHILDREN_CNT)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))

# 工作對信用卡
ggplot(custom_card_female, aes(WORK_MTHS)) + 
  geom_bar(aes(fill=TXN_DT), position = position_fill(reverse = F))
