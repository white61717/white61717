behavior <- read.csv('C:/Users/Student/Desktop/dataset/TBN_CUST_BEHAVIOR.csv')

require(dplyr)

behavior %>% head()
behavior %>% unique()

require(dplyr)
require(parallel)
ptm <- proc.time()
# 設定工作目錄
setwd('C:/Users/Student/Desktop/dataset/')

# 客戶行為
logData <- read.csv('TBN_CUST_BEHAVIOR.csv',stringsAsFactors = F)

# 外匯交易
FXTable <- read.csv('TBN_FX_TXN.csv',stringsAsFactors = F)

# 客戶資料表
customerTable <- read.csv('TBN_CIF.csv',stringsAsFactors = F)

# 將有看網頁的所有客戶存入變數
webUser <- logData$CUST_NO %>% unique()

# 起cluster
cl <- makeCluster(detectCores() - 1)

# 將 webUser 傳給每個節點
clusterExport(cl,'webUser')

# 平行運算出CIF表中的客戶是否為webUser
customerTable$web <- customerTable$CUST_NO %>% parLapply(cl,.,function(t){
  if(t %in% webUser){
    output <- 1
  } else {
    output <- 0
  }
  return(output)
}) %>% unlist()

proc.time() - ptm