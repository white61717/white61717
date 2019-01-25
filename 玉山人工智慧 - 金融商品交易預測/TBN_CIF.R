custom <- read.csv('E:/Desktop/大數據/TBN_CIF.csv')


custom.id <- custom[1]
custom.children <- custom[3]
custom.date <- custom[4]
custom.work <- custom[8]
#改2,6,7欄位
custom$AGE <- custom$AGE %>% as.factor(.)
custom$EDU_CODE <- custom$EDU_CODE %>% as.factor(.)
custom$INCOME_RANGE_CODE <- custom$INCOME_RANGE_CODE %>% as.factor(.)
custom$WORK_MTHS <- custom$WORK_MTHS %>% as.factor(.)
cleanCustom <- function(x){
  x = sub("1","0",x)
  x = sub("2","0.33",x)
  x = sub("3","0.66",x)
  x = sub("4","1",x)
  
  x = sub("M","1",x)
  x = sub("F","0",x)
  x = as.double(x)
  
  return(x)
}

custom2 <- as.data.frame((lapply(custom[c(2,6)], cleanCustom)))
custom2 %>% head()

#5欄位攤平
custom.edu.c <- custom[,c("EDU_CODE")]
library("nnet")
dummy.edu.c <- as.data.frame(class.ind(custom.edu.c))
names(dummy.edu.c) <- c("Edu-1", "Edu-2", "Edu-3", "Edu-4","Edu-5","Edu-6")


#8欄位 
cleanCustom_2 <- function(x){
  
  x = sub("1","0",x)
  x = sub("2","0.25",x)
  x = sub("3","0.5",x)
  x = sub("4","0.75",x)
  x = sub("5","1",x)
  x = as.double(x)
  return(x)
}
custom3 <- as.data.frame(lapply(custom[8], cleanCustom_2))

#合併
test <- cbind(custom.id,custom2,custom.date,custom.children,dummy.edu.c,custom.work)
library(DMwR)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)
clusterEvalQ(cl, { library(DMwR) })
clusterExport(cl, "test")
imputedData <- knnImputation(test, k=3, scale = T)
#==============================
custom$INCOME_RANGE_CODE %>% unique()
