#Factor型態數據品質表
factorQuality <- function(data){

  factorDataQuality <- lapply(data %>% select_if(is.factor),function(x){
      factorData <- data.frame(matrix(,nrow=6, ncol=0))
      uniqueCount <- x %>% unique() %>% length()
      NAcount <- sum( is.na(x) ) 
      NApercentage <- ( sum( is.na(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
      mostAppear <- names( which.max( table(x) ) )
      mostAppearNum <- max( table(x) )
      unbalence <- ( max( table(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
      columnsQuality <- rbind(uniqueCount, NAcount, NApercentage, mostAppear, mostAppearNum, unbalence)
      factorData <- cbind(factorData, columnsQuality)
      return(factorData)
  }) %>% bind_cols() 
  
  row.names(factorDataQuality) = c('不同值個數','空值個數','空值比例','次數最多的值','最多出現次數','不平衡度')
  columnsName <- data %>% select_if(is.factor) %>% names
  colnames(factorDataQuality) <- columnsName
  return(factorDataQuality)
}




#數字型態數據品質表
numsQuality <- function(data){
  
  cl <- makeCluster(detectCores() - 2)
  clusterEvalQ(cl,{require(dplyr)})
  clusterExport(cl,'data')
  
  numsDataQuality <- parLapply(cl,data %>% select_if(function(x){is.numeric(x) || is.integer(x)}),function(x){
    numsData <- data.frame(matrix(,nrow=6, ncol=0))
    uniqueCount <- x %>% unique() %>% length()
    NAcount <- sum( is.na(x) ) 
    NApercentage <- ( sum( is.na(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
    mostAppear <- names( which.max( table(x) ) )
    mostAppearNum <- max( table(x) )
    unbalence <- ( max( table(x) ) * 100 / length(x) ) %>% round() %>% paste(.,'%')
    minNum <- min(x, na.rm = T) %>% round()
    maxNum <- max(x, na.rm = T) %>% round()
    meanNum <- mean(x, na.rm = T) %>% round()
    stdev <- sd(x, na.rm = T) %>% round()
    minus3Std <- ( meanNum - 3 * stdev ) %>% round()
    plus3Std <- ( meanNum + 3 * stdev ) %>% round()

    columnsQuality <- rbind(uniqueCount, NAcount, NApercentage, mostAppear, mostAppearNum, unbalence,
                            minNum,maxNum,meanNum,stdev,minus3Std,plus3Std)
    numsData <- cbind(numsData, columnsQuality)
    return(numsData)
  }) %>% bind_cols() 
  
  row.names(numsDataQuality) = c('不同值個數','空值個數','空值比例','次數最多的值','最多出現次數','不平衡度',
                                 '最小值','最大值','平均值','標準差','M-3','M+3')
  columnsName <- data %>% select_if(function(x){is.numeric(x) || is.integer(x)}) %>% names
  colnames(numsDataQuality) <- columnsName
  stopCluster(cl)
  return(numsDataQuality)
}
