require('rvest')
require('dplyr')
require('parallel')
require('data.table')

url = fread('E:\\Desktop\\a_lvr_land_a.csv') %>% .[,1]

cl <- makeCluster(5)

#設定平行環境
clusterEvalQ(cl,{
  require('rvest')
  require('dplyr')
  OpenOrNot <- c()
})

#指定變數到節點
clusterExport(cl,"url",envir = environment())


OpenOrNot <- parLapply(cl,url,function(x){
  tryCatch({
  doc <- read_html(x)
  
  data <- doc %>% html_node('body > script:nth-child(2)') %>%　html_text()
  
  temp <- data %>% grepl(pattern = 'src')
  
  OpenOrNot <- c(OpenOrNot,temp)
  },error = function(msg){
    message('one node produced an error: HTTP error 404.')
    return('ChangeURL')
  })
}) %>% unlist()

stopCluster(cl)
gc()

check <- cbind(url,OpenOrNot) %>% data.frame(.,stringsAsFactors =F)
check %>% filter(OpenOrNot == T) %>% .$url %>% as.data.frame()
check %>% filter(OpenOrNot == 'ChangeURL') %>% .$url %>% as.data.frame()
