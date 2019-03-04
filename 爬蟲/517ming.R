require('rvest')
require('dplyr')

headUrl <-'http://www.517ming.com/'     #此網站已不存在
En <- NULL;Cn <- NULL;Pt <- NULL
En2 <- NULL;Cn2 <- NULL;Pt2 <- NULL;i2 <- NULL
for (i in letters[1:26]){
  #網址
  Sys.sleep(1)
  doc <- paste0(headUrl,i) %>% paste0(.,'-kaitou-danci.html') %>% read_html()
  wordEn <- doc %>% html_nodes('.ctable tr td:nth-child(2)') %>% html_text()
 
  for (j in 1:length(wordEn)){
    test <- wordEn[j]
    if (length(En)<5){
      if(nchar(test)>4){
        #中文
        wordCn <- doc %>% html_nodes('.ctable tr td:nth-child(3)')  %>% html_text() %>% .[[j]] %>% strsplit(.,split=".",fixed=T) %>% .[[1]] %>% .[1] 
        #詞性
        property <- doc %>% html_nodes('.ctable tr td:nth-child(3)')  %>% html_text() %>% .[[j]] %>% strsplit(.,split=".",fixed=T) %>% .[[1]] %>% .[2] 
        if (!is.na(property)){
          if (wordCn !=' n' & wordCn !='ad' & wordCn !='起飞；adj'){
        En <- En %>% append(.,test)
        Cn <- Cn %>% append(.,wordCn)
        Pt <- Pt %>% append(.,property)
        i2 <- i2 %>% append(.,i)
        }}
      }
    }
  }
    En2 <- En2 %>% append(.,En)
    Cn2 <- Cn2 %>% append(.,Cn)
    Pt2 <- Pt2 %>% append(.,Pt)
    En <- NULL;Cn <- NULL;Pt <- NULL
}
dictTable <- cbind(En2,Cn2,Pt2)
colnames(dictTable) <- c('char','word','partOfspeed','chinese')
dictTable

writeLines(toJSON(dictTable,auto_unbox = T, dataframe = 'rows',useBytes=T),'C:/Users/Student/Desktop/sampleData/homeworkend_wordTable.json')

#3
dictTable2 <- cbind(i2,Cn2,Pt2)
colnames(dictTable2) <- c('char','partOfspeed','chinese')

data.frame(dictTable2) %>% group_by(.,char,partOfspeed) %>% summarise(count=n())

#3-1
data.frame(dictTable) %>% group_by(.,partOfspeed) %>% summarise(count=n())

