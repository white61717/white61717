library(jiebaR)
library(tidyverse)
library(tmcn)


#找出昨、前天的四大報出現次數最多的前十個詞

setwd('D:\\crawl\\')
today <- Sys.Date()
paper <- c('apple', 'chinatimes', 'ltn', 'udn')
filename <- paste0(today, '_', paper, '_international.csv')

apple <- read.csv(filename[1])
chinatimes <- read.csv(filename[2])
ltn <- read.csv(filename[3])
udn <- read.csv(filename[4])

temp <- bind_rows(apple, chinatimes, ltn, udn)
temp <- temp[temp$dateTime == (today-1) | temp$dateTime == (today-2) ,]

allNews <- temp %>% 
  mutate(
    dateTime = dateTime %>% as.Date(),
    month = format(dateTime, '%m'),
    day = format(dateTime, '%d')
  ) %>%
  filter(.,
         content != ""
  )



#斷詞
print("NLPing......")
jieba.worker <- worker()

article.date <- allNews %>%
  group_by(dateTime) %>% # 以每⽇做分組
  do((function(input) {
    freq(segment(input$content, jieba.worker)) %>% # 斷詞後計算詞頻   # 分詞segment(文章,結巴引擎) output= 'X' 'A' 'B'...
      filter(
        !(char %in% toTrad(stopwordsCN())), # 過濾 stopword (的、吧、阿...)  # toTrad把簡體轉繁體
        !str_detect(char, "[A-z0-9]"), # 過濾英⽂數字
        nchar(char) > 1 # 過濾單個字
      ) %>%
      arrange(desc(freq)) %>% # 以詞頻排序
      slice(1:100) %>% # 取前 100
      return
  })(.)) %>%
  ungroup



# #統計所有天數出現次數前五的詞的次數 ex 1/1 台北 , 1/2 台北 -> 台北 2
# article.date.words <- freq(article.date$char) %>%
#   rename(freq.all = freq)
# 
# article.everyday <- article.date %>%
#   left_join( # ⽐對全部詞
#     article.date.words,
#     by = 'char'
#   ) %>%
#   group_by(dateTime) %>% # 以每⽇做分組
#   arrange(freq.all) %>% # 每組的詞頻做排序由⼩到⼤
#   slice(1:10) %>% # 取每組前 5
#   summarise( # 合併詞並對詞頻加總
#     char = str_c(char, collapse = ", "),
#     freq = sum(freq)
#   ) %>%
#   ungroup


today <- Sys.Date() %>% as.character()
filepath <- paste0("D:\\crawl\\", today, "_total_international.csv" )
write.csv(article.date,file = filepath, row.names = F)
