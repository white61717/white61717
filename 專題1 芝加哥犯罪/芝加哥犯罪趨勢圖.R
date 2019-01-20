library(dplyr)
require(parallel)
library(lubridate)
library(ggplot2)
library(data.table)
data <- fread("E:/Desktop/Crimes_-_2001_to_present.csv")
data_backup <- data


#=========================================犯罪趨勢圖=========================================
data$Date <- data$Date %>% as.character()

cl <- makeCluster(detectCores() - 1)
a <-data$Date
clusterExport(cl,'a')
f <- parLapply(cl,a,function(x){
  a <- strsplit(x,split=" ",fixed = T)
  b <- a[[1]]
  c <- b[1]
})

data$Date <- f 
data$Date <- mdy(data$Date)
data_year <- data
count <- aggregate(ID ~ Date , data , length)

count %>%
  
  group_by(Date) %>%
  
  summarise(Crime_count = median(ID)) %>%
  
  ggplot(aes(Date, Crime_count)) +
  
  geom_line(color = "black") +
  
  geom_smooth(method = "loess", color = "red", span = 1/5)



#=========================================犯罪地區統計=========================================

data <- data_backup
options(scipen = 200)

location_count <-aggregate(ID ~ `Location Description` , data ,length) %>% as.data.frame()
type <- location_count$`Location Description`[location_count$ID >20000] %>% unique()
data <- data[data$`Location Description` == type,]
data <- reorder(data$`Location Description`,data$`Location Description`,FUN = function(x) -length(x)) %>% as.data.frame()
colnames(data) = "Location Description"

data %>%
  
  ggplot(aes(`Location Description`, fill = `Location Description`)) +
  
  geom_bar() +
  
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))


#=========================================犯罪種類統計=========================================

data <- data_backup
data <- data[data$`Primary Type` != c("NON-CRIMINAL"),]


data <- reorder(data$`Primary Type`,data$`Primary Type`,FUN = function(x) -length(x)) %>% as.data.frame()
colnames(data) = "Primary Type"


library(ggplot2)

data %>%
  
  ggplot(aes(`Primary Type`, fill = `Primary Type`)) +
  
  geom_bar() +
  
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))


#===================================殺人犯罪統計===================================

data <- data_year
#data <- fread("E:/Desktop/Crimes_-_2001_to_present.csv")
#data_backup <- data
#data$Date <- data$Date %>% as.character()
#cl <- makeCluster(detectCores() - 1)
#a <-data$Date
#clusterExport(cl,'a')
#f <- parLapply(cl,a,function(x){
#  a <- strsplit(x,split=" ",fixed = T)
#  b <- a[[1]]
#  c <- b[1]
#})
#data$Date <- f 
#data$Date <- mdy(data$Date)
data_weapon <- data[data$`Primary Type` == "HOMICIDE",]
count <- aggregate(ID ~ year(Date) , data_weapon , length) %>% as.data.frame()
colnames(count) = c("Year","Count")


count %>%
  
  ggplot(aes(Year, Count)) +
  
  geom_line(color = "red",size=1) +
  
  geom_point(color = "red",size=2)


#===================================槍枝犯罪統計===================================

data <- data_year

#data <- fread("E:/Desktop/Crimes_-_2001_to_present.csv")
#data_backup <- data
#data$Date <- data$Date %>% as.character()
#cl <- makeCluster(detectCores() - 1)
#a <-data$Date
#clusterExport(cl,'a')
#f <- parLapply(cl,a,function(x){
#  a <- strsplit(x,split=" ",fixed = T)
#  b <- a[[1]]
#  c <- b[1]
#})
#data$Date <- f 
#data$Date <- mdy(data$Date)

data_weapon <- data[data$`Primary Type` == "WEAPONS VIOLATION",]
count <- aggregate(ID ~ Date , data_weapon , length)
count %>%
  
  group_by(Date) %>%
  
  summarise(Weapon_count = median(ID)) %>%
  
  ggplot(aes(Date, Weapon_count)) +
  
  geom_line(color = "black") +
  
  geom_smooth(method = "loess", color = "red", span = 1/5)



#===================================歷年犯罪統計===================================


data <- data_backup

data <- data %>% group_by(Year,`Primary Type`) %>% summarise(count=length(`Primary Type`))
                
data %>%
  
  ggplot(data=.,aes(Year, count, color = `Primary Type`)) +
  
  geom_smooth(method = "loess", span = 1/2, se = FALSE)

