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

data2001 <- data[data$Year ==2001 ,]
data2002 <- data[data$Year ==2002 ,]
data2003 <- data[data$Year ==2003 ,]
data2004 <- data[data$Year ==2004 ,]
data2005 <- data[data$Year ==2005 ,]
data2006 <- data[data$Year ==2006 ,]
data2007 <- data[data$Year ==2007 ,]
data2008 <- data[data$Year ==2008 ,]
data2009 <- data[data$Year ==2009 ,]
data2010 <- data[data$Year ==2010 ,]
data2011 <- data[data$Year ==2011 ,]
data2012 <- data[data$Year ==2012 ,]
data2013 <- data[data$Year ==2013 ,]
data2014 <- data[data$Year ==2014 ,]
data2015 <- data[data$Year ==2015 ,]
data2016 <- data[data$Year ==2016 ,]
data2017 <- data[data$Year ==2017 ,]
data2018 <- data[data$Year ==2018 ,]

count2001 <- aggregate(Year ~ `Primary Type` , data2001 , length)
count2002 <- aggregate(Year ~ `Primary Type` , data2002 , length)
count2003 <- aggregate(Year ~ `Primary Type` , data2003 , length)
count2004 <- aggregate(Year ~ `Primary Type` , data2004 , length)
count2005 <- aggregate(Year ~ `Primary Type` , data2005 , length)
count2006 <- aggregate(Year ~ `Primary Type` , data2006 , length)
count2007 <- aggregate(Year ~ `Primary Type` , data2007 , length)
count2008 <- aggregate(Year ~ `Primary Type` , data2008 , length)
count2009 <- aggregate(Year ~ `Primary Type` , data2009 , length)
count2010 <- aggregate(Year ~ `Primary Type` , data2010 , length)
count2011 <- aggregate(Year ~ `Primary Type` , data2011 , length)
count2012 <- aggregate(Year ~ `Primary Type` , data2012 , length)
count2013 <- aggregate(Year ~ `Primary Type` , data2013 , length)
count2014 <- aggregate(Year ~ `Primary Type` , data2014 , length)
count2015 <- aggregate(Year ~ `Primary Type` , data2015 , length)
count2016 <- aggregate(Year ~ `Primary Type` , data2016 , length)
count2017 <- aggregate(Year ~ `Primary Type` , data2017 , length)
count2018 <- aggregate(Year ~ `Primary Type` , data2018 , length)

count2001$year <- "2001"
count2002$year <- "2002"
count2003$year <- "2003"
count2004$year <- "2004"
count2005$year <- "2005"
count2006$year <- "2006"
count2007$year <- "2007"
count2008$year <- "2008"
count2009$year <- "2009"
count2010$year <- "2010"
count2011$year <- "2011"
count2012$year <- "2012"
count2013$year <- "2013"
count2014$year <- "2014"
count2015$year <- "2015"
count2016$year <- "2016"
count2017$year <- "2017"
count2018$year <- "2018"

all_count <- rbind(count2001,count2002,count2003,count2004,count2005,count2006,count2007,count2008,count2009,
                   count2010,count2011,count2012,count2013,count2014,count2015,count2016,count2017,count2018)
colnames(all_count) = c("type","crime_count","year")
data <- reorder(data$`Location Description`,data$`Location Description`,FUN = function(x) -length(x)) %>% as.data.frame()
colnames(data) = "Location Description"
all_count$year <- all_count$year %>% as.integer()
data$`Primary Type`
all_count %>%
  
  ggplot(data=.,aes(year, crime_count, color = type)) +
  
  geom_smooth(method = "loess", span = 1/2, se = FALSE)
