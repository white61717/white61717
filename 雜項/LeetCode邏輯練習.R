y <- c(1,1,1,1,1,6,2,7,8,3,3,7,5,4,6,2,4,7,3,7,8)


change <- function(x){
  
  freqence <- x %>% as.character() %>% freq()
  freqence %>% arrange(desc(.$freq)) %>% head(1) %>% .$char %>% cat()
  
}

change(y)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

z <- c(1,1,2,2,3,3,4,1,1,1)

change2 <- function(x){
  
  freqence <- x %>% as.character(.) %>% freq
  freqence %>% filter(freq == 1) %>% .$char %>% cat()
  
}

change2(z)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

a <- c(2,8,0,3,2,0,0,3,7,0)

change3 <- function(a){
  
  frequence <- a %>% as.character() %>% freq %>% filter(char == 0) %>% .$freq
  
  for(i in 1:length(a)){
    if(a[i] != 0){
      cat(a[i])
    }
  }
  for(j in 1:frequence){
    cat(0)
  }
}

change3(a)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

a <- "let's go to play"

inverseStr <- function(a){
  
  splitstr <- a %>% strsplit(.,split = "") %>% unlist()
  
  for(i in length(splitstr):1){
    cat(splitstr[i])
  }
}

inverseStr(a)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

data <- c(5,2,7,3,7,9,4,8,4)

change <- function(data){
  
final <- c()
for(i in 1:length(data)){
  temp <- data[i]*data[i+1]
  final <- c(final,temp)
}
return(max(final,na.rm = T))
}

change(data)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

data <- 6

change2 <- function(data){
  
for(i in 1:data){
  if(i == data){
    cat(2^(data-1))
    break
  }
  cat(2^(data-1))
  cat('+')
}
cat('=')
cat(2^(data-1)*data)

}

change2(data)
