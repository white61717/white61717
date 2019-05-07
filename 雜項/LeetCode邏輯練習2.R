library(dplyr)
library(purrr)
a <- c('12345','12')

strsplit(a,'') %>% lapply(.,rev) %>% sapply(.,paste,collapse='')

paste('a','b',,sep = '')




paste(c("red", "green"), "apple", collapse = ", ")  # "red apple, green apple"
paste(c("red", "green"), "apple", sep = ", ") # "red, apple"   "green, apple"

paste("red", "apple", collapse = ", ")
paste("red", "apple", sep = ", ")

alarm(11)



#https://leetcode.com/problems/two-sum/
nums <- c(2,7,11,15)

twoSum <- function(nums,target){
  for(i in 1:length(nums)){
    for(j in 1:length(nums)){
      if(nums[i]+nums[j] == target){
        return(c(i,j))
      }
    }
  }
}
twoSum(nums,17)

#https://leetcode.com/problems/add-two-numbers/
input1 <- c(2,4,3)
input2 <- c(5,6,4)

addTwoNumbers <- function(input1,input2){
  numList <- NULL
  for(i in 1:length(input1)){
    numList <- c(numList,input1[i] + input2[i])
    if(numList[i] >= 10){
      numList[i] = numList[i] - 10
      numList[i-1] = numList[i-1] + 1
    }
  }  
  return(rev(numList))
}
addTwoNumbers(input1,input2)

#https://leetcode.com/problems/longest-substring-without-repeating-characters/


#https://leetcode.com/problems/median-of-two-sorted-arrays/
nums1 = c(1,2)
nums2 = c(3,4)

findMedianSortedArrays <- function(nums1,nums2){
  numList <- c(nums1,nums2) %>% sort()
  if(length(numList) %% 2 == 0){
    return( (numList[length(numList)/2] + numList[(length(numList)+2)/2])/2 )
  }else{
    return(numList[(length(numList)+1)/2])
  }
}
findMedianSortedArrays(nums1,nums2)


#https://leetcode.com/problems/container-with-most-water/
input <- c(1,8,6,2,5,4,8,3,7,9)
maxArea <- function(input){
  areaList <- NULL
  for(i in 1:length(input)){
    for(j in 1:length(input)){
      if(input[i]>input[j]){
        areaList <- c(areaList,input[j] * abs(j-i))
      }
    }
  }
  return(max(areaList))
}
maxArea(input)


#https://leetcode.com/problems/3sum/
nums <- c(-1, 0, 1, 2, -1, -4)
threeSum <- function(nums){
  allList <- list()
  for(i in 1:(length(nums)-2)){
    for(j in (i+1):(length(nums)-1)){
      for(k in (j+1):length(nums)){
        if(nums[i] + nums[j] + nums[k] == 0){
          sumList <- NULL
          sumList <- c(nums[i], nums[j], nums[k]) %>% sort()
          allList <- c(allList,list(sumList))
          # allList <- append(allList,list(sumList))
        }
      }
    }
  }
  return(unique(allList))
}
threeSum(nums)


#https://leetcode.com/problems/reverse-integer/

input <- '12013543650'
reverse <- function(input){
  if(grepl(input,pattern = '-')){
    strsplit(input,split = '-') %>% unlist() %>% .[2] %>% strsplit(input,split = '') %>% lapply(., rev) %>% sapply(.,paste,collapse = '') %>% paste0('-',.) 
  }else if(strsplit(input,split = '') %>% unlist() %>% .[nchar(input)] == 0){
    strsplit(input,split = '') %>% lapply(., rev) %>% sapply(.,paste,collapse = '') %>% strsplit(.,'^0') %>% unlist() %>% .[2]
  }else{
    strsplit(input,split = '') %>% lapply(., rev) %>% sapply(.,paste,collapse = '')
  }
}
reverse(input)


#https://leetcode.com/problems/regular-expression-matching/

s <- 'ab'
p <- '.*'
isMatch <- function(s,p){
  
}


#https://leetcode.com/problems/longest-common-prefix/
strs <- c("flower","flow","flight")

longestCommonPrefix <- function(strs){
  prefix = NULL
  prefixList = NULL
  strsList <- strs %>% strsplit(.,split = '')
  for( i in 1: min(nchar(strs))){
    for(j in 1: length(strs)){
      c <- strsList[[1]][i]
      if(strsList[[1]][i] == strsList[[j]][i]){
        prefix <- c(prefix, strsList[[1]][i])
        
      }
    }
  }
  a <- prefix %>% freq
  a <- filter(a,freq == length(strs))
  for(i in nrow(a):1){
    prefixList <- paste0(prefixList,a[i,1])
  }
  return(prefixList)
}
longestCommonPrefix(strs)


#https://leetcode.com/problems/letter-combinations-of-a-phone-number/

digits <- '23'
letterCombinations <- function(digits){
  splitStr <- strsplit(digits,split = '') %>% unlist
  x <- NULL
  for(i in 1:length(splitStr)){
    if('2' %in% splitStr[i]){
      x <- c(x,'abc')
    }else if('3' %in% splitStr[i]){
      x <- c(x,'def')
    }else if('4' %in% splitStr[i]){
      x <- c(x,'ghi')
    }else if('5' %in% splitStr[i]){
      x <- c(x,'jkl')
    }else if('6' %in% splitStr[i]){
      x <- c(x,'mno')
    }else if('7' %in% splitStr[i]){
      x <- c(x,'pqrs')
    }else if('8' %in% splitStr[i]){
      x <- c(x,'tuv')
    }else if('9' %in% splitStr[i]){
      x <- c(x,'wxyz')
    }
  }
  x <- strsplit(x,split = '')
  y <- expand.grid(x[[1]],x[[2]])
  return(y)
}
letterCombinations(digits)


expand.grid(0:1, 0:1, 0:1)


#https://leetcode.com/problems/valid-parentheses/

s <- '(()(())()()())'
isValid <- function(s){
  splitS <- strsplit(s,split = '') %>% unlist
  mapList <- NULL
  if(s == ''){
    return(F)
  }
  for(i in 1:nchar(s)){
    if(splitS[i] %in% c('(','[','{')){
      mapList <- c(mapList,splitS[i])
    }else if(splitS[i] %in% c(')',']','}')){
      mapList <- mapList[-(length(mapList))]
    }
  }
  if(identical(mapList,character(0))){
    return(T)
  }else{
    return(F)
  }
}
isValid(s)


#https://leetcode.com/problems/merge-two-sorted-lists/
l1 <- '1->2->4'
l2 <- '1->3->4'
mergeTwoLists <- function(l1,l2){
  splitL1 <-strsplit(l1,split = '->') %>% unlist
  splitL2 <-strsplit(l2,split = '->') %>% unlist
  numsList <- c(splitL1,splitL2) %>% sort() %>% as.matrix() %>% apply(.,2,paste,collapse='->')
}


x <- c("A B","*.")
sapply(lapply(strsplit(as.character(x), NULL), rev), paste, collapse="")


#https://leetcode.com/problems/climbing-stairs/

n = 3
climbStairs <- function(n){
  two <- n %% 2 
  one <- n - 2 * two
  for(i in 1:two){
    ways <- factorial(two+one)/(factorial(two)+factorial(one))
  }
}
