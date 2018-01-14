source("data_processing.R")

y <- readRDS("./training_set/trigrams.rds")

library(stringr)
library(dplyr)

split=str_split_fixed(names(y),pattern = " ",n = 3)
trigrams <- data.frame(freq=y,first=split[,1],second=split[,2],third=split[,3],stringsAsFactors = F)


predict <- function(input){
  input= str_to_lower(input)
  input = tail(str_split(input," ")[[1]],2)
  
  if (length(input)==0){
    return("")
  }
  if(length(input)==2){
      guess=trigrams %>% 
        filter(first==input[1],second==input[2]) %>% 
        top_n(1,freq) %>% 
        select(third)
      
      if (nrow(guess)>0){
        return (guess[1,1])
      }
      else{
        guess=trigrams %>% 
          filter(first==input[2]) %>% 
          top_n(1,freq) %>% 
          select(second)
        
        if (nrow(guess)>0){
          return (guess[1,1])
        }
        else{
          return("the")
        }
      }
  }
  else{
    guess=trigrams %>% 
      filter(first==input[1]) %>% 
      top_n(1,freq) %>% 
      select(second)
    if (nrow(guess)>0){
      return (guess[1,1])
    }
    else{
      return("the")
    }
  }
}

test <- function(test_set){
  yy<- readRDS("./test_set/trigrams.rds")
  test_split=str_split_fixed(names(yy),pattern = " ",n = 3)
  test_trigrams <- data.frame(freq=yy,
                              first=test_split[,1],
                              second=test_split[,2],
                              third=test_split[,3],
                              stringsAsFactors = F)
}


  
ids <- 1:length(yy) #sample(1:length(yy),100)
questions=paste(test_trigrams[ids,2],test_trigrams[ids,3])
answers=sapply(FUN = predict,X = questions)
correct=test_trigrams[ids,4]
protocoll=data.frame(questions,answers,correct,stringsAsFactors = F) %>% mutate(match=(answers==correct))
protocoll %>% summarise(accuracy=mean(match))
