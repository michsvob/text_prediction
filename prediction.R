#source("data_sampling.R")
#source("data_processing.R")

ngramdb <- readRDS("./training_set/ngramdb.rds")

library(stringr)
library(dplyr)

##Ukoly:
##upravit predict a test, aby reflektovaly zmenenou strukturu vstupnich dat (ngramdb)

predict_2 <- function(input,alternatives=F){
  input= str_to_lower(gsub("//s"," ",input)) # lowercase and strip extra whitespace
  input = tail(str_split(input," ")[[1]],4)
  input=c(rep("",4-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(fourth==input[1],third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(f=sum(freq)) %>% 
    mutate(score=f*5^2) #ngrams are up-voted by factor n square
    #arrange(desc(f)) 
    #top_n(1,freq) %>% 
  
  gr4=ngramdb %>% 
    filter(third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(f=sum(freq)) %>% 
    mutate(score=f*4^2)
    #arrange(desc(f)) 
    #top_n(1,freq) %>% 
  
  gr3=ngramdb %>% 
    filter(second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(f=sum(freq)) %>% 
    mutate(score=f*3^2)
    #arrange(desc(f))  
    #top_n(1,freq) %>% 
  
  gr2=ngramdb %>% 
    filter(first==input[4]) %>% 
    group_by(pred) %>% summarise(f=sum(freq)) %>% 
    mutate(score=f*.1)
    #arrange(desc(f)) 
    #top_n(1,freq) %>% 
  
  joined <- bind_rows(gr5,gr4,gr3,gr2) %>%
    group_by(pred) %>% summarise(score=sum(score,na.rm=TRUE)) %>% top_n(10,score) %>% 
    arrange(desc(score))
  
  if(nrow(joined)==0){
    pred="the"
  }
  else{
    pred=joined[1,1]
  }
  if (alternatives){
    print(joined)
  }
  
  as.character(pred)
}

#obsolete
predict <- function(input){
  input= str_to_lower(input)
  input = tail(str_split(input," ")[[1]],2)
  input=c(rep("",5-length(input)),input)
  
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

test <- function(test_set="./test_set/ngramdb.rds"){
  yy<- readRDS(test_set)
  yy <- yy %>% filter(grams==5) %>% transmute(question=paste(fourth,third,second,first),correct=pred)
  
  #ids <- 1:length(yy) 
  ids <- sample(1:nrow(yy),200)
  questions=yy$question[ids]
  correct=yy$correct[ids]
  answers=sapply(FUN = predict_2,X = questions)
  protocoll=data.frame(questions,answers,correct,stringsAsFactors = F) %>% mutate(match=(answers==correct))
  print(protocoll %>% summarise(accuracy=mean(match)))
  protocoll
}

protocoll <- test()
View(protocoll)

  

