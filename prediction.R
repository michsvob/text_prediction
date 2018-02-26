#source("data_sampling.R")
#source("data_processing.R")

library(stringr)
library(dplyr)
library(tm)

#ngramdb <- readRDS("./training_set/ngramdb.rds")
#knowns <- readRDS("./training_set/knowns.rds")

#ngramdb <- readRDS("./training_set/ngramdb_big.rds")
#ngramdb <- readRDS("./training_set/knowns_big.rds")

ngramdb <- readRDS("./training_set/ngramdb_medium.rds")
knowns <- readRDS("./training_set/knowns_medium.rds")


#Function to predict next word
#Principle: Interpolation - word is guessed based on weighted conditional 
#probabilities of ngrams of size 1-5
#predict_2 <- function(input,alternatives=T,c5=1,c4=1,c3=1,c2=1,c1=1){
  input=stripWhitespace(removeNumbers(removePunctuation(input)))
  input= str_to_lower(gsub("^ * | $ *","",input)) # lowercase and strip extra whitespace before and after

  input = tail(str_split(input," ")[[1]],4)
  input=replace(input,!(input %in% knowns),"<unk>")
  input=c(rep("",4-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(grams==5,predictor==paste(input,collapse=" ")) %>%# top_n(10,condprob) %>% 
    group_by(prediction) %>% summarise(score=c5*sum(condprob))
    #ngrams are up-voted by factor c
  
  score5=0
  if(nrow(gr5)>0){
    score5=top_n(gr5,1,score)[1,2]
  }
  
  gr4=ngramdb %>% 
    filter(grams==4,predictor==paste(input[2:4],collapse=" ")) %>% 
    group_by(prediction) %>% summarise(score=c4*sum(condprob))
  
  score4=0
  if(nrow(gr4)>0){
    score4=top_n(gr4,1,score)[1,2]
  }
  
  gr3=ngramdb %>% 
    filter(grams==3,predictor==paste(input[3:4],collapse=" ")) %>% 
    group_by(prediction) %>% summarise(score=c3*sum(condprob))
  
  score3=0
  if(nrow(gr3)>0){
    score3=top_n(gr3,1,score)[1,2]
  }
  
  gr2=ngramdb %>% 
    filter(grams==2,predictor==paste(input[4],collapse=" ")) %>% top_n(10,condprob) %>% 
    group_by(prediction) %>% summarise(score=c2*sum(condprob))
  #top_n is here for performance reasons
  
  score2=0
  if(nrow(gr2)>0){
    score2=top_n(gr2,1,score)[1,2]
  }
  
  pred <- (bind_rows(gr5,gr4,gr3,gr2) %>% 
    group_by(prediction) %>% summarise(score=sum(score,na.rm=TRUE)) %>% top_n(((alternatives*2)+1),score) %>% 
    arrange(desc(score)))$prediction
  
  if(length(pred)<3){
    pred=head(append(pred,c('the', 'on', 'a')),3)
  }
  pred
}

predict_3 <- function(input,alternatives=T,c5=1,c4=1,c3=1,c2=1,c1=1){
  input=stripWhitespace(removeNumbers(removePunctuation(input)))
  input= str_to_lower(gsub("^ * | $ *","",input)) # lowercase and strip extra whitespace before and after
  input = tail(str_split(input," ")[[1]],4)
  input=replace(input,!(input %in% knowns),"<unk>")
  input=c(rep("",4-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(grams==5,predictor==paste(input,collapse=" ")) 
  #ngrams are up-voted by factor c
  
  score5=0
  if(nrow(gr5)>0){
    score5=top_n(gr5,1,condprob)[1,2]
  }
  
  gr4=ngramdb %>% 
    filter(grams==4,predictor==paste(input[2:4],collapse=" ")) 
  
  score4=0
  if(nrow(gr4)>0){
    score4=top_n(gr4,1,condprob)[1,2]
  }
  
  gr3=ngramdb %>% 
    filter(grams==3,predictor==paste(input[3:4],collapse=" "))
  
  score3=0
  if(nrow(gr3)>0){
    score3=top_n(gr3,1,condprob)[1,2]
  }
  
  gr2=ngramdb %>% 
    filter(grams==2,predictor==paste(input[4],collapse=" "))
  #top_n is here for performance reasons
  
  score2=0
  if(nrow(gr2)>0){
    score2=top_n(gr2,1,condprob)[1,2]
  }
  
  pred <- (bind_rows(gr5,gr4,gr3,gr2) %>% 
             group_by(prediction) %>% summarise(score=sum(condprob,na.rm=TRUE)) %>% top_n(((alternatives*4)+1),score) %>% 
             arrange(desc(score)))$prediction
  
  if(length(pred)<5){
    pred=head(append(pred,c('the', 'on', 'a','at','on')),5)
  }
  pred
}

test_set="./test_set/ngramdb.rds"
yy<- readRDS(test_set)
yy <- yy %>% filter(grams==5) %>% transmute(question=paste(fourth,third,second,first),correct=pred)
ids <- sample(1:nrow(yy),1000)

test <- function(k1,k2,k3,k4,k5){
  time1 <- Sys.time()

  #ids <- 1:length(yy) 
  #######################
  #######################
  #######################
  #######################
  questions=yy$question[ids]
  correct=yy$correct[ids]
  answers=c()
  answers2=c()
  answers3=c()
  answers4=c()
  answers5=c()
  for (i in 1:length(questions)){
    res=predict_3(questions[i],T,c1=k1,c2=k2,c3=k3,c4=k4,c5=k5)
    answers=append(answers,res[[1]])
    answers2=append(answers2,res[[2]])
    answers3=append(answers3,res[[3]])
    answers4=append(answers4,res[[4]])
    answers5=append(answers5,res[[5]])
  }
  protocoll=data.frame(questions,answers,correct,answers2,answers3,stringsAsFactors = F) %>% 
    mutate(match=(answers==correct),
           match2=(answers==correct | answers2==correct),
           match3=(answers==correct | answers2==correct| answers3==correct),
           match4=(answers==correct | answers2==correct| answers3==correct| answers4==correct),
           match5=(answers==correct | answers2==correct| answers3==correct| answers4==correct| answers5==correct))
  x <- (protocoll %>% 
          summarise(accuracy1=mean(match),accuracy2=mean(match2),
                    accuracy3=mean(match3),accuracy4=mean(match4),
                    accuracy5=mean(match5)))
  print(x)
  plot(t(x[1,]),ylim = c(0,1))
  time2 <- Sys.time()
  print((time2-time1)/length(ids))
  protocoll
}

protocoll <- test(k5=1,k4=1,k3=1,k2=1,k1=1)
View(protocoll)
