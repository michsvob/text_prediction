#source("data_sampling.R")
#source("data_processing.R")

ngramdb <- readRDS("./training_set/ngramdb.rds")

library(stringr)
library(dplyr)


predict_2 <- function(input,alternatives=F,c5=2000000,c4=250000,c3=18000,c2=100,c1=1){
  input= str_to_lower(gsub("//s"," ",input)) # lowercase and strip extra whitespace
  input = tail(str_split(input," ")[[1]],4)
  input=c(rep("",4-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(grams==5,fourth==input[1],third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c5*sum(freq))
    #ngrams are up-voted by factor c
    #arrange(desc(f)) 
    #top_n(1,freq) %>% 
  
  score5=0
  if(nrow(gr5)>0){
    score5=top_n(gr5,1,score)[1,2]
  }
  
  gr4=ngramdb %>% 
    filter(grams==4,third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c4*sum(freq)) 
    #arrange(desc(f)) 
    #top_n(1,freq) %>% 
  
  
  score4=0
  if(nrow(gr4)>0){
    score4=top_n(gr4,1,score)[1,2]
  }
  
  gr3=ngramdb %>% 
    filter(grams==3,second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c3*sum(freq))
    #arrange(desc(f))  
    #top_n(1,freq) %>% 
  
  
  score3=0
  if(nrow(gr3)>0){
    score3=top_n(gr3,1,score)[1,2]
  }
  
  gr2=ngramdb %>% 
    filter(grams==2,first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c2*sum(freq)) 
    #arrange(desc(f)) 
    #top_n(1,freq) %>% 
  
  score2=0
  if(nrow(gr2)>0){
    score2=top_n(gr2,1,score)[1,2]
  }
  
  gr1=ngramdb %>% 
    filter(grams==1, pred %in% gr2$pred) %>% 
    group_by(pred) %>% summarise(score=c1*sum(freq)) 
  #arrange(desc(f)) 
  #top_n(1,freq) %>% 
  
  
  joined <- bind_rows(gr5,gr4,gr3,gr2,gr1) %>%
    group_by(pred) %>% summarise(score=sum(score,na.rm=TRUE)) %>% top_n(10,score) %>% 
    arrange(desc(score))
  
  if(nrow(joined)==0){
    pred="the"
  }
  else{
    pred=joined[1,1]
  }
  if (alternatives){
    print(arrange(gr5,desc(score)))
    print(arrange(gr4,desc(score)))
    print(arrange(gr3,desc(score)))
    print(arrange(gr2,desc(score)))
    print(arrange(gr1,desc(score)))
    #print(joined)
  }
  c(as.character(pred),score5,score4,score3,score2)
}

test_set="./test_set/ngramdb.rds"
yy<- readRDS(test_set)
yy <- yy %>% filter(grams==5) %>% transmute(question=paste(fourth,third,second,first),correct=pred)
ids <- sample(1:nrow(yy),600)

test <- function(k1,k2,k3,k4,k5){

  #ids <- 1:length(yy) 
  #######################
  #######################
  #######################
  #######################
  questions=yy$question[ids]
  correct=yy$correct[ids]
  answers=c()
  scores2=numeric()
  scores3=numeric()
  scores4=numeric()
  scores5=numeric()
  for (i in 1:length(questions)){
    res=predict_2(questions[i],F,c1=k1,c2=k2,c3=k3,c4=k4,c5=k5)
    answers=append(answers,res[[1]])
    scores5=append(scores5,as.numeric(res[[2]]))
    scores4=append(scores4,as.numeric(res[[3]]))
    scores3=append(scores3,as.numeric(res[[4]]))
    scores2=append(scores2,as.numeric(res[[5]]))
  }
  #answers=sapply(FUN = predict_2,X = questions,F,c1=k1,c2=k2,c3=k3,c4=k4,c5=k5)
  protocoll=data.frame(questions,answers,correct,scores5,scores4,scores3,scores2,stringsAsFactors = F) %>% mutate(match=(answers==correct))
  print(protocoll %>% summarise(accuracy=mean(match)))
  protocoll
}

protocoll <- test(k5=2000000,k4=500000,k3=18000,k2=100,k1=0.01)
#0.46 protocoll <- test(k5=2000000,k4=250000,k3=18000,k2=100,k1=1)
#0.485 protocoll <- test(k5=2000000,k4=250000,k3=18000,k2=100,k1=0)
#0.485 protocoll <- test(k5=2000000,k4=250000,k3=18000,k2=100,k1=0.01)
#0.505 protocoll <- test(k5=2000000,k4=500000,k3=18000,k2=100,k1=0.01)
View(protocoll)
model <- lm(match~scores5+scores4+scores3+scores2,data=protocoll)

optimize <- function(optset){
  c5min=200000;c4min=50000;c3min=10000;c2min=100;c1min=1
  c5max=20000000;c4max=5000000;c3max=1000000;c2max=1000;c1max=10
  minres <- test(c1min,c2min,c3min,c4min,c5min)
  maxres <- test(c1max,c2min,c3min,c4min,c5min)
  x <- test(c1min/2+c2min/2,c2min,c3min,c4min,c5min)
  print(minres)
  print(x)
  print(maxres)
}
  
optimize(yy)
