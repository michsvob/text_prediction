#source("data_sampling.R")
#source("data_processing.R")

library(stringr)
library(dplyr)
library(tm)

ngramdb <- readRDS("./training_set/ngramdb.rds")
ngramdb <- readRDS("./training_set/ngramdb_big.rds")

#Smoothing:
#Replace all words that occur less than 4times as unigram by <unk>

unks <- (ngramdb %>% filter(grams==1,freq<3) %>% select(pred))$pred

ngramdb$pred <- replace(x = ngramdb$pred,list = ngramdb$pred %in% unks,"<unk>")
ngramdb$fourth <- replace(x = ngramdb$fourth,list = ngramdb$fourth %in% unks,"<unk>")
ngramdb$third <- replace(x = ngramdb$third,list = ngramdb$third %in% unks,"<unk>")
ngramdb$second <- replace(x = ngramdb$second,list = ngramdb$second %in% unks,"<unk>")
ngramdb$first <- replace(x = ngramdb$first,list = ngramdb$first %in% unks,"<unk>")

ngramdb <- ngramdb %>% filter(grams>1) %>% group_by(grams,fourth,third,second,first,pred) %>% 
  summarise(freq=sum(freq)) %>% ungroup()%>% filter(freq>1, pred!="<unk>") 

knowns <- unique(c(ngramdb$fourth,ngramdb$third,ngramdb$second,ngramdb$first,ngramdb$pred))

#Get number of records for n-gram groups *NOT USED SO FAR
tot_grams <- ngramdb %>% group_by(grams) %>% summarise(sum(freq))

#Add conditional probabilities
totals <- ngramdb %>% group_by(grams,first,second,third,fourth) %>% summarise(tot=sum(freq))
ngramdb <- ngramdb %>% left_join(totals) %>% mutate(condprob=freq/tot) %>% select(-tot)

get_score <- function(input){
  input= str_to_lower(gsub("//s"," ",input)) # lowercase and strip extra whitespace
  input = tail(str_split(input," ")[[1]],5)
  input=c(rep("",5-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(pred==input[5],grams==5,fourth==input[1],third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=sum(condprob)) %>% select(score)
  
  score5=0
  if(nrow(gr5)>0){
    score5=gr5[[1,1]]
  }
  
  gr4=ngramdb %>% 
    filter(pred==input[5],grams==4,third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=sum(condprob)) %>% select(score)
  
  score4=0
  if(nrow(gr4)>0){
    score4=gr4[[1,1]]
  }
  
  gr3=ngramdb %>%
    filter(pred==input[5],grams==3,second==input[3],first==input[4]) %>%
    group_by(pred) %>% summarise(score=sum(condprob))%>% select(score)

  score3=0
  if(nrow(gr3)>0){
   score3=gr3[[1,1]]
  }
  
  gr2=ngramdb %>%
    filter(pred==input[5],grams==2,first==input[4]) %>%
    group_by(pred) %>% summarise(score=sum(condprob))%>% select(score)

  score2=0
  if(nrow(gr2)>0){
    score2=gr2[[1,1]]
  }

  gr1=ngramdb %>%
    filter(grams==1, pred ==input[5]) %>%
    group_by(pred) %>% summarise(score=sum(condprob))%>% select(score)
  
  score1=0
  if(nrow(gr1)>0){
    score1=gr1[[1,1]]
  }

  c(score1,score2,score3,score4,score5)
}

dev_set_addr="./test_set/ngramdb.rds"
dev_set<- readRDS(dev_set_addr)
dev_set <- dev_set %>% filter(grams==5) %>% transmute(question=paste(fourth,third,second,first,pred))
set.seed(12)
ids_dev <- sample(1:nrow(dev_set),600)

optimise_coeffs <- function(){
  # The prediction model will be following: 
  # P_hat(W|W-1,W-2...)=P(W|W-1,...W-k+1) + lambda*P(W|W-1...W-k).
  # 1. Run function get_score on the development set
  # 2. Sort the results to groups depending on the longest token found in the ngram database (k)
  # 3. Optimise the lambda coefficients for the given groups.
  #
  
  r <- sapply(FUN = get_score,X = dev_set$question[ids_dev][1:100])
  
  groups <- vector(mode = "numeric",length = ncol(r))
  for (i in 1:ncol(r)){
    for(j in 5:1){
      if(r[j,i]!=0){
        groups[i]=j
        break
      }
    }
  }
  
  r[,groups==5]
}

#Function to predict next word
#Principle: Interpolation - word is guessed based on weighted conditional 
#probabilities of ngrams of size 1-5
predict_2 <- function(input,alternatives=T,c5=1,c4=1,c3=1,c2=1,c1=1){
  input=stripWhitespace(removeNumbers(removePunctuation(input)))
  input= str_to_lower(gsub("^ * | $ *","",input)) # lowercase and strip extra whitespace before and after
  
  input = tail(str_split(input," ")[[1]],4)
  input=replace(input,!(input %in% knowns),"<unk>")
  input=c(rep("",4-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(grams==5,fourth==input[1],third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c5*sum(condprob))
    #ngrams are up-voted by factor c
  
  score5=0
  if(nrow(gr5)>0){
    score5=top_n(gr5,1,score)[1,2]
  }
  
  gr4=ngramdb %>% 
    filter(grams==4,third==input[2],second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c4*sum(condprob)) 
  
  score4=0
  if(nrow(gr4)>0){
    score4=top_n(gr4,1,score)[1,2]
  }
  
  gr3=ngramdb %>% 
    filter(grams==3,second==input[3],first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c3*sum(condprob))
  
  score3=0
  if(nrow(gr3)>0){
    score3=top_n(gr3,1,score)[1,2]
  }
  
  gr2=ngramdb %>% 
    filter(grams==2,first==input[4]) %>% 
    group_by(pred) %>% summarise(score=c2*sum(condprob)) 
  
  score2=0
  if(nrow(gr2)>0){
    score2=top_n(gr2,1,score)[1,2]
  }
  
  #gr1=ngramdb %>% 
  #  filter(grams==1, pred %in% gr2$pred) %>% 
  #  group_by(pred) %>% summarise(score=c1*sum(condprob)) 
  
  pred <- (bind_rows(gr5,gr4,gr3,gr2) %>% #,gr1) %>%
    group_by(pred) %>% summarise(score=sum(score,na.rm=TRUE)) %>% top_n(((alternatives*2)+1),score) %>% 
    arrange(desc(score)))$pred
  
  if(length(pred)<3){
    pred=head(append(pred,c('the', 'on', 'a')),3)
  }
  pred
}

test_set="./test_set/ngramdb.rds"
yy<- readRDS(test_set)
yy <- yy %>% filter(grams==5) %>% transmute(question=paste(fourth,third,second,first),correct=pred)
ids <- sample(1:nrow(yy),500)

test <- function(k1,k2,k3,k4,k5){

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
  for (i in 1:length(questions)){
    res=predict_2(questions[i],T,c1=k1,c2=k2,c3=k3,c4=k4,c5=k5)
    answers=append(answers,res[[1]])
    answers2=append(answers2,res[[2]])
    answers3=append(answers3,res[[3]])
  }
  protocoll=data.frame(questions,answers,correct,answers2,answers3,stringsAsFactors = F) %>% mutate(match=(answers==correct))
  print(protocoll %>% summarise(accuracy=mean(match)))
  protocoll
}

protocoll <- test(k5=1,k4=1,k3=1,k2=1,k1=1)
View(protocoll)
