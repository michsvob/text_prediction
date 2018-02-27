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

predict_3 <- function(input,alternatives=T,c5=1,c4=1,c3=1,c2=1,c1=1){
  input=stripWhitespace(removeNumbers(removePunctuation(input)))
  input= str_to_lower(gsub("^ * | $ *","",input)) # lowercase and strip extra whitespace before and after
  input = tail(str_split(input," ")[[1]],4)
  input=replace(input,!(input %in% knowns),"<unk>")
  input=c(rep("",4-length(input)),input)
  
  gr5=ngramdb %>% 
    filter(grams==5,predictor==paste(input,collapse=" ")) 
  #ngrams are up-voted by factor c
  
  gr4=ngramdb %>% 
    filter(grams==4,predictor==paste(input[2:4],collapse=" ")) 
  
  gr3=ngramdb %>% 
    filter(grams==3,predictor==paste(input[3:4],collapse=" "))
  
  gr2=ngramdb %>% 
    filter(grams==2,predictor==paste(input[4],collapse=" "))

  pred <- (bind_rows(gr5,gr4,gr3,gr2) %>% 
             group_by(prediction) %>% summarise(score=sum(condprob,na.rm=TRUE)) %>% top_n(((alternatives*4)+1),score) %>% 
             arrange(desc(score)))
  #top_n is here for performance reasons
  
  scores <- pred$score
  predictions <- pred$prediction
  
  if(length(predictions)<5){
    predictions=head(append(predictions,c('the', 'to','and', 'a','of')),5)
    scores=head(append(scores,c(0,0,0,0,0)),5) 
  }
  list(predictions,scores)
}
