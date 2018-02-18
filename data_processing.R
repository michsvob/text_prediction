# create corpora using tm package
library(tm)
corp <- VCorpus(DirSource("./sample/en_US/",encoding = "UTF-8"))
#corp <- VCorpus(DirSource("./sample2/en_US/",encoding = "UTF-8"))
#corp <- VCorpus(DirSource("./test_set/",encoding = "UTF-8"))

profanities<-read.table("profanities.txt",stringsAsFactors = F,strip.white = T,sep = "\n")[,1] #source: "https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en"
skipWords <- function(x) removeWords(x,profanities)
remPunct <- function(x) removePunctuation(x,ucp=TRUE)#  (when ucp == False, ” are not removed)
removeNonWesternCharacters <- content_transformer(function(x) {
  id <- !is.na(iconv(x, from = "UTF-8", to = "WINDOWS-1252")) 
  print(paste("removed",as.character(length(x)-sum(id)),"lines"))
  x[id]
}) # removal of lines with non-Western characters 

funs <- list(skipWords,
             stripWhitespace,
             remPunct,
             removeNumbers,
             content_transformer(tolower),
             removeNonWesternCharacters)

corp <- tm_map(corp,FUN=tm_reduce,tmFuns=funs)
flatcorp <- c(corp[[1]][[1]],corp[[2]][[1]],corp[[3]][[1]])#make just a char vector by appending document contents

library(quanteda)
library(RWeka)
library(stringr)
library(dplyr)

#Clustering, tokenisation, handling of unknown words
#---------------------------------------------------
single_words <- NGramTokenizer(flatcorp,control=Weka_control(min=1,max=1))
single_words <- tibble(feature=single_words) %>% group_by(feature) %>% summarise(freq=n())

unks <- (single_words %>% filter(freq<3,freq>1) %>% select(feature))$feature
#unique words do not need to be checked because only tokens with frequency >1 have been filtered
#Handling unknown words
#Replace all words that occur as unigram rarely by <unk>
replace_unknowns <- function(features){
  split <- str_split(features,pattern = "_",simplify=TRUE) 
  indexes <- split %in% unks
  split[indexes] <- "<unk>"
  str_trim(apply(split, 1, paste,collapse=" "))  
}

freqs <- textstat_frequency(dfm(tokens(flatcorp,ngrams=2:5))) %>% filter(frequency>1)
ngramdb <- tibble(feature=freqs$feature,frequency=as.integer(freqs$frequency))
ngramdb <- ngramdb %>% transmute(
  grams=as.integer(1+str_count(feature,pattern="_")),
  prediction=str_extract(feature,pattern="[^_]+$"),
  predictor=sub(feature,pattern = "_[^_]+$",replacement =""),
  predictor=replace_unknowns(predictor),
  frequency)

knowns <- unique(as.character(str_split(ngramdb$predictor," ",simplify = T)))

#calculate conditional probabilities of predictor given the prediction
totals <- ngramdb %>% group_by(predictor) %>% summarise(tot=sum(frequency))
ngramdb <- ngramdb %>% left_join(totals) %>% mutate(condprob=frequency/tot) %>% select(-tot)


#saveRDS(ngramdb,"./training_set/ngramdb_big.rds")
saveRDS(ngramdb,"./training_set/ngramdb.rds")
saveRDS(knowns,"./training_set/knowns.rds")
#saveRDS(ngramdb,"./test_set/ngramdb.rds")


#x <- table(flatten(gr3)) #Distrubution of occuration counts
#y <- as.numeric(names(x))*x #Weighting
#y_dist <- ecdf(y)
#plot(y_dist)

