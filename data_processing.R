# create corpora using tm package
library(tm)
corp <- VCorpus(DirSource("./sample/en_US/",encoding = "UTF-8"))
#corp <- VCorpus(DirSource("./sample2/en_US/",encoding = "UTF-8"))
#corp <- VCorpus(DirSource("./test_set/",encoding = "UTF-8"))

corp <- tm_map(corp,content_transformer(tolower)) # to lowercase | improve: Correction for names
corp <- tm_map(corp,removePunctuation, ucp=TRUE) # remove punctuation (when ucp == False, ” are not removed)
corp <- tm_map(corp,removeNumbers) # remove numbers
corp <- tm_map(corp, content_transformer(function(x) {
  id <- !is.na(iconv(x, from = "UTF-8", to = "WINDOWS-1252")) 
  print(paste("removed",as.character(length(x)-sum(id)),"lines"))
  x[id]
})) # removal of lines with non-Western characters 
corp <- tm_map(corp,stripWhitespace) # remove extra whitespace
#profanities<-read.table("profanities.txt",stringsAsFactors = F)[,1] #source: "https://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
#tm_map(corp,removeWords,words=profanities) #SLOW!

# do clustering - 1-gram, 2-gram, 3-gram using weka 
# clusters appearing just once will be ignored
library(RWeka)
gr1 <- TermDocumentMatrix(corp,control = list(tokenize=function(x){
                            NGramTokenizer(x,control=Weka_control(min=1,max=1))
                          },
                          bounds=list(local=c(1,Inf))))

gr2 <- TermDocumentMatrix(corp,control = list(tokenize=function(x){
                                   NGramTokenizer(x,control=Weka_control(min=2,max=2))
                                   },
                                   bounds=list(local=c(1,Inf)),
                                   stopwords=TRUE))
                                   

gr3 <- TermDocumentMatrix(corp,control = list(tokenize=function(x){
                                   NGramTokenizer(x,control=Weka_control(min=3,max=3))
                                 },
                                 bounds=list(local=c(1,Inf))))

gr4 <- TermDocumentMatrix(corp,control = list(tokenize=function(x){
                                NGramTokenizer(x,control=Weka_control(min=4,max=4))
                               },
                               bounds=list(local=c(1,Inf))))

gr5 <- TermDocumentMatrix(corp,control = list(tokenize=function(x){
  NGramTokenizer(x,control=Weka_control(min=5,max=5))
},
bounds=list(local=c(1,Inf))))

flatten <- function(tdm){
  #summing up TermDocumentMatrices
  rowSums(as.matrix(tdm))
}

gr <- list(gr1,gr2,gr3,gr4,gr5)

library(stringr)
library(dplyr)

ngramdb <-  tibble(grams=numeric(),freq=numeric(),fourth=character(),third=character(),second=character(),first=character(),pred=character())

for(i in 1:5){
  flat <- flatten(gr[[i]])
  split=str_split_fixed(names(flat),pattern = " ",n = 5)
  gram_tib <-  tibble(
    grams=i,
    freq=flat, # frequency of occurance in the given order (n in ngram)
                      fourth=split[,c(2,3,4,5,1)[i]],
                      third=split[,c(3,4,5,1,2)[i]], # third
                      second=split[,c(4,5,1,2,3)[i]], # second predictor (2 words before last)
                      first=split[,c(5,1,2,3,4)[i]], # first predictor (penultimate word)
                      pred=split[,i] #last word - prediction
                      )
  ngramdb <- bind_rows(ngramdb,gram_tib)
}


saveRDS(ngramdb,"./training_set/ngramdb_big.rds")

#saveRDS(ngramdb,"./test_set/ngramdb.rds")


#x <- table(flatten(gr3)) #Distrubution of occuration counts
#y <- as.numeric(names(x))*x #Weighting
#y_dist <- ecdf(y)
#plot(y_dist)

