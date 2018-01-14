if(!file.exists("data0.zip")){
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                "data0.zip",
                method="auto")
  unzip("data0.zip")
}
library(tm)
library(stringr)

news<-readLines("./final/en_US/en_US.news.txt")
blogs<-readLines("./final/en_US/en_US.blogs.txt")
twitter<-readLines("./final/en_US/en_US.twitter.txt")

saveRDS(twitter,"twitter")
saveRDS(news,"news")
saveRDS(blogs,"blogs")

news<-readRDS("news")
blogs<-readRDS("blogs")
twitter<-readRDS("twitter")

#Q1
file.size("./final/en_US/en_US.blogs.txt")/1024^2
file.size("./final/en_US/en_US.twitter.txt")/1024^2

#Q3
max(sapply(blogs,str_length))

#Q4
sum(grepl("love",twitter))/sum(grepl("hate",twitter))

#Q5
twitter[grep("biostat",twitter)]

#Q6
sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing",twitter))

