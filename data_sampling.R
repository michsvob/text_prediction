# Create a smaller version of initial data

con<-file("./final/en_US/en_US.twitter.txt",open = "r")
twit<-readLines(con,n=-1)
close(con)

con2<-file("./final/en_US/en_US.blogs.txt",open = "r")
blog<-readLines(con2,n=-1)
close(con2)

con3<-file("./final/en_US/en_US.news.txt",open = "r")
news<-readLines(con3,n=-1)
close(con3)

set.seed(12345)
sample_size<-100000 #puvodne 50000, psani do slozky sample
i_twit<-sample(1:length(twit),size = sample_size,replace = F)
i_blog<-sample(1:length(blog),size = sample_size,replace = F)
i_news<-sample(1:length(news),size = sample_size,replace = F)

twit_s<-twit[i_twit]
blog_s<-blog[i_blog]
news_s<-news[i_news]

write(blog_s,"./sample3/en_US/en_US.blogs.txt")
write(news_s,"./sample3/en_US/en_US.news.txt")
write(twit_s,"./sample3/en_US/en_US.twitter.txt")


#TEST set
set.seed(1)
i_twit<-sample(1:length(twit),size = sample_size,replace = F)
i_blog<-sample(1:length(blog),size = sample_size,replace = F)
i_news<-sample(1:length(news),size = sample_size,replace = F)
twit_s<-twit[i_twit]
blog_s<-blog[i_blog]
news_s<-news[i_news]
write(c(twit_s,blog_s,news_s),"./test_set/test_set.txt")


#Big Sample

set.seed(2)
sample_size<-500000
i_twit<-sample(1:length(twit),size = sample_size,replace = F)
i_blog<-sample(1:length(blog),size = sample_size,replace = F)
i_news<-sample(1:length(news),size = sample_size,replace = F)

twit_s<-twit[i_twit]
blog_s<-blog[i_blog]
news_s<-news[i_news]

write(blog_s,"./sample2/en_US/en_US.blogs.txt")
write(news_s,"./sample2/en_US/en_US.news.txt")
write(twit_s,"./sample2/en_US/en_US.twitter.txt")



rm(twit)
rm(blog)
rm(news)
rm(con)
rm(con2)
rm(con3)
rm(sample_size)
rm(blog_s)
rm(news_s)
rm(twit_s)
rm(i_blog)
rm(i_news)
rm(i_twit)
