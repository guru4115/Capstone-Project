
###############readfiles##################################################

library(tidyr)
library(dplyr)
library(quanteda)
library(caTools)
library(R.utils)
#Path
blogsPath <- "./final/en_US/en_US.blogs.txt"
newsPath <- "./final/en_US/en_US.news.txt"
twitterPath <- "./final/en_US/en_US.twitter.txt"

#read data into R
blogs <- readLines(blogsPath, encoding = "UTF-8", skipNul=TRUE)
news <- readLines(newsPath, encoding = "UTF-8", skipNul=TRUE)
twitter <- readLines(twitterPath,encoding = "UTF-8", skipNul=TRUE)


#save for easier loading
saveRDS(blogs,"./blogs.rds")
saveRDS(news,"./news.rds")
saveRDS(twitter,"./twitter.rds")
#loadingfiles
blogs<-readRDS("./blogs.rds")
news<-readRDS("./news.rds")
twitter<-readRDS("./twitter.rds")

###countlines
blogsLength <- countLines(blogsPath)
newsLength <- countLines(newsPath)
twitterLength <- countLines(twitterPath)

saveRDS(blogsLength,"./blogsLength.rds")
saveRDS(newsLength,"./newsLength.rds")
saveRDS(twitterLength,"./twitterLength.rds")

blogsLength<-readRDS("./blogsLength.rds")
newsLength<-readRDS("./newsLength.rds")
twitterLength<-readRDS("./twitterLength.rds")
##############################read files########################################
###################sampling and create train data############################
#set.seed(101)
blogsSample <- blogs[sample(1:blogsLength,blogsLength*0.01)]
newsSample <- news[sample(1:newsLength,newsLength*0.01)]
twitterSample <- blogs[sample(1:twitterLength,twitterLength*0.01)]
blogsSample<-blogsSample[!is.na(blogsSample)]
newsSample<-newsSample[!is.na(newsSample)]
twitterSample<-twitterSample[!is.na(twitterSample)]

saveRDS(blogsSample,"./blogsSample.rds")
saveRDS(newsSample,"./newsSample.rds")
saveRDS(twitterSample,"./twitterSample.rds")

blogsSample<-readRDS("./blogsSample.rds")
newsSample<-readRDS("./newsSample.rds")
twitterSample<-readRDS("./twitterSample.rds")

sample <- c(twitterSample,newsSample,blogsSample)
saveRDS(sample,"./sample.rds")
sample<-readRDS("./sample.rds")


# Split into train and validation sets
split = sample.split(sample, 0.8)
train = subset(sample, split == T)
valid = subset(sample, split == F)
saveRDS(train,"./train.rds")
saveRDS(valid,"./valid.rds")

train<-readRDS("./train.rds")
######################sampling and create train data########################################################
#####profanity######################################################################

#profanity <- read.table("./Terms-to-Block.csv", sep = ",")
#profanity<- as.character(profanity[5:727,2])
#profanity <- gsub(",", "", profanity)
#saveRDS(profanity,"./profanity.rds")

###################################### Tokenization #############################################
### do all cleaning here and tokenize
profanity<-readRDS("./profanity.rds")
tokenizer = function(x, ngramSize = 1) {
        tokens_select(
                tokens_tolower(
                        tokens(x,"word", 
                               remove_numbers = TRUE, remove_punct = TRUE,
                               remove_symbols=TRUE,remove_separators=TRUE, remove_hyphens=TRUE, 
                               remove_url=TRUE,ngrams=ngramSize, concatenator = " " ,verbose=TRUE)),profanity,"remove")
        
}
##create cleanded ngram and save
train1 <- tokenizer(train)
saveRDS(train1,"./train1gram.rds")
train2<-tokens_ngrams(train1,2,concatenator = " ")
saveRDS(train2,"./train2gram.rds")
train3<-tokens_ngrams(train1,3,concatenator = " ")
saveRDS(train3,"./train3gram.rds")
########################################Tokenization################################
#####################make data frame of ngram frequency####################################################



train1<-readRDS("./train1gram.rds")
train2<-readRDS("./train2gram.rds")
train3<-readRDS("./train3gram.rds")
freqfunc<- function(x){
                df <- dfm(x,verbose = TRUE)
        df <- data.frame(ngram = featnames(df), freq = colSums(df), 
                         row.names = NULL, stringsAsFactors = FALSE)
        arrange(df,desc(freq))
}



dftrain1<-freqfunc(train1)
dftrain2<-freqfunc(train2) 
dftrain3<-freqfunc(train3)

#####further cleaning, remove 
furthercleaning<-function(x)
{ x[!grepl("[[:punct:]]|[[:digit:]]",x$ngram),]}
df1gram<-furthercleaning(dftrain1)
df2gram<-furthercleaning(dftrain2)
df3gram<-furthercleaning(dftrain3)
####save
saveRDS(df1gram,"./df1gram.rds")
saveRDS(df2gram,"./df2gram.rds")
saveRDS(df3gram,"./df3gram.rds")
#################################make data frame of ngram frequency##############

########load ngramdf
df1gram<-readRDS("./df1gram.rds")
df2gram<-readRDS("./df2gram.rds")
df3gram<-readRDS("./df3gram.rds")



























