# prediction.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Script for predicting a NextWord given an input of any length using stupid backoff algorithm
# 2016-01-23
# Libraries and options ####
library(quanteda)
library(stringr)
library(dplyr)
library(knitr)
source('mydata.R')

# Tokenize ####
tokenizer = function(x, ngramSize = 1) {
        tokens_select(
                tokens_tolower(
                        tokens(x,"word", 
                               remove_numbers = TRUE, remove_punct = TRUE,
                               remove_symbols=TRUE,remove_separators=TRUE, remove_hyphens=TRUE, 
                               remove_url=TRUE,ngrams=ngramSize, concatenator = " " ,verbose=TRUE)),profanity,"remove")
        
}

processInput <- function(inputText) {
        inputText<-tokenizer(inputText)
        inputTokens <- unlist(str_split(inputText, " "))
        processedInput<- combine_words(inputTokens,sep=" ",and = " ")
        if(length(processedInput) == 0) {
                processedInput<- "Please insert valid input"
        }
        
        return(processedInput)
}

nextword<-function(inputText,df1gram=df1gram,df2gram=df2gram,df3gram=df3gram)
        {
        ###Process input sentence
        inputText<-tokenizer(inputText)
        inputTokens <- unlist(str_split(inputText, " "))
        if(length(inputTokens) > 1) 
        {
                lastIndex <- length(inputTokens)
                firstInput <- inputTokens[(lastIndex-1)]
                secondInput <- inputTokens[(lastIndex)] 
                bothInput<-paste(firstInput, secondInput, sep=" ")
        }else
        {
                prediction<-"Please enter at least 2 words for prediction"
        }   
        ##get observed 3-gram
        regex <- sprintf("%s%s%s", "^", bothInput, "")
        Observed3grams<-df3gram[grepl(regex, df3gram$ngram),]
        ### get unoboserved 3 gram tails      
        observed3gramsTails<- str_split_fixed(Observed3grams$ngram, " ", 3)[, 3]
        unobserved3gramsTails<-df1gram$ngram[!df1gram$ngram %in% observed3gramsTails]
        ## get backed off 2-gram
        backedOff2gram<-paste(secondInput, unobserved3gramsTails, sep = " ")
        ## get observed backedoff 2-gram
        observedbackedOff2gram <- df2gram[df2gram$ngram %in% backedOff2gram, ]
        #### get unobserved backed off 2-gram
        unobservedbackedOff2gram <- backedOff2gram[!(backedOff2gram %in% observedbackedOff2gram$ngram)]
        ## get observed backedoff 2-gram probablility disount set to 0.5
        ## this is if found in 2-gram table
        firstWords <- str_split_fixed(observedbackedOff2gram$ngram, " ", 2)[, 1]
        firstWordfreq <- df1gram[df1gram$ngram %in% firstWords, ]
        observed2gramProb <- (observedbackedOff2gram$freq - 0.5) / firstWordfreq$freq
        #*** calc alpha2gram and unobserved 2-gram probablility 
        # this is when not found in 2-gram table
        #1-gram is accounted
        unobserved2gram<- str_split_fixed(unobservedbackedOff2gram, " ", 2)[, 2]
        unobserved2gramTails<-df1gram[!(df1gram$ngram %in% unobserved2gram), ]
        unobserved2gramProb <- df1gram[df1gram$ngram %in% unobserved2gram, ]
        freqSum <- sum(unobserved2gramProb$freq)
        
        #calculate alpha2gram
        regex <- sprintf("%s%s%s", "^", secondInput, " ")
        bigramtStartingSecondword <- df2gram[grep(regex, df2gram$ngram),]
        if(length(bigramtStartingSecondword$ngram) == 0) 
        {
        alpha2gram<- 0
        }else
        {
        unigram <- df1gram[df1gram$ngram == secondInput,]
        alpha2gram <- 1 - (sum(bigramtStartingSecondword$freq - 0.5) / unigram$freq)
        }
        
        unobserved2gramProb <- data.frame(ngram=unobservedbackedOff2gram,
                                   prob=(alpha2gram * unobserved2gramProb$freq / freqSum))
        
        ##get observed 3-gram probabilities in 2-gram table
        observedCount2gram <- filter(df2gram, ngram==bothInput)$freq
        observed3gramProb <- mutate(Observed3grams, freq=((freq - 0.5) / observedCount2gram))
        colnames(observed3gramProb) <- c("ngram", "prob")
        #########################
        #get unobserved 3-gram probability 
        observed2gramProb<-data.frame(ngram=observedbackedOff2gram$ngram, prob=observed2gramProb)
        bigramProb <- rbind(observed2gramProb, unobserved2gramProb)
        bigramProb <- bigramProb[order(bigramProb$prob,decreasing = TRUE), ]
        bigramProbSum <- sum(bigramProb$prob)
        newUnobserved3grams <- paste(firstInput, bigramProb$ngram, sep=" ")
        ####get alpha3gram
        bigram <- df2gram[df2gram$ngram %in% bothInput, ]
        if(length(Observed3grams)==0) 
          {     alpha3gram <- 1
        }else
        {
        alpha3gram <- 1 - sum((Observed3grams$freq - 0.5) / bigram$freq)
        }
        newUnobserved3gramsProbs <- alpha3gram * bigramProb$prob / bigramProbSum
        unobsTrigDf <- data.frame(ngram=newUnobserved3grams, prob=newUnobserved3gramsProbs)
        
       trigramProb <- rbind(unobsTrigDf, observed3gramProb)
        
       trigramProb <- trigramProb[order(trigramProb$prob,decreasing = TRUE), ]
       prediction <- str_split(trigramProb$ngram[1], " ")[[1]][3]

        return(prediction)    
        
}

