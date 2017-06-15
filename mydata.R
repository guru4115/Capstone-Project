# Libraries and options ####
library(shiny)

# Load data  ####
df1gram<-readRDS("./df1gram.rds")
df2gram<-readRDS("./df2gram.rds")
df3gram<-readRDS("./df3gram.rds")
profanity<-readRDS("./profanity.rds")