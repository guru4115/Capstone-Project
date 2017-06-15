# server.R ####
# Coursera Data Science Capstone Project (https://www.coursera.org/course/dsscapstone)
# Shiny server script
# 2016-01-23

# Libraries and options ####
source('prediction.R')
df1gram<-readRDS("./df1gram.rds")
df2gram<-readRDS("./df2gram.rds")
df3gram<-readRDS("./df3gram.rds")
profanity<-readRDS("./profanity.rds")
library(shiny)

# Define application ####

shinyServer(function(input, output){
      

        inputText<-eventReactive(input$predictButton,
                         {input$text})
        processedInput <- eventReactive(input$predictButton,
                                        {processInput(input$text)})
        
        
        # Output ####

        output$sugesstion1 <- renderPrint(
                withProgress(message = 'Predicting', value = 0,{
                        {nextword(inputText(),df1gram,df2gram,df3gram)}}))
        
        output$inputSentence <- renderPrint({processedInput()})

})