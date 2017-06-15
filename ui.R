# ui.R ####
# Shiny UI script

# Libraries and options ####
library(shiny)
library(shinythemes)
shinyUI(navbarPage("Capstone Project",
# Define the app ####
theme = shinytheme("superhero"),

tabPanel("Application",
 
        fluidRow(column(6,offset = 4,
                br(),      
                textInput("text", h4("Enter text here:"), value=" "),
                br(),      
                actionButton("predictButton", "Predict"),
                br(),
                h4("Processed Inserted Sentence"),
                h4(textOutput("inputSentence")),
                br(),
                h4("The predicted next word is:"),
                h4(textOutput("sugesstion1")
   
        )


        ))
        
        
        
),
#####about the application
tabPanel("Using The Application",
         h2("Next Word Prediction"),
         br(),
         h4("1. Insert the sentence to predict the next word and click predict", style = "font-family: 'times'; font-si16pt"),
         h4("2. Wait for awhile until the predicting box at the bottom to disappear(5-10Seconds)", style = "font-family: 'times'; font-si16pt"),
         h4("3. The input is processed(cleaned and remove profanity words) and displayed at the 'Processed Inserted Sentence'", style = "font-family: 'times'; font-si16pt"),
         h4("4. The predicted word is displayed at the 'The predicted next word is:'", style = "font-family: 'times'; font-si16pt"),
         
         br()

        )
)

)

   
        
             


