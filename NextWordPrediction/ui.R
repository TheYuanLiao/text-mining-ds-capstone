#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that predicts next word based on the input
shinyUI(fluidPage(

    # Application title
    titlePanel("Next Word Prediction"),
    h4("Author: Yuan Liao"),
    h4("Date: 11/14/2020"),
    sidebarLayout(
        sidebarPanel(
            textAreaInput("text", "Type something, e.g., I love", ""),
            actionButton("predictButton", "Predict")
        ),
        
        mainPanel(
            h4(htmlOutput("hint", container = span)),
            h5(htmlOutput("text", container = span))
            )
    )
)
)
