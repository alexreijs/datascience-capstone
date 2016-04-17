library(data.table)

shinyUI(fluidPage(
    
    titlePanel("Data Science - Swiftkey Capstone Project"),
    
    sidebarLayout(
        sidebarPanel(
            h3("Input"),
            textInput("tokens", "", placeholder = "Please enter a string of text here"),
            hr()
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Prediction table",  DT::dataTableOutput("predictions")),
            tabPanel("About", textOutput("predictionsAbout"))
          )
        )
    )
))