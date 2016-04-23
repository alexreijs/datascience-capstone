library(data.table)

shinyUI(fluidPage(
    
    titlePanel("Data Science - Swiftkey Capstone Project"),
    
    sidebarLayout(
        sidebarPanel(
            h3("Input"),
            textInput("tokens", "", placeholder = "Please enter a string of text here"),
            checkboxInput("backOff", label = "Use stupid backoff?", value = T),
            hr()
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Home",  textOutput("home")),
            tabPanel("Prediction table",  DT::dataTableOutput("predictions")),
            tabPanel("About", textOutput("predictionsAbout")),
            id = "mainTab",
            position = "left"
          )
        )
    )
))