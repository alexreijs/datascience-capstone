library(data.table)
source("functions.R")


function(input, output, session) {
  
    defaultTab <- "Home"

    withProgress(message = 'Loading model, please wait ...', value = 0, {
      probabilities <- readRDS("./probabilities.750k.750k.500k.dt")
      #probabilities <- readRDS("./probabilities.5k.dt")
    })
    
    getNextWords <- function() {
        predictNextWords(input$tokens, probabilities)
    }

    output$predictions <- DT::renderDataTable(DT::datatable({
        getNextWords()
    }))

    output$sidePanel = renderUI({
      sidebarPanel(
        h3("Input"),
        textInput("tokens", "", placeholder = "Please enter a string of text here"),
        #checkboxInput("backOff", label = "Allow stupid backoff", value = T),
        width = 4
      )
    })
        
    output$mainTabs = renderUI({
      homePanel <- tabPanel("Home",  htmlOutput("home"))
      aboutPanel <- tabPanel("About", htmlOutput("about"))
      
      if (nchar(input$tokens) > 0) {
        defaultTab <<- "Predictions"
        if (!is.null(getNextWords()))
          predictionPanel <- tabPanel("Predictions", DT::dataTableOutput("predictions"))
        else
          predictionPanel <- tabPanel("Predictions", htmlOutput("noNextWords"))
      }
      else
        predictionPanel <- tabPanel("Predictions", htmlOutput("noInput"))
      
      tabsetPanel(homePanel, predictionPanel, aboutPanel, selected = defaultTab)
    })
    
    output$about <- renderText({
      "<br/><h3>About</h3>
      <div style='height:60px'>This app was created by Alexander Reijs. All of the code is available at:
      <br/><a href='https://github.com/sjakil/datascience-capstone'>https://github.com/sjakil/datascience-capstone</a></div>
      <div style='height:60px'>A slidify presentation of this project is available at:<br/>
      <a href='http://rpubs.com/Sjakil/datascience-capstone'>http://rpubs.com/Sjakil/datascience-capstone</a></div>
      A big thank you goes out to:
      <br/><ul><li>JHU Data Science Course: <a href='https://www.coursera.org/specializations/jhu-data-science'>www.coursera.org/specializations/jhu-data-science</a></li>
      <li>Swiftkey: <a href='https://www.swiftkey.com/en'>www.swiftkey.com</a></li></ul>"
    })

    output$noInput <- renderText({
      "<br/><h3>Please enter some text input</h3>"
    })
    
    output$noNextWords <- renderText({
      "<br/><h3>Could not find any predictions</h3>
      Techniques to work around this problem do exists, unfortunately I ran out of time to implement them"
    })
    
    output$home <- renderText({
      "<br/><h3>Welcome to the Data Science Capstone Project</h3>
      <br/>This app tries to predict the next word you will most likely want to type!
      <br/>To use it, simply enter a string of text into the input field on the left.
      <br/>Once input has been entered, a table with prediction(s) should appear."
    })
}