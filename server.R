library(data.table)
source("functions.R")

function(input, output, session) {
   
    withProgress(message = 'Loading model, please wait ...', value = 0, {
       probabilities <- readRDS("./probabilities.750k.750k.500k.dt")
    })

    output$predictions <- DT::renderDataTable(DT::datatable({
        nextWords <- predictNextWords(input$tokens, probabilities, backOff = input$backOff)
        updateTabsetPanel(session, "mainTab", selected = "Prediction table")
        #nextWords <- nextWords[, .(2, 3)]
        nextWords
    }))
    
    
    output$predictionsAbout <- renderText({
      
    })

    output$home <- renderText({

    })
}