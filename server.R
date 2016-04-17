library(data.table)
source("functions.R")

function(input, output) {
   
    withProgress(message = 'Loading model ...', value = 0, {
       probabilities <- readRDS("./probabilities.750k.750k.500k.dt")
    })

    output$predictions <- DT::renderDataTable(DT::datatable({
        nextWords <- predictNextWords(input$tokens, probabilities)
        #nextWords <- nextWords[, .(2, 3)]
        nextWords
    }))
    
    
    output$predictionsAbout <- renderText({
      
    
    })
}