library(data.table)

shinyUI(fluidPage(
    
    titlePanel("Data Science - Capstone Project"),
    
    sidebarLayout(
        uiOutput("sidePanel"),
        mainPanel(
          uiOutput('mainTabs') 
        )
    )
))