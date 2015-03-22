library(shiny)
# library(plyr)

# Define UI for Bank of England Base Rate History application
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Bank of England Base Rate Viewer", 
                windowTitle="Bank of England Base Rate Changes Plotter Application"
    ),
    
    sidebarPanel(
        h4("Enter a year (format YYYY) to view the BoE base rate changes since that year", 
           col="red"),
        
        numericInput(inputId='YearID', label="Enter a year (data from 1694 onwards)", 
                     value=2000, 
                     step=1
        ),
        
        checkboxInput(inputId='TypeID', 
                      label="Click for Line Chart (default stair-steps chart) ", 
                      value = FALSE
        ),
        
        submitButton(text = "Submit", 
                     icon = NULL
        )
    ),
    
    mainPanel(
        verbatimTextOutput('caption'),
        plotOutput('newHist')
    )
))
