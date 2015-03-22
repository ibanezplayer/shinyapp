library(shiny)
library(plyr)

# shinyapps::showLogs()

# Load Bank of England Base Rate History file. It is available at:
# http://www.bankofengland.co.uk/boeapps/iadb/
# index.asp?Travel=NIxASxRPx&From=Repo&C=13T&G0Xtop.x=1&G0Xtop.y=1
LoadFile <- function() {
    df <- read.csv("./baserate.csv", 
                   header=FALSE)
    colnames(df) <- c("Year", "Day", "Month", "Rate")
    df
}

CleanData <- function(df) {
    # Step 1: Remove rows with missing values in Rate column  
    df <- subset(df, !is.na(Rate))
    
    # Step 2: Fill in missing Year values as per the last Year value
    for (i in 1:nrow(df)) {
        if (!is.na(df$Year[i])) {
            latestYear = df$Year[i]
            next
        }
        df$Year[i] = latestYear
    }
    
    # Step 3: Impute missing Day values as middle of the month
    df$Day[which(is.na(df$Day))] = 15
    
    # Step 4: Set the column types correctly 
    df$Year <- as.integer(df$Year)
    df$Day <- as.integer(df$Day)
    
    # Step 5: Retain first 3 chars of Month only
    df$Month <- substr(df$Month,1,3)
    
    # Step 6: Remove NA columns from end
    df <- df[, 1:4]
    
    df
}

CreateDateCol <- function(df) {
    # Add a column to base table for full date
    df$Date <- with(df, paste(Year, Month, Day, sep="-"))
    df$Date <- as.Date(df$Date, format="%Y-%b-%d", origin="1600-01-01")
    df
}

RowForYear <- function(df, y) {
    # print(c("Params are:", nrow(df), y))
    # Step 1: Get maximum year for which there is an entry prior to y
    maxPrevYear <- df[df$Date == max(df[df$Year<=y,"Date"]),"Year"]
    # print(c("values after:", maxPrevYear, y))
    # Step 2: If no entry matching for requested year, go ahead and create dummy entry
    if (is.na(maxPrevYear) | is.na(y)) { print(c("why:", maxPrevYear, y))}
    if (maxPrevYear < y) {
        bRate <- df[df$Date==max(df[df$Year==maxPrevYear,"Date"]),"Rate"]
        bDate <- paste(y, "Jan", "01", sep="-")
        bDate <- as.Date(bDate, format="%Y-%b-%d", origin="1600-01-01")

        dfc <- data.frame(Year=as.integer(y), 
                          Day=as.integer(01),
                          Month=as.character("Jan"),
                          Rate=bRate,
                          Date=bDate)
        df <- rbind(df, dfc)
        df <- arrange(df, Date)
    }
    
    df
}

BRateData <- LoadFile()
BRateData <- CleanData(df=BRateData)
BRateData <- CreateDateCol(df=BRateData)
# If there were any corrupt data, Date column will be NA. Remove such rows
BRateData <- na.omit(BRateData)

# Store range of Year values at start of application
minYear <- min(BRateData$Year)
maxYear <- max(BRateData$Year)
currYear <- as.integer(format(Sys.Date(), "%Y"))
lastYear <- currYear - 1

# If latest entry in dataset not for this year, then add an entry for the current year
if (currYear > maxYear) {
    BRateData <- RowForYear(df=BRateData, y=currYear)    
}

# Define server logic required to plot the chart based on user input
shinyServer(function(input, output, session) {
    
    # Ensure websocket is released when session ends
    session$onSessionEnded(function() {
        stopApp()
    })
        
    # Subset BoE Data based on Input Year Provided
    chartData <- reactive({
        subset(BRateData, Year >= input$YearID)
    })
    
    # Get Input Year
    # inputYear <- reactive({
    #    input$YearID
    # })
    
    # Get the type of chart to be plotted
    # chartType <- reactive({
    #    input$TypeID
    # })
    
    # Return text to print as caption for the chart
    output$caption <- renderText({
        
        if (input$YearID < minYear | input$YearID >= currYear) {
            paste("Error: Valid values for Year are between", 
                  minYear, 
                  "and",
                  lastYear, 
                  "\nDisplaying all the available data ...",
                  sep=" "
            )
        } else {
            paste("Bank of England Base Rate Changes between", 
                  input$YearID, 
                  "and", 
                  currYear, 
                  sep=" "
            )
        }
    })
    
    # Plot the chart based on inputs
    output$newHist <- renderPlot({
        if (input$YearID < minYear | input$YearID > currYear) {
            BRateData <- BRateData
        } else {
            BRateData <- RowForYear(df=BRateData, y=input$YearID)    
        }
                
        if (input$YearID >= currYear) {
            chartData <- subset(BRateData, Year >= minYear)    
        } else {
            chartData <- subset(BRateData, Year >= input$YearID)
        }
                
        if (input$TypeID == 0) {
            plot(chartData$Date, chartData$Rate, type="s", col="blue",
                 xlab="Date of Rate Change", ylab="Base Rate (%)")
        } else {
            plot(chartData$Date, chartData$Rate, type="l", col="red",
                 xlab="Date of Rate Change", ylab="Base Rate (%)")
        }
    })
})
