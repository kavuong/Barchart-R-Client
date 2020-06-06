#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(XML)
library(ggplot2)
# TO-DO: add helper for labels to equities, futures, and forex

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Financial Market Data"),
    tabsetPanel(
      
      tabPanel("Equities", 
               sidebarLayout(
                 sidebarPanel(
                   textInput("stockChosen", h3("Stock Quote (ex. AAPL)")),
                   selectInput("selectTimeEquity", h3("Time"),
                               choices = list("1D", "5D", "1M", "3M", "6M")),
                   selectInput("selectFormatEquity", h3("Data"),
                               choices = list("Price($)", "Volume")),
                   actionButton("submitEquity", "Submit")
                   ),
                 mainPanel(
                   plotOutput("stockPlot"),
                   textOutput("stockLabel"),
                           textOutput("currentPrice"),
                           textOutput("openPrice"),
                           textOutput("highPrice"),
                           textOutput("lowPrice"),
                           textOutput("52WeekLow"),
                           textOutput("52WeekLowDate"),
                           textOutput("52WeekHigh"),
                           textOutput("52WeekHighDate"))
                 )
               ),
      tabPanel("Futures", 
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selectTimeFuture", h3("Time"),
                               choices = list("1D", "5D", "1M", "3M", "6M")),
                 ),
                 mainPanel(
                   plotOutput("futurePlot")
                 )
               )
      ),
      tabPanel("Forex", 
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selectTimeForex", h3("Time"),
                               choices = list("1D", "5D", "1M", "3M", "6M")),
                   numericInput("amountCurrencyFrom", "From", value = 0),
                   selectInput("currencyFrom", NULL,
                               choices = list("USD", "EUR", "JPY", "GBP", "CHF", "CAD", "AUD", "NZD")),
                   numericInput("amountCurrencyTo", "To", value = 0),
                   selectInput("currencyTo", NULL,
                               choices = list("USD", "EUR", "JPY", "GBP", "CHF", "CAD", "AUD", "NZD"))
                 ),
                 mainPanel(
                   plotOutput("forexPlot")
                 )
               )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  readRenviron(".Renviron")
  
  chosenEquity = reactive({
    # need to update for time and %/$ later
    c(StockChosen = input$stockChosen, SelectTimeEquity = input$selectTimeEquity, SelectFormatEquity = input$selectFormatEquity)
  })
  
  observeEvent(input$submitEquity, {
    # need input validation for stock inputted
    
    # handling stock quote
    test.data <- GET(
      "https://marketdata.websol.barchart.com/getQuote.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbols = {input$stockChosen},
        fields = c("fiftyTwoWkHigh,fiftyTwoWkHighDate,fiftyTwoWkLow,fiftyTwoWkLowDate")
      )
    )
    
    stop_for_status(test.data)
    json <- content(test.data, as = "text")
    data <- fromJSON(json, flatten = T)
    
    output$stockLabel <- renderText(paste("Stock Symbol: ", {data$results$symbol[1]}))
    output$currentPrice <- renderText({paste("Current Price: ", data$results$lastPrice[1])})
    output$openPrice <- renderText({paste("Open: ", data$results$open[1])})
    output$highPrice <- renderText({paste("Today's High: ", data$results$high[1])})
    output$lowPrice <- renderText({paste("Today's Low: ", data$results$low[1])})
    output$`52WeekLow` <- renderText({paste("52-wk Low:", data$results$fiftyTwoWkLow[1])})
    output$`52WeekLowDate` <- renderText({paste("52-wk Low on: ", data$results$fiftyTwoWkLowDate[1])})
    output$`52WeekHigh` <- renderText({paste("52-wk High: ", data$results$fiftyTwoWkHigh[1])})
    output$`52WeekHighDate` <- renderText({paste("52-wk High on: ", data$results$fiftyTwoWkHighDate[1])})
    
    inType <- ""
    time.text <- ""
    interv <- 5
    needDays <- 0
    if (chosenEquity()["SelectTimeEquity"] == "1D" || chosenEquity()["SelectTimeEquity"] == "5D"){
      inType <- "minutes"
      time.text <- "day"
      needDays <- 1
      if (chosenEquity()["SelectTimeEquity"] == "5D"){
        interv <- 30
        needDays <- 5
        time.text <- "5 days"
      }
    }
    else {
      inType <- "daily"
      if (chosenEquity()["SelectTimeEquity"] == "1M"){
        needDays <- 20
        time.text <- "month"
      }
      else if (chosenEquity()["SelectTimeEquity"] == "3M"){
        needDays <- 60
        time.text <- "3 months"
      }
      else if (chosenEquity()["SelectTimeEquity"] == "6M"){
        needDays <- 120
        time.text <- "6 months"
      }
      
    }
    # handling plot of stock history
    test.data.history <- GET(
      "https://marketdata.websol.barchart.com/getHistory.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbol = {input$stockChosen},
        type = inType,
        startDate = "20200101",
        interval = interv
      )
    )
    stop_for_status(test.data.history)
    json <- content(test.data.history, as = "text")
    data.history <- fromJSON(json, flatten = T)
    
    rev.data.history <- map_df(data.history$results, rev)
    needed.days <- unique(rev.data.history$tradingDay)[1:needDays]
    
    
    data.to.display <- data.history$results[data.history$results$tradingDay %in% needed.days,]
    
    data.to.display$timestamp <- as.POSIXct(data.to.display$timestamp, format="%Y-%m-%dT%H:%M", tz = "America/Los Angeles")
    
    by <- 0
    if (chosenEquity()["SelectTimeEquity"] == "1D"){
      by <- 30
    } else if (chosenEquity()["SelectTimeEquity"] == "5D"){
      # displaying label for every day in 5 days (13 timeslots per day)
      by <- 13
    } else if (chosenEquity()["SelectTimeEquity"] == "1M"){
      # displaying label for every 4 days in 1 month
      by <- 4
    } else if (chosenEquity()["SelectTimeEquity"] == "3M"){
      by <- 12
    } else {
      by <- 24
    }
    my_breaks <- data.to.display$timestamp[seq.int(1,length(data.to.display$timestamp) , by = by)]
    my_labels <- ""
    if (chosenEquity()["SelectTimeEquity"] == "1D"){
      my_labels <- as.character(my_breaks, "%H:%M")
    } else if (chosenEquity()["SelectTimeEquity"] == "5D"){
      my_labels <- as.character(my_breaks, "%b %d")
    } else {
      my_labels <- format(my_breaks, "%Y-%m-%d")
    }
    my_breaks <- as.character(my_breaks)
    
    
    if (chosenEquity()["SelectFormatEquity"] == "Price($)"){
    output$stockPlot <- renderPlot({
        ggplot(data.to.display, aes(x=as.character(timestamp), y=open, group = 1)) + geom_line() + xlab("Time") + ylab("Price ($)") + ggtitle(paste(input$stockChosen, "in the past", time.text, sep = " ")) + scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
    else {
      output$stockPlot <- renderPlot({
        ggplot(data.to.display) + geom_col(aes(x=as.character(timestamp), y=volume)) + xlab("Time") + ylab("Volume") + ggtitle(paste(input$stockChosen, "Volume in the past", time.text, sep = " ")) + scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
    }
    
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
