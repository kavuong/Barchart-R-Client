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
    tabPanel(
      "Equities",
      sidebarLayout(
        sidebarPanel(
          textInput("stockChosen", h3("Stock Quote (ex. AAPL)")),
          selectInput("selectTimeEquity", h3("Time"),
            choices = list("1D", "5D", "1M", "3M", "6M")
          ),
          selectInput("selectFormatEquity", h3("Data"),
            choices = list("Price($)", "Volume")
          ),
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
          textOutput("52WeekHighDate")
        )
      )
    ),
    tabPanel(
      "Futures",
      sidebarLayout(
        sidebarPanel(
          textInput("futureChosen", h3("Future Quote (ex. CL)")),
          selectInput("selectTimeFuture", h3("Time"),
            choices = list("1D", "5D", "1M", "3M", "6M")
          ),
          selectInput("selectFormatFuture", h3("Data"),
            choices = list("Price($)", "Volume")
          ),
          actionButton("submitFuture", "Submit")
        ),
        mainPanel(
          plotOutput("futurePlot"),
          textOutput("futureLabel"),
          textOutput("futureCurrentPrice"),
          textOutput("futureOpenPrice"),
          textOutput("futureHighPrice"),
          textOutput("futureLowPrice"),
          textOutput("future52WeekLow"),
          textOutput("future52WeekLowDate"),
          textOutput("future52WeekHigh"),
          textOutput("future52WeekHighDate")
        )
      )
    ),
    tabPanel(
      "Forex",
      sidebarLayout(
        sidebarPanel(
          selectInput("selectTimeForex", h3("Time"),
            choices = list("1D", "5D", "1M", "3M", "6M")
          ),
          numericInput("amountCurrencyFrom", h4("From"), value = 0),
          selectInput("currencyFrom", NULL,
            choices = list("USD", "EUR", "JPY", "GBP", "CHF", "CAD", "AUD", "NZD")
          ),
          selectInput("currencyTo", h4("To"),
            choices = list("USD", "EUR", "JPY", "GBP", "CHF", "CAD", "AUD", "NZD")
          ),
          textOutput("forexResult"),
          selectInput("selectFormatForex", h3("Data"),
            choices = list("Price($)", "Volume")
          ),
          actionButton("submitForex", "Submit")
        ),
        mainPanel(
          plotOutput("forexPlot"),
          textOutput("forexLabel"),
          textOutput("forexCurrentPrice"),
          textOutput("forexOpenPrice"),
          textOutput("forexHighPrice"),
          textOutput("forexLowPrice"),
          textOutput("forex52WeekLow"),
          textOutput("forex52WeekLowDate"),
          textOutput("forex52WeekHigh"),
          textOutput("forex52WeekHighDate")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  readRenviron(".Renviron")

  chosenEquity <- reactive({
    c(StockChosen = input$stockChosen, SelectTimeEquity = input$selectTimeEquity, SelectFormatEquity = input$selectFormatEquity)
  })

  chosenFuture <- reactive({
    c(FutureChosen = input$futureChosen, SelectTimeFuture = input$selectTimeFuture, SelectFormatFuture = input$selectFormatFuture)
  })

  chosenForex <- reactive({
    c(
      ForexChosen = paste("^", input$currencyFrom, input$currencyTo, sep = ""), SelectTimeForex = input$selectTimeForex,
      SelectFormatForex = input$selectFormatForex
    )
  })

  observeEvent(input$submitEquity, {
    # need input validation for stock inputted

    # handling stock quote
    test.data <- GET(
      "https://marketdata.websol.barchart.com/getQuote.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbols = {
          input$stockChosen
        },
        fields = c("fiftyTwoWkHigh,fiftyTwoWkHighDate,fiftyTwoWkLow,fiftyTwoWkLowDate")
      )
    )

    stop_for_status(test.data)
    json <- content(test.data, as = "text")
    data <- fromJSON(json, flatten = T)

    output$stockLabel <- renderText(paste("Stock Symbol: ", {
      data$results$symbol[1]
    }))
    output$currentPrice <- renderText({
      paste("Current Price: ", data$results$lastPrice[1])
    })
    output$openPrice <- renderText({
      paste("Open: ", data$results$open[1])
    })
    output$highPrice <- renderText({
      paste("Today's High: ", data$results$high[1])
    })
    output$lowPrice <- renderText({
      paste("Today's Low: ", data$results$low[1])
    })
    output$`52WeekLow` <- renderText({
      paste("52-wk Low:", data$results$fiftyTwoWkLow[1])
    })
    output$`52WeekLowDate` <- renderText({
      paste("52-wk Low on: ", data$results$fiftyTwoWkLowDate[1])
    })
    output$`52WeekHigh` <- renderText({
      paste("52-wk High: ", data$results$fiftyTwoWkHigh[1])
    })
    output$`52WeekHighDate` <- renderText({
      paste("52-wk High on: ", data$results$fiftyTwoWkHighDate[1])
    })

    inType <- ""
    time.text <- ""
    interv <- 5
    needDays <- 0
    if (chosenEquity()["SelectTimeEquity"] == "1D" || chosenEquity()["SelectTimeEquity"] == "5D") {
      inType <- "minutes"
      time.text <- "day"
      needDays <- 1
      if (chosenEquity()["SelectTimeEquity"] == "5D") {
        interv <- 30
        needDays <- 5
        time.text <- "5 days"
      }
    }
    else {
      inType <- "daily"
      if (chosenEquity()["SelectTimeEquity"] == "1M") {
        needDays <- 20
        time.text <- "month"
      }
      else if (chosenEquity()["SelectTimeEquity"] == "3M") {
        needDays <- 60
        time.text <- "3 months"
      }
      else if (chosenEquity()["SelectTimeEquity"] == "6M") {
        needDays <- 120
        time.text <- "6 months"
      }
    }
    # handling plot of stock history
    test.data.history <- GET(
      "https://marketdata.websol.barchart.com/getHistory.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbol = {
          input$stockChosen
        },
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

    data.to.display <- data.history$results[data.history$results$tradingDay %in% needed.days, ]

    data.to.display$timestamp <- as.POSIXct(data.to.display$timestamp, format = "%Y-%m-%dT%H:%M", tz = "America/Los Angeles")

    by <- 0
    if (chosenEquity()["SelectTimeEquity"] == "1D") {
      by <- 30
    } else if (chosenEquity()["SelectTimeEquity"] == "5D") {
      # displaying label for every day in 5 days (13 timeslots per day)
      by <- 13
    } else if (chosenEquity()["SelectTimeEquity"] == "1M") {
      # displaying label for every 4 days in 1 month
      by <- 4
    } else if (chosenEquity()["SelectTimeEquity"] == "3M") {
      by <- 12
    } else {
      by <- 24
    }
    my_breaks <- data.to.display$timestamp[seq.int(1, length(data.to.display$timestamp), by = by)]
    my_labels <- ""
    if (chosenEquity()["SelectTimeEquity"] == "1D") {
      my_labels <- as.character(my_breaks, "%H:%M")
    } else if (chosenEquity()["SelectTimeEquity"] == "5D") {
      my_labels <- as.character(my_breaks, "%b %d")
    } else {
      my_labels <- format(my_breaks, "%Y-%m-%d")
    }
    my_breaks <- as.character(my_breaks)


    if (chosenEquity()["SelectFormatEquity"] == "Price($)") {
      output$stockPlot <- renderPlot({
        ggplot(data.to.display, aes(x = as.character(timestamp), y = open, group = 1)) +
          geom_line() +
          xlab("Time") +
          ylab("Price ($)") +
          ggtitle(paste(data$results$symbol[1], "in the past", time.text, sep = " ")) +
          scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
    else {
      output$stockPlot <- renderPlot({
        ggplot(data.to.display) +
          geom_col(aes(x = as.character(timestamp), y = volume)) +
          xlab("Time") +
          ylab("Volume") +
          ggtitle(paste(data$results$symbol[1], "Volume in the past", time.text, sep = " ")) +
          scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
  })

  observeEvent(input$submitFuture, {
    # need input validation for stock inputted

    # handling stock quote
    test.data <- GET(
      "https://marketdata.websol.barchart.com/getQuote.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbols = {
          input$futureChosen
        },
        fields = c("fiftyTwoWkHigh,fiftyTwoWkHighDate,fiftyTwoWkLow,fiftyTwoWkLowDate")
      )
    )

    stop_for_status(test.data)
    json <- content(test.data, as = "text")
    data <- fromJSON(json, flatten = T)

    output$futureLabel <- renderText(paste("Stock Symbol: ", {
      data$results$symbol[1]
    }))
    output$futureCurrentPrice <- renderText({
      paste("Current Price: ", data$results$lastPrice[1])
    })
    output$futureOpenPrice <- renderText({
      paste("Open: ", data$results$open[1])
    })
    output$futureHighPrice <- renderText({
      paste("Today's High: ", data$results$high[1])
    })
    output$futureLowPrice <- renderText({
      paste("Today's Low: ", data$results$low[1])
    })
    output$future52WeekLow <- renderText({
      paste("52-wk Low:", data$results$fiftyTwoWkLow[1])
    })
    output$future52WeekLowDate <- renderText({
      paste("52-wk Low on: ", data$results$fiftyTwoWkLowDate[1])
    })
    output$future52WeekHigh <- renderText({
      paste("52-wk High: ", data$results$fiftyTwoWkHigh[1])
    })
    output$future52WeekHighDate <- renderText({
      paste("52-wk High on: ", data$results$fiftyTwoWkHighDate[1])
    })

    inType <- ""
    time.text <- ""
    interv <- 5
    needDays <- 0
    if (chosenFuture()["SelectTimeFuture"] == "1D" || chosenFuture()["SelectTimeFuture"] == "5D") {
      inType <- "minutes"
      time.text <- "day"
      needDays <- 1
      if (chosenFuture()["SelectTimeFuture"] == "5D") {
        interv <- 30
        needDays <- 5
        time.text <- "5 days"
      }
    }
    else {
      inType <- "daily"
      if (chosenFuture()["SelectTimeFuture"] == "1M") {
        needDays <- 20
        time.text <- "month"
      }
      else if (chosenFuture()["SelectTimeFuture"] == "3M") {
        needDays <- 60
        time.text <- "3 months"
      }
      else if (chosenFuture()["SelectTimeFuture"] == "6M") {
        needDays <- 120
        time.text <- "6 months"
      }
    }
    # handling plot of stock history
    test.data.history <- GET(
      "https://marketdata.websol.barchart.com/getHistory.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbol = {
          input$futureChosen
        },
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


    data.to.display <- data.history$results[data.history$results$tradingDay %in% needed.days, ]

    data.to.display$timestamp <- as.POSIXct(data.to.display$timestamp, format = "%Y-%m-%dT%H:%M", tz = "America/Los Angeles")

    by <- 0
    if (chosenFuture()["SelectTimeFuture"] == "1D") {
      by <- 30
    } else if (chosenFuture()["SelectTimeFuture"] == "5D") {
      # displaying label for every day in 5 days (13 timeslots per day)
      by <- 13
    } else if (chosenFuture()["SelectTimeFuture"] == "1M") {
      # displaying label for every 4 days in 1 month
      by <- 4
    } else if (chosenFuture()["SelectTimeFuture"] == "3M") {
      by <- 12
    } else {
      by <- 24
    }
    my_breaks <- data.to.display$timestamp[seq.int(1, length(data.to.display$timestamp), by = by)]
    my_labels <- ""
    if (chosenFuture()["SelectTimeFuture"] == "1D") {
      my_labels <- as.character(my_breaks, "%H:%M")
    } else if (chosenFuture()["SelectTimeFuture"] == "5D") {
      my_labels <- as.character(my_breaks, "%b %d")
    } else {
      my_labels <- format(my_breaks, "%Y-%m-%d")
    }
    my_breaks <- as.character(my_breaks)


    if (chosenFuture()["SelectFormatFuture"] == "Price($)") {
      output$futurePlot <- renderPlot({
        ggplot(data.to.display, aes(x = as.character(timestamp), y = open, group = 1)) +
          geom_line() +
          xlab("Time") +
          ylab("Price ($)") +
          ggtitle(paste(data$results$symbol[1], "in the past", time.text, sep = " ")) +
          scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
    else {
      output$futurePlot <- renderPlot({
        ggplot(data.to.display) +
          geom_col(aes(x = as.character(timestamp), y = volume)) +
          xlab("Time") +
          ylab("Volume") +
          ggtitle(paste(data$results$symbol[1], "Volume in the past", time.text, sep = " ")) +
          scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
  })

  observeEvent(input$submitForex, {
    # need input validation for stock inputted

    # handling stock quote
    test.data <- GET(
      "https://marketdata.websol.barchart.com/getQuote.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbols = {
          chosenForex()["ForexChosen"]
        },
        fields = c("fiftyTwoWkHigh,fiftyTwoWkHighDate,fiftyTwoWkLow,fiftyTwoWkLowDate")
      )
    )

    stop_for_status(test.data)
    json <- content(test.data, as = "text")
    data <- fromJSON(json, flatten = T)

    output$forexLabel <- renderText(paste("Stock Symbol: ", {
      data$results$symbol[1]
    }))
    output$forexCurrentPrice <- renderText({
      paste("Current Price: ", data$results$lastPrice[1])
    })
    output$forexOpenPrice <- renderText({
      paste("Open: ", data$results$open[1])
    })
    output$forexHighPrice <- renderText({
      paste("Today's High: ", data$results$high[1])
    })
    output$forexLowPrice <- renderText({
      paste("Today's Low: ", data$results$low[1])
    })
    output$forex52WeekLow <- renderText({
      paste("52-wk Low:", data$results$fiftyTwoWkLow[1])
    })
    output$forex52WeekLowDate <- renderText({
      paste("52-wk Low on: ", data$results$fiftyTwoWkLowDate[1])
    })
    output$forex52WeekHigh <- renderText({
      paste("52-wk High: ", data$results$fiftyTwoWkHigh[1])
    })
    output$forex52WeekHighDate <- renderText({
      paste("52-wk High on: ", data$results$fiftyTwoWkHighDate[1])
    })

    inType <- ""
    time.text <- ""
    interv <- 5
    # different approach to querying, because 24 hour trading period -> only query data needed
    # 1D - 288 max records (24 hours * 60 minutes / 5 minute interval), by 48 (4 hours)
    # 1D - labels: as.character(my_breaks, "%b %d %H:%M")
    # 5D - 240 max records (24 hours * 5 days * 60 minutes / 30 minute interval), by 79 (daily)
    # 5D - labels: as.character(my_breaks, "%b %d")
    # 1M - 30 max records (30 days / 1 day interval), DAILY MODE, by 4 (every 4 days)
    # 3M - 90 max records (30 days / 1 day interval), DAILY MODE, by 12 (every 12 days)
    # 6M - 180 max records (30 days / 1 day interval), DAILY MODE, by 24 (every 24 days)
    maxRecords <- 0
    if (chosenForex()["SelectTimeForex"] == "1D" || chosenForex()["SelectTimeForex"] == "5D") {
      inType <- "minutes"
      time.text <- "day"
      maxRecords <- 288
      if (chosenForex()["SelectTimeForex"] == "5D") {
        interv <- 30
        maxRecords <- 240
        time.text <- "5 days"
      }
    }
    else {
      inType <- "daily"
      if (chosenForex()["SelectTimeForex"] == "1M") {
        maxRecords <- 30
        time.text <- "month"
      }
      else if (chosenForex()["SelectTimeForex"] == "3M") {
        maxRecords <- 90
        time.text <- "3 months"
      }
      else if (chosenForex()["SelectTimeForex"] == "6M") {
        maxRecords <- 180
        time.text <- "6 months"
      }
    }
    # handling plot of stock history
    test.data.history <- GET(
      "https://marketdata.websol.barchart.com/getHistory.json",
      query = list(
        apikey = Sys.getenv("BARCHART_TOKEN"),
        symbol = {
          chosenForex()["ForexChosen"]
        },
        type = inType,
        startDate = "20200101",
        interval = interv,
        maxRecords = maxRecords
      )
    )
    stop_for_status(test.data.history)
    json <- content(test.data.history, as = "text")
    data.to.display <- fromJSON(json, flatten = T)

    # rev.data.history <- map_df(data.history$results, rev)
    # needed.days <- unique(rev.data.history$tradingDay)[1:needDays]
    # data.to.display <- data.history$results[data.history$results$tradingDay %in% needed.days,]
    data.to.display <- data.to.display$results
    data.to.display$timestamp <- as.POSIXct(data.to.display$timestamp, format = "%Y-%m-%dT%H:%M", tz = "America/Los Angeles")

    by <- 0
    if (chosenForex()["SelectTimeForex"] == "1D") {
      # displaying label every 4 hours
      by <- 48
    } else if (chosenForex()["SelectTimeForex"] == "5D") {
      # displaying label for every day in 5 days
      by <- 79
    } else if (chosenForex()["SelectTimeForex"] == "1M") {
      # displaying label for every 4 days in 1 month
      by <- 4
    } else if (chosenForex()["SelectTimeForex"] == "3M") {
      by <- 12
    } else {
      by <- 24
    }
    my_breaks <- data.to.display$timestamp[seq.int(1, length(data.to.display$timestamp), by = by)]
    my_labels <- ""
    if (chosenForex()["SelectTimeForex"] == "1D") {
      my_labels <- as.character(my_breaks, "%b %d %H:%M")
    } else if (chosenFuture()["SelectTimeFuture"] == "5D") {
      my_labels <- as.character(my_breaks, "%b %d")
    } else {
      my_labels <- format(my_breaks, "%Y-%m-%d")
    }
    my_breaks <- as.character(my_breaks)

    ex.display <- paste(input$currencyFrom, ":", input$currencyTo, sep = "")

    if (chosenForex()["SelectFormatForex"] == "Price($)") {
      output$forexPlot <- renderPlot({
        ggplot(data.to.display, aes(x = as.character(timestamp), y = open, group = 1)) +
          geom_line() +
          xlab("Time") +
          ylab(paste("Price of", ex.display, sep = " ")) +
          ggtitle(paste(ex.display, "in the past", time.text, sep = " ")) +
          scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
    else {
      output$forexPlot <- renderPlot({
        ggplot(data.to.display, aes(x = as.character(timestamp), y = volume, group = 1)) +
          geom_col() +
          xlab("Time") +
          ylab(paste("Volume of", ex.display, sep = " ")) +
          ggtitle(paste(ex.display, "Volume in the past", time.text, sep = " ")) +
          scale_x_discrete(breaks = my_breaks, labels = my_labels)
      })
    }
    output$forexResult <- renderText(paste(input$amountCurrencyFrom,
      input$currencyFrom,
      "is equal to",
      {
        input$amountCurrencyFrom * data$results$lastPrice[1]
      },
      input$currencyTo,
      sep = " "
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
