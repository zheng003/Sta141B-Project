library(httr)
library(lubridate)
library(tidyverse)
library(rtweet)
library(tidytext)
library(jsonlite)
library(ggmap)
library(dplyr)
library(shiny)
library(wordcloud)
library(plotly)
library(threejs)
library(mapdeck)
library(styler)
library(usethis)

fluidPage(
  shinythemes::themeSelector(),
  titlePanel("Twitter Sentiment Analysis"),
  headerPanel(h4("Tweets being analyzed are tweets with tag #nba created in last seven days")),

  sidebarPanel(
    # Date Range
    dateRangeInput("date",
      strong("Date Range of the Tweets"),
      start = "2020-03-12",
      end = "2020-03-19",
      min = "2020-03-12",
      max = "2020-03-19"
    ),

    conditionalPanel(
      "input.tabselected=='Word Cloud'",
      sliderInput("freq",
        "Minimum Frequency:",
        min = 1,  max = 50, value = 15
      ),
      sliderInput("max",
        "Maximum Number of Words:",
        min = 1,  max = 300,  value = 100
      )
    ),
    conditionalPanel(
      "input.tabselected=='Word Count'",
      sliderInput("max_word",
        "Maximum Number of Words:",
        min = 1,  max = 20, value = 10
      )
    ),
    conditionalPanel(
      "input.tabselected=='Term Frequency'",
      helpText("Click the column header to sort a column.")
    )
  ),

  # Visualization
  mainPanel(
    tabsetPanel(
      tabPanel("Globe",
        globeOutput("globe"),
        h4(textOutput("globe_text")),
        value = "globe"
      ),
      tabPanel(
        "Word Cloud",
        plotOutput("word_cloud"),
        h4(textOutput("cloud_text"))
      ),
      tabPanel(
        "Word Count",
        plotlyOutput("word_count"),
        h4(textOutput("word_count_text"))
      ),
      tabPanel(
        "Term Frequency",
        DT::dataTableOutput("tf_idf"),
        h4(textOutput("tf_idf_text"))
      ),
      tabPanel(
        "Sentiment Analysis",
        plotlyOutput("hist"),
        h4(textOutput("hist_text"))
      ),
      tabPanel(
        "Sentiment Map",
        mapdeckOutput("Map", height = "600px"),
        h4(textOutput("mymap_text"))
      ),
      tabPanel(
        "TimeSeries",
        plotOutput("series"),
        h4(textOutput("series_text"))
      ),
      id = "tabselected"
    )
  )
)

