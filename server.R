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

tweets <- load("~/Sta141B-Project/shiny app data.Rdata")

function(input, output) {
  # subset data in the date range
  selected_date <- reactive({
    req(input$date)
    validate(need(
      !is.na(input$date[1]) & !is.na(input$date[2]),
      "Error: Please provide both a start and an end date."
    ))
    validate(need(
      input$date[1] <= input$date[2],
      "Error: Start date should be earlier than end date."
    ))
    tweets %>%
      filter(created_at >= as.POSIXct(input$date[1], "UTC") &
        created_at < (as.POSIXct(input$date[2], "UTC") + days(1))) %>%
      select(stripped_text, lon, lat, sentiment, created_at)
  })

  # remove stop word & count the words
  countword <- reactive(
    selected_date() %>%
      mutate(id = 1:nrow(selected_date()), text = stripped_text) %>%
      unnest_tokens(word, text) %>%
      anti_join(stop_words) %>% # remove stop words
      group_by(id) %>%
      count(word, sort = TRUE) %>%
      arrange(id)
  )

  # key for the map
  key <- "pk.eyJ1IjoiamFuc29uYm9zc3MiLCJhIjoiY2s3ZWcxODA5MDhiYjNybXhkcWV0cnY4YyJ9.UzLgmwlMAATvgKmDEGo7lQ"

  # plot the location
  output$globe <- renderGlobe({
    globejs(
      lat = selected_date()$lat,
      long = selected_date()$lon,
      value = 10,
      bg = "white", color = "#ffff00", atmosphere = TRUE,
    )
  })

  output$globe_text <- renderText({
    "The globe shows the location of the tweets in the selected date range."
  })

  # plot the word cloud
  output$word_cloud <- renderPlot(
    countword() %>%
      group_by(word) %>%
      summarise(count = n()) %>%
      with(wordcloud(word,
        count,
        min.freq = input$freq,
        max.words = input$max,
        random.order = FALSE,
        colors = brewer.pal(8, "Dark2")
      ))
  )

  output$cloud_text <- renderText({
    "The word cloud shows the main words of the tweets in the selected date range."
  })

  # plot the bar chart of the most frequently used words
  output$word_count <- renderPlotly(
    countword() %>%
      group_by(word) %>%
      summarise(count = n()) %>%
      top_n(input$max_word, count) %>%
      plot_ly(x = ~count, y = ~ fct_reorder(word, count)) %>%
      add_bars(alpha = .7) %>%
      layout(xaxis = list(title = "count"))
  )

  output$word_count_text <- renderText({
    "The bar chart shows the frequencies of the top 10 frequently used words of the tweets in the selected date range."
  })

  # table of tf-idf
  output$tf_idf <- DT::renderDataTable(
    DT::datatable(countword() %>%
      bind_tf_idf(word, id, n) %>%
      top_n(3, tf_idf) %>%
      select(id, word, n, tf_idf) %>%
      left_join(get_sentiments("bing")),
    options = list(pageLength = 20)
    )
  )

  output$tf_idf_text <- renderText({
    "The table shows the 3 most important words of the tweets in the selected date range."
  })

  # plot the histogram of the sentiments
  output$hist <- renderPlotly(
    selected_date() %>%
      plot_ly(y = ~sentiment, color = ~ format(created_at, "%y/%m/%d")) %>%
      add_histogram(alpha = .7) %>%
      layout(
        xaxis = list(title = "count"),
        yaxis = list(title = "sentiment"),
        barmode = "overlay"
      )
  )

  output$hist_text <- renderText({
    "The histogram shows the frequencies of the tweets' sentiment in the selected date range."
  })

  # plot the location of tweets with sentiment in different colors
  output$Map <- renderMapdeck({
    mapdeck(
      token = key, style = mapdeck_style("light"),
      # Angle camera down.
      pitch = 20,
      zoom = 100
    ) %>%
      add_scatterplot(
        data = selected_date(),
        lat = "lat",
        lon = "lon",
        radius = 100000,
        layer_id = "scatter_layer",
        palette = "viridis",
        fill_colour = "sentiment",
        legend = TRUE,
        fill_opacity = 100, auto_highlight = TRUE,
      )
  })

  output$mymap_text <- renderText({
    "The map shows the location of the tweets with the tweets' sentiments in the selected date range."
  })

  ## plot time series of tweets
  output$series <- renderPlot({
    selected_date() %>%
      mutate(freq = date(created_at)) %>%
      count(freq) %>%
      ggplot(aes(x = freq, y = n)) +
      geom_area(fill = "#69b3a2", alpha = 0.5) +
      geom_line(color = "#69b3a2") +
      ggplot2::theme_minimal() +
      labs(
        x = "", y = "Count",
        title = "Twitter Frequency about NBA"
      )
  })

  output$series_text <- renderText({
    "The time series plot plots the created time of the tweets in the selected date range."
  })
}
