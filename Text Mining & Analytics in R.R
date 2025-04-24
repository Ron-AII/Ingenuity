# Detach Packages
detach("package:tidyverse", unload = TRUE)
detach("package:tidytext", unload = TRUE)
detach("package:dplyr", unload = TRUE)

# Load required libraries
library(shiny)
library(dplyr)
library(tidytext)
library(tidyr)
library(openxlsx)
library(sentimentr)
library(tidyselect)
library(qdapRegex)
library(qdap)

# Set the maximum upload size
options(shiny.maxRequestSize = 30 * 1024^2)  # Set to 30 MB (adjust as needed)

# Function to perform sentiment analysis
perform_sentiment_analysis <- function(data, column) {
  data %>%
    select({{ column }}) %>%
    get_sentences() %>%
    sentiment()
}

# Function to perform emotion analysis
perform_emotion_analysis <- function(data, column) {
  emotion_data <- emotion_by(data[[column]])
  emotion_summary <- plyr::ddply(emotion_data, ~emotion_type, summarise, mean = mean(ave_emotion), sd = sd(ave_emotion))
  emotion_summary[order(emotion_summary$mean, decreasing = TRUE), ]
}

# Define UI
ui <- fluidPage(
  tags$style(HTML('
    body {
      background-color: #ffffff; /* White background */
      margin: 0; /* Remove default margin */
    }

    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #337ab7; /* Bluish Grey highlight for active tab */
      color: #ffffff; /* White text for active tab */
    }

    .navbar {
      background-color: #ffffff; /* White background for title panel */
    }

    .navbar-brand {
      color: #000000; /* Black text for title panel */
      font-size: 24px; /* Adjust title font size */
    }
  ')),
  titlePanel("Text Mining & Analytics Tool", windowTitle = "Sentiment Analysis App"),
  tabsetPanel(
    tabPanel("PII Masking",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_pii", "Choose CSV File"),
                 selectInput("column_pii", "Select Column:", ""),
                 actionButton("analyze_pii", "PII Masking"),
                 downloadButton("download_pii", "Download PII Masked Data"),
                 checkboxInput("name_masking", "Name Masking (Recommended to uncheck for Brand or Process Name based Text Mining)", value = TRUE)
               ),
               mainPanel(
                 tableOutput("table_pii"),
                 textOutput("rowCount_pii")
               )
             )),
    
    tabPanel("QA Optimization/ NGram",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose CSV File"),
                 selectInput("ngram", "Select N-gram:", choices = c("1-gram", "Bigram", "Trigram", "4-gram", "5-gram")),
                 selectInput("column", "Select Column:", ""),
                 actionButton("analyze", "Analyze"),
                 downloadButton("download", "Download Results")
               ),
               mainPanel(
                 tableOutput("table"),
                 textOutput("rowCount")
               )
             )),
    
    tabPanel("Sentiment Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_sentiment", "Choose CSV File for Sentiment Analysis"),
                 selectInput("column_sentiment", "Select Column:", ""),
                 actionButton("analyze_sentiment", "Analyze Sentiment"),
                 downloadButton("download_sentiment", "Download Transcriptwise Sentiment Scores "),
                 downloadButton("downloadSentimentScores", "Download Linewise Sentiment Scores")
               ),
               mainPanel(
                 tableOutput("table_sentiment"),
                 textOutput("rowCount_sentiment")
               )
             )),
    
    tabPanel("Emotion Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file_emotion", "Choose CSV File for Emotion Analysis"),
                 selectInput("column_emotion", "Select Column:", ""),
                 actionButton("analyze_emotion", "Analyze Emotion"),
                 downloadButton("download_emotion", "Download Transcriptwise Emotion Scores")
               ),
               mainPanel(
                 tableOutput("table_emotion"),
                 textOutput("rowCount_emotion")
               )
             ))
  )
)

# Define server
server <- function(input, output, session) {
  Sys.setlocale("LC_ALL", "C")  # Set the locale
  
  # Code for PII Masking
  data_pii <- reactive({
    req(input$file_pii)
    read.csv(input$file_pii$datapath, stringsAsFactors = FALSE)
  })
  
  observe({
    if (!is.null(data_pii())) {
      columns_pii <- names(data_pii())
      updateSelectInput(session, "column_pii", choices = columns_pii, selected = columns_pii[1])
    }
  })
  
  pii_masking_data <- eventReactive(input$analyze_pii, {
    pii_masked_data <- data_pii()
    
    # PII Masking logic
    if (input$name_masking) {
      pii_masked_data[[input$column_pii]] <- str_replace_all(pii_masked_data[[input$column_pii]], "\\b[A-Z][a-z]+\\b", "##masked##")
    }
    
    pii_masked_data[[input$column_pii]] <- rm_email(pii_masked_data[[input$column_pii]], replacement = "#masked email#")
    pii_masked_data[[input$column_pii]] <- gsub("\\b\\d+\\b", "#masked#", pii_masked_data[[input$column_pii]])
    pii_masked_data[[input$column_pii]] <- gsub("[[:punct:]]+", "#", pii_masked_data[[input$column_pii]])
    pii_masked_data[[input$column_pii]] <- gsub("\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\b", "Date", pii_masked_data[[input$column_pii]])
    pii_masked_data[[input$column_pii]] <- gsub("\\d+", "#masked#", pii_masked_data[[input$column_pii]])
    pii_masked_data
  })
  
  output$table_pii <- renderTable({
    pii_masking_data()
  })
  
  output$download_pii <- downloadHandler(
    filename = function() {
      paste("pii_masked_data.csv")
    },
    content = function(file) {
      write.csv(pii_masking_data(), file)
    }
  )
  
  output$rowCount_pii <- renderText({
    if (!is.null(data_pii())) {
      paste("Total Rows in Dataset: ", nrow(data_pii()))
    } else {
      "Total Rows in Dataset: 0"
    }
  })
  
  # Code for QA Optimization/NGram
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, stringsAsFactors = FALSE)
  })
  
  observe({
    if (!is.null(data())) {
      columns <- names(data())
      updateSelectInput(session, "column", choices = columns, selected = columns[1])
    }
  })
  
  ngram_data <- eventReactive(input$analyze, {
    n <- switch(input$ngram, "1-gram" = 1, "Bigram" = 2, "Trigram" = 3, "4-gram" = 4, "5-gram" = 5)
    
    data() %>%
      unnest_tokens(word, input$column, token = "ngrams", n = n) %>%
      separate(word, into = paste0("word", 1:n), sep = " ") %>%
      filter(if_any(starts_with("word"), ~ !.x %in% stop_words$word)) %>%
      unite(word, starts_with("word"), sep = " ") %>%
      count(word, sort = TRUE)
  })
  
  output$table <- renderTable({
    ngram_data()
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste("ngram_results_", input$ngram, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ngram_data(), file)
    }
  )
  
  output$rowCount <- renderText({
    if (!is.null(data())) {
      paste("Total Rows in Dataset: ", nrow(data()))
    } else {
      "Total Rows in Dataset: 0"
    }
  })
  
  # Code for Sentiment Analysis
  data_sentiment <- reactive({
    req(input$file_sentiment)
    read.csv(input$file_sentiment$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  })
  
  observe({
    if (!is.null(data_sentiment())) {
      columns <- names(data_sentiment())
      updateSelectInput(session, "column_sentiment", choices = columns, selected = columns[1])
    }
  })
  
  sentiment_data <- eventReactive(input$analyze_sentiment, {
    sentiment_scores <- perform_sentiment_analysis(data_sentiment(), input$column_sentiment)
    sentiment_by_transcript <- sentiment_by(sentiment_scores)
    
    output$table_sentiment <- renderTable({
      sentiment_by_transcript
    })
    
    return(sentiment_by_transcript)
  })
  
  output$rowCount_sentiment <- renderText({
    if (!is.null(data_sentiment())) {
      paste("Total Rows in Dataset: ", nrow(data_sentiment()))
    } else {
      "Total Rows in Dataset: 0"
    }
  })
  
  observe({
    output$table_sentiment <- renderTable({
      sentiment_data()
    })
  })
  
  output$download_sentiment <- downloadHandler(
    filename = function() {
      paste("transcriptwise_sentiment_scores.xlsx")
    },
    content = function(file) {
      write.xlsx(sentiment_data(), file)
    }
  )
  
  output$downloadSentimentScores <- downloadHandler(
    filename = function() {
      paste("linewise_sentiment_scores.xlsx")
    },
    content = function(file) {
      write.xlsx(perform_sentiment_analysis(data_sentiment(), input$column_sentiment), file)
    }
  )
  
  # Code for Emotion Analysis
  data_emotion <- reactive({
    req(input$file_emotion)
    read.csv(input$file_emotion$datapath, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  })
  
  observe({
    if (!is.null(data_emotion())) {
      columns <- names(data_emotion())
      updateSelectInput(session, "column_emotion", choices = columns, selected = columns[1])
    }
  })
  
  emotion_data <- eventReactive(input$analyze_emotion, {
    emotion_scores <- perform_emotion_analysis(data_emotion(), input$column_emotion)
    
    output$table_emotion <- renderTable({
      emotion_scores
    })
    
    return(emotion_scores)
  })
  
  output$rowCount_emotion <- renderText({
    if (!is.null(data_emotion())) {
      paste("Total Rows in Dataset: ", nrow(data_emotion()))
    } else {
      "Total Rows in Dataset: 0"
    }
  })
  
  observe({
    output$table_emotion <- renderTable({
      emotion_data()
    })
  })
  
  output$download_emotion <- downloadHandler(
    filename = function() {
      paste("transcriptwise_emotion_scores.xlsx")
    },
    content = function(file) {
      write.xlsx(emotion_data(), file)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
