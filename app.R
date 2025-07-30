library(shiny)
library(plotly)
library(dplyr)

ui <- fluidPage(
  titlePanel("Nonprofit Revenue Prediction Setup"),
  
  uiOutput("mainUI")
)

server <- function(input, output, session) {
  # Reactive values to control flow
  rv <- reactiveValues(confirmed = FALSE)
  
  # Available NTEE categories for dropdown
  ntee_choices <- c("A - Arts, Culture and Humanities",
                    "B - Education",
                    "C - Environment",
                    "D - Animal Related",
                    "E - Health",
                    "F - Mental Health & Crisis Intervention")
  
  # UI shown conditionally
  output$mainUI <- renderUI({
    if (!rv$confirmed) {
      # Input screen
      tagList(
        selectInput("ntee", "Choose NTEE Category", choices = ntee_choices),
        textInput("ein", "Enter EIN (Format: XX-XXXXXXX)", placeholder = "12-3456789"),
        numericInput("n_years", "How many years of data do you have (2-5)?", min = 2, max = 5, value = 2),
        uiOutput("revenueInputs"),
        selectInput("predict_years", "How many years do you want to predict?", choices = 1:3),
        actionButton("submit", "Submit")
      )
    } else {
      # Confirmation + output screen
      tagList(
        tableOutput("summaryTable"),
        radioButtons("confirm", "Does this look correct?", choices = c("Yes", "No")),
        actionButton("confirm_btn", "Submit Confirmation"),
        textOutput("confirmation_message"),
        plotlyOutput("revenuePlot")
      )
    }
  })
  
  # Dynamically create revenue and year inputs
  output$revenueInputs <- renderUI({
    req(input$n_years)
    tagList(
      lapply(1:input$n_years, function(i) {
        fluidRow(
          column(6, numericInput(paste0("year_", i), paste("Year", i), value = 2020 + i)),
          column(6, numericInput(paste0("rev_", i), paste("Revenue", i), value = 100000 * i))
        )
      })
    )
  })
  
  # Capture user input data
  summary_data <- eventReactive(input$submit, {
    req(input$ein, input$ntee, input$n_years)
    years <- sapply(1:input$n_years, function(i) input[[paste0("year_", i)]])
    revs  <- sapply(1:input$n_years, function(i) input[[paste0("rev_", i)]])
    
    # Convert EIN to "EIN-XX-XXXXXXX"
    formatted_ein <- paste0("EIN-", input$ein)
    
    data.frame(
      EIN = rep(formatted_ein, input$n_years),
      NTEE = rep(input$ntee, input$n_years),
      YEAR = years,
      REVENUE = revs
    )
  })
  
  
  # Display table
  output$summaryTable <- renderTable({
    summary_data()
  })
  
  observeEvent(input$submit, {
    rv$confirmed <- TRUE
  })
  
  observeEvent(input$confirm_btn, {
    if (input$confirm == "Yes") {
      output$confirmation_message <- renderText({
        paste("Great, we are proceeding with predicting revenue for the next", input$predict_years, "year(s).")
      })
      
      # Plot: user data + placeholder predictions
      output$revenuePlot <- renderPlotly({
        user_df <- summary_data()
        last_year <- max(user_df$YEAR)
        pred_n <- as.numeric(input$predict_years)
        
        # Placeholder predictions: value of 1 for now
        formatted_ein <- paste0("EIN-", input$ein)
        pred_df <- data.frame(
          EIN = formatted_ein,
          NTEE = input$ntee,
          YEAR = seq(last_year + 1, last_year + pred_n),
          REVENUE = rep(1, pred_n)  # <-- Replace this later
        )
        
        
        full_df <- bind_rows(
          user_df %>% mutate(TYPE = "Reported"),
          pred_df %>% mutate(TYPE = "Predicted")
        )
        
        plot_ly(full_df, x = ~YEAR, y = ~REVENUE, color = ~TYPE, type = 'scatter', mode = 'lines+markers') %>%
          layout(title = paste("EIN:", input$ein, "| NTEE:", input$ntee),
                 xaxis = list(title = "Year"),
                 yaxis = list(title = "Revenue"))
      })
      
    } else {
      rv$confirmed <- FALSE
      output$confirmation_message <- renderText("")
      output$revenuePlot <- renderPlotly(NULL)
    }
  })
}

shinyApp(ui = ui, server = server)
