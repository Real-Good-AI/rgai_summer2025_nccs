library(shiny)
library(plotly)
library(dplyr)
library(data.table)
library(shinycssloaders)
library(future)
library(promises)
source("app_helper.R")

plan(multisession)

ui <- fluidPage(
      titlePanel("Nonprofit Revenue Prediction Setup"),
      uiOutput("mainUI")
)

server <- function(input, output, session) {
      rv <- reactiveValues(confirmed = FALSE)
      loading <- reactiveVal(FALSE)
      start_nn <- reactiveVal(FALSE)
      
      # NTEE dropdown: visible label => backend code
      ntee_choices <- c(
            "Arts, Culture, and Humanities" = "ART",
            "Education (minus Universities)" = "EDU",
            "Environment and Animals" = "ENV",
            "Health (minus Hospitals)" = "HEL",
            "Human Services" = "HMS",
            "Hospitals" = "HOS",
            "International, Foreign Affairs" = "IFA",
            "Mutual/Membership Benefit" = "MMB",
            "Public, Societal Benefit" = "PSB",
            "Religion Related" = "REL",
            "Universities" = "UNI"
      )
      
      # UI layout
      output$mainUI <- renderUI({
            if (!rv$confirmed) {
                  tagList(
                        selectInput("ntee", "Choose NTEE Category", choices = ntee_choices),
                        textInput("ein", "Enter EIN (Format: XX-XXXXXXX)", placeholder = "12-3456789"),
                        numericInput("n_years", "How many years of data do you have (2-5)?", min = 2, max = 5, value = 2),
                        uiOutput("revenueInputs"),
                        selectInput("predict_years", "How many years do you want to predict?", choices = 1:3),
                        actionButton("submit", "Submit")
                  )
            } else {
                  tagList(
                        tableOutput("summaryTable"),
                        radioButtons("confirm", "Does this look correct?", choices = c("Yes", "No")),
                        actionButton("confirm_btn", "Submit Confirmation"),
                        textOutput("confirmation_message"),
                        textOutput("loadingText"),
                        uiOutput("resultsUI"),
                        conditionalPanel("output.showRestart", actionButton("restart", "Start Over"))
                  )
            }
      })
      
      outputOptions(output, "mainUI", suspendWhenHidden = FALSE)
      
      # Dynamic revenue/year inputs
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
      
      summary_data <- eventReactive(input$submit, {
            req(input$ein, input$ntee, input$n_years)
            validate(
                  need(grepl("^\\d{2}-\\d{7}$", input$ein), "EIN must be in format XX-XXXXXXX")
            )
            years <- sapply(1:input$n_years, function(i) input[[paste0("year_", i)]])
            revs <- sapply(1:input$n_years, function(i) input[[paste0("rev_", i)]])
            formatted_ein <- paste0("EIN-", input$ein)
            
            data.frame(
                  EIN = rep(formatted_ein, input$n_years),
                  NTEE = rep(input$ntee, input$n_years),
                  YEAR = years,
                  REVENUE = revs
            )
      })
      
      output$summaryTable <- renderTable({
            summary_data()
      })
      
      observeEvent(input$submit, {
            rv$confirmed <- TRUE
      })
      
      output$loadingText <- renderText({
            if (loading()) "Working on it... Please wait..." else ""
      })
      
      output$showRestart <- reactive({
            !loading() && !is.null(input$confirm) && input$confirm == "Yes"
      })
      outputOptions(output, "showRestart", suspendWhenHidden = FALSE)
      
      observeEvent(input$confirm_btn, {
            if (input$confirm == "No") {
                  rv$confirmed <- FALSE
                  output$confirmation_message <- renderText("")
                  output$loadingText <- renderText("")
                  output$resultsUI <- renderUI(NULL)
            } else {
                  output$confirmation_message <- renderText({
                        paste("Great, we are proceeding with predicting revenue for the next", input$predict_years, "year(s). Please wait...")
                  })
                  output$resultsUI <- renderUI(NULL)
                  loading(TRUE)
                  start_nn(TRUE)
            }
      })
      
      observe({
            req(start_nn())
            start_nn(FALSE)
            
            user_df <- summary_data()
            formatted_ein <- unique(user_df$EIN)
            user_years <- user_df$YEAR
            user_revenue <- user_df$REVENUE
            n_pred <- as.numeric(input$predict_years)
            ntee_cat <- input$ntee
            
            future({
                  tryCatch({
                        res <- nn.search(
                              category = ntee_cat,
                              user.EIN = formatted_ein,
                              user.years = user_years,
                              user.history = user_revenue,
                              n.predict = n_pred
                        )
                        
                        gp_res <- gp.param.opt(
                              user.EIN = formatted_ein,
                              user.years = user_years,
                              user.history = user_revenue,
                              res = res,
                              n.predict = n_pred
                        )
                        
                        user_data_df <- gp.predict(
                              res = res,
                              res.pars = gp_res,
                              user.EIN = formatted_ein,
                              user.years = user_years,
                              user.history = user_revenue,
                              n.predict = n_pred
                        )
                        
                        list(res = res, gp_res = gp_res, user_data_df = user_data_df)
                        
                  }, error = function(e) {
                        message("ERROR in future block: ", e$message)
                        stop(e)  # rethrow for %...!% to catch
                  })
            }, seed = 2727) %...>% (function(results) {
                  res <- results$res
                  gp_res <- results$gp_res
                  user_data_df <- results$user_data_df
                  
                  # Step 3: Render nn.search result table
                  output$resOutput <- renderTable({
                        head(res, 5)
                  })
                  
                  # Step 4: Render gp.predict result
                  output$gpOutput <- renderTable({
                        gp_res
                  })
                  
                  # Step 5: Render gp.param.opt result
                  output$gpPredOutput <- renderTable({
                        head(user_data_df, 5)
                  })
                  
                  # Step 6: Plot
                  output$revenuePlot <- renderPlotly({
                        first_year <- min(user_years)
                        last_year <- max(user_years)
                        user_data_df$TAX_YEAR <- seq(first_year, last_year + n_pred)
                        
                        df_reported <- user_data_df %>% filter(IMPUTE_STATUS == "Reported")
                        df_predicted <- user_data_df %>% filter(IMPUTE_STATUS == "Predicted")
                        df_transition <- user_data_df %>% filter((TAX_YEAR == last_year) | (TAX_YEAR == last_year+1))
                        
                        plot_ly() %>%
                              # Line for reported
                              add_trace(data = df_reported,
                                        x = ~TAX_YEAR, y = ~TOT_REV,
                                        type = 'scatter', mode = 'lines+markers',
                                        line = list(dash = "solid", color = "black"),
                                        marker = list(symbol = "circle", color = "black"),
                                        name = "Reported") %>%
                              
                              # Line for predicted
                              add_trace(data = df_predicted,
                                        x = ~TAX_YEAR, y = ~TOT_REV,
                                        type = 'scatter', mode = 'lines+markers',
                                        line = list(dash = "dash", color = "black"),
                                        marker = list(symbol = "diamond", color = "red"),
                                        name = "Predicted") %>%
                              
                              # Line for transition
                              add_trace(data = df_transition,
                                        x = ~TAX_YEAR, y = ~TOT_REV,
                                        type = 'scatter', mode = 'lines',
                                        line = list(dash = "dash", color = "black"),
                                        name = "Predicted") %>%
                              
                              layout(
                                    title = paste("EIN:", formatted_ein, "| NTEE:", ntee_cat),
                                    xaxis = list(title = "Year"),
                                    yaxis = list(title = "Revenue"),
                                    legend = list(title = list(text = "Data Type"))
                              )
                  })
                  
                  
                  # Render in specified order
                  output$resultsUI <- renderUI({
                        tagList(
                              h4("Nearest Neighbors Result:"),
                              withSpinner(tableOutput("resOutput")),
                              
                              h4("Gaussian Process Optimization Result:"),
                              withSpinner(tableOutput("gpOutput")),
                              
                              h4("Gaussian Process Prediction Output:"),
                              withSpinner(tableOutput("gpPredOutput")),
                              
                              h4("Revenue Plot:"),
                              withSpinner(plotlyOutput("revenuePlot"))
                        )
                  })
                  
                  loading(FALSE)
            }) %...!% {
                  output$confirmation_message <- renderText("Something went wrong while running nn.search or gp.param.opt.")
                  loading(FALSE)
            }
      })
      
      
      observeEvent(input$restart, {
            rv$confirmed <- FALSE
            updateTextInput(session, "ein", value = "")
            updateNumericInput(session, "n_years", value = 2)
            updateSelectInput(session, "ntee", selected = character(0))
            updateSelectInput(session, "predict_years", selected = "1")
            output$confirmation_message <- renderText("")
            output$loadingText <- renderText("")
            output$resultsUI <- renderUI(NULL)
      })
}

shinyApp(ui = ui, server = server)
