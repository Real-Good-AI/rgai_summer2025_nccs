library(shiny)
library(later)
library(plotly)
library(dplyr)
library(data.table)
library(shinycssloaders)
library(future)
library(promises)
library(webshot2)
source("app_helper.R")

plan(multisession)

ui <- fluidPage(
      titlePanel("Nonprofit Revenue Prediction Tool"),
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

      
      observeEvent(input$submit, {
            req(input$ein, input$ntee, input$n_years)
            
            # EIN format check
            if (!grepl("^\\d{2}-\\d{7}$", input$ein)) {
                  showModal(modalDialog(
                        title = "Input Error",
                        "EIN must be in format XX-XXXXXXX (e.g., 12-3456789).",
                        easyClose = TRUE
                  ))
                  return()
            }
            
            # Dynamically gather year and revenue values
            years <- sapply(1:input$n_years, function(i) input[[paste0("year_", i)]])
            revs  <- sapply(1:input$n_years, function(i) input[[paste0("rev_", i)]])
            
            # Check for at least two fully filled-in entries
            valid_entries <- which(!is.na(years) & !is.na(revs))
            if (length(valid_entries) < 2) {
                  showModal(modalDialog(
                        title = "Input Error",
                        "You must fill in at least two years of revenue data to proceed.",
                        easyClose = TRUE
                  ))
                  return()
            }
            
            # All checks passed — now it's safe to flip to the confirmation page
            rv$confirmed <- TRUE
      })
      
      summary_data <- eventReactive(input$submit, {
            req(input$ein, input$ntee, input$n_years)
            
            # EIN format check
            validate(
                  need(grepl("^\\d{2}-\\d{7}$", input$ein), "EIN must be in format XX-XXXXXXX")
            )
            
            years <- sapply(1:input$n_years, function(i) input[[paste0("year_", i)]])
            revs <- sapply(1:input$n_years, function(i) input[[paste0("rev_", i)]])
            
            # Require at least two year-rev pairs
            valid_entries <- which(!is.na(years) & !is.na(revs))
            validate(
                  need(length(valid_entries) >= 2, "You must fill in at least two years of revenue data to proceed.")
            )
            
            formatted_ein <- paste0("EIN-", input$ein)
            
            data.frame(
                  EIN = rep(formatted_ein, length(valid_entries)),
                  NTEE = rep(input$ntee, length(valid_entries)),
                  YEAR = years[valid_entries],
                  REVENUE = revs[valid_entries]
            )
      })
      
      
      output$summaryTable <- renderTable({
            summary_data()
      })
      
      output$loadingText <- renderText({
            if (loading()) "Please wait while we compute..." else ""
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
                        paste("Great, we are proceeding with predicting revenue for the next", input$predict_years, "year(s).")
                  })
                  output$loadingText <- renderText("Please wait while we work on this...")
                  output$resultsUI <- renderUI(NULL)
                  loading(TRUE)
                  
                  # Slight delay so UI can update before heavy computation starts
                  later::later(function() {
                        start_nn(TRUE)
                  }, delay = 0.1)  # 100 milliseconds
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
                        user_data_df <- user_data_df |> 
                              select(-YEAR, -NEIGHBOR_ID) |> 
                              rename(EIN = EIN2, Year = TAX_YEAR, Data_Origin = IMPUTE_STATUS, Revenue = TOT_REV)
                        deg.freedom <- nrow(res |> filter(IMPUTE_STATUS=="original")) - 1
                        user_data_df <- user_data_df |>
                              mutate(Lower_Estimate = case_when(
                                    Data_Origin == "Reported" ~ NA,
                                    Data_Origin == "Predicted" ~ Revenue - qt(0.95, df = deg.freedom) * SE)) |>
                              mutate(Upper_Estimate = case_when(
                                    Data_Origin == "Reported" ~ NA,
                                    Data_Origin == "Predicted" ~ Revenue + qt(0.95, df = deg.freedom) * SE)) |>
                              select(-SE)
                        user_data_df$Year <- seq(2022,2024 + 2)
                        user_data_df |> mutate(Revenue = case_when(
                              Data_Origin == "Reported" ~ dollar_format()(Revenue),
                              Data_Origin == "Predicted" ~ dollar_format()(signif(Revenue, 3)))) |>
                              mutate(Lower_Estimate = dollar_format()(signif(Lower_Estimate,3)),
                                     Upper_Estimate = dollar_format()(signif(Upper_Estimate,3))) |>
                              relocate(Revenue, .after = Data_Origin) |>
                              mutate(Lower_Estimate = replace_na(Lower_Estimate, "Not Applicable"),
                                     Upper_Estimate = replace_na(Upper_Estimate, "Not Applicable"))

                  })
                  
                  # Step 6: Plot
                  output$revenuePlot <- renderPlotly({
                        first_year <- min(user_years)
                        last_year <- max(user_years)
                        user_data_df$TAX_YEAR <- seq(first_year, last_year + n_pred)
                        
                        deg.freedom <- nrow(res |> filter(IMPUTE_STATUS=="original")) - 1
                        user_data_df <- user_data_df |>
                              mutate(CI.LOWER = case_when(
                                    IMPUTE_STATUS == "Reported" ~ TOT_REV,
                                    IMPUTE_STATUS == "Predicted" ~ TOT_REV - qt(0.95, df = deg.freedom) * SE)) |>
                              mutate(CI.UPPER = case_when(
                                    IMPUTE_STATUS == "Reported" ~ TOT_REV,
                                    IMPUTE_STATUS == "Predicted" ~ TOT_REV + qt(0.95, df = deg.freedom) * SE)) 
                        
                        df_reported <- user_data_df %>% filter(IMPUTE_STATUS == "Reported")
                        df_predicted <- user_data_df %>% filter(IMPUTE_STATUS == "Predicted")
                        df_transition <- user_data_df %>% filter((TAX_YEAR == last_year) | (TAX_YEAR == last_year+1))
                        
                        df_peers <- res |> 
                              mutate(label = paste("Organization ", NEIGHBOR_ID, ": ", EIN2, sep = "")) |> 
                              group_by(NEIGHBOR_ID) |> 
                              mutate(TAX_YEAR = seq(first_year, last_year + n_pred)) |> 
                              ungroup() |> 
                              mutate(label = factor(label, levels = unique(label[order(NEIGHBOR_ID)])))
                        
                        
                        p <- plot_ly() %>%
                              # Lines for neighbor organizations
                              add_trace(data = df_peers,
                                        x = ~TAX_YEAR, y = ~TOT_REV, color = ~label, symbol = ~label, 
                                        colors = c("#648FFF", "#785EF0", "#DC267F","#FE6100", "#FFB000"),
                                        opacity = 0.6, type = 'scatter', mode = 'lines+markers') %>%
                              
                              # Line for reported
                              add_trace(data = df_reported,
                                        x = ~TAX_YEAR, y = ~TOT_REV,
                                        type = 'scatter', mode = 'lines+markers',
                                        line = list(dash = "solid", color = "black", width = 3),
                                        marker = list(symbol = "circle", color = "black", size = 8.5),
                                        name = "Reported")
                              
                              # Conditional CI addition
                              if (nrow(df_predicted) > 1) {
                                    p <- p %>%
                                          # Lower bound
                                          add_trace(data = df_predicted,
                                                    x = ~TAX_YEAR, y = ~CI.LOWER,
                                                    type = 'scatter', mode = 'lines',
                                                    line = list(width = 1, color="grey"),
                                                    showlegend = FALSE,
                                                    name = "CI Lower",
                                                    hoverinfo = "none") %>%
                                          # Upper bound with fill
                                          add_trace(data = df_predicted,
                                                    x = ~TAX_YEAR, y = ~CI.UPPER,
                                                    type = 'scatter', mode = 'lines',
                                                    fill = 'tonexty',
                                                    fillcolor = 'rgba(128, 128, 128, 0.2)',
                                                    line = list(width = 1, color="grey"),
                                                    showlegend = TRUE,
                                                    name = "95% Confidence Interval",
                                                    hoverinfo = "none")
                              } else {
                                    # Error bar version
                                    p <- p %>%
                                          add_trace(data = df_predicted,
                                                    x = ~TAX_YEAR, y = ~TOT_REV,
                                                    type = 'scatter', mode = 'markers',
                                                    marker = list(symbol = "circle-open", color = "black", size = 8.5),
                                                    error_y = list(
                                                          type = "data",
                                                          symmetric = FALSE,
                                                          array = df_predicted$CI.UPPER - df_predicted$TOT_REV,
                                                          arrayminus = df_predicted$TOT_REV - df_predicted$CI.LOWER,
                                                          color = "gray",
                                                          showlegend = TRUE
                                                    ),
                                                    name = "95% Confidence Interval")
                              }
                        
                        # Line for predicted
                        p <- p |> add_trace(data = df_predicted,
                                            x = ~TAX_YEAR, y = ~TOT_REV,
                                            type = 'scatter', mode = 'lines+markers',
                                            line = list(dash = "dash", color = "black", width = 2.5),
                                            marker = list(symbol = "circle-open", color = "black", size = 8.5),
                                            name = "Predicted") %>%
                              
                              # Line for transition
                              add_trace(data = df_transition,
                                        x = ~TAX_YEAR, y = ~TOT_REV,
                                        type = 'scatter', mode = 'lines',
                                        line = list(dash = "dash", color = "black", width = 2.5),
                                        name = "", showlegend = FALSE, hoverinfo = "none") %>%
                              
                              
                              layout(
                                    title = paste("Comparing Your Organization to Similar ", ntee_cat, " Organizations"),
                                    xaxis = list(
                                          title = "Year",
                                          tickformat = ".0f",    # force no decimals
                                          tickmode = "linear",   # evenly spaced ticks
                                          dtick = 1,             # step size = 1 year
                                          separatethousands = FALSE  # prevent commas
                                    ),
                                    yaxis = list(title = "Revenue"),
                                    legend = list(title = list(text = "Legend"), traceorder = "normal")
                              )
                        p
                  })
                  
                  output$resNeighborSummary <- renderTable({
                        res |> group_by(NEIGHBOR_ID) |>
                              slice(1) |>
                              ungroup() |>
                              select(EIN2, START_YEAR, END_YEAR, END_PLUS, NEIGHBOR_ID) |>
                              mutate(Matched_Years = paste(START_YEAR, "-", END_YEAR),
                                     Prediction_Years = paste(END_YEAR+1, "-", END_PLUS)) |>
                              select(-START_YEAR, -END_YEAR, -END_PLUS) |>
                              rename(Similarity_Ranking = NEIGHBOR_ID, EIN = EIN2) |>
                              relocate(Similarity_Ranking, .after = last_col())
                  })
                  
                  # output$downloadPlotPNG <- downloadHandler(
                  #       filename = function() {
                  #             paste0("revenue_plot_", Sys.Date(), ".png")
                  #       },
                  #       content = function(file) {
                  #             temp_html <- tempfile(fileext = ".html")
                  #             
                  #             # Build the same plot used in renderPlotly
                  #             plt <- plotly_build(output$revenuePlot())
                  #             
                  #             # Save temporary HTML
                  #             htmlwidgets::saveWidget(plt, temp_html, selfcontained = TRUE)
                  #             
                  #             # Convert to PNG using webshot2
                  #             webshot2::webshot(temp_html, file = file, vwidth = 1000, vheight = 700)
                  #       }
                  # )
                  
                  
                  # Render in specified order
                  output$resultsUI <- renderUI({
                        tagList(
                              # h4("Nearest Neighbors Result:"),
                              # withSpinner(tableOutput("resOutput")),
                              # 
                              # h4("Gaussian Process Optimization Result:"),
                              # withSpinner(tableOutput("gpOutput")),
                              
                              h4("Interactive Revenue Plot:"),
                              withSpinner(plotlyOutput("revenuePlot")),
                              p("Pssttt... This plot is interactive, try hovering over the plot or clicking different aspects like legend items!"),
                              
                              h4("Similar Organizations:"),
                              p("According to our data, these are the 5 organizations with revenue histories most similar to yours! The most similar organization has Similarity Ranking 1. We also show the years for that organization that matched your revenue history, and the years from their organization that we used to predict your future revenue."),
                              withSpinner(tableOutput("resNeighborSummary")),
                              
                              h4("Gaussian Process Prediction Output:"),
                              p(
                                    paste("Using data from the top 5 most similar organizations, we used Gaussian Processes to predict what your next", n_pred, "years will look like.",
                                          "For the predicted revenue, we provide a 95% confidence interval: if we were to low-ball our estimate of that year's revenue, we would give Lower_Estimate. If we were to high-ball it, we'd give Upper_Estimate.")
                              ),
                              withSpinner(tableOutput("gpPredOutput"))
                              
                              # downloadButton("downloadPlotPNG", "Download Plot as PNG")
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
