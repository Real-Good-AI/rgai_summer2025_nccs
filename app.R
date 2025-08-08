library(shiny)
library(later)
library(plotly)
library(dplyr)
library(data.table)
library(shinycssloaders)
library(future)
library(promises)
library(webshot2)
library(progressr)
library(shinyBS)
source("app_helper.R")

handlers("shiny")
plan(multisession)

df <- readMegaDF()
ui <- fluidPage(
      titlePanel("Nonprofit Revenue Prediction Tool"),
      div(
            style = "position: absolute; top: 15px; right: 15px; z-index: 1000;",
            # use either a Bootstrap class for color, or inline CSS:
            actionButton(
                  "help", "Help",
                  style = "background-color: #0075a3; color: white;"
            )
      ),
      uiOutput("mainUI")
)

server <- function(input, output, session) {
      rv <- reactiveValues(confirmed = FALSE, existingData = NULL)
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
      
      # Help modal
      observeEvent(input$help, {
            showModal(modalDialog(
                  title = NULL,
                  # wrap the two lines in tagList()
                  tagList(
                        p("If you need help or have any feedback, please email us at ", 
                          tags$a(href = "mailto:contact@realgoodai.org", "contact@realgoodai.org")),
                        p(
                              "You can also read the ",
                              tags$a(
                                    "User Guide",
                                    href   = "https://www.realgoodai.org/contact",
                                    target = "_blank"
                              ),
                              " for step-by-step instructions."
                        )
                  ),
                  footer    = modalButton("Close"),
                  easyClose = TRUE
            ))
      })
      
      # UI layout for the first page (user input)
      output$mainUI <- renderUI({
            if (!rv$confirmed) {
                  tagList(
                        selectInput("ntee", "Choose NTEE Broad Category", choices = ntee_choices),
                        textInput("ein", "Enter EIN (Format: XX-XXXXXXX)", placeholder = "12-3456789", value = "12-3456789"),
                        numericInput("n_years", "How many years of data do you have (2-5)?", min = 2, max = 5, value = 2),
                        uiOutput("revenueInputs"),
                        selectInput("predict_years", "How many years do you want to predict?", choices = 1:3),
                        actionButton("submit", "Submit"),
                        
                        # Now define tooltips for those IDs:
                        shinyBS::bsTooltip("ntee",
                                           "Refer to the user guide for help determining your category!",
                                           placement = "right", trigger = "hover"
                        ),
                        shinyBS::bsTooltip("ein",
                                           "Your organization’s Employer ID Number. Format must be 12-3456789.",
                                           placement = "right", trigger = "hover"
                        ),
                        shinyBS::bsTooltip("n_years",
                                           "You will need total revenue values for each year (refer to user guide for more info)",
                                           placement = "right", trigger = "hover"
                        )
                  )
                  
            } else {
                  tagList(
                        # a row with two columns: user input vs. existing data (if any)
                        fluidRow(
                              column(6,
                                     h4("Your entries"),
                                     tableOutput("summaryTable")
                              ),
                              # only show column 2 if we found existingData
                              if (!is.null(rv$existingData)) column(6,
                                                                    h4("NCCS Core Dataset"),
                                                                    p("Oh! It looks like we already have data for some of these years:"),
                                                                    tableOutput("existingDataTable")
                              )
                        ),
                        # an explanatory note when we did find existing data
                        if (!is.null(rv$existingData)) p(
                              "If your numbers match ours, great—just confirm below. No need to worry if the numbers are off by only a few dollars. ",
                              "If the numbers have a big discrepancy, it's possible that we have a mistake in our records, but please double-check your inputs before proceeding. If your entries look good, confirm below. Otherwise, feel free to start over."
                        ),
                        # now the old confirm UI
                        radioButtons("confirm", "Does this look correct?",
                                     choices = c("Yes", "No")),
                        actionButton("confirm_btn", "Submit Confirmation"),
                        textOutput("confirmation_message"),
                        textOutput("loadingText"),
                        uiOutput("resultsUI"),
                        conditionalPanel("output.showRestart",
                                         actionButton("restart", "Start Over")
                        )
                  )
            }
      })
      
      outputOptions(output, "mainUI", suspendWhenHidden = FALSE)
      
      # Dynamic revenue/year inputs depending on whether user has 2-5 of revenue history
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

      # Second page, after the user hits the "submit" button on the first page
      # Includes user input validation steps
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
            
            # Make sure the user did not put in only 0's
            if (all(revs == 0)) {
                  showModal(modalDialog(
                        title = "Input Error",
                        "It looks like you only input $0 revenue. We require at least one year of nonzero revenue history because this tool isn't reliable if the revenue history is all zeros. If your revenue history in the last 5 years is all zeros, this tool won't work for you--but you can still play around with hypothetical numbers, just proceed with caution.",
                        easyClose = TRUE
                  ))
                  return()
            }
            
            # look up in your preloaded `df` to see if they input information we already have
            formatted_ein <- paste0("EIN-", input$ein)
            df_existing <- df |>
                  filter(EIN2 == formatted_ein, TAX_YEAR %in% years) |>
                  filter(IMPUTE_STATUS == "original")
            
            if (nrow(df_existing) > 0) {
                  rv$existingData <- df_existing |> select(-IMPUTE_STATUS) |> mutate(TAX_YEAR = as.integer(TAX_YEAR)) |> rename(EIN = EIN2, YEAR = TAX_YEAR, REVENUE = TOT_REV, CURRENT_NAME = ORG_NAME_CURRENT)
            }
            
            # All checks passed — now it's safe to flip to the confirmation page
            rv$confirmed <- TRUE
      })
      
      # Table we show the user with the information they submitted, asking them to confirm if it looks correct
      summary_data <- eventReactive(input$submit, {
            req(input$ein, input$ntee, input$n_years)
            df <- readMegaDF()
            
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
      
      output$existingDataTable <- renderTable({
            req(rv$existingData)
            rv$existingData
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
      
      # Askign user to confirm if their data looks correct so we can proceed with analysis
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
                  output$loadingText <- renderText("Please wait. Thanks!")
                  output$resultsUI <- renderUI(NULL)
                  loading(TRUE)
                  
                  showModal(modalDialog(
                        title = "Working…",
                        p("Please hang tight while we crunch the numbers--this may take a minute or two! Take a water or bathroom break, or do some deep breathing. :)"),
                        footer = NULL,
                        easyClose = FALSE
                  ))
                  
                  # Slight delay so UI can update before heavy computation starts
                  later::later(function() {
                        start_nn(TRUE)
                  }, delay = 0.1)  # 100 milliseconds
            }
      })
      
      # Step 1: Analysis i.e. nearest neighbor search (nn.search), GP tuning (gp.param.opt), and prediction (gp.predict)
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
                        # res is a dataframe with the data corresponding to the top 5 nearest neighbors
                        res <- nn.search(
                              category = ntee_cat,
                              user.EIN = formatted_ein,
                              user.years = user_years,
                              user.history = user_revenue,
                              n.predict = n_pred
                        )
                        # gp_res is a dataframe with one row containing the optimal nu and nugget values for the GP
                        gp_res <- gp.param.opt(
                              user.EIN = formatted_ein,
                              user.years = user_years,
                              user.history = user_revenue,
                              res = res,
                              n.predict = n_pred
                        )
                        # user_data_df is a data.frame with the prediction for the user's organization from the GP trained on top 5 neighbors' data
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
                  removeModal()
                  res <- results$res
                  gp_res <- results$gp_res
                  user_data_df <- results$user_data_df
                  
                  # Step 2: Create the GP output table for the user, with user-friendly terms
                  # We rename the columns, add confidence intervals, make sure the years are filled in for each value and do some formatting so the output looks nice
                  output$gpPredOutput <- renderTable({
                        user_data_df <- user_data_df |> 
                              select(-YEAR, -NEIGHBOR_ID) |> 
                              rename(EIN = EIN2, Year = TAX_YEAR, Data_Origin = IMPUTE_STATUS, Revenue = TOT_REV)
                        deg.freedom <- nrow(res |> filter(!IMPUTED)) - 1
                        user_data_df <- user_data_df |>
                              mutate(Lower_Estimate = case_when(
                                    Data_Origin == "Reported" ~ NA,
                                    Data_Origin == "Predicted" ~ Revenue - qt(0.95, df = deg.freedom) * SE)) |>
                              mutate(Upper_Estimate = case_when(
                                    Data_Origin == "Reported" ~ NA,
                                    Data_Origin == "Predicted" ~ Revenue + qt(0.95, df = deg.freedom) * SE)) |>
                              select(-SE)
                        
                        first_year <- min(user_years)
                        last_year <- max(user_years)
                        user_data_df$Year <- seq(first_year, last_year + n_pred)
                        
                        user_data_df |> mutate(Revenue = case_when(
                              Data_Origin == "Reported" ~ dollar_format()(Revenue),
                              Data_Origin == "Predicted" ~ dollar_format()(signif(Revenue, 3)))) |>
                              mutate(Lower_Estimate = dollar_format()(signif(Lower_Estimate,3)),
                                     Upper_Estimate = dollar_format()(signif(Upper_Estimate,3))) |>
                              relocate(Revenue, .after = Data_Origin) |>
                              mutate(Lower_Estimate = replace_na(Lower_Estimate, "Not Applicable"),
                                     Upper_Estimate = replace_na(Upper_Estimate, "Not Applicable"))

                  })
                  
                  # Step 3: Create the interactive plot
                  output$revenuePlot <- renderPlotly({
                        p <- app.plot(user_years, ntee_cat, n_pred, user_data_df, res)
                        p
                  })
                  
                  # Step 4: Create the "top 5 similar organizations" summary table 
                  output$resNeighborSummary <- renderTable({
                        res |> group_by(NEIGHBOR_ID) |>
                              slice(1) |>
                              ungroup() |>
                              select(EIN2, START_YEAR, END_YEAR, END_PLUS, NEIGHBOR_ID, ORG_NAME_CURRENT) |>
                              mutate(Matched_Years = paste(START_YEAR, "-", END_YEAR),
                                     Prediction_Years = paste(END_YEAR+1, "-", END_PLUS)) |>
                              select(-START_YEAR, -END_YEAR, -END_PLUS) |>
                              rename(Similarity_Ranking = NEIGHBOR_ID, EIN = EIN2, Current_Organization_Name = ORG_NAME_CURRENT) |>
                              relocate(Similarity_Ranking, .after = last_col())
                  })
                  
                  
                  
                  # Render in specified order
                  output$resultsUI <- renderUI({
                        tagList(
                              h4("Interactive Revenue Plot"),
                              # collapsible explainer
                              bsCollapse(
                                    id     = "plot_help",
                                    open   = NULL,              # start closed
                                    bsCollapsePanel(
                                          "Did you know the plot is interactive?",
                                          "Click on legend items or lines to isolate a series or zoom in. Try it!",
                                          style = "info"
                                    )
                              ),
                              withSpinner(plotlyOutput("revenuePlot")),
                              
                              h4("Similar Organizations"),
                              bsCollapse(
                                    id   = "similar_help",
                                    bsCollapsePanel(
                                          "Click me for more info!",
                                          "We find the 5 nonprofits whose past revenue histories best match yours and then use their trajectories to inform your forecast.
                                          The most similar organization has Similarity Ranking 1. We also show the years for that organization that matched your revenue history, and the years from their organization that we used to predict your future revenue."
                                    )
                              ),
                              withSpinner(tableOutput("resNeighborSummary")),

                              
                              h4("Gaussian Process Prediction Output"),
                              bsCollapse(
                                    id   = "gp_help",
                                    bsCollapsePanel(
                                          "Click me for more info!",
                                          paste("Using data from the top 5 most similar organizations, we used Gaussian Processes to predict what your next", n_pred, "years will look like.",
                                                "For the predicted revenue, we provide a 95% confidence interval: if we were to low-ball our estimate of that year's revenue, we would give Lower_Estimate. If we were to high-ball it, we'd give Upper_Estimate.")
                                    )
                              ),
                              withSpinner(tableOutput("gpPredOutput"))
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
