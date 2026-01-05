# Momentum Predictor Shiny App
# Downloads Bloomberg data, calculates momentum, and runs predictive regressions

library(shiny)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Source helper functions
source("R/bloomberg.R")
source("R/momentum.R")
source("R/regression.R")

# UI Definition
ui <- fluidPage(

  # App title
  titlePanel("Momentum Predictor Analysis"),

  # Sidebar layout
  sidebarLayout(

    # Sidebar panel for inputs
    sidebarPanel(
      width = 3,

      h4("Data Input"),

      textInput(
        "ticker",
        "Bloomberg Ticker 1:",
        value = "SPY US Equity",
        placeholder = "e.g., SPY US Equity"
      ),

      textInput(
        "ticker2",
        "Bloomberg Ticker 2 (optional):",
        value = "",
        placeholder = "e.g., AGG US Equity"
      ),

      helpText("If Ticker 2 is provided, analysis uses the spread (Ticker1 - Ticker2)."),

      dateRangeInput(
        "date_range",
        "Date Range:",
        start = Sys.Date() - 365 * 10,  # 10 years back
        end = Sys.Date() - 1,
        min = "1990-01-01",
        max = Sys.Date()
      ),

      actionButton(
        "fetch_btn",
        "Fetch Data & Analyze",
        class = "btn-primary",
        width = "100%"
      ),

      hr(),

      h4("Analysis Info"),
      p("This app calculates:"),
      tags$ul(
        tags$li("Monthly momentum (1-24 months)"),
        tags$li("EWMA momentum (alpha: 0.01-0.50)"),
        tags$li("Predictive regressions for 1-month forward returns")
      ),

      hr(),

      # Status display
      h4("Status"),
      verbatimTextOutput("status_text")
    ),

    # Main panel for outputs
    mainPanel(
      width = 9,

      tabsetPanel(
        id = "results_tabs",
        type = "tabs",

        # Tab 1: Monthly Momentum Results
        tabPanel(
          "Monthly Momentum",
          br(),
          h4("Monthly Momentum Regression Results"),
          p("Predicting 1-month forward returns using past momentum (1-24 months)"),
          withSpinner(DTOutput("monthly_table")),
          br(),
          h4("Scatter Plots (Significant Predictors)"),
          withSpinner(plotOutput("monthly_plots", height = "600px"))
        ),

        # Tab 2: EWMA Momentum Results
        tabPanel(
          "EWMA Momentum",
          br(),
          h4("EWMA Momentum Regression Results"),
          p("Predicting 1-month forward returns using EWMA of daily returns"),
          withSpinner(DTOutput("ewma_table")),
          br(),
          h4("Scatter Plots (Significant Predictors)"),
          withSpinner(plotOutput("ewma_plots", height = "600px"))
        ),

        # Tab 3: Summary of Significant Predictors
        tabPanel(
          "Significant Predictors",
          br(),
          h4("Summary of Significant Predictors (p < 0.05)"),
          p("All momentum measures that significantly predict 1-month forward returns"),
          withSpinner(DTOutput("significant_table")),
          br(),
          h4("Significance Overview"),
          withSpinner(verbatimTextOutput("significance_summary"))
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values to store results
  rv <- reactiveValues(
    data = NULL,
    data2 = NULL,
    monthly_mom = NULL,
    ewma_mom = NULL,
    monthly_results = NULL,
    ewma_results = NULL,
    is_spread = FALSE,
    analysis_label = "",
    status = "Ready. Enter a ticker and click 'Fetch Data & Analyze'."
  )

  # Status output
  output$status_text <- renderText({
    rv$status
  })

  # Main analysis triggered by button click
  observeEvent(input$fetch_btn, {

    # Validate inputs
    req(input$ticker)
    req(input$date_range)

    ticker <- trimws(input$ticker)
    ticker2 <- trimws(input$ticker2)
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]

    if (nchar(ticker) == 0) {
      rv$status <- "Error: Please enter a valid ticker."
      return()
    }

    if (start_date >= end_date) {
      rv$status <- "Error: Start date must be before end date."
      return()
    }

    # Check if we're doing spread analysis
    use_spread <- nchar(ticker2) > 0
    rv$is_spread <- use_spread

    if (use_spread) {
      rv$analysis_label <- paste0("Spread (", ticker, " - ", ticker2, ")")
    } else {
      rv$analysis_label <- ticker
    }

    # Start analysis
    rv$status <- "Connecting to Bloomberg..."

    tryCatch({

      # Step 1: Connect to Bloomberg
      connected <- connect_bloomberg()
      if (!connected) {
        rv$status <- "Error: Could not connect to Bloomberg Terminal."
        return()
      }
      rv$status <- "Connected. Fetching data..."

      # Step 2: Fetch data for ticker 1
      raw_data1 <- fetch_bloomberg_data(ticker, start_date, end_date)
      rv$data <- raw_data1

      # Step 2b: If spread mode, fetch ticker 2 and calculate spread
      if (use_spread) {
        rv$status <- paste("Fetched", ticker, "(", nrow(raw_data1), "obs). Fetching", ticker2, "...")
        raw_data2 <- fetch_bloomberg_data(ticker2, start_date, end_date)
        rv$data2 <- raw_data2
        rv$status <- paste("Fetched both tickers. Calculating spread returns...")

        # Calculate spread data
        analysis_data <- calc_spread_data(raw_data1, raw_data2)
        rv$status <- paste("Spread calculated:", nrow(analysis_data), "common dates. Calculating momentum...")
      } else {
        analysis_data <- raw_data1
        rv$status <- paste("Data fetched:", nrow(analysis_data), "daily observations. Calculating momentum...")
      }

      # Step 3: Calculate momentum
      momentum_data <- prepare_momentum_data(analysis_data)
      rv$monthly_mom <- momentum_data$monthly_mom
      rv$ewma_mom <- momentum_data$ewma_mom
      rv$status <- "Momentum calculated. Running regressions..."

      # Step 4: Run regressions
      rv$monthly_results <- run_monthly_momentum_regressions(rv$monthly_mom)
      rv$ewma_results <- run_ewma_momentum_regressions(rv$ewma_mom)

      # Count significant predictors
      n_sig_monthly <- sum(rv$monthly_results$significant, na.rm = TRUE)
      n_sig_ewma <- sum(rv$ewma_results$significant, na.rm = TRUE)

      analysis_type <- if (use_spread) "Spread analysis" else "Analysis"
      rv$status <- paste0(
        analysis_type, " complete!\n",
        "Analyzing: ", rv$analysis_label, "\n",
        "Monthly momentum: ", n_sig_monthly, "/24 significant\n",
        "EWMA momentum: ", n_sig_ewma, "/50 significant"
      )

    }, error = function(e) {
      rv$status <- paste("Error:", e$message)
    })
  })

  # Monthly momentum table
  output$monthly_table <- renderDT({
    req(rv$monthly_results)

    formatted <- format_regression_results(rv$monthly_results)

    datatable(
      formatted,
      options = list(
        pageLength = 24,
        dom = 't',
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Significant',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#C8E6C9', '#FFCDD2'))
      ) %>%
      formatStyle(
        'p-value',
        color = styleInterval(0.05, c('green', 'red'))
      )
  })

  # EWMA momentum table
  output$ewma_table <- renderDT({
    req(rv$ewma_results)

    formatted <- format_regression_results(rv$ewma_results)

    datatable(
      formatted,
      options = list(
        pageLength = 25,
        dom = 'tp',
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatStyle(
        'Significant',
        backgroundColor = styleEqual(c(TRUE, FALSE), c('#C8E6C9', '#FFCDD2'))
      ) %>%
      formatStyle(
        'p-value',
        color = styleInterval(0.05, c('green', 'red'))
      )
  })

  # Significant predictors table
  output$significant_table <- renderDT({
    req(rv$monthly_results, rv$ewma_results)

    sig_summary <- get_significant_summary(rv$monthly_results, rv$ewma_results)

    if (nrow(sig_summary) == 0) {
      return(datatable(
        data.frame(Message = "No significant predictors found at p < 0.05"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }

    datatable(
      sig_summary,
      options = list(
        pageLength = 50,
        dom = 't',
        ordering = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c('Coefficient', 't-stat', 'p-value', 'R-squared (%)'), digits = 4)
  })

  # Significance summary text
  output$significance_summary <- renderText({
    req(rv$monthly_results, rv$ewma_results)

    n_monthly_sig <- sum(rv$monthly_results$significant, na.rm = TRUE)
    n_ewma_sig <- sum(rv$ewma_results$significant, na.rm = TRUE)

    # Best monthly predictor
    best_monthly <- rv$monthly_results %>%
      filter(significant == TRUE) %>%
      arrange(p_value) %>%
      slice(1)

    # Best EWMA predictor
    best_ewma <- rv$ewma_results %>%
      filter(significant == TRUE) %>%
      arrange(p_value) %>%
      slice(1)

    summary_text <- paste0(
      "SUMMARY\n",
      "=======\n",
      "Analyzing: ", rv$analysis_label, "\n\n",
      "Monthly Momentum:\n",
      "  - Significant predictors: ", n_monthly_sig, " out of 24\n"
    )

    if (nrow(best_monthly) > 0) {
      summary_text <- paste0(
        summary_text,
        "  - Best predictor: ", best_monthly$predictor_label, "\n",
        "    (t-stat = ", round(best_monthly$t_statistic, 3),
        ", p = ", round(best_monthly$p_value, 4),
        ", R2 = ", round(best_monthly$r_squared * 100, 2), "%)\n"
      )
    }

    summary_text <- paste0(
      summary_text,
      "\nEWMA Momentum:\n",
      "  - Significant predictors: ", n_ewma_sig, " out of 50\n"
    )

    if (nrow(best_ewma) > 0) {
      summary_text <- paste0(
        summary_text,
        "  - Best predictor: ", best_ewma$predictor_label, "\n",
        "    (t-stat = ", round(best_ewma$t_statistic, 3),
        ", p = ", round(best_ewma$p_value, 4),
        ", R2 = ", round(best_ewma$r_squared * 100, 2), "%)\n"
      )
    }

    if (n_monthly_sig == 0 && n_ewma_sig == 0) {
      summary_text <- paste0(
        summary_text,
        "\nCONCLUSION: No momentum measure significantly predicts\n",
        "1-month forward returns for this ticker at the 5% level."
      )
    } else {
      summary_text <- paste0(
        summary_text,
        "\nCONCLUSION: ", n_monthly_sig + n_ewma_sig,
        " momentum measures significantly predict\n",
        "1-month forward returns at the 5% level."
      )
    }

    return(summary_text)
  })

  # Monthly momentum scatter plots
  output$monthly_plots <- renderPlot({
    req(rv$monthly_results, rv$monthly_mom)

    # Get significant predictors
    sig_predictors <- rv$monthly_results %>%
      filter(significant == TRUE)

    if (nrow(sig_predictors) == 0) {
      # Show message if no significant predictors
      plot.new()
      text(0.5, 0.5, "No significant monthly momentum predictors found.",
           cex = 1.5, col = "gray50")
      return()
    }

    # Limit to top 6 most significant
    sig_predictors <- sig_predictors %>%
      arrange(p_value) %>%
      head(6)

    # Create plots
    plots <- lapply(1:nrow(sig_predictors), function(i) {
      row <- sig_predictors[i, ]
      create_scatter_plot(
        rv$monthly_mom,
        row$predictor,
        row$predictor_label,
        significant = TRUE
      )
    })

    # Arrange in grid
    n_plots <- length(plots)
    n_cols <- min(3, n_plots)
    n_rows <- ceiling(n_plots / n_cols)

    do.call(gridExtra::grid.arrange, c(plots, ncol = n_cols))
  })

  # EWMA momentum scatter plots
  output$ewma_plots <- renderPlot({
    req(rv$ewma_results, rv$ewma_mom)

    # Get significant predictors
    sig_predictors <- rv$ewma_results %>%
      filter(significant == TRUE)

    if (nrow(sig_predictors) == 0) {
      plot.new()
      text(0.5, 0.5, "No significant EWMA momentum predictors found.",
           cex = 1.5, col = "gray50")
      return()
    }

    # Limit to top 6 most significant
    sig_predictors <- sig_predictors %>%
      arrange(p_value) %>%
      head(6)

    # Create plots
    plots <- lapply(1:nrow(sig_predictors), function(i) {
      row <- sig_predictors[i, ]
      create_scatter_plot(
        rv$ewma_mom,
        row$predictor,
        row$predictor_label,
        significant = TRUE
      )
    })

    # Arrange in grid
    n_plots <- length(plots)
    n_cols <- min(3, n_plots)
    n_rows <- ceiling(n_plots / n_cols)

    do.call(gridExtra::grid.arrange, c(plots, ncol = n_cols))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
