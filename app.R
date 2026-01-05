# Momentum Predictor Shiny App
# Downloads Bloomberg data, calculates momentum, and runs predictive regressions
# Supports multiple forecast horizons, Newey-West standard errors, and robust regression

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

# All forecast horizons to calculate
ALL_HORIZONS <- c(1, 3, 12, 36)

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

      hr(),

      h4("Regression Settings"),

      selectInput(
        "horizon",
        "Display Horizon:",
        choices = c(
          "1-month" = 1,
          "3-month" = 3,
          "12-month" = 12,
          "36-month" = 36
        ),
        selected = 1
      ),

      checkboxInput(
        "use_robust",
        "Use Robust Regression (rlm)",
        value = FALSE
      ),

      helpText("All horizons (1, 3, 12, 36 months) are calculated on fetch. Use dropdown to switch views."),
      helpText("Standard errors use Newey-West HAC estimator."),

      hr(),

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
        tags$li("Regressions for 1, 3, 12, 36-month returns")
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
          h4(textOutput("monthly_title")),
          p(textOutput("monthly_subtitle")),
          withSpinner(DTOutput("monthly_table")),
          br(),
          h4("Scatter Plots (Significant Predictors)"),
          withSpinner(plotOutput("monthly_plots", height = "600px"))
        ),

        # Tab 2: EWMA Momentum Results
        tabPanel(
          "EWMA Momentum",
          br(),
          h4(textOutput("ewma_title")),
          p(textOutput("ewma_subtitle")),
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
          p(textOutput("sig_subtitle")),
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
    # Store results for ALL horizons and BOTH regression types
    monthly_results_ols = NULL,     # OLS results for all horizons
    monthly_results_robust = NULL,  # Robust results for all horizons
    ewma_results_ols = NULL,        # OLS results for all horizons
    ewma_results_robust = NULL,     # Robust results for all horizons
    is_spread = FALSE,
    analysis_label = "",
    data_loaded = FALSE,
    status = "Ready. Enter a ticker and click 'Fetch Data & Analyze'."
  )

  # Get currently selected horizon
  selected_horizon <- reactive({
    as.integer(input$horizon)
  })


  # Check if robust regression is selected
  use_robust <- reactive({
    input$use_robust
  })

  # Filter results for selected horizon and regression type
  monthly_results_filtered <- reactive({
    if (use_robust()) {
      req(rv$monthly_results_robust)
      rv$monthly_results_robust %>%
        filter(horizon == selected_horizon())
    } else {
      req(rv$monthly_results_ols)
      rv$monthly_results_ols %>%
        filter(horizon == selected_horizon())
    }
  })

  ewma_results_filtered <- reactive({
    if (use_robust()) {
      req(rv$ewma_results_robust)
      rv$ewma_results_robust %>%
        filter(horizon == selected_horizon())
    } else {
      req(rv$ewma_results_ols)
      rv$ewma_results_ols %>%
        filter(horizon == selected_horizon())
    }
  })

  # Status output
  output$status_text <- renderText({
    rv$status
  })

  # Dynamic titles
  output$monthly_title <- renderText({
    horizon <- selected_horizon()
    robust_label <- if (use_robust()) " (Robust)" else ""
    paste0("Monthly Momentum Regression Results", robust_label)
  })

  output$monthly_subtitle <- renderText({
    horizon <- selected_horizon()
    paste0("Predicting ", horizon, "-month forward returns using past momentum (1-24 months). ",
           "Standard errors: Newey-West HAC (lag = ", max(0, horizon - 1), ")")
  })

  output$ewma_title <- renderText({
    robust_label <- if (use_robust()) " (Robust)" else ""
    paste0("EWMA Momentum Regression Results", robust_label)
  })

  output$ewma_subtitle <- renderText({
    horizon <- selected_horizon()
    paste0("Predicting ", horizon, "-month forward returns using EWMA of daily returns. ",
           "Standard errors: Newey-West HAC (lag = ", max(0, horizon - 1), ")")
  })

  output$sig_subtitle <- renderText({
    horizon <- selected_horizon()
    paste0("All momentum measures that significantly predict ", horizon, "-month forward returns")
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

      rv$status <- "Momentum calculated. Running OLS regressions for all horizons..."

      # Step 4: Run OLS regressions for ALL horizons
      monthly_ols_list <- lapply(ALL_HORIZONS, function(h) {
        run_monthly_momentum_regressions(rv$monthly_mom, horizon = h, use_robust = FALSE)
      })
      ewma_ols_list <- lapply(ALL_HORIZONS, function(h) {
        run_ewma_momentum_regressions(rv$ewma_mom, horizon = h, use_robust = FALSE)
      })

      rv$status <- "OLS complete. Running robust regressions for all horizons..."

      # Step 5: Run Robust regressions for ALL horizons
      monthly_robust_list <- lapply(ALL_HORIZONS, function(h) {
        run_monthly_momentum_regressions(rv$monthly_mom, horizon = h, use_robust = TRUE)
      })
      ewma_robust_list <- lapply(ALL_HORIZONS, function(h) {
        run_ewma_momentum_regressions(rv$ewma_mom, horizon = h, use_robust = TRUE)
      })

      # Combine all results
      rv$monthly_results_ols <- do.call(rbind, monthly_ols_list)
      rv$ewma_results_ols <- do.call(rbind, ewma_ols_list)
      rv$monthly_results_robust <- do.call(rbind, monthly_robust_list)
      rv$ewma_results_robust <- do.call(rbind, ewma_robust_list)
      rv$data_loaded <- TRUE

      # Count significant predictors for each horizon (OLS)
      sig_counts_ols <- sapply(ALL_HORIZONS, function(h) {
        n_monthly <- sum(rv$monthly_results_ols$significant[rv$monthly_results_ols$horizon == h], na.rm = TRUE)
        n_ewma <- sum(rv$ewma_results_ols$significant[rv$ewma_results_ols$horizon == h], na.rm = TRUE)
        n_monthly + n_ewma
      })

      analysis_type <- if (use_spread) "Spread analysis" else "Analysis"
      rv$status <- paste0(
        analysis_type, " complete!\n",
        "Analyzing: ", rv$analysis_label, "\n",
        "Both OLS and Robust regressions calculated.\n",
        "OLS significant predictors:\n  ",
        paste(paste0(ALL_HORIZONS, "m: ", sig_counts_ols), collapse = ", ")
      )

    }, error = function(e) {
      rv$status <- paste("Error:", e$message)
    })
  })

  # Monthly momentum table
  output$monthly_table <- renderDT({
    req(monthly_results_filtered())

    formatted <- format_regression_results(monthly_results_filtered())

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
    req(ewma_results_filtered())

    formatted <- format_regression_results(ewma_results_filtered())

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
    req(monthly_results_filtered(), ewma_results_filtered())

    sig_summary <- get_significant_summary(monthly_results_filtered(), ewma_results_filtered())

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
    req(monthly_results_filtered(), ewma_results_filtered())

    horizon <- selected_horizon()
    monthly_filtered <- monthly_results_filtered()
    ewma_filtered <- ewma_results_filtered()

    n_monthly_sig <- sum(monthly_filtered$significant, na.rm = TRUE)
    n_ewma_sig <- sum(ewma_filtered$significant, na.rm = TRUE)

    # Best monthly predictor
    best_monthly <- monthly_filtered %>%
      filter(significant == TRUE) %>%
      arrange(p_value) %>%
      slice(1)

    # Best EWMA predictor
    best_ewma <- ewma_filtered %>%
      filter(significant == TRUE) %>%
      arrange(p_value) %>%
      slice(1)

    regression_type <- if (use_robust()) "Robust (rlm)" else "OLS"
    nw_lag <- max(0, horizon - 1)

    summary_text <- paste0(
      "SUMMARY\n",
      "=======\n",
      "Analyzing: ", rv$analysis_label, "\n",
      "Forecast horizon: ", horizon, " month(s)\n",
      "Regression: ", regression_type, " | Newey-West lag: ", nw_lag, "\n\n",
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
        horizon, "-month forward returns at the 5% level."
      )
    } else {
      summary_text <- paste0(
        summary_text,
        "\nCONCLUSION: ", n_monthly_sig + n_ewma_sig,
        " momentum measures significantly predict\n",
        horizon, "-month forward returns at the 5% level."
      )
    }

    return(summary_text)
  })

  # Monthly momentum scatter plots
  output$monthly_plots <- renderPlot({
    req(monthly_results_filtered(), rv$monthly_mom)

    horizon <- selected_horizon()

    # Get significant predictors
    sig_predictors <- monthly_results_filtered() %>%
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
        horizon = horizon,
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
    req(ewma_results_filtered(), rv$ewma_mom)

    horizon <- selected_horizon()

    # Get significant predictors
    sig_predictors <- ewma_results_filtered() %>%
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
        horizon = horizon,
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
