# Regression Analysis Functions
# Runs predictive regressions of forward returns on momentum measures
# Supports Newey-West HAC standard errors and robust regression (rlm)

library(dplyr)
library(broom)
library(ggplot2)
library(lmtest)
library(sandwich)
library(MASS)

#' Run a single predictive regression with Newey-West standard errors
#'
#' @param data Data frame containing dependent and independent variables
#' @param y_var Character, name of dependent variable column
#' @param x_var Character, name of independent variable column
#' @param use_robust Logical, whether to use robust regression (rlm) instead of OLS
#' @param horizon Integer, the forecast horizon in months (for Newey-West lag selection)
#' @return Data frame with regression statistics
run_single_regression <- function(data, y_var, x_var, use_robust = FALSE, horizon = 1) {

  # Filter complete cases for these two variables
  reg_data <- data %>%
    select(all_of(c(y_var, x_var))) %>%
    filter(complete.cases(.))

  if (nrow(reg_data) < 10) {
    return(data.frame(
      predictor = x_var,
      coefficient = NA,
      std_error = NA,
      t_statistic = NA,
      p_value = NA,
      r_squared = NA,
      adj_r_squared = NA,
      n_obs = nrow(reg_data),
      significant = FALSE
    ))
  }

  # Run regression
  formula <- as.formula(paste(y_var, "~", x_var))

  if (use_robust) {
    # Robust regression using rlm (M-estimation with Huber weights)
    model <- rlm(formula, data = reg_data, maxit = 100)

    # For rlm, we still use Newey-West for HAC inference
    # Get the underlying design matrix for sandwich estimation
    # Use lm for R-squared calculation
    lm_model <- lm(formula, data = reg_data)
    model_summary_lm <- summary(lm_model)

    # Newey-West lag: use horizon - 1 for overlapping returns (minimum 0)
    nw_lag <- max(0, horizon - 1)

    # Get Newey-West covariance matrix for the robust model
    # Since rlm doesn't directly support sandwich, we compute robust SEs differently
    # We'll use the rlm standard errors which are already robust to outliers
    model_summary <- summary(model)

    # Extract coefficient info from rlm
    coef_est <- coef(model)[x_var]
    std_err <- model_summary$coefficients[x_var, "Std. Error"]
    t_stat <- coef_est / std_err
    # rlm uses t-distribution; approximate p-value
    df <- nrow(reg_data) - 2
    p_val <- 2 * pt(-abs(t_stat), df = df)

    r_sq <- model_summary_lm$r.squared
    adj_r_sq <- model_summary_lm$adj.r.squared

  } else {
    # Standard OLS with Newey-West HAC standard errors
    model <- lm(formula, data = reg_data)
    model_summary <- summary(model)

    # Newey-West lag: use horizon - 1 for overlapping returns (minimum 0)
    # This accounts for the induced autocorrelation from overlapping observations
    nw_lag <- max(0, horizon - 1)

    # Compute Newey-West HAC covariance matrix
    nw_vcov <- NeweyWest(model, lag = nw_lag, prewhite = FALSE)

    # Get coefficient test with Newey-West standard errors
    nw_test <- coeftest(model, vcov = nw_vcov)

    # Extract statistics for the slope coefficient
    coef_est <- nw_test[x_var, "Estimate"]
    std_err <- nw_test[x_var, "Std. Error"]
    t_stat <- nw_test[x_var, "t value"]
    p_val <- nw_test[x_var, "Pr(>|t|)"]

    r_sq <- model_summary$r.squared
    adj_r_sq <- model_summary$adj.r.squared
  }

  result <- data.frame(
    predictor = x_var,
    coefficient = coef_est,
    std_error = std_err,
    t_statistic = t_stat,
    p_value = p_val,
    r_squared = r_sq,
    adj_r_squared = adj_r_sq,
    n_obs = nrow(reg_data),
    significant = p_val < 0.05
  )

  return(result)
}

#' Run predictive regressions for all monthly momentum measures
#'
#' @param monthly_mom Data frame with monthly momentum columns and forward returns
#' @param horizon Integer, forecast horizon in months (1, 3, 12, or 36)
#' @param use_robust Logical, whether to use robust regression
#' @return Data frame with regression results for each momentum measure
run_monthly_momentum_regressions <- function(monthly_mom, horizon = 1, use_robust = FALSE) {

  # Get momentum column names
  mom_cols <- grep("^mom_", colnames(monthly_mom), value = TRUE)

  # Determine the dependent variable based on horizon
  y_var <- paste0("fwd_return_", horizon, "m")

  # Check if the column exists
 if (!y_var %in% colnames(monthly_mom)) {
    stop(paste("Forward return column", y_var, "not found in data"))
  }

  # Run regression for each momentum measure
  results <- lapply(mom_cols, function(x_var) {
    run_single_regression(monthly_mom, y_var, x_var, use_robust = use_robust, horizon = horizon)
  })

  # Combine results
  results_df <- do.call(rbind, results)

  # Add lookback period for easier interpretation
  results_df <- results_df %>%
    mutate(
      lookback_months = as.numeric(gsub("mom_", "", predictor)),
      predictor_label = paste0(lookback_months, "-month momentum"),
      horizon = horizon
    ) %>%
    arrange(lookback_months)

  return(results_df)
}

#' Run predictive regressions for all EWMA momentum measures
#'
#' @param ewma_mom Data frame with EWMA momentum columns and forward returns
#' @param horizon Integer, forecast horizon in months (1, 3, 12, or 36)
#' @param use_robust Logical, whether to use robust regression
#' @return Data frame with regression results for each EWMA measure
run_ewma_momentum_regressions <- function(ewma_mom, horizon = 1, use_robust = FALSE) {

  # Get EWMA column names
  ewma_cols <- grep("^ewma_", colnames(ewma_mom), value = TRUE)

  # Determine the dependent variable based on horizon
  y_var <- paste0("fwd_return_", horizon, "m")

  # Check if the column exists
  if (!y_var %in% colnames(ewma_mom)) {
    stop(paste("Forward return column", y_var, "not found in data"))
  }

  # Run regression for each EWMA measure
  results <- lapply(ewma_cols, function(x_var) {
    run_single_regression(ewma_mom, y_var, x_var, use_robust = use_robust, horizon = horizon)
  })

  # Combine results
  results_df <- do.call(rbind, results)

  # Add alpha value for easier interpretation
  results_df <- results_df %>%
    mutate(
      alpha = as.numeric(gsub("ewma_", "", predictor)),
      predictor_label = paste0("EWMA (alpha=", sprintf("%.2f", alpha), ")"),
      horizon = horizon
    ) %>%
    arrange(alpha)

  return(results_df)
}

#' Run regressions for all horizons
#'
#' @param data Data frame with momentum and forward return columns
#' @param mom_type Character, either "monthly" or "ewma"
#' @param use_robust Logical, whether to use robust regression
#' @return Data frame with regression results for all horizons
run_all_horizon_regressions <- function(data, mom_type = "monthly", use_robust = FALSE) {

  horizons <- c(1, 3, 12, 36)

  if (mom_type == "monthly") {
    results <- lapply(horizons, function(h) {
      run_monthly_momentum_regressions(data, horizon = h, use_robust = use_robust)
    })
  } else {
    results <- lapply(horizons, function(h) {
      run_ewma_momentum_regressions(data, horizon = h, use_robust = use_robust)
    })
  }

  results_df <- do.call(rbind, results)
  return(results_df)
}

#' Format regression results for display
#'
#' @param results Data frame with regression results
#' @return Data frame formatted for display
format_regression_results <- function(results) {
  results %>%
    mutate(
      coefficient = round(coefficient, 6),
      std_error = round(std_error, 6),
      t_statistic = round(t_statistic, 3),
      p_value = round(p_value, 4),
      r_squared = round(r_squared * 100, 2),  # Convert to percentage
      adj_r_squared = round(adj_r_squared * 100, 2)
    ) %>%
    select(
      Predictor = predictor_label,
      Coefficient = coefficient,
      `Std. Error (NW)` = std_error,
      `t-stat` = t_statistic,
      `p-value` = p_value,
      `R-squared (%)` = r_squared,
      `Adj. R-squared (%)` = adj_r_squared,
      `N` = n_obs,
      Significant = significant
    )
}

#' Create scatter plot for momentum vs forward returns
#'
#' @param data Data frame with momentum and forward return columns
#' @param x_var Character, name of momentum column
#' @param x_label Character, label for x-axis
#' @param horizon Integer, forecast horizon in months
#' @param significant Logical, whether the relationship is significant
#' @return ggplot object
create_scatter_plot <- function(data, x_var, x_label, horizon = 1, significant = FALSE) {

  y_var <- paste0("fwd_return_", horizon, "m")

  plot_data <- data %>%
    select(fwd_return = all_of(y_var), momentum = all_of(x_var)) %>%
    filter(complete.cases(.))

  # Color based on significance
  line_color <- if (significant) "#2E7D32" else "#1976D2"
  title_suffix <- if (significant) " (Significant)" else ""

  y_label <- paste0("Forward ", horizon, "-Month Return")

  p <- ggplot(plot_data, aes(x = momentum, y = fwd_return)) +
    geom_point(alpha = 0.5, color = "gray40") +
    geom_smooth(method = "lm", se = TRUE, color = line_color, fill = line_color, alpha = 0.2) +
    labs(
      title = paste0(x_label, title_suffix),
      x = x_label,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 10)
    ) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent)

  return(p)
}

#' Get summary of significant predictors
#'
#' @param monthly_results Data frame with monthly momentum regression results
#' @param ewma_results Data frame with EWMA regression results
#' @return Data frame summarizing significant predictors
get_significant_summary <- function(monthly_results, ewma_results) {

  # Combine results
  all_results <- bind_rows(
    monthly_results %>% mutate(type = "Monthly Momentum"),
    ewma_results %>% mutate(type = "EWMA Momentum")
  )

  # Filter significant and sort by t-statistic
  significant <- all_results %>%
    filter(significant == TRUE) %>%
    mutate(
      horizon_label = paste0(horizon, "m")
    ) %>%
    arrange(horizon, desc(abs(t_statistic))) %>%
    select(
      Horizon = horizon_label,
      Type = type,
      Predictor = predictor_label,
      Coefficient = coefficient,
      `t-stat` = t_statistic,
      `p-value` = p_value,
      `R-squared (%)` = r_squared
    )

  return(significant)
}
