# Regression Analysis Functions
# Runs predictive regressions of forward returns on momentum measures

library(dplyr)
library(broom)
library(ggplot2)

#' Run a single predictive regression
#'
#' @param data Data frame containing dependent and independent variables
#' @param y_var Character, name of dependent variable column
#' @param x_var Character, name of independent variable column
#' @return Data frame with regression statistics
run_single_regression <- function(data, y_var, x_var) {

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
  model <- lm(formula, data = reg_data)

  # Extract statistics
  model_summary <- summary(model)
  coef_table <- tidy(model)

  # Get slope coefficient (not intercept)
  slope_row <- coef_table %>% filter(term == x_var)

  result <- data.frame(
    predictor = x_var,
    coefficient = slope_row$estimate,
    std_error = slope_row$std.error,
    t_statistic = slope_row$statistic,
    p_value = slope_row$p.value,
    r_squared = model_summary$r.squared,
    adj_r_squared = model_summary$adj.r.squared,
    n_obs = nrow(reg_data),
    significant = slope_row$p.value < 0.05
  )

  return(result)
}

#' Run predictive regressions for all monthly momentum measures
#'
#' @param monthly_mom Data frame with monthly momentum columns and fwd_return_1m
#' @return Data frame with regression results for each momentum measure
run_monthly_momentum_regressions <- function(monthly_mom) {

  # Get momentum column names
  mom_cols <- grep("^mom_", colnames(monthly_mom), value = TRUE)

  # Run regression for each momentum measure
  results <- lapply(mom_cols, function(x_var) {
    run_single_regression(monthly_mom, "fwd_return_1m", x_var)
  })

  # Combine results
  results_df <- do.call(rbind, results)

  # Add lookback period for easier interpretation
  results_df <- results_df %>%
    mutate(
      lookback_months = as.numeric(gsub("mom_", "", predictor)),
      predictor_label = paste0(lookback_months, "-month momentum")
    ) %>%
    arrange(lookback_months)

  return(results_df)
}

#' Run predictive regressions for all EWMA momentum measures
#'
#' @param ewma_mom Data frame with EWMA momentum columns and fwd_return_1m
#' @return Data frame with regression results for each EWMA measure
run_ewma_momentum_regressions <- function(ewma_mom) {

  # Get EWMA column names
  ewma_cols <- grep("^ewma_", colnames(ewma_mom), value = TRUE)

  # Run regression for each EWMA measure
  results <- lapply(ewma_cols, function(x_var) {
    run_single_regression(ewma_mom, "fwd_return_1m", x_var)
  })

  # Combine results
  results_df <- do.call(rbind, results)

  # Add alpha value for easier interpretation
  results_df <- results_df %>%
    mutate(
      alpha = as.numeric(gsub("ewma_", "", predictor)),
      predictor_label = paste0("EWMA (alpha=", sprintf("%.2f", alpha), ")")
    ) %>%
    arrange(alpha)

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
      `Std. Error` = std_error,
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
#' @param significant Logical, whether the relationship is significant
#' @return ggplot object
create_scatter_plot <- function(data, x_var, x_label, significant = FALSE) {

  plot_data <- data %>%
    select(fwd_return = fwd_return_1m, momentum = all_of(x_var)) %>%
    filter(complete.cases(.))

  # Color based on significance
  line_color <- if (significant) "#2E7D32" else "#1976D2"
  title_suffix <- if (significant) " (Significant)" else ""

  p <- ggplot(plot_data, aes(x = momentum, y = fwd_return)) +
    geom_point(alpha = 0.5, color = "gray40") +
    geom_smooth(method = "lm", se = TRUE, color = line_color, fill = line_color, alpha = 0.2) +
    labs(
      title = paste0(x_label, title_suffix),
      x = x_label,
      y = "Forward 1-Month Return"
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
    arrange(desc(abs(t_statistic))) %>%
    select(
      Type = type,
      Predictor = predictor_label,
      Coefficient = coefficient,
      `t-stat` = t_statistic,
      `p-value` = p_value,
      `R-squared (%)` = r_squared
    )

  return(significant)
}
