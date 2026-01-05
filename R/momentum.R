# Momentum Calculation Functions
# Calculates monthly momentum (1-24 months) and EWMA momentum from daily returns

library(dplyr)
library(lubridate)
library(tidyr)

#' Calculate daily returns from total return index
#'
#' @param data Data frame with columns: date, tri
#' @return Data frame with added column: daily_return
calc_daily_returns <- function(data) {
  data <- data %>%
    arrange(date) %>%
    mutate(daily_return = tri / lag(tri) - 1)

  return(data)
}

#' Aggregate daily data to monthly (end of month)
#'
#' @param data Data frame with columns: date, tri
#' @return Data frame with monthly data: date (end of month), tri
aggregate_to_monthly <- function(data) {
  data %>%
    mutate(year_month = floor_date(date, "month")) %>%
    group_by(year_month) %>%
    summarise(
      date = max(date),  # Last trading day of month
      tri = last(tri),   # End of month TRI
      .groups = "drop"
    ) %>%
    dplyr::select(date, tri) %>%
    arrange(date)
}

#' Calculate monthly returns from monthly TRI data
#'
#' @param monthly_data Data frame with columns: date, tri (monthly)
#' @return Data frame with added column: monthly_return
calc_monthly_returns <- function(monthly_data) {
  monthly_data %>%
    arrange(date) %>%
    mutate(monthly_return = tri / lag(tri) - 1)
}

#' Calculate monthly momentum for various lookback periods
#'
#' @param monthly_data Data frame with columns: date, tri, monthly_return
#' @param lookback_months Vector of lookback periods (default 1:24)
#' @return Data frame with momentum columns: mom_1, mom_2, ..., mom_24
calc_monthly_momentum <- function(monthly_data, lookback_months = 1:24) {

  result <- monthly_data %>%
    arrange(date)

  # Calculate momentum for each lookback period
  # Momentum = cumulative return over lookback period = price(t) / price(t-n) - 1
  for (n in lookback_months) {
    col_name <- paste0("mom_", n)
    result <- result %>%
      mutate(!!col_name := tri / lag(tri, n) - 1)
  }

  return(result)
}

#' Calculate EWMA momentum from daily returns
#'
#' @param daily_data Data frame with columns: date, daily_return
#' @param alphas Vector of alpha values (default seq(0.01, 0.50, by = 0.01))
#' @return Data frame with EWMA columns aggregated to monthly
calc_ewma_momentum <- function(daily_data, alphas = seq(0.01, 0.50, by = 0.01)) {

  # Ensure data is sorted
 daily_data <- daily_data %>%
    arrange(date) %>%
    filter(!is.na(daily_return))

  n_obs <- nrow(daily_data)

  # Initialize EWMA matrix
  ewma_matrix <- matrix(NA, nrow = n_obs, ncol = length(alphas))
  colnames(ewma_matrix) <- paste0("ewma_", sprintf("%.2f", alphas))

  # Calculate EWMA for each alpha
  for (j in seq_along(alphas)) {
    alpha <- alphas[j]
    ewma <- numeric(n_obs)

    # Initialize first EWMA value with first return
    ewma[1] <- daily_data$daily_return[1]

    # Recursive EWMA calculation
    for (i in 2:n_obs) {
      ewma[i] <- alpha * daily_data$daily_return[i] + (1 - alpha) * ewma[i - 1]
    }

    ewma_matrix[, j] <- ewma
  }

  # Combine with original data
  result <- cbind(daily_data, as.data.frame(ewma_matrix))

  # Aggregate to monthly (take end of month values)
  ewma_cols <- colnames(ewma_matrix)

  monthly_ewma <- result %>%
    mutate(year_month = floor_date(date, "month")) %>%
    group_by(year_month) %>%
    summarise(
      date = max(date),
      across(all_of(ewma_cols), ~ last(.x)),
      .groups = "drop"
    ) %>%
    dplyr::select(-year_month) %>%
    arrange(date)

  return(monthly_ewma)
}

#' Calculate forward returns for multiple horizons
#'
#' @param monthly_data Data frame with columns: date, tri, monthly_return
#' @param horizons Vector of forward return horizons in months (default c(1, 3, 12, 36))
#' @return Data frame with added columns: fwd_return_1m, fwd_return_3m, fwd_return_12m, fwd_return_36m
calc_forward_returns <- function(monthly_data, horizons = c(1, 3, 12, 36)) {
  result <- monthly_data %>%
    arrange(date)

  # Calculate forward returns for each horizon
 # Forward return = TRI(t+h) / TRI(t) - 1
  for (h in horizons) {
    col_name <- paste0("fwd_return_", h, "m")
    result <- result %>%
      mutate(!!col_name := lead(tri, h) / tri - 1)
  }

  return(result)
}

#' Calculate spread returns from two TRI series
#'
#' @param data1 Data frame with columns: date, tri (ticker 1)
#' @param data2 Data frame with columns: date, tri (ticker 2)
#' @return Data frame with columns: date, tri (synthetic spread TRI), daily_return (spread return)
calc_spread_data <- function(data1, data2) {

  # Calculate daily returns for each ticker
  data1 <- data1 %>%
    arrange(date) %>%
    mutate(return1 = tri / lag(tri) - 1)

  data2 <- data2 %>%
    arrange(date) %>%
    mutate(return2 = tri / lag(tri) - 1)

  # Merge on date (inner join to keep only common dates)
  # Use dplyr::select explicitly to avoid conflict with MASS::select
  merged <- inner_join(
    data1 %>% dplyr::select(date, tri1 = tri, return1),
    data2 %>% dplyr::select(date, tri2 = tri, return2),
    by = "date"
  )

  # Calculate spread return (ticker1 - ticker2)
  merged <- merged %>%
    arrange(date) %>%
    mutate(
      daily_return = return1 - return2
    )

  # Create synthetic TRI from spread returns (for momentum calculations)
  # Start at 100 and compound spread returns
  merged$tri <- NA
  merged$tri[1] <- 100
  for (i in 2:nrow(merged)) {
    if (!is.na(merged$daily_return[i])) {
      merged$tri[i] <- merged$tri[i-1] * (1 + merged$daily_return[i])
    } else {
      merged$tri[i] <- merged$tri[i-1]
    }
  }

  result <- merged %>%
    dplyr::select(date, tri, daily_return)

  return(result)
}

#' Prepare complete momentum dataset for regression analysis
#'
#' @param daily_data Data frame with columns: date, tri (daily)
#' @return List with two data frames: monthly_mom (monthly momentum) and ewma_mom (EWMA momentum)
prepare_momentum_data <- function(daily_data) {

  # Calculate daily returns
  daily_with_returns <- calc_daily_returns(daily_data)

  # Aggregate to monthly
  monthly_data <- aggregate_to_monthly(daily_data)

  # Calculate monthly returns
  monthly_data <- calc_monthly_returns(monthly_data)

  # Calculate monthly momentum (1-24 months)
  monthly_mom <- calc_monthly_momentum(monthly_data)

  # Add forward returns
  monthly_mom <- calc_forward_returns(monthly_mom)

  # Calculate EWMA momentum from daily data
  ewma_monthly <- calc_ewma_momentum(daily_with_returns)

  # Merge EWMA with monthly data (by date)
  # Get all forward return columns
  fwd_cols <- grep("^fwd_return_", colnames(monthly_mom), value = TRUE)
  monthly_base <- monthly_mom %>%
    dplyr::select(date, all_of(fwd_cols))

  ewma_mom <- ewma_monthly %>%
    left_join(monthly_base, by = "date")

  return(list(
    monthly_mom = monthly_mom,
    ewma_mom = ewma_mom
  ))
}
