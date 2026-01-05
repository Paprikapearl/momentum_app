# Bloomberg API Connection and Data Fetching
# Uses Rblpapi package for Bloomberg Terminal connectivity

library(Rblpapi)

#' Connect to Bloomberg Terminal
#'
#' @return TRUE if connection successful, FALSE otherwise
connect_bloomberg <- function() {
  tryCatch({
    blpConnect()
    return(TRUE)
  }, error = function(e) {
    warning(paste("Bloomberg connection failed:", e$message))
    return(FALSE)
  })
}

#' Fetch historical total return index data from Bloomberg
#'
#' @param ticker Character string, Bloomberg ticker (e.g., "SPY US Equity")
#' @param start_date Date object, start of data period
#' @param end_date Date object, end of data period
#' @return Data frame with columns: date, tri (total return index)
fetch_bloomberg_data <- function(ticker, start_date, end_date) {

  # Validate inputs
  if (is.null(ticker) || nchar(trimws(ticker)) == 0) {
    stop("Ticker cannot be empty")
  }

  # Fetch TOT_RETURN_INDEX_GROSS_DVDS (total return index including dividends)
  data <- tryCatch({
    bdh(
      securities = ticker,
      fields = "TOT_RETURN_INDEX_GROSS_DVDS",
      start.date = start_date,
      end.date = end_date,
      options = c("periodicitySelection" = "DAILY")
    )
  }, error = function(e) {
    stop(paste("Failed to fetch data for", ticker, ":", e$message))
  })

  # Handle case where no data returned
  if (is.null(data) || nrow(data) == 0) {
    stop(paste("No data returned for ticker:", ticker))
  }

  # Standardize column names
  colnames(data) <- c("date", "tri")

  # Ensure date is Date type and sort
  data$date <- as.Date(data$date)
  data <- data[order(data$date), ]

  # Remove any NA values
  data <- data[complete.cases(data), ]

  return(data)
}

#' Check if Bloomberg connection is active
#'
#' @return TRUE if connected, FALSE otherwise
is_bloomberg_connected <- function() {
  tryCatch({
    # Try a simple query to check connection
    blpConnect()
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}
