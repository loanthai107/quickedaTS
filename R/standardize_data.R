#' Standardize tabular data
#'
#' @param data: original data
#' @param values_cols: list of value column, ex: 'co2', c('co2', 'temp')
#' @param time_col: the most complete time column, ex: 'date', 'month', 'year'
#' @param time_frequency: time frequency, ex: 'daily', 'monthly', 'yearly'
#'
#' @returns standardized data
#' @export tabular_preprocess_data
#'
#' @examples data <- tabular_preprocess_data(daily_temperature, values_cols = c('temp'), time_col = 'date', time_frequency = 'daily')
#' @examples data <- tabular_preprocess_data(monthly_co2, values_cols = c('co2'), time_col = 'date', time_frequency = 'monthly')
#' @examples data <- tabular_preprocess_data(yearly_emission, values_cols = 'emission', time_col = 'year', time_frequency = 'yearly')
tabular_preprocess_data <- function(data, values_cols, time_col='date', time_frequency='daily') {

  ## Rename value cols
  if (length(values_cols) == 0) {
    print("There is no value column")
    return(NULL)

  } else if (length(values_cols) == 1) {
    std_cols <- c('time_step', 'value')

  } else {
    new_values_cols <- paste0('value_', seq_along(values_cols))
    std_cols <- append('time_step', new_values_cols)
  }

  data <- data[, c(time_col, values_cols)]
  colnames(data) <- std_cols

  # Add year, month, date, doy columns
  if (time_frequency == 'daily') {
    data$time_step <- as.Date(data$time_step)
    data$year <- format(data$time_step, "%Y")
    data$month <- format(data$time_step, "%m")
    data$date <- data$time_step

  } else if (time_frequency == 'monthly') {
    data$year <- substr(data$time_step, 1, 4)
    data$month <- substr(data$time_step, 6, 7)
    data$date <- paste0(data$year, "-", data$month, "-01")

  } else if (time_frequency == 'yearly') {
    data$year <- data$time_step
    data$month <- '01'
    data$date <- paste0(data$year, "-01-01")
  }

  data$doy <- lubridate::yday(as.POSIXct(data$date))

  # Ensure 'date' is a Date object
  data$date <- as.Date(data$date)

  # Sort data by date
  # data <- data[with(data, order(-date)), ] ## (- is descending, otherwise ascending)
  data <- data[with(data, order(date)), ]

  return(data)
}
