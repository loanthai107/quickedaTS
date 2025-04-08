# library(ggplot2)
# library(dplyr)
# library(forecast)

# usethis::use_package("ggplot2")
# usethis::use_package("dplyr")
# usethis::use_package("forecast")

#' Visualization functions for tabular data, single value
#'
#' @param data: standardized data
#' @param cname: short name representing for value, ex: 'co2', 'temp'
#' @param time_frequency: time frequency, ex: 'daily', 'monthly', 'yearly'
#'
#' @returns plot functions
#' @export
#' @import dplyr
#' @import ggplot2
#'
#' @examples
#' plot_func <- tabular_single_value_plots(data, 'temp', time_frequency = 'daily')
#' p <- plot_func$line_plot()
#' p
tabular_single_value_plots <- function(data, cname, time_frequency = "daily") {

  # Create a new environment/object to store our data and methods
  self <- new.env()

  if (time_frequency == "daily") {
    freq <- 365
    h <- 30

  } else if (time_frequency == 'monthly') {
    freq <- 12
    h <- 12

  } else if (time_frequency == 'yearly') {
    freq <- 2
    h <- 10
  }

  data$diff_value <- c(NA, diff(data$value))
  value_ts <- ts(data$value, frequency = freq)

  # fit takes long time to run -> set null first, only calculate when call function and store it right away for later usage
  self$fit <- NULL

  # 1. Line Plot
  self$line_plot <- function() {
    p <- ggplot(data, aes(x = date, y = value)) +
      geom_line() +
      labs(title = paste(toupper(cname), 'Levels Over Time'), x = 'time_step', y = cname)
    return(p)
  }


  # 2. DOY line plot
  # v01
  self$doy_line_plot_v01 <- function() {
    p <- ggplot(data, aes(x = doy, y = value, group = year, color = year)) +
      geom_line() +
      ylab(toupper(cname))
    return(p)
  }

  # v02
  self$doy_line_plot_v02 <- function() {
    recent_year <- max(data$year)

    p <- ggplot() +
      # Previous years
      geom_line(data = data %>% filter(year < recent_year), aes(x = doy, y = value, color = 'Previous years')) +
      # Recent year
      geom_line(data = data %>% filter(year == recent_year), aes(x = doy, y = value, color = recent_year)) +
      labs(title = paste('Day-of-year', toupper(cname), 'over years'), x = 'doy', y = cname)
    return(p)
  }


  # 3 Difference across day per year
  self$diff_value_plot <- function(time_col='date') {
    if (time_col == 'date') {
      p <- ggplot(data, aes(date, diff_value))

    } else if (time_col == 'month') {
      p <- ggplot(data, aes(month, diff_value))

    } else {
      # time_col == 'year'
      p <- ggplot(data, aes(year, diff_value))

    }

    p <- p + geom_point() + geom_smooth(formula = y ~ x, method = lm) +
      labs(title = paste('Difference values of', toupper(cname), 'per', (time_col)))
    return(p)
  }

  # 4. Seasonal Decomposition Plot
  self$seasonal_decomposition_plot <- function() {
    decomp <- stl(value_ts, s.window = 'periodic')
    # decomp_plot <- plot(decomp, main = 'Seasonal Decomposition', labels.freq = 12, xaxt = 'n')
    decomp_plot <- plot(decomp, main = 'Seasonal Decomposition', xaxt = 'n')

    p <- cowplot::ggdraw(recordPlot())
    return(p)
  }

  # 5. Moving Average Plot
  self$moving_average_plot <- function(k=10) {
    data$ma <- zoo::rollmean(data$value, k = k, fill = NA)
    MA_str_name <- paste0(freq, '-step MA')

    p <- ggplot(data, aes(x = date)) +
      geom_line(aes(y = value, color = 'Original')) +
      geom_line(aes(y = ma, color = 'MA')) +
      labs(title = paste0(toupper(cname), ' with ', k, '-step Moving Average'), x = 'time_step', y = cname) +
      scale_color_manual(values = c('Original' = 'black', 'MA' = 'orange')) +
      # Format x-axis to match the original
      scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m")
    return(p)
  }

  # 6. Seasonality Boxplot
  self$seasonality_boxplot <- function() {
    p <- ggplot(data, aes(x = month, y = value)) +
      geom_boxplot() +
      labs(title = paste(toupper(cname), 'Levels by Month'), x = 'month', y = cname)
    return(p)
  }

  # 7. Monthly mean
  self$avg_monthly_plot <- function() {
    df_tmp <- data %>%
      group_by(year, month) %>%
      summarise(avg_value = mean(value), .groups = "drop")

    p <- ggplot(df_tmp, aes(year, avg_value)) +
      geom_point() +
      geom_smooth(formula = y ~ x, method = lm) +
      facet_wrap(month~.)
    return(p)
  }

  # 8. ACF and PACF Plots
  self$acf_plot <- function() {
    acf_plot <- acf(value_ts, main = paste("ACF of", toupper(cname)), plot = FALSE)
    p <- autoplot(acf_plot)
    return(p)
  }


  self$pacf_plot <- function() {
    pacf_plot <- pacf(value_ts, main = paste("PACF of", toupper(cname)), plot = FALSE)
    p <- autoplot(pacf_plot)
    return(p)
  }


  # 9. Time Series Decomposition with forecast
  self$forecast_plot <- function(n_forecast=NULL) {

    if (is.null(self$fit)) {
      self$fit <- forecast::auto.arima(value_ts)
    }

    if (is.null(n_forecast)) {
      n_forecast <- h
    }

    forecast_values <- forecast::forecast(self$fit, h = n_forecast)
    p <- autoplot(forecast_values, main = paste(toupper(cname), "Forecast", n_forecast, "data points"))
    return(p)
  }

  # 7. Residual Plot
  self$residual_plot <- function() {

    if (is.null(self$fit)) {
      self$fit <- forecast::auto.arima(value_ts)
    }

    residual <- residuals(self$fit)
    residual_df <- data.frame(residual = residual, Time = time(residual))

    p <- ggplot(residual_df, aes(x = as.numeric(Time), y = as.numeric(residual))) +
      geom_line() +
      geom_hline(yintercept = 0, color = "red") +
      labs(title = "Residuals of ARIMA Model")
    return(p)
  }









  # Return the environment with all methods
  return(self)
}


