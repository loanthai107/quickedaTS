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
    h <- 5
  }

  value_ts <- ts(data$value, frequency = freq)

  # fit takes long time to run -> set null first, only calculate when call function and store it right away for later usage
  self$fit <- NULL

  # 1. Line Plot
  self$line_plot <- function() {
    p <- ggplot(data, aes(x = date, y = value)) +
      geom_line() +
      labs(title = paste(toupper(cname), 'Levels Over Time'), x = 'time_step', y = cname) +
      # Format x-axis to match the original
      scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
      # Rotate x label
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
  self$diff_value_plot <- function(time_col = 'date') {

    data$diff_value <- c(NA, diff(data$value))

    if (time_col == 'date') {
      p <- ggplot(data, aes(date, diff_value))

    } else if (time_col == 'month') {
      p <- ggplot(data, aes(month, diff_value))

    } else {
      # time_col == 'year'
      n_years <- length(unique(data$year))

      # Handle too many years
      if (n_years > 23) {
        data$year_dummy <- as.Date(paste0(data$year, "-01-01"))

        n_years_break <- ceiling(n_years / 23)
        n_years_break <- paste(n_years_break, 'years')

        p <- ggplot(data, aes(year_dummy, diff_value)) +
          scale_x_date(date_breaks = n_years_break, date_labels = "%Y") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

      } else {
        p <- ggplot(data, aes(year, diff_value))
      }
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
  # v01
  self$moving_average_plot_v01 <- function(ma_step = NULL) {
    if (is.null(ma_step)) {
      ma_step <- h
    }

    data$ma <- zoo::rollmean(data$value, k = ma_step, fill = NA)
    MA_str_name <- paste0(freq, '-step MA')

    p <- ggplot(data, aes(x = date)) +
      geom_line(aes(y = value, color = 'Original')) +
      geom_line(aes(y = ma, color = 'MA')) +
      labs(title = paste0(toupper(cname), ' with ', k, '-step Moving Average'), x = 'time_step', y = cname) +
      scale_color_manual(values = c('Original' = 'black', 'MA' = 'blue'))
    return(p)
  }

  # v02
  self$moving_average_plot_v02 <- function(ma_step = NULL) {
    if (is.null(ma_step)) {
      ma_step <- h
    }

    data$ma <- zoo::rollmean(data$value, k = ma_step, fill = NA)
    MA_str_name <- paste0(freq, '-step MA')

    p <- ggplot(data, aes(x = date)) +
      geom_line(aes(y = value, color = 'Original data')) +
      geom_line(aes(y = ma, color = 'MA')) +
      labs(title = paste0(toupper(cname), ' over time with ', ma_step, '-step Moving Average'), x = 'time_step', y = cname) +
      scale_color_manual(values = c('Original data' = 'black', 'MA' = 'green'))

    if (nrow(data) < 2000 & time_frequency == "daily") {
      p <- p +
        # Format x-axis to match the original
        scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
        # Rotate x label
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }

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
      facet_wrap(month~.) +
      labs(title = 'Average monthly aggregation per year')
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
  self$forecast_plot <- function(n_forecast = NULL) {

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


  # 10. Residual Plot
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


  # 11. Scatter Plot with Lagged Values (Lagged Relationships)
  self$lag_plot <- function(n_lag = 1) {
    data$lagk <- lag(data$value, n_lag)
    df_tmp <- data[-n_lag,] # Remove the first n_lag rows with NA lag

    p <- ggplot(df_tmp, aes(x = lagk, y = value)) +
      geom_point() +
      labs(title = paste(toupper(cname), "vs. Lagged", toupper(cname)),
           x = paste0("(t-", n_lag, ")"), y = "(t)")
    return(p)
  }


  # 12. Change Point Detection Plot
  self$changepoint_plot <- function(n_changes = 5) {
    cp <- changepoint::cpt.mean(data$value, method = "PELT")
    # cp <- changepoint::cpt.mean(data$value, method="BinSeg", penalty="AIC", Q=n_changes)
    change_points <- changepoint::cpts(cp)
    cp_data <- data.frame(index = 1:length(data$value), value = data$value)
    cp_data$change_point <- ifelse(cp_data$index %in% change_points, "Change Point", "No Change")

    p <- ggplot(cp_data, aes(x = index, y = value, color = change_point)) +
      geom_line() +
      # geom_vline(xintercept = change_points, linetype = "dashed", color = "red") +
      labs(title = paste("Change Points in", toupper(cname)), x = "index", y = cname) +
      scale_color_manual(values = c("Change Point" = "red", "No Change" = "black"))
    return(p)
  }


  # 13. Heatmap of Seasonal Patterns
  self$heatmap_plot <- function() {

    heatmap_data <- data %>%
      group_by(year, month) %>% summarise(value = mean(value), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = month, values_from = value) %>%
      select(-year) %>%
      as.matrix()

    rownames(heatmap_data) <- unique(data$year)

    ## Remove rows (last year) with NaN
    heatmap_data <- na.omit(heatmap_data)

    heatmap(heatmap_data, scale = "column", main = paste(toupper(cname), "Seasonal Heatmap"))
    p <- cowplot::ggdraw(recordPlot())
    return(p)
  }


  # Return the environment with all methods
  return(self)
}


raster_multiple_bands_plots <- function(raster_data,
                                        RGB_bands = c("B4", "B3", "B2"),
                                        indices_formula = c("NDSI" = "(B3 - B11) / (B3 + B11)",
                                                            "NDVI" = "(B8 - B4) / (B8 + B4)"),
                                        band_order = 1, date_order = 1,
                                        n_bands_explore = 6, n_first_date = 6,
                                        x_order = 1, y_order = 1, x_value = NULL, y_value = NULL) {

  # Create a new environment/object to store our data and methods
  self <- new.env()

  # Control plot size
  options(repr.plot.width=8, repr.plot.height=4)

  # For plot title
  mtext_kwargs <- list(side=3, adj=0, line=0, outer=TRUE, cex=1.5, font=3)

  # Raster attributes
  band_names <- names(raster_data)
  time_dates <- stars::st_get_dimension_values(raster_data, "time")

  # Align params to fit raster attr
  n_bands_explore <- min(n_bands_explore, length(band_names))
  n_first_date <- min(n_first_date, length(time_dates))

  if (band_order > length(band_names)) {
    cat('ERROR: band_order out of range\nAvailable values are:', c(1:length(band_names)))
  }
  if (date_order > length(time_dates)) {
    cat('ERROR: date_order out of range\nAvailable values are:', c(1:length(time_dates)))
  }


  # 0. Check attr of stars raster object
  # 0.1 band_names
  self$get_band_names <- function() {
    cat('Raster data has', length(band_names), 'bands\n')
    return(band_names)
  }

  # 0.2 time_dates
  self$get_time_dates <- function() {
    cat('Raster data has', length(time_dates), 'dates\n')
    return(time_dates)
  }


  # 1. Plot for one date of all bands
  self$one_date_all_band_plot <- function() {
    ## Filter first band and first date
    # tmp_raster <- raster_data[band_names[1]] %>%
    #                 filter(time == as.Date(time_dates[1])) %>%
    #                 as("SpatRaster")

    ## or
    # tmp_raster <- raster_data[1,,,1]

    # Filter all bands of first date
    tmp_raster <- terra::rast(raster_data[1:n_bands_explore,,,date_order])

    # Create outer margin area for the title
    par(oma=c(0,0,2,0))
    terra::plot(tmp_raster)

    # Add an overall title
    do.call("mtext", c(paste0('Plot for date (', time_dates[date_order], ') of all bands'),
                       mtext_kwargs))
    # mtext(paste0('Plot for date (', time_dates[date_order], ') of all bands'),
    #       side=3, adj=0, line=0, outer=TRUE, cex=1.5, font=3
    #       )

    p <- cowplot::ggdraw(recordPlot())
    return(p)
  }


  # 2. Scatter plot of pairs of bands of one date
  self$one_date_scatter_band_plot <- function() {
    target_band <- band_names[band_order]
    rest_bands <- band_names[-band_order]

    # Get raster of date_order
    tmp_raster <- raster_data[,,,date_order]

    # Convert to dataframe for plotting
    tmp_df <- tmp_raster %>% as.data.frame()

    # Set up a 2x3 plotting area with space for a title
    par(mfrow=c(n_bands_explore/3,3), oma=c(0,0,2,0))

    for (i in 1:length(rest_bands)) {
      tmp_df_2bands <- tmp_df %>%
                          select(c(target_band, rest_bands[i])) %>%
                          rename(
                            target_band = target_band,
                            other_band = rest_bands[i])

      # Create scatter plots of target band with others
      plot(tmp_df_2bands$target_band, tmp_df_2bands$other_band,
           main=paste(target_band, 'vs', rest_bands[i]),
           xlab=target_band, ylab=rest_bands[i], pch=20)
    }

    # Add the overall title
    do.call("mtext", c(paste0('Scatter plot of ', target_band, ' with other bands of date (', time_dates[date_order], ')'),
                       mtext_kwargs))

    p <- cowplot::ggdraw(recordPlot())
    return(p)
  }


  # 3. Plot of one band of first N dates
  self$one_band_first_N_dates <- function(band_name = NULL, is_gray_scale = FALSE, customr_color = viridis::viridis(100)) {
    # Filter data
    if (is.null(band_name)) {
      band_name <- band_names[band_order]
      tmp_raster <- raster_data[band_order,,,1:n_first_date]

    } else {
      tmp_raster <- raster_data[band_name][,,,1:n_first_date]
    }

    # Create a combined plot
    if (is_gray_scale) {
      terra::plot(tmp_raster)
    } else {
      terra::plot(tmp_raster, col = customr_color)
    }

    # Add an overall title
    do.call("mtext", c(paste("Plot of band", band_name, "of first", n_first_date, "dates"),
                       mtext_kwargs))

    p <- cowplot::ggdraw(recordPlot())
    return(p)
  }


  # 4. Plot of RGB images of first 6 dates
  self$RGB_images_first_N_dates <- function() {
    # Filter data
    tmp_raster <- raster_data[RGB_bands][,,,1:n_first_date]

    # Convert to data frame
    tmp_df <- as.data.frame(tmp_raster, xy = TRUE)

    # Generalize cnames
    colnames(tmp_df) <- c("x", "y", "time", "R", "G", "B" )

    # Drop NA
    tmp_df <- tmp_df %>% tidyr::drop_na(R, G, B)

    # Set up RGB values with scaling (0-255)
    # Robust scaling (similar to robust=True)
    scale_robust <- function(x) {
      q <- quantile(x, c(0.02, 0.98), na.rm = TRUE)
      x[x < q[1]] <- q[1]
      x[x > q[2]] <- q[2]
      return((x - q[1]) / (q[2] - q[1]) * 255)
    }

    tmp_df$R <- scale_robust(tmp_df$R)
    tmp_df$G <- scale_robust(tmp_df$G)
    tmp_df$B <- scale_robust(tmp_df$B)

    # Create RGB hex colors
    tmp_df$color <- rgb(tmp_df$R/255, tmp_df$G/255, tmp_df$B/255)

    p <- ggplot(tmp_df, aes(x = x, y = y, fill = color)) +
      geom_raster()  +
      facet_wrap(time~.) +
      scale_fill_identity() +
      coord_equal() +
      labs(title = paste("RGB Composite of first", n_first_date, "dates")) +
      theme_minimal() +
      theme(aspect.ratio=0.6)
    return(p)
  }


  # 5 Compute indices
  compute_index <- function(formula_str) {
    # Create an environment with the raster bands as variables
    band_env <- new.env()
    for(band_name in band_names) {
      band_env[[band_name]] <- raster_data[[band_name]]
    }

    # Evaluate the formula in that environment
    eval(parse(text = formula_str), envir = band_env)
  }

  if (length(indices_formula) > 0) {
    for (i in seq(1, length(indices_formula))){
      formula <- indices_formula[i]
      # Print out for double check
      print(paste('DOUBLE CHECK:', names(formula), "is", formula))

      # Compute new index value (a new band) in raster data
      raster_data[[names(formula)]] <- compute_index(as.character(formula))
    }
    # Store names of all indices
    self$index_names <- names(indices_formula)
    cat('Computing indices:', names(indices_formula), 'done!')
  }


  # 6. Line chart of all bands at specific location (x,y) during time
  self$all_bands_all_dates_xy <- function() {
    # Filter data
    if (is.null(x_value)) {
      tmp_raster <- raster_data[, x_order, y_order, ]

    } else {
      # Get dimension values
      x_values <- stars::st_get_dimension_values(raster_data, "x")
      y_values <- stars::st_get_dimension_values(raster_data, "y")

      # Find closest indices
      x_order <- which.min(abs(x_values - x_value))
      y_order <- which.min(abs(y_values - y_value))

      # Extract the value at those indices
      tmp_raster <- raster_data[, x_order, y_order, ]
    }


    # Convert to a data frame for easier manipulation
    tmp_df <- as.data.frame(tmp_raster, long = TRUE)

    # Resample to daily time steps and interpolate missing values
    # First, ensure the 'time' column is of Date type
    tmp_df$time <- as.Date(tmp_df$time)


    # p1 <- ggplot(tmp_df, aes(x = time))
    # for (b in band_names) {
    #   print(b)
    #   p1 <- p1 + geom_line(aes(y = tmp_df[, b]))
    # }
    #
    # p1 <- p1 + labs(title = paste0('Band values across dates at loc x=', tmp_df$x[1], ' and y=', tmp_df$y[1]), color = "Legend") +
    #   theme_minimal()


    # Reshape data to long format
    long_df <- tmp_df %>%
                  tidyr::pivot_longer(cols = all_of(c(band_names, names(indices_formula))),
                                      names_to = "band",
                                      values_to = "value")
    # Add band_type column
    long_df$band_type <- with(long_df, ifelse(band %in% band_names, "band", "index"))

    # Create plot
    p <- ggplot(long_df, aes(x = time, y = value, color = band)) +
      geom_line() +
      # facet_wrap(band_type~., scales = "free_y", nrow = 2) +
      facet_grid(band_type~., scales = "free_y") +
      labs(title = paste0('Band values across dates at loc x=', tmp_df$x[1], ' and y=', tmp_df$y[1]))

    return(p)
  }











  # Return the environment with all methods
  return(self)
}




raster_data <- raster_preprocess_data('data/sentinel2', suffix = '.tif', band_names = c("B2", "B3", "B4", "B8", "B11", "B12"))
plot_func <- raster_multiple_bands_plots(raster_data, x_order = 100, y_order = 50)

plot_func$get_band_names()
plot_func$get_time_dates()

p <- plot_func$one_date_all_band_plot()
p <- plot_func$one_date_scatter_band_plot()
p <- plot_func$one_band_first_N_dates(is_gray_scale = TRUE)
p <- plot_func$RGB_images_first_N_dates()
p <- plot_func$one_band_first_N_dates(band_name = 'NDSI', customr_color = viridis::mako)
p <- plot_func$one_band_first_N_dates(band_name = 'NDVI', customr_color = viridis::viridis)
p <- plot_func$all_bands_all_dates_xy()
p


raster_data <- raster_preprocess_data("/Users/phuongloan/Documents/Study/01_Master_EAGLE/Program/03_introduction_to_programming_and_geostatistics_in_EO/git_repo/Time_Series_Analysis_in_Remote Sensing/data/module1_data/T1/s2")
plot_func <- raster_multiple_bands_plots(raster_data, x_order = 100, y_order = 50)
p <- plot_func$all_bands_all_dates_xy()
p

