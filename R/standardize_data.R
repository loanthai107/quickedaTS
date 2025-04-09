#' Standardize tabular data
#'
#' @param data: original data
#' @param values_cols: list of value column, ex: 'co2', c('co2', 'temp')
#' @param time_col: the most complete time column, ex: 'date', 'month', 'year'
#' @param time_frequency: time frequency, ex: 'daily', 'monthly', 'yearly'
#'
#' @returns standardized data
#' @export
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
    data$year <- as.character(data$time_step)
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



# Function to get datetime from filenames
paths_to_datetimeindex <- function(paths) {
  # Extract filenames from paths
  filenames <- basename(paths)
  # Extract first 8 characters and convert to dates
  dates <- lubridate::ymd(substr(filenames, 1, 8))

  # Drop duplicated dates
  dup_ind <- duplicated(dates) | duplicated(dates, fromLast = FALSE)
  if (length(dup_ind) > 0) {
    paths <- paths[!dup_ind]
    dates <- dates[!dup_ind]
  }

  return(list("paths" = paths, "dates" = dates))
}



#' Standardize raster data
#'
#' @param data_path, ex: 'data/sentinel2'
#' @param suffix: file type, ex: '.tif'
#' @param band_names: list of bands, ex: c("B2", "B3", "B4", "B8", "B11", "B12")
#'
#' @returns standardized raster data as a stars object with 3 dimensions (x, y, time)
#' @export
#'
#' @examples raster_data <- raster_preprocess_data('data/sentinel2', suffix = '.tif', band_names = c("B2", "B3", "B4", "B8", "B11", "B12"))
raster_preprocess_data <- function(data_path, suffix = '.tif', band_names = c("B2", "B3", "B4", "B8", "B11", "B12")) {
  # Get list of GeoTIFF files
  file_ls <- Sys.glob(paste0(data_path, '/*', suffix))

  # Create a time variable from filenames
  res <- paths_to_datetimeindex(file_ls)
  file_ls <- res$paths
  time_dates <- res$dates

  # Process each file individually
  all_rasters_by_band <- list()

  # Initialize lists for each band
  for (band in band_names) {
    all_rasters_by_band[[band]] <- list()
  }

  # Process each file and organize by band
  for (i in seq_along(file_ls)) {
    # Read the raster
    r <- terra::rast(file_ls[i])

    # Check if number of layers matches expected band count
    n_actual_layers <- terra::nlyr(r)
    if (n_actual_layers != length(band_names)) {
      warning(paste("File", file_ls[i], "has", n_actual_layers, "layers, expected", length(band_names)))
      next
    }

    # Assign proper names to each layer
    names(r) <- band_names

    # Separate each band and add to its respective list
    for (j in seq_along(band_names)) {
      band_layer <- r[[j]]
      terra::time(band_layer) <- time_dates[i]
      all_rasters_by_band[[band_names[j]]][[i]] <- band_layer
    }
  }

  # Combine rasters by band
  band_stacks <- list()
  for (band in band_names) {
    if (length(all_rasters_by_band[[band]]) > 0) {
      band_stacks[[band]] <- do.call(c, all_rasters_by_band[[band]])
    }
  }

  # Convert each band stack to stars objects
  stars_list <- list()
  for (band in names(band_stacks)) {
    stars_list[[band]] <- stars::st_as_stars(band_stacks[[band]])
  }

  # Combine all stars objects into one
  raster_data <- do.call(c, stars_list)
  return(raster_data)
}





