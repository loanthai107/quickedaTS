test_that("tabular - check standardized dataframe of 1 value", {
  data <- tabular_preprocess_data(daily_temperature, values_cols=c('temp'), time_col='date', time_frequency='daily')

  # Check column names
  expect_equal(colnames(data), c("time_step", "value", "year", "month", "date", "doy"))

  # Check type of "date" column
  expect_equal(class(data$date), "Date")
})
