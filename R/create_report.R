#' Create a pdf report for tabular single value data
#'
#' @param original_data: raw data, ex: daily_temperature, monthly_co2, yearly_emission
#' @param cname: value column, ex: 'co2', 'temp', 'emission'
#' @param time_col: the most complete time column, ex: 'date', 'month', 'year'
#' @param time_frequency: time frequency, ex: 'daily', 'monthly', 'yearly'
#' @param output_path: output dir and name of report, ex: 'output/tabular_single_value_report.pdf'
#'
#' @returns a pdf report
#' @export
#'
#' @examples
#' tabular_single_value_report_combine(daily_temperature, cname='temp', time_col='date', time_frequency='daily', output_path='output/daily_temperature_report.pdf')
#' tabular_single_value_report_combine(monthly_co2, cname='co2', time_col='date', time_frequency='monthly', output_path='output/monthly_co2_report.pdf')
#' tabular_single_value_report_combine(yearly_emission, cname='emission', time_col='year', time_frequency='yearly', output_path='output/yearly_emission_report.pdf')

tabular_single_value_report_combine <- function(original_data, cname, time_col='date', time_frequency='daily', output_path='output/tabular_single_value_report.pdf') {

  # Standardize data
  data <- tabular_preprocess_data(original_data, cname, time_col, time_frequency)

  # Plot functions
  plot_func <- tabular_single_value_plots(data, cname, time_frequency)

  # Generate single pdf reports
  fname <- strsplit(output_path, '.pdf')[[1]][1]

  tabular_single_value_report_P01(plot_func, output_path=paste0(fname, '_P01.pdf'))
  tabular_single_value_report_P02(plot_func, output_path=paste0(fname, '_P02.pdf'))
  tabular_single_value_report_P03(plot_func, output_path=paste0(fname, '_P03.pdf'))
  tabular_single_value_report_P04(plot_func, output_path=paste0(fname, '_P04.pdf'))

  n_report <- 4

  # Merge single pdf reports into one
  pdf_ls <- c(sapply(1:n_report, function(i) paste0(fname, '_P0', i, '.pdf')))
  pdftools::pdf_combine(pdf_ls, output = output_path)
  cat(paste0("Report saved to: ", output_path, "\n"))

  # Remove single reports
  file.remove(pdf_ls)
  print('Done!')
}


tabular_single_value_report_P01 <- function(plot_func, output_path='output/tabular_single_value_report_P01.pdf') {
  # Generate plots
  p1 <- sanity_check_plot(plot_func$moving_average_plot_v02())
  p2 <- sanity_check_plot(plot_func$forecast_plot())
  p3 <- sanity_check_plot(plot_func$residual_plot())
  p4 <- sanity_check_plot(plot_func$changepoint_plot())
  p5 <- sanity_check_plot(plot_func$diff_value_plot(time_col = 'date'))


  # Combine plots
  n_plots = 5
  rel_heights = c(1,1,1,1,1)

  combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5,
                                      nrow = n_plots,
                                      rel_heights = rel_heights,
                                      align='hv')

  # Save as PDF
  ggsave(output_path, combined_plot, width = 8, height = sum(rel_heights)*3)
  cat(paste0("Report saved to: ", output_path, "\n"))
}


tabular_single_value_report_P02 <- function(plot_func, output_path='output/tabular_single_value_report_P02.pdf') {
  # Generate plots
  p1 <- sanity_check_plot(plot_func$diff_value_plot(time_col='month'))
  p2 <- sanity_check_plot(plot_func$seasonality_boxplot())
  p3 <- sanity_check_plot(plot_func$diff_value_plot(time_col='year'))
  p4 <- sanity_check_plot(plot_func$doy_line_plot_v02())
  p5 <- sanity_check_plot(plot_func$lag_plot())

  # Combine plots
  n_plots = 5
  rel_heights = c(1,1,1,1,1)

  combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5,
                                      nrow = n_plots,
                                      rel_heights = rel_heights,
                                      align='hv')

  # Save as PDF
  ggsave(output_path, combined_plot, width = 8, height = sum(rel_heights)*3)
  cat(paste0("Report saved to: ", output_path, "\n"))
}


tabular_single_value_report_P03 <- function(plot_func, output_path='output/tabular_single_value_report_P03.pdf') {
  # Generate plots
  p1 <- sanity_check_plot(plot_func$acf_plot())
  p2 <- sanity_check_plot(plot_func$pacf_plot())
  p3 <- sanity_check_plot(plot_func$avg_monthly_plot())

  # Combine plots
  n_plots = 3
  rel_heights = c(1,1,3)

  combined_plot <- cowplot::plot_grid(p1, p2, p3,
                                      nrow = n_plots,
                                      rel_heights = rel_heights,
                                      align='hv')

  # Save as PDF
  ggsave(output_path, combined_plot, width = 8, height = sum(rel_heights)*3)
  cat(paste0("Report saved to: ", output_path, "\n"))
}


tabular_single_value_report_P04 <- function(plot_func, output_path='output/tabular_single_value_report_P04.pdf') {
  # Generate plots
  p1 <- sanity_check_plot(plot_func$seasonal_decomposition_plot())
  p2 <- sanity_check_plot(plot_func$heatmap_plot())

  # Combine plots
  n_plots = 3
  rel_heights = c(2, 0.5, 2)

  combined_plot <- cowplot::plot_grid(p1, NULL, p2,
                                      nrow = n_plots,
                                      rel_heights = rel_heights,
                                      align='hv')

  # Save as PDF
  ggsave(output_path, combined_plot, width = 8, height = sum(rel_heights)*3)
  cat(paste0("Report saved to: ", output_path, "\n"))
}

# Function to check whether plotting function has any error
sanity_check_plot <- function(func_name) {
  result <- try(func_name, silent = TRUE)
  if (inherits(result, 'try-error')) {
    cat(result)
    return(NULL)
  }
  return(result)
}



raster_multiple_bands_report_P01 <- function(plot_func, output_path='output/raster_multiple_bands_report_P01.pdf') {
  # Generate plots
  p1 <- sanity_check_plot(plot_func$one_date_all_band_plot())
  p2 <- sanity_check_plot(plot_func$one_date_scatter_band_plot())

  # Combine plots
  n_plots = 3
  rel_heights = c(1, 0.5, 2)

  combined_plot <- cowplot::plot_grid(p1, NULL, p2,
                                      nrow = n_plots,
                                      rel_heights = rel_heights,
                                      align='hv')

  # Save as PDF
  ggsave(output_path, combined_plot, width = 8, height = sum(rel_heights)*3)
  cat(paste0("Report saved to: ", output_path, "\n"))
}


plot_func <- raster_multiple_bands_plots(raster_data)
raster_multiple_bands_report_P01(plot_func)


