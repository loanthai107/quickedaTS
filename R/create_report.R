tabular_single_value_report_P01 <- function(plot_func, output_path='output/tabular_single_value_report_P01.pdf') {
  # Generate plots
  p1 <- plot_func$moving_average_plot_v02()
  p2 <- plot_func$forecast_plot()
  p3 <- plot_func$residual_plot()
  p4 <- plot_func$changepoint_plot()
  p5 <- plot_func$diff_value_plot(time_col = 'date')


  # Combine plots
  n_plots = 5
  rel_heights = c(1,1,1,1,1)

  combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5,
                                      nrow = n_plots,
                                      rel_heights = rel_heights,
                                      align='hv')

  # Save as PDF
  ggsave(output_path, combined_plot, width = 8, height = sum(rel_heights)*3)
  cat(paste("Report saved to:", output_path))
}


tabular_single_value_report_P02 <- function(plot_func, output_path='output/tabular_single_value_report_P02.pdf') {
  # Generate plots
  p1 <- plot_func$diff_value_plot(time_col='month')
  p2 <- plot_func$seasonality_boxplot()
  p3 <- plot_func$diff_value_plot(time_col='year')
  p4 <- plot_func$doy_line_plot_v02()
  p5 <- plot_func$lag_plot()

  # Combine plots
  n_plots = 5
  rel_heights = c(1,1,1,1,1)

  combined_plot <- cowplot::plot_grid(p1, p2, p3, p4, p5,
                                      nrow = n_plots,
                                      rel_heights = rel_heights,
                                      align='hv')

  # Save as PDF
  ggsave(output_path, combined_plot, width = 8, height = sum(rel_heights)*3)
  cat(paste("Report saved to:", output_path))
}
