% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_report.R
\name{tabular_single_value_report_combine}
\alias{tabular_single_value_report_combine}
\title{Create a pdf report for tabular single value data}
\usage{
tabular_single_value_report_combine(
  original_data,
  cname,
  time_col = "date",
  time_frequency = "daily",
  output_path = "output/tabular_single_value_report.pdf"
)
}
\arguments{
\item{original_data:}{raw data, ex: daily_temperature, monthly_co2, yearly_emission}

\item{cname:}{value column, ex: 'co2', 'temp', 'emission'}

\item{time_col:}{the most complete time column, ex: 'date', 'month', 'year'}

\item{time_frequency:}{time frequency, ex: 'daily', 'monthly', 'yearly'}

\item{output_path:}{output dir and name of report, ex: 'output/tabular_single_value_report.pdf'}
}
\value{
a pdf report
}
\description{
Create a pdf report for tabular single value data
}
\examples{
tabular_single_value_report_combine(daily_temperature, cname='temp', time_col='date', time_frequency='daily', output_path='output/daily_temperature_report.pdf')
tabular_single_value_report_combine(monthly_co2, cname='co2', time_col='date', time_frequency='monthly', output_path='output/monthly_co2_report.pdf')
tabular_single_value_report_combine(yearly_emission, cname='emission', time_col='year', time_frequency='yearly', output_path='output/yearly_emission_report.pdf')
}
