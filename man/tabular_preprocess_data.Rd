% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_data.R
\name{tabular_preprocess_data}
\alias{tabular_preprocess_data}
\title{Standardize tabular data}
\usage{
tabular_preprocess_data(
  data,
  values_cols,
  time_col = "date",
  time_frequency = "daily"
)
}
\arguments{
\item{data:}{original data}

\item{values_cols:}{list of value column, ex: 'co2', c('co2', 'temp')}

\item{time_col:}{the most complete time column, ex: 'date', 'month', 'year'}

\item{time_frequency:}{time frequency, ex: 'daily', 'monthly', 'yearly'}
}
\value{
standardized data
}
\description{
Standardize tabular data
}
\examples{
data <- tabular_preprocess_data(daily_temperature, values_cols = c('temp'), time_col = 'date', time_frequency = 'daily')
data <- tabular_preprocess_data(monthly_co2, values_cols = c('co2'), time_col = 'date', time_frequency = 'monthly')
data <- tabular_preprocess_data(yearly_emission, values_cols = 'emission', time_col = 'year', time_frequency = 'yearly')
}
