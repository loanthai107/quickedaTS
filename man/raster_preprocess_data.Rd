% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standardize_data.R
\name{raster_preprocess_data}
\alias{raster_preprocess_data}
\title{Standardize raster data}
\usage{
raster_preprocess_data(
  data_path,
  suffix = ".tif",
  band_names = c("B2", "B3", "B4", "B8", "B11", "B12")
)
}
\arguments{
\item{data_path, }{ex: 'data/sentinel2'}

\item{suffix:}{file type, ex: '.tif'}

\item{band_names:}{list of bands, ex: c("B2", "B3", "B4", "B8", "B11", "B12")}
}
\value{
standardized raster data as a stars object with 3 dimensions (x, y, time)
}
\description{
Standardize raster data
}
\examples{
raster_data <- raster_preprocess_data('data/sentinel2', suffix = '.tif', band_names = c("B2", "B3", "B4", "B8", "B11", "B12"))
}
