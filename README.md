# quickedaTS
[![DOI](https://zenodo.org/badge/961933957.svg)](https://doi.org/10.5281/zenodo.15276260)

R package to generate reports for quick Exploratory Time Series Data Analysis

## Installation
To install the current version, use `devtools`.

```R
devtools::install_github("loanthai107/quickedaTS")
library(quickedaTS)
```

## Available Functions

* `tabular_single_value_report_combine()` This function automatically generates a pdf report including many charts to explore tabular time series data.
* `raster_multiple_bands_report_combine()` This function automatically generates a pdf report including many charts to explore raster time series data.


## Example 1: Tabular data

```R
# Assume this is your tabular data
head(daily_temperature)
#         date year   temp
# 1 2020-01-01 2020 13.098
# 2 2020-01-02 2020 13.192
# 3 2020-01-03 2020 13.177
# 4 2020-01-04 2020 13.152
# 5 2020-01-05 2020 13.061
# 6 2020-01-06 2020 13.023

# Create a directory for output
dir.create('output')

# Generate report
tabular_single_value_report_combine(daily_temperature, cname='temp', time_col='date', time_frequency='daily',
                                    output_path='output/daily_temperature_report.pdf')

# Report
```
<object data="output/daily_temperature_report.pdf" type="application/pdf">
    <embed src="output/daily_temperature_report.pdf">
        <p>This browser does not support PDFs. See full report at: <a href="https://github.com/loanthai107/quickedaTS/blob/main/output/daily_temperature_report.pdf">quickedaTS/output/daily_temperature_report.pdf</a>.</p>
    </embed>
</object>


## Example 2: Raster data
```R
# Path to tif files
data_path <- system.file(package = "quickedaTS", "extdata/s2_sample")

# List of files
Sys.glob(paste0(data_path, '/*.tif'))
# [1] "quickedaTS/extdata/s2_sample/20180102T102421_20180102T102420_T32TPS.tif"
# [2] "quickedaTS/extdata/s2_sample/20180112T102401_20180112T102630_T32TPS.tif"
# [3] "quickedaTS/extdata/s2_sample/20180127T102259_20180127T102314_T32TPS.tif"
# [4] "quickedaTS/extdata/s2_sample/20180129T101251_20180129T101247_T32TPS.tif"
# [5] "quickedaTS/extdata/s2_sample/20180208T101151_20180208T101153_T32TPS.tif"
# [6] "quickedaTS/extdata/s2_sample/20180211T102141_20180211T102559_T32TPS.tif"

## Note: To use this package, the name of tif files must start with 8 characters representing for "YYYYmmdd"

# Generate report
raster_multiple_bands_report_combine(data_path, suffix = '.tif',
                                     band_names = c("B2", "B3", "B4", "B8", "B11", "B12"),
                                     RGB_bands = c("B4", "B3", "B2"),
                                     indices_formula = c("NDSI" = "(B3 - B11) / (B3 + B11)",
                                                         "NDVI" = "(B8 - B4) / (B8 + B4)"),
                                     x_order = 100, y_order = 50,
                                     output_path='output/s2_sample_report.pdf')

# Report
```
<object data="output/s2_sample_report.pdf" type="application/pdf">
    <embed src="output/s2_sample_report.pdf">
        <p>This browser does not support PDFs. See full report at: <a href="https://github.com/loanthai107/quickedaTS/blob/main/output/s2_full_report.pdf">quickedaTS/output/s2_full_report.pdf</a>.</p>
    </embed>
</object>


## Citation
Loan Thai. (2025). loanthai107/quickedaTS (v1.0.1). Zenodo. https://doi.org/10.5281/zenodo.15276296

## References
This package was developed as a final project for the course Introduction to Programming and Geostatistics held by Dr. Martin Wegmann,

EAGLE Master Program, University of Würzburg.

