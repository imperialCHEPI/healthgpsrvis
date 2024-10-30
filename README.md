
<!-- README.md is generated from README.Rmd. Please edit that file -->

# healthgpsrvis

<!-- badges: start -->

[![R-CMD-check](https://github.com/imperialCHEPI/healthgpsrvis/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/imperialCHEPI/healthgpsrvis/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `healthgpsrvis` is to plot and visualise data related to
Health-GPS.

## Getting Started

### Prerequisites

- [RStudio](https://posit.co/download/rstudio-desktop/) installed
  <!-- - [renv](https://rstudio.github.io/renv/) installed -->

### Installation

You can install the development version of `healthgpsrvis` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("imperialCHEPI/healthgpsrvis")
# devtools::install_github("imperialCHEPI/healthgpsrvis", ref = "branch-name")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(healthgpsrvis)

# Get the path to the .rds file
filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

# Read the .rds file
data <- readRDS(filepath)

# Generate the weighted data
data_weighted <- gen_data_weighted(data)

# Generate the weighted data for the risk factors
data_weighted_rf_wide_collapse <- gen_data_weighted_rf(data_weighted)

# Summarise the weighted data for the risk factors
summary(data_weighted_rf_wide_collapse)
#>       time      diff_sodium_mean diff_sodium_min  diff_sodium_max 
#>  Min.   :2022   Min.   :-33.67   Min.   :-33.92   Min.   :-33.65  
#>  1st Qu.:2030   1st Qu.:-33.55   1st Qu.:-33.73   1st Qu.:-33.36  
#>  Median :2038   Median :-33.24   Median :-33.56   Median :-32.99  
#>  Mean   :2038   Mean   :-31.35   Mean   :-31.60   Mean   :-31.07  
#>  3rd Qu.:2047   3rd Qu.:-33.07   3rd Qu.:-33.34   3rd Qu.:-32.54  
#>  Max.   :2055   Max.   :  0.00   Max.   :  0.00   Max.   :  0.00  
#>   diff_ei_mean     diff_ei_min      diff_ei_max     diff_bmi_mean    
#>  Min.   :-23.15   Min.   :-23.19   Min.   :-23.14   Min.   :-0.1460  
#>  1st Qu.:-23.08   1st Qu.:-23.14   1st Qu.:-23.04   1st Qu.:-0.1445  
#>  Median :-22.88   Median :-23.01   Median :-22.78   Median :-0.1429  
#>  Mean   :-21.58   Mean   :-21.67   Mean   :-21.48   Mean   :-0.1321  
#>  3rd Qu.:-22.77   3rd Qu.:-22.92   3rd Qu.:-22.59   3rd Qu.:-0.1409  
#>  Max.   :  0.00   Max.   :  0.00   Max.   :  0.00   Max.   : 0.0000  
#>   diff_bmi_min      diff_bmi_max     diff_obesity_mean   diff_obesity_min   
#>  Min.   :-0.1470   Min.   :-0.1457   Min.   :-0.004656   Min.   :-0.004688  
#>  1st Qu.:-0.1456   1st Qu.:-0.1432   1st Qu.:-0.004463   1st Qu.:-0.004518  
#>  Median :-0.1438   Median :-0.1417   Median :-0.004195   Median :-0.004245  
#>  Mean   :-0.1329   Mean   :-0.1312   Mean   :-0.003953   Mean   :-0.004009  
#>  3rd Qu.:-0.1419   3rd Qu.:-0.1399   3rd Qu.:-0.003997   3rd Qu.:-0.004048  
#>  Max.   : 0.0000   Max.   : 0.0000   Max.   : 0.000000   Max.   : 0.000000  
#>  diff_obesity_max   
#>  Min.   :-0.004606  
#>  1st Qu.:-0.004401  
#>  Median :-0.004163  
#>  Mean   :-0.003896  
#>  3rd Qu.:-0.003922  
#>  Max.   : 0.000000
```

<!--
### Cloning the Repository
&#10;```bash
git clone https://github.com/imperialCHEPI/healthgps-plots.git
cd healthgps-plots
```
&#10;### Setting Up the Project Environment with renv
&#10;1. Activate the renv environment:
```bash
Rscript -e 'renv::activate()'
```
&#10;2. Install project dependencies:
```bash
Rscript -e 'renv::restore()'
```
&#10;### Running the Script
You can now run the visualisation script using:
```bash
Rscript Visualisation.R path/to/HealthGPS_Result_S1.csv path/to/HealthGPS_Result_S2.csv path/to/HealthGPS_Result_S4.csv path/to/HealthGPS_Result_S5.csv
```
&#10;### Updating Dependencies
If you make changes to the project dependencies or want to ensure you have the latest versions, you can update the renv.lock file:
```bash
Rscript -e 'renv::snapshot()'
```
&#10;### Deactivating the renv Environment
After you are done using the project, you can deactivate the renv environment:
```bash
Rscript -e 'renv::deactivate()'
```
-->
