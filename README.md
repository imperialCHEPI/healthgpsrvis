
<!-- README.md is generated from README.Rmd. Please edit that file -->

# healthgpsrvis

<!-- badges: start -->
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
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(healthgpsrvis)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
# summary(cars)
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

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
