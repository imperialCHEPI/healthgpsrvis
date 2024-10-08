% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/health-plots.R
\name{combine_plots}
\alias{combine_plots}
\title{Combined Plot of several metrics}
\usage{
combine_plots(
  metrics,
  data_mean_weighted = NULL,
  data_mean_weighted_rf_wide = NULL,
  data_mean_weighted_inc_wide = NULL,
  data_weighted_burden_wide_collapse = NULL,
  data_ple_wide = NULL,
  output_file
)
}
\arguments{
\item{metrics}{A list specifying the metrics to plot.}

\item{data_mean_weighted}{A data frame with weighted mean values for various metrics.}

\item{data_mean_weighted_rf_wide}{A data frame containing the weighted mean values of risk factors.}

\item{data_mean_weighted_inc_wide}{A data frame containing the weighted mean values of incidences.}

\item{data_weighted_burden_wide_collapse}{A data frame with differences between intervention and baseline values for burden of disease.}

\item{data_ple_wide}{A data frame containing the life expectancy.}

\item{output_file}{Name of the output PDF as a string}
}
\value{
A combined ggplot object arranged in a grid.
}
\description{
Creates a combined plot of several metrics.
}
