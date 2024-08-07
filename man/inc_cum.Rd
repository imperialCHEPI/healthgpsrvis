% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/health-plots.R
\name{inc_cum}
\alias{inc_cum}
\title{Plot of Cumulative Incidence Difference}
\usage{
inc_cum(inc, data_mean_weighted_inc_wide)
}
\arguments{
\item{inc}{A character string specifying the incidence to plot.
Options are: "asthma", "ckd", "diabetes", "ischemia", "stroke".}

\item{data_mean_weighted_inc_wide}{A data frame containing the weighted mean values of incidences.}
}
\value{
A ggplot object representing the specified plot.
}
\description{
Creates a line plot showing the cumulative reduction in a specified incidence number over time.
}
