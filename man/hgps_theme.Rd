% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup-graphics.R
\name{hgps_theme}
\alias{hgps_theme}
\title{Customised theme for ggplot2}
\usage{
hgps_theme()
}
\value{
A ggplot2 theme object with customised settings for plot title, axis titles,
axis text, legend text, legend title, major grid lines, legend key background, and legend position.
}
\description{
This function creates a customised theme for ggplot2 plots. It modifies various
elements of the default \code{theme_classic()} to provide a specific style.
}
\details{
The \code{hgps_theme} function modifies the following elements of the classic theme:
\itemize{
\item \code{plot.title}: Sets the font size of the plot title (default is 15).
\item \code{axis.title.x}: Sets the font size of the x-axis title (default is 10).
\item \code{axis.title.y}: Sets the font size of the y-axis title (default is 10).
\item \code{axis.text.x}: Sets the font size of the x-axis text (default is 8).
\item \code{axis.text.y}: Sets the font size of the y-axis text (default is 8).
\item \code{legend.text}: Sets the font size of the legend text (default is 8).
\item \code{legend.title}: Removes the legend title.
\item \code{panel.grid.major.x}: Adds major grid lines on the x-axis.
\item \code{legend.key}: Sets the legend key background (default is "transparent").
\item \code{legend.position}: Positions the legend (default coordinates are (0.8, 0.9)).
}
}
\examples{
\dontrun{
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    hgps_theme()
}

}
