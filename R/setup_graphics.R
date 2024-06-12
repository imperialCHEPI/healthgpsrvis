#' Set up a new plotting device
#'
#' This function closes all existing graphical devices and opens a new one
#' with specified width and height dimensions.
#'
#' @param width Numeric value specifying the width of the new plotting device in centimeters.
#' @param height Numeric value specifying the height of the new plotting device in centimeters.
#'
#' @details
#' The `set_graphics` function first closes all open graphical devices using `graphics.off()`.
#' It then opens a new graphical device with the given dimensions using `dev.new()`.
#' The `unit` parameter is set to "cm" to use centimeters for the dimensions,
#' and `noRStudioGD` is set to `TRUE` to avoid using RStudio's graphical device.
#'
#' @examples
#' \dontrun{
#' # Set up a new plotting device with width 10 cm and height 7 cm
#' set_graphics(10, 7)
#' }
#'
#' @export
set_graphics <- function(width, height){
  graphics.off()
  dev.new(width = width,
          height = height,
          unit = "cm",
          noRStudioGD = TRUE)
}


#' Customised theme for ggplot2
#'
#' This function creates a customised theme for ggplot2 plots. It modifies various
#' elements of the default `theme_classic()` to provide a specific style.
#'
#' @return A ggplot2 theme object with customised settings for plot title, axis titles,
#' axis text, legend text, legend title, major grid lines, legend key background, and legend position.
#'
#' @details
#' The `hgps_theme` function modifies the following elements of the classic theme:
#' \itemize{
#'   \item \code{plot.title}: Sets the font size of the plot title (default is 15).
#'   \item \code{axis.title.x}: Sets the font size of the x-axis title (default is 10).
#'   \item \code{axis.title.y}: Sets the font size of the y-axis title (default is 10).
#'   \item \code{axis.text.x}: Sets the font size of the x-axis text (default is 8).
#'   \item \code{axis.text.y}: Sets the font size of the y-axis text (default is 8).
#'   \item \code{legend.text}: Sets the font size of the legend text (default is 8).
#'   \item \code{legend.title}: Removes the legend title.
#'   \item \code{panel.grid.major.x}: Adds major grid lines on the x-axis.
#'   \item \code{legend.key}: Sets the legend key background (default is "transparent").
#'   \item \code{legend.position}: Positions the legend (default coordinates are (0.8, 0.9)).
#' }
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     hgps_theme()
#' }
#'
#' @export
hgps_theme <- function(){
  theme_classic() +
  theme(
    plot.title = element_text(size = 15),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    panel.grid.major.x = element_line(),
    legend.key = element_rect(fill = "transparent"),
    legend.position = c(.8,.9)
  )
}


NULL
