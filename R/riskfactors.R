#' Plot Risk Factors Over Time
#'
#' Generates a line plot of a specified risk factor over time, grouped by source.
#'
#' @param riskft A character string specifying the risk factor to plot.
#'        Options are: "sodium", "fat", "protein", "ei", "bmi", "obese".
#' @param data_mean_weighted A data frame with weighted mean values for various metrics.
#' @return A ggplot object representing the specified plot.
#' @export
riskfactors <- function(riskft, data_mean_weighted) {
  riskfts <- c("sodium", "fat", "protein", "ei", "bmi", "obese")

  if (!(riskft %in% riskfts)) {
    stop("Invalid risk factor. Choose from: 'sodium', 'fat', 'protein', 'ei', 'bmi', 'obese'.")
  }

  y_label <- switch(riskft,
                    sodium = "Sodium (weighted)",
                    fat = "Fat (weighted)",
                    protein = "Protein (weighted)",
                    ei = "Energy intake (weighted)",
                    bmi = "BMI (weighted)",
                    obese = "Obesity (weighted)")

  y_value <- switch(riskft,
                    sodium = "weighted_sodium",
                    fat = "weighted_fat",
                    protein = "weighted_protein",
                    ei = "weighted_energyintake",
                    bmi = "weighted_bmi",
                    obese = "weighted_obesity")

  ggplot2::ggplot(data = data_mean_weighted,
                  ggplot2::aes(x = data_mean_weighted$time,
                               y = get(y_value),
                               group = source)) +
    ggplot2::geom_line(ggplot2::aes(col = source), size = 1) +
    ggplot2::ggtitle(toupper(riskft)) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::labs(fill = "Source") +
    ggplot2::scale_x_continuous(limits = c(2020, 2055), breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    ggplot2::scale_y_continuous(labels = scaleFUN) +
    hgps_theme
}
