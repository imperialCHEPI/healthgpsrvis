#' Plot Risk Factors Over Time
#'
#' Generates a line plot of a specified risk factor over time, grouped by source.
#'
#' @param riskft A character string specifying the risk factor to plot.
#'        Options are: "bmi", "ei", "fat", "obese", "protein", "sodium".
#' @param data_mean_weighted A data frame with weighted mean values for various metrics.
#' @return A ggplot object representing the specified plot.
#' @export
riskfactors <- function(riskft, data_mean_weighted) {
  riskfts <- c("bmi", "ei", "fat", "obese", "protein", "sodium")

  if (!(riskft %in% riskfts)) {
    stop("Invalid risk factor. Choose from: 'bmi', 'ei', 'fat', 'obese', 'protein', 'sodium'.")
  }

  y_label <- switch(riskft,
                    bmi = "BMI (weighted)",
                    ei = "Energy intake (weighted)",
                    fat = "Fat (weighted)",
                    obese = "Obesity (weighted)",
                    protein = "Protein (weighted)",
                    sodium = "Sodium (weighted)")

  y_value <- switch(riskft,
                    bmi = "weighted_bmi",
                    ei = "weighted_energyintake",
                    fat = "weighted_fat",
                    obese = "weighted_obesity",
                    protein = "weighted_protein",
                    sodium = "weighted_sodium")

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

#' Plot of Difference in Risk Factor
#'
#' Creates a line plot showing the reduction in a specified risk factor under intervention over time.
#'
#' @param riskft_diff A character string specifying the difference in risk factor to plot.
#'        Options are: "bmi", "ei", "obesity", "sodium".
#' @param data_mean_weighted_rf_wide A data frame containing the weighted mean values of risk factors.
#' @return A ggplot object representing the specified plot.
#' @export
riskfactors_diff <- function(riskft_diff, data_mean_weighted_rf_wide) {
  riskft_diffs <- c("bmi", "ei", "obesity", "sodium")

  if (!(riskft_diff %in% riskft_diffs)) {
    stop("Invalid risk factor difference. Choose from: 'bmi', 'ei', 'obesity', 'sodium'.")
  }

  y_label <- switch(riskft_diff,
                    bmi = "BMI",
                    ei = "Energy",
                    obesity = "Obesity",
                    sodium = "Sodium")

  y_value <- switch(riskft_diff,
                    bmi = "diff_bmi",
                    ei = "diff_ei",
                    obesity = "diff_obesity",
                    sodium = "diff_sodium")

  plot_title <- switch(riskft_diff,
                       bmi = "Reduction in BMI under intervention",
                       ei = "Reduction in energy intake (kcal) under intervention",
                       obesity = "Reduction in obesity prevalence under intervention",
                       sodium = "Reduction in sodium (mg) under intervention")

  ggplot2::ggplot(data = data_mean_weighted_rf_wide,
                  ggplot2::aes(x = data_mean_weighted_rf_wide$timediff,
                               y = get(y_value))) +
    ggplot2::geom_line(color = "blue", size = 1) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(limits = c(-3, 32),
                       breaks = c(-3, 2, 7, 12, 17, 22, 27, 32),
                       labels = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    hgps_theme
}

#' Plot of Incidence Difference
#'
#' Creates a line plot showing the reduction in a specified incidence number over time.
#'
#' @param inc A character string specifying the incidence to plot.
#'        Options are: "asthma", "ckd", "diabetes", "ischemia", "stroke".
#' @param data_mean_weighted_inc_wide A data frame containing the weighted mean values of incidences.
#' @return A ggplot object representing the specified plot.
#' @export
inc_diff <- function(inc, data_mean_weighted_inc_wide) {
  incs <- c("asthma", "ckd", "diabetes", "ischemia", "stroke")

  if (!(inc %in% incs)) {
    stop("Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'.")
  }

  y_label <- switch(inc,
                    asthma = "Asthma incidence",
                    ckd = "CKD incidence",
                    diabetes = "Diabetes incidence",
                    ischemia = "Ischemia incidence",
                    stroke = "Stroke incidence")

  y_value <- switch(inc,
                    asthma = "diff_asthma",
                    ckd = "diff_ckd",
                    diabetes = "diff_diabetes",
                    ischemia = "diff_ihd",
                    stroke = "diff_stroke")

  plot_title <- switch(inc,
                       asthma = "Asthma - Reduction in incidence number",
                       ckd = "Chronic kidney disease - Reduction in incidence number",
                       diabetes = "Diabetes - Reduction in incidence number",
                       ischemia = "Ischemic heart disease - Reduction in incidence number",
                       stroke = "Stroke - Reduction in incidence number")

  ggplot2::ggplot(data = data_mean_weighted_inc_wide,
                  ggplot2::aes(data_mean_weighted_inc_wide$timediff,
                               y = get(y_value))) +
    ggplot2::geom_line(color = "red", size = 1) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(limits = c(-3, 32),
                     breaks = c(-3, 2, 7, 12, 17, 22, 27, 32),
                     labels = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    hgps_theme
}

#' Plot of Cumulative Incidence Difference
#'
#' Creates a line plot showing the cumulative reduction in a specified incidence number over time.
#'
#' @param inc A character string specifying the incidence to plot.
#'        Options are: "asthma", "ckd", "diabetes", "ischemia", "stroke".
#' @param data_mean_weighted_inc_wide A data frame containing the weighted mean values of incidences.
#' @return A ggplot object representing the specified plot.
#' @export
inc_cum <- function(inc, data_mean_weighted_inc_wide) {
  incs <- c("asthma", "ckd", "diabetes", "ischemia", "stroke")

  if (!(inc %in% incs)) {
    stop("Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'.")
  }

  y_label <- switch(inc,
                    asthma = "Asthma incidence",
                    ckd = "CKD incidence",
                    diabetes = "Diabetes incidence",
                    ischemia = "Ischemia incidence",
                    stroke = "Stroke incidence")

  y_value <- switch(inc,
                    asthma = "cumdiff_asthma",
                    ckd = "cumdiff_ckd",
                    diabetes = "cumdiff_diabetes",
                    ischemia = "cumdiff_ihd",
                    stroke = "cumdiff_stroke")

  plot_title <- switch(inc,
                       asthma = "Asthma - Cumulative reduction in incidence number",
                       ckd = "Chronic kidney disease - Cumulative reduction in incidence number",
                       diabetes = "Diabetes - Cumulative reduction in incidence number",
                       ischemia = "Ischemic heart disease - Cumulative reduction in incidence number",
                       stroke = "Stroke - Cumulative reduction in incidence number")

  ggplot2::ggplot(data = data_mean_weighted_inc_wide,
                  ggplot2::aes(data_mean_weighted_inc_wide$timediff,
                               y = get(y_value))) +
    ggplot2::geom_line(color = "purple", size = 1) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(limits = c(-3, 32),
                     breaks = c(-3, 2, 7, 12, 17, 22, 27, 32),
                     labels = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    hgps_theme +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10))
}
