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
    ggplot2::geom_line(ggplot2::aes(col = source), linewidth = 1) +
    ggplot2::ggtitle(toupper(riskft)) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::labs(fill = "Source") +
    ggplot2::scale_x_continuous(limits = c(2020, 2055), breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    ggplot2::scale_y_continuous(labels = scaleFUN) +
    ggplot2::labs(alt = "A line plot of a specified risk factor over time, grouped by source") +
    hgps_theme()
}

#' Plot of Difference in Risk Factor
#'
#' Creates a line plot showing the reduction in a specified risk factor under intervention over time.
#'
#' @param riskft_diff A character string specifying the difference in risk factor to plot.
#'        Options are: "bmi", "ei", "obesity", "sodium".
#' @param data_weighted_rf_wide_collapse A data frame with differences between intervention and baseline values for risk factors.
#' @param scale_y_continuous_limits A numeric vector specifying the limits of the scales for continuous y aesthetics.
#' @param scale_y_continuous_breaks A numeric vector specifying the breaks of the scales for continuous y aesthetics.
#' @param scale_y_continuous_labels A numeric vector specifying the labels of the scales for continuous y aesthetics.
#' @return A ggplot object representing the specified plot.
#' @export
riskfactors_diff <- function(riskft_diff,
                             data_weighted_rf_wide_collapse,
                             scale_y_continuous_limits = NULL,
                             scale_y_continuous_breaks = ggplot2::waiver(),
                             scale_y_continuous_labels = ggplot2::waiver()) {
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
                    bmi = "diff_bmi_mean",
                    ei = "diff_ei_mean",
                    obesity = "diff_obesity_mean",
                    sodium = "diff_sodium_mean")

  y_min <- switch(riskft_diff,
                  bmi = "diff_bmi_min",
                  ei = "diff_ei_min",
                  obesity = "diff_obesity_min",
                  sodium = "diff_sodium_min")

  y_max <- switch(riskft_diff,
                  bmi = "diff_bmi_max",
                  ei = "diff_ei_max",
                  obesity = "diff_obesity_max",
                  sodium = "diff_sodium_max")

  plot_title <- switch(riskft_diff,
                       bmi = "Reduction in BMI by income class",
                       ei = "Reduction in energy intake (kcal) by income class",
                       obesity = "Reduction in obesity prevalence by income class",
                       sodium = "Reduction in sodium (mg) by income class")

  ggplot2::ggplot(data = data_weighted_rf_wide_collapse,
                  ggplot2::aes(x = data_weighted_rf_wide_collapse$time,
                               y = get(y_value),
                               colour = data_weighted_rf_wide_collapse$income)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = y_min,
                                      ymax = y_max),
                         alpha = 0.2) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(limits = c(-3, 32),
                       breaks = c(-3, 2, 7, 12, 17, 22, 27, 32),
                       labels = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    ggplot2::scale_y_continuous(limits = scale_y_continuous_limits,
                       breaks = scale_y_continuous_breaks,
                       labels = scale_y_continuous_labels) +
    ggplot2::labs(alt = "A line plot showing the reduction in a specified risk factor under intervention over time") +
    hgps_theme() +
    ggplot2::theme(legend.position.inside = c(0.85,0.22))
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
    ggplot2::geom_line(colour = "red", linewidth = 1) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(limits = c(-3, 32),
                     breaks = c(-3, 2, 7, 12, 17, 22, 27, 32),
                     labels = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    ggplot2::labs(alt = "A line plot showing the reduction in a specified incidence number over time") +
    hgps_theme()
}

#' Plot of Cumulative Incidence Difference
#'
#' Creates a line plot showing the cumulative reduction in a specified incidence number over time.
#'
#' @param inc A character string specifying the incidence to plot.
#'        Options are: "asthma", "ckd", "diabetes", "ischemia", "stroke".
#' @param data_weighted_ds_wide_collapse A data frame with differences between intervention and baseline values for incidences.
#' @param scale_y_continuous_limits A numeric vector specifying the limits of the scales for continuous y aesthetics.
#' @param scale_y_continuous_breaks A numeric vector specifying the breaks of the scales for continuous y aesthetics.
#' @param scale_y_continuous_labels A numeric vector specifying the labels of the scales for continuous y aesthetics.
#' @return A ggplot object representing the specified plot.
#' @export
inc_cum <- function(inc,
                    data_weighted_ds_wide_collapse,
                    scale_y_continuous_limits = NULL,
                    scale_y_continuous_breaks = ggplot2::waiver(),
                    scale_y_continuous_labels = ggplot2::waiver()) {
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
                    asthma = "cumdiff_inc_asthma_mean",
                    ckd = "cumdiff_inc_ckd_mean",
                    diabetes = "cumdiff_inc_db_mean",
                    ischemia = "cumdiff_inc_ihd_mean",
                    stroke = "cumdiff_inc_stroke_mean")

  y_min <- switch(inc,
                  asthma = "cumdiff_inc_asthma_min",
                  ckd = "cumdiff_inc_ckd_min",
                  diabetes = "cumdiff_inc_db_min",
                  ischemia = "cumdiff_inc_ihd_min",
                  stroke = "cumdiff_inc_stroke_min")

  y_max <- switch(inc,
                  asthma = "cumdiff_inc_asthma_max",
                  ckd = "cumdiff_inc_ckd_max",
                  diabetes = "cumdiff_inc_db_max",
                  ischemia = "cumdiff_inc_ihd_max",
                  stroke = "cumdiff_inc_stroke_max")

  plot_title <- switch(inc,
                       asthma = "Asthma - Cumulative reduction by income class",
                       ckd = "Chronic kidney disease - Cumulative reduction by income class",
                       diabetes = "Diabetes - Cumulative reduction by income class",
                       ischemia = "Ischemic heart disease - Cumulative reduction by income class",
                       stroke = "Stroke - Cumulative reduction by income class")

  ggplot2::ggplot(data = data_weighted_ds_wide_collapse,
                  ggplot2::aes(data_weighted_ds_wide_collapse$time,
                               y = get(y_value),
                               colour = data_weighted_ds_wide_collapse$income)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = y_min,
                                      ymax = y_max),
                         alpha = 0.2) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    ggplot2::scale_y_continuous(limits = scale_y_continuous_limits,
                                breaks = scale_y_continuous_breaks,
                                labels = scale_y_continuous_labels) +
    ggplot2::labs(alt = "A line plot showing the cumulative reduction in a specified incidence number over time") +
    hgps_theme() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10)) +
    ggplot2::theme(legend.position.inside = c(0.2,0.2))
}

#' Plot of Burden of Disease
#'
#' Creates a line plot showing the reduction in a specified burden of disease over time.
#'
#' @param burden A character string specifying the burden of disease to plot.
#'        Options are: "daly", "dalycum", "yld", "yll".
#' @param data_weighted_burden_wide_collapse A data frame with differences between intervention and baseline values for burden of disease.
#' @param scale_y_continuous_limits A numeric vector specifying the limits of the scales for continuous y aesthetics.
#' @param scale_y_continuous_breaks A numeric vector specifying the breaks of the scales for continuous y aesthetics.
#' @param scale_y_continuous_labels A numeric vector specifying the labels of the scales for continuous y aesthetics.
#' @return A ggplot object representing the specified plot.
#' @export
burden_disease <- function(burden,
                           data_weighted_burden_wide_collapse,
                           scale_y_continuous_limits = NULL,
                           scale_y_continuous_breaks = ggplot2::waiver(),
                           scale_y_continuous_labels = ggplot2::waiver()) {
  burdens <- c("daly", "dalycum", "yld", "yll")

  if (!(burden %in% burdens)) {
    stop("Invalid burden of disease. Choose from: 'daly', 'dalycum', 'yld', 'yll'.")
  }

  y_label <- switch(burden,
                    daly = "DALYs",
                    dalycum = "DALYs",
                    yld = "YLDs",
                    yll = "YLLs")

  y_value <- switch(burden,
                    daly = "diff_daly_mean",
                    dalycum = "cumdiff_daly_mean",
                    yld = "diff_yld_mean",
                    yll = "diff_yll_mean")

  y_min <- switch(burden,
                  daly = "diff_daly_min",
                  dalycum = "cumdiff_daly_min",
                  yld = "diff_yld_min",
                  yll = "diff_yll_min")

  y_max <- switch(burden,
                  daly = "diff_daly_max",
                  dalycum = "cumdiff_daly_max",
                  yld = "diff_yld_max",
                  yll = "diff_yll_max")

  plot_title <- switch(burden,
                       daly = "Reduction in DALYs by income class",
                       dalycum = "Cumulative reduction in DALYs by income class",
                       yld = "Reduction in YLDs by income class",
                       yll = "Reduction in YLLs by income class")

  ggplot2::ggplot(data = data_weighted_burden_wide_collapse,
                  ggplot2::aes(x = data_weighted_burden_wide_collapse$time,
                               y = get(y_value),
                               colour = data_weighted_burden_wide_collapse$income)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = y_min,
                                      ymax = y_max),
                         alpha = 0.2) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    ggplot2::scale_y_continuous(limits = scale_y_continuous_limits,
                                breaks = scale_y_continuous_breaks,
                                labels = scale_y_continuous_labels) +
    ggplot2::labs(alt = "A line plot showing the reduction in a specified burden of disease over time") +
    hgps_theme() +
    ggplot2::theme(legend.position.inside = c(0.85,0.2))
}

#' Plot of Life Expectancy under Intervention
#'
#' Creates a line plot showing the increase in life expectancy under intervention over time.
#'
#' @param diff A character string specifying the life expectancy to plot.
#' @param data_ple_wide A data frame containing the life expectancy.
#' @return A ggplot object representing the specified plot.
#' @export
life_exp <- function(diff, data_ple_wide) {
  ggplot2::ggplot(data = data_ple_wide,
                  ggplot2::aes(x = data_ple_wide$timediff, y = diff)) +
    ggplot2::geom_line(color = "purple", linewidth = 1) +
    ggplot2::ggtitle("Increase in life expectancy under intervention") +
    ggplot2::xlab("Year") +
    ggplot2::ylab("Life expectancy (years)") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(limits = c(-3, 32),
                     breaks = c(-3, 2, 7, 12, 17, 22, 27, 32),
                     labels = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)) +
    ggplot2::labs(alt = "A line plot showing the increase in life expectancy under intervention over time") +
    hgps_theme()
}

#' Combined Plot of several metrics
#'
#' Creates a combined plot of several metrics.
#'
#' @param metrics A list specifying the metrics to plot.
#' @param data_mean_weighted A data frame with weighted mean values for various metrics.
#' @param data_mean_weighted_rf_wide A data frame containing the weighted mean values of risk factors.
#' @param data_mean_weighted_inc_wide A data frame containing the weighted mean values of incidences.
#' @param data_weighted_burden_wide_collapse A data frame with differences between intervention and baseline values for burden of disease.
#' @param data_ple_wide A data frame containing the life expectancy.
#' @param output_file Name of the output PDF as a string
#' @return A combined ggplot object arranged in a grid.
#' @export
combine_plots <- function(metrics,
                           data_mean_weighted = NULL,
                           data_mean_weighted_rf_wide = NULL,
                           data_mean_weighted_inc_wide = NULL,
                           data_weighted_burden_wide_collapse = NULL,
                           data_ple_wide = NULL,
                           output_file) {

  plots <- list()

  if (!is.null(metrics$risk_factors) && !is.null(data_mean_weighted)) {
    for (riskft in metrics$risk_factors) {
      plots <- c(plots, list(riskfactors(riskft, data_mean_weighted)))
    }
  }

  if (!is.null(metrics$risk_factors_diff) && !is.null(data_mean_weighted_rf_wide)) {
    for (riskft_diff in metrics$risk_factors_diff) {
      plots <- c(plots, list(riskfactors_diff(riskft_diff, data_mean_weighted_rf_wide)))
    }
  }

  if (!is.null(metrics$inc_diff) && !is.null(data_mean_weighted_inc_wide)) {
    for (inc in metrics$inc_diff) {
      plots <- c(plots, list(inc_diff(inc, data_mean_weighted_inc_wide)))
    }
  }

  if (!is.null(metrics$inc_cum) && !is.null(data_mean_weighted_inc_wide)) {
    for (inc in metrics$inc_cum) {
      plots <- c(plots, list(inc_cum(inc, data_mean_weighted_inc_wide)))
    }
  }

  if (!is.null(metrics$burden_disease) && !is.null(data_weighted_burden_wide_collapse)) {
    for (burden in metrics$burden_disease) {
      plots <- c(plots, list(burden_disease(burden, data_weighted_burden_wide_collapse)))
    }
  }

  if (!is.null(metrics$life_exp) && !is.null(data_ple_wide)) {
    for (diff in metrics$life_exp) {
      plots <- c(plots, list(life_exp(diff, data_ple_wide)))
    }
  }

  # Combine plots into a single PDF
  grDevices::pdf(output_file, width = 11, height = 8.5)

  for (i in seq(1, length(plots), by = 4)) {
    gridExtra::grid.arrange(grobs = plots[i:min(i+3, length(plots))], ncol = 2)
  }

  grDevices::dev.off()
}
