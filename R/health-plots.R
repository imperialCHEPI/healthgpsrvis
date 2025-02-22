#' Plot Risk Factors Over Time
#'
#' Generates line plot of a specified risk factor over time, grouped by source.
#'
#' @param riskft A character string specifying the risk factor to plot.
#'        Options: "bmi", "energyintake", "fat", "obesity", "protein", "sodium".
#' @param data_weighted A data frame with weighted values for various metrics.
#' @return A ggplot object representing the specified plot.
#' @export
riskfactors <- function(riskft, data_weighted) {
  riskfts <- c("bmi", "energyintake", "fat", "obesity", "protein", "sodium")

  if (!(riskft %in% riskfts)) {
    stop("Invalid risk factor. Choose from: 'bmi', 'energyintake', 'fat', 'obesity', 'protein', 'sodium'.")
  }

  y_label <- switch(riskft,
    bmi = "BMI (weighted)",
    energyintake = "Energy intake (weighted)",
    fat = "Fat (weighted)",
    obesity = "Obesity (weighted)",
    protein = "Protein (weighted)",
    sodium = "Sodium (weighted)"
  )

  y_value <- switch(riskft,
    bmi = "weighted_bmi",
    energyintake = "weighted_energyintake",
    fat = "weighted_fat",
    obesity = "weighted_obesity",
    protein = "weighted_protein",
    sodium = "weighted_sodium"
  )

  ggplot2::ggplot(
    data = data_weighted,
    ggplot2::aes(
      x = data_weighted$time,
      y = get(y_value),
      group = source
    )
  ) +
    ggplot2::geom_line(ggplot2::aes(col = source), linewidth = 1) +
    ggplot2::ggtitle(toupper(riskft)) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::labs(fill = "Source") +
    ggplot2::scale_x_continuous(limits = c(2020, 2055),
                                breaks = c(2020, 2025, 2030, 2035,
                                           2040, 2045, 2050, 2055)) +
    ggplot2::scale_y_continuous(labels = scale_fun) +
    ggplot2::labs(alt = "A line plot of a specified risk factor over time,
                  grouped by source") +
    hgps_theme()
}

#' Plot of Difference in Risk Factor
#'
#' Creates a line plot showing the reduction in a specified risk factor under
#' intervention over time.
#'
#' @param riskft_diff A character string specifying the difference in risk
#' factor to plot.
#'        Options: "bmi", "ei", "obesity", "sodium".
#' @param data_weighted_rf_wide_collapse A data frame with differences between
#' intervention and baseline values for risk factors.
#' @param scale_y_continuous_limits A numeric vector specifying the limits of
#' the scales for continuous y aesthetics.
#' @param scale_y_continuous_breaks A numeric vector specifying the breaks of
#' the scales for continuous y aesthetics.
#' @param scale_y_continuous_labels A numeric vector specifying the labels of
#' the scales for continuous y aesthetics.
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
    sodium = "Sodium"
  )

  y_value <- switch(riskft_diff,
    bmi = "diff_bmi_mean",
    ei = "diff_ei_mean",
    obesity = "diff_obesity_mean",
    sodium = "diff_sodium_mean"
  )

  y_ci_low <- switch(riskft_diff,
    bmi = "diff_bmi_ci_low",
    ei = "diff_ei_ci_low",
    obesity = "diff_obesity_ci_low",
    sodium = "diff_sodium_ci_low"
  )

  y_ci_high <- switch(riskft_diff,
    bmi = "diff_bmi_ci_high",
    ei = "diff_ei_ci_high",
    obesity = "diff_obesity_ci_high",
    sodium = "diff_sodium_ci_high"
  )

  plot_title <- switch(riskft_diff,
    bmi = "Reduction in BMI",
    ei = "Reduction in energy intake (kcal)",
    obesity = "Reduction in obesity prevalence",
    sodium = "Reduction in sodium (mg)"
  )

  ggplot2::ggplot(
    data = data_weighted_rf_wide_collapse,
    ggplot2::aes(
      x = data_weighted_rf_wide_collapse$time,
      y = get(y_value)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = get(y_ci_low),
        ymax = get(y_ci_high)
      ),
      alpha = 0.2
    ) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040,
                                           2045, 2050, 2055)) +
    ggplot2::scale_y_continuous(
      limits = scale_y_continuous_limits,
      breaks = scale_y_continuous_breaks,
      labels = scale_y_continuous_labels
    ) +
    ggplot2::labs(alt = "A line plot showing the reduction in a specified risk
                  factor under intervention over time") +
    hgps_theme() +
    ggplot2::theme(legend.position.inside = c(0.85, 0.22))
}

#' Plot of Incidence Difference
#'
#' Creates a line plot showing the reduction in a specified incidence number
#' over time.
#'
#' @param inc A character string specifying the incidence to plot.
#'        Options: "asthma", "ckd", "diabetes", "ischemia", "stroke".
#' @param data_weighted_ds_wide_diff A data frame containing the weighted mean
#' values of differences of incidences.
#' @return A ggplot object representing the specified plot.
#' @export
inc_diff <- function(inc,
                     data_weighted_ds_wide_diff) {
  incs <- c("asthma", "ckd", "diabetes", "ischemia", "stroke")

  if (!(inc %in% incs)) {
    stop("Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'.")
  }

  y_label <- switch(inc,
    asthma = "Asthma incidence",
    ckd = "CKD incidence",
    diabetes = "Diabetes incidence",
    ischemia = "Ischemia incidence",
    stroke = "Stroke incidence"
  )

  y_value <- switch(inc,
    asthma = "diff_inc_asthma",
    ckd = "diff_inc_ckd",
    diabetes = "diff_inc_db",
    ischemia = "diff_inc_ihd",
    stroke = "diff_inc_stroke"
  )

  plot_title <- switch(inc,
    asthma = "Asthma - Reduction in incidence number",
    ckd = "Chronic kidney disease - Reduction in incidence number",
    diabetes = "Diabetes - Reduction in incidence number",
    ischemia = "Ischemic heart disease - Reduction in incidence number",
    stroke = "Stroke - Reduction in incidence number"
  )

  ggplot2::ggplot(
    data = data_weighted_ds_wide_diff,
    ggplot2::aes(data_weighted_ds_wide_diff$time,
      y = get(y_value)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_x_continuous(
      limits = c(-3, 32),
      breaks = c(-3, 2, 7, 12, 17, 22, 27, 32),
      labels = c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055)
    ) +
    ggplot2::labs(alt = "A line plot showing the reduction in a specified
                  incidence number over time") +
    hgps_theme()
}

#' Plot of Cumulative Incidence Difference
#'
#' Creates a line plot showing the cumulative reduction in a specified incidence
#' number over time.
#'
#' @param inc A character string specifying the incidence to plot.
#'        Options: "asthma", "ckd", "diabetes", "ischemia", "stroke".
#' @param data_weighted_ds_wide_collapse A data frame with differences between
#' intervention and baseline values for incidences.
#' @param scale_y_continuous_limits A numeric vector specifying the limits of
#' the scales for continuous y aesthetics.
#' @param scale_y_continuous_breaks A numeric vector specifying the breaks of
#' the scales for continuous y aesthetics.
#' @param scale_y_continuous_labels A numeric vector specifying the labels of
#' the scales for continuous y aesthetics.
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
    stroke = "Stroke incidence"
  )

  y_value <- switch(inc,
    asthma = "cumdiff_inc_asthma_mean",
    ckd = "cumdiff_inc_ckd_mean",
    diabetes = "cumdiff_inc_db_mean",
    ischemia = "cumdiff_inc_ihd_mean",
    stroke = "cumdiff_inc_stroke_mean"
  )

  y_ci_low <- switch(inc,
    asthma = "cumdiff_inc_asthma_ci_low",
    ckd = "cumdiff_inc_ckd_ci_low",
    diabetes = "cumdiff_inc_db_ci_low",
    ischemia = "cumdiff_inc_ihd_ci_low",
    stroke = "cumdiff_inc_stroke_ci_low"
  )

  y_ci_high <- switch(inc,
    asthma = "cumdiff_inc_asthma_ci_high",
    ckd = "cumdiff_inc_ckd_ci_high",
    diabetes = "cumdiff_inc_db_ci_high",
    ischemia = "cumdiff_inc_ihd_ci_high",
    stroke = "cumdiff_inc_stroke_ci_high"
  )

  plot_title <- switch(inc,
    asthma = "Asthma - Cumulative reduction in incidence",
    ckd = "Chronic kidney disease - Cumulative reduction in incidence",
    diabetes = "Diabetes - Cumulative reduction in incidence",
    ischemia = "Ischemic heart disease - Cumulative reduction in incidence",
    stroke = "Stroke - Cumulative reduction in incidence"
  )

  ggplot2::ggplot(
    data = data_weighted_ds_wide_collapse,
    ggplot2::aes(data_weighted_ds_wide_collapse$time,
      y = get(y_value)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = get(y_ci_low),
        ymax = get(y_ci_high)
      ),
      alpha = 0.2
    ) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045,
                                           2050, 2055)) +
    ggplot2::scale_y_continuous(
      limits = scale_y_continuous_limits,
      breaks = scale_y_continuous_breaks,
      labels = scale_y_continuous_labels
    ) +
    ggplot2::labs(alt = "A line plot showing the cumulative reduction in a
                  specified incidence number over time") +
    hgps_theme() +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 10)) +
    ggplot2::theme(legend.position.inside = c(0.2, 0.2))
}

#' Plot of Burden of Disease
#'
#' Creates a line plot showing the reduction in a specified burden of disease
#' over time.
#'
#' @param burden A character string specifying the burden of disease to plot.
#'        Options: "daly", "dalycum", "yld", "yll".
#' @param data_weighted_bd_wide_collapse A data frame with differences between
#' intervention and baseline values for burden of disease.
#' @param scale_y_continuous_limits A numeric vector specifying the limits of
#' the scales for continuous y aesthetics.
#' @param scale_y_continuous_breaks A numeric vector specifying the breaks of
#' the scales for continuous y aesthetics.
#' @param scale_y_continuous_labels A numeric vector specifying the labels of
#' the scales for continuous y aesthetics.
#' @return A ggplot object representing the specified plot.
#' @export
burden_disease <- function(burden,
                           data_weighted_bd_wide_collapse,
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
    yll = "YLLs"
  )

  y_value <- switch(burden,
    daly = "diff_daly_mean",
    dalycum = "cumdiff_daly_mean",
    yld = "diff_yld_mean",
    yll = "diff_yll_mean"
  )

  y_ci_low <- switch(burden,
    daly = "diff_daly_ci_low",
    dalycum = "cumdiff_daly_ci_low",
    yld = "diff_yld_ci_low",
    yll = "diff_yll_ci_low"
  )

  y_ci_high <- switch(burden,
    daly = "diff_daly_ci_high",
    dalycum = "cumdiff_daly_ci_high",
    yld = "diff_yld_ci_high",
    yll = "diff_yll_ci_high"
  )

  plot_title <- switch(burden,
    daly = "Reduction in DALYs",
    dalycum = "Cumulative reduction in DALYs",
    yld = "Reduction in YLDs",
    yll = "Reduction in YLLs"
  )

  ggplot2::ggplot(
    data = data_weighted_bd_wide_collapse,
    ggplot2::aes(
      x = data_weighted_bd_wide_collapse$time,
      y = get(y_value)
    )
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = get(y_ci_low),
        ymax = get(y_ci_high)
      ),
      alpha = 0.2
    ) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::xlab("Year") +
    ggplot2::ylab(y_label) +
    ggplot2::scale_x_continuous(breaks = c(2020, 2025, 2030, 2035, 2040, 2045,
                                           2050, 2055)) +
    ggplot2::scale_y_continuous(
      limits = scale_y_continuous_limits,
      breaks = scale_y_continuous_breaks,
      labels = scale_y_continuous_labels
    ) +
    ggplot2::labs(alt = "A line plot showing the reduction in a specified
                  burden of disease over time") +
    hgps_theme() +
    ggplot2::theme(legend.position.inside = c(0.85, 0.2))
}

#' Combined Plot of several metrics
#'
#' Creates a combined plot of several metrics.
#'
#' @param metrics A list specifying the metrics to plot.
#' @param data_weighted A data frame with weighted values for various metrics.
#' @param data_mean_weighted_rf_wide A data frame containing the weighted mean
#' values of risk factors.
#' @param data_mean_weighted_inc_wide A data frame containing the weighted mean
#' values of incidences.
#' @param data_weighted_bd_wide_collapse A data frame with differences between
#' intervention and baseline values for burden of disease.
#' @param output_file Name of the output PDF as a string
#' @return A combined ggplot object arranged in a grid.
#' @export
combine_plots <- function(metrics,
                          data_weighted = NULL,
                          data_mean_weighted_rf_wide = NULL,
                          data_mean_weighted_inc_wide = NULL,
                          data_weighted_bd_wide_collapse = NULL,
                          output_file) {
  plots <- list()

  if (!is.null(metrics$risk_factors) && !is.null(data_weighted)) {
    for (riskft in metrics$risk_factors) {
      plots <- c(plots, list(riskfactors(riskft, data_weighted)))
    }
  }

  if (!is.null(metrics$risk_factors_diff) &&
        !is.null(data_mean_weighted_rf_wide)) {
    for (riskft_diff in metrics$risk_factors_diff) {
      plots <- c(plots, list(riskfactors_diff(riskft_diff,
                                              data_mean_weighted_rf_wide)))
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

  if (!is.null(metrics$burden_disease) &&
        !is.null(data_weighted_bd_wide_collapse)) {
    for (burden in metrics$burden_disease) {
      plots <- c(plots, list(burden_disease(burden,
                                            data_weighted_bd_wide_collapse)))
    }
  }

  # Combine plots into a single PDF
  grDevices::pdf(output_file, width = 11, height = 8.5)

  for (i in seq(1, length(plots), by = 4)) {
    gridExtra::grid.arrange(grobs = plots[i:min(i + 3, length(plots))],
                            ncol = 2)
  }

  grDevices::dev.off()
}
