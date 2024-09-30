#' Data Processing
#'
#' This file contains a set of functions designed to work together for processing the data.
#' Below is a description of how to use these functions in sequence.
#'
#' ## Step-by-Step Usage:
#'
#' 1. **Read the data**: This function reads the data from the location specified `data <-  readRDS("data.rds")`.
#'
#' 1. **`gen_data_mean`**: Calculates weighted mean values for various metrics over years `data_weighted <- gen_data_mean(data)`.
#'
#' 1. **`gen_data_weighted_rf`**: Calculates the differences between intervention and baseline values for risk factors `data_weighted_rf_wide_collapse <- gen_data_weighted_rf(data_weighted)`.
#'
#' 1. **`gen_data_weighted_ds`**: Calculates the differences between intervention and baseline values for incidences `data_weighted_ds_wide_collapse <- gen_data_weighted_ds(data_weighted)`.
#'
#' 1. **`gen_data_weighted_burden`**: Calculates the differences between intervention and baseline values for burden of disease `data_weighted_burden_wide_collapse <- gen_data_weighted_burden(data_weighted)`.
#'
#' 1. **`gen_data_weighted_burden_spline`**: Performs data smoothing for burden of disease, when necessary. For instance, with only a few simulations, there can be positive values in difference in burden of disease `data_weighted_burden_spline <- gen_data_weighted_burden_spline(data_weighted_burden_wide_collapse)`.
#'
#' 1. **`gen_data_le`**: Calculates life expectancy for various age and groups `data_ple_wide <- gen_data_le(data_weighted)`.
#'
#' ## Examples
#' ```r
#' # Example of using all functions together
#' data <- readRDS("data.rds")
#' data_weighted <- gen_data_mean(data)
#' data_weighted_rf_wide_collapse <- gen_data_weighted_rf(data_weighted)
#' data_weighted_ds_wide_collapse <- gen_data_weighted_ds(data_weighted)
#' data_weighted_burden_wide_collapse <- gen_data_weighted_burden(data_weighted)
#' data_weighted_burden_spline <- gen_data_weighted_burden_spline(data_weighted_burden_wide_collapse)
#' data_ple_wide <- gen_data_le(data_weighted)
#' ```
#'
#' @name DataProcessing
NULL

#' Generate Weighted Mean Values Over Years
#'
#' This function calculates weighted mean values for various metrics over years.
#'
#' @param data A data frame containing values for various metrics.
#' @return A data frame with weighted values for various metrics over years.
#' @export
gen_data_weighted <- function(data) {
  config <- load_config("default")
  data_weighted <- dplyr::group_by_at(data, config$grouping_vars)
  data_weighted <- data_weighted |>
    dplyr::mutate(data_weighted$prevalence_stroke <- data_weighted$prevalence_intracerebralhemorrhage +
                                           data_weighted$prevalence_ischemicstroke +
                                           data_weighted$prevalence_subarachnoidhemorrhage,
                  data_weighted$incidence_stroke <- data_weighted$incidence_intracerebralhemorrhage +
                                          data_weighted$incidence_ischemicstroke +
                                          data_weighted$incidence_subarachnoidhemorrhage)

    weighted_mean <- purrr::map(config$weighted_vars, function(value){
      stats::weighted.mean(data_weighted[[value]], data_weighted$count, na.rm = TRUE)
    })
    names(weighted_mean) <- paste0("weighted_", config$weighted_vars)

    prevalence_mean <- purrr::map(paste0("prevalence_", config$disease), function(value){
      stats::weighted.mean(data_weighted[[value]], data_weighted$count, na.rm = TRUE)
    })
    names(prevalence_mean) <- paste0("wprev_", config$disease)

    prevalence_sum <- purrr::map(paste0("prevalence_", config$disease), function(value){
      sum(data_weighted[[value]] * data_weighted$count, na.rm = TRUE)
    })
    names(prevalence_sum) <- paste0("prevcase_", config$disease)

    totalcase_sum <- purrr::map(paste0("incidence_", config$disease), function(value){
      sum(data_weighted[[value]] * data_weighted$count, na.rm = TRUE)
    })
    names(totalcase_sum) <- paste0("totalcase_", config$disease)

    total_sum <- purrr::map(config$burden, function(value){
      sum(data_weighted[[value]] * data_weighted$count, na.rm = TRUE)
    })
    names(total_sum) <- paste0("total_", config$burden)

    data_weighted <- dplyr::summarise(
      !!!weighted_mean,
      !!!prevalence_mean,
      !!!prevalence_sum,
      !!!totalcase_sum
    )
  return(data_weighted)
}

#' Calculate Differences for Risk Factors
#'
#' This function calculates the differences between intervention and baseline values for risk factors.
#'
#' @param data_weighted A data frame containing weighted mean values for various metrics.
#' @return A data frame with differences between intervention and baseline values for risk factors.
#' @export
gen_data_weighted_rf <- function(data_weighted) {
  config <- load_config("default")
  data_Weighted_rf <- dplyr::select(data_weighted,
                                    config$names_from,
                                    config$id_cols,
                                    config$weighted_rf)

  data_weighted_rf_wide <- tidyr::pivot_wider(data_weighted_rf,
                                            names_from = config$names_from,
                                            id_cols = config$id_cols,
                                            values_from = config$weighted_rf)

  data_weighted_rf_wide <- data_weighted_rf_wide |>
    dplyr::mutate(data_weighted_rf_wide$diff_sodium <- data_weighted_rf_wide$weighted_sodium_intervention - data_weighted_rf_wide$weighted_sodium_baseline,
                  data_weighted_rf_wide$diff_ei <- data_weighted_rf_wide$weighted_energyintake_intervention - data_weighted_rf_wide$weighted_energyintake_baseline,
                  data_weighted_rf_wide$diff_bmi <- data_weighted_rf_wide$weighted_bmi_intervention - data_weighted_rf_wide$weighted_bmi_baseline,
                  data_weighted_rf_wide$diff_obesity <- data_weighted_rf_wide$weighted_obesity_intervention - data_weighted_rf_wide$weighted_obesity_baseline)
  data_weighted_rf_wide_collapse <- data_weighted_rf_wide |>
    dplyr::group_by(data_weighted_rf_wide$time) |>
    dplyr::summarise(data_weighted_rf_wide$diff_sodium_mean <- mean(data_weighted_rf_wide$diff_sodium),
                     data_weighted_rf_wide$diff_sodium_min <- min(data_weighted_rf_wide$diff_sodium),
                     data_weighted_rf_wide$diff_sodium_max <- max(data_weighted_rf_wide$diff_sodium),
                     data_weighted_rf_wide$diff_ei_mean <- mean(data_weighted_rf_wide$diff_ei),
                     data_weighted_rf_wide$diff_ei_min <- min(data_weighted_rf_wide$diff_ei),
                     data_weighted_rf_wide$diff_ei_max <- max(data_weighted_rf_wide$diff_ei),
                     data_weighted_rf_wide$diff_bmi_mean <- mean(data_weighted_rf_wide$diff_bmi),
                     data_weighted_rf_wide$diff_bmi_min <- min(data_weighted_rf_wide$diff_bmi),
                     data_weighted_rf_wide$diff_bmi_max <- max(data_weighted_rf_wide$diff_bmi),
                     data_weighted_rf_wide$diff_obesity_mean <- mean(data_weighted_rf_wide$diff_obesity),
                     data_weighted_rf_wide$diff_obesity_min <- min(data_weighted_rf_wide$diff_obesity),
                     data_weighted_rf_wide$diff_obesity_max <- max(data_weighted_rf_wide$diff_obesity))

  return(data_weighted_rf_wide_collapse)
}

#' Calculate Differences for Incidences
#'
#' This function calculates the differences between intervention and baseline values for incidences.
#'
#' @param data_weighted A data frame containing weighted mean values for various metrics.
#' @return A data frame with differences between intervention and baseline values for incidences.
#' @export
gen_data_weighted_ds <- function(data_weighted) {
  data_weighted_ds <- dplyr::select(data_weighted,
                                          data_weighted$source,
                                          data_weighted$time,
                                          data_weighted$simID,
                                          data_weighted$totalcase_ihd,
                                          data_weighted$totalcase_diabetes,
                                          data_weighted$totalcase_stroke,
                                          data_weighted$totalcase_asthma,
                                          data_weighted$totalcase_ckd)

  data_weighted_ds_wide <- tidyr::pivot_wider(data_weighted_ds,
                                             names_from = data_weighted_ds$source,
                                             id_cols = c(data_weighted_ds$time, data_weighted_ds$simID),
                                             values_from = c(data_weighted_ds$totalcase_ihd,
                                                             data_weighted_ds$totalcase_diabetes,
                                                             data_weighted_ds$totalcase_stroke,
                                                             data_weighted_ds$totalcase_asthma,
                                                             data_weighted_ds$totalcase_ckd))

  data_weighted_ds_wide <- data_weighted_ds_wide |>
    dplyr::mutate(data_weighted_ds_wide$diff_inc_ihd <- 100*(data_weighted_ds_wide$totalcase_ihd_intervention - data_weighted_ds_wide$totalcase_ihd_baseline),
                  data_weighted_ds_wide$diff_inc_db <- 100*(data_weighted_ds_wide$totalcase_diabetes_intervention - data_weighted_ds_wide$totalcase_diabetes_baseline),
                  data_weighted_ds_wide$diff_inc_stroke <- 100*(data_weighted_ds_wide$totalcase_stroke_intervention - data_weighted_ds_wide$totalcase_stroke_baseline),
                  data_weighted_ds_wide$diff_inc_asthma <- 100*(data_weighted_ds_wide$totalcase_asthma_intervention - data_weighted_ds_wide$totalcase_asthma_baseline),
                  data_weighted_ds_wide$diff_inc_ckd <- 100*(data_weighted_ds_wide$totalcase_ckd_intervention - data_weighted_ds_wide$totalcase_ckd_baseline))

  data_weighted_ds_wide <- data_weighted_ds_wide |>
    dplyr::group_by(data_weighted_ds_wide$simID) |>
    dplyr::mutate(data_weighted_ds_wide$cumdiff_inc_ihd <- cumsum(data_weighted_ds_wide$diff_inc_ihd),
                  data_weighted_ds_wide$cumdiff_inc_db <- cumsum(data_weighted_ds_wide$diff_inc_db),
                  data_weighted_ds_wide$cumdiff_inc_stroke <- cumsum(data_weighted_ds_wide$diff_inc_stroke),
                  data_weighted_ds_wide$cumdiff_inc_asthma <- cumsum(data_weighted_ds_wide$diff_inc_asthma),
                  data_weighted_ds_wide$cumdiff_inc_ckd <- cumsum(data_weighted_ds_wide$diff_inc_ckd))

  data_weighted_ds_wide_collapse <- data_weighted_ds_wide |>
    dplyr::group_by(data_weighted_ds_wide$time) |>
    dplyr::summarise(data_weighted_ds_wide$cumdiff_inc_ihd_mean <- mean(data_weighted_ds_wide$cumdiff_inc_ihd),
                     data_weighted_ds_wide$cumdiff_inc_ihd_min <- min(data_weighted_ds_wide$cumdiff_inc_ihd),
                     data_weighted_ds_wide$cumdiff_inc_ihd_max <- max(data_weighted_ds_wide$cumdiff_inc_ihd),
                     data_weighted_ds_wide$cumdiff_inc_db_mean <- mean(data_weighted_ds_wide$cumdiff_inc_db),
                     data_weighted_ds_wide$cumdiff_inc_db_min <- min(data_weighted_ds_wide$cumdiff_inc_db),
                     data_weighted_ds_wide$cumdiff_inc_db_max <- max(data_weighted_ds_wide$cumdiff_inc_db),
                     data_weighted_ds_wide$cumdiff_inc_stroke_mean <- mean(data_weighted_ds_wide$cumdiff_inc_stroke),
                     data_weighted_ds_wide$cumdiff_inc_stroke_min <- min(data_weighted_ds_wide$cumdiff_inc_stroke),
                     data_weighted_ds_wide$cumdiff_inc_stroke_max <- max(data_weighted_ds_wide$cumdiff_inc_stroke),
                     data_weighted_ds_wide$cumdiff_inc_asthma_mean <- mean(data_weighted_ds_wide$cumdiff_inc_asthma),
                     data_weighted_ds_wide$cumdiff_inc_asthma_min <- min(data_weighted_ds_wide$cumdiff_inc_asthma),
                     data_weighted_ds_wide$cumdiff_inc_asthma_max <- max(data_weighted_ds_wide$cumdiff_inc_asthma),
                     data_weighted_ds_wide$cumdiff_inc_ckd_mean <- mean(data_weighted_ds_wide$cumdiff_inc_ckd),
                     data_weighted_ds_wide$cumdiff_inc_ckd_min <- min(data_weighted_ds_wide$cumdiff_inc_ckd),
                     data_weighted_ds_wide$cumdiff_inc_ckd_max <- max(data_weighted_ds_wide$cumdiff_inc_ckd))

  return(data_weighted_ds_wide_collapse)
}

#' Calculate Differences for Burden of Disease
#'
#' This function calculates the differences between intervention and baseline values for burden of disease.
#'
#' @param data_weighted A data frame containing weighted mean values for various metrics.
#' @return A data frame with differences between intervention and baseline values for burden of disease.
#' @export
gen_data_weighted_burden <- function(data_weighted) {
  data_weighted_burden <- dplyr::select(data_weighted,
                                             data_weighted$source,
                                             data_weighted$time,
                                             data_weighted$simID,
                                             data_weighted$total_yll,
                                             data_weighted$total_yld,
                                             data_weighted$total_daly)

  data_weighted_burden_wide <- tidyr::pivot_wider(data_weighted_burden,
                                                names_from = data_weighted_burden$source,
                                                id_cols = c(data_weighted_burden$time, data_weighted_burden$simID),
                                                values_from = c(data_weighted_burden$total_yll,
                                                                data_weighted_burden$total_yld,
                                                                data_weighted_burden$total_daly))

  data_weighted_burden_wide <- data_weighted_burden_wide |>
    dplyr::mutate(data_weighted_burden_wide$diff_yll <- (data_weighted_burden_wide$total_yll_intervention - data_weighted_burden_wide$total_yll_baseline)/1000,
                  data_weighted_burden_wide$diff_yld <- (data_weighted_burden_wide$total_yld_intervention - data_weighted_burden_wide$total_yld_baseline)/1000,
                  data_weighted_burden_wide$diff_daly <- (data_weighted_burden_wide$total_daly_intervention - data_weighted_burden_wide$total_daly_baseline)/1000)

  data_weighted_burden_wide <- data_weighted_burden_wide |>
    dplyr::group_by(data_weighted_burden_wide$simID) |>
    dplyr::mutate(data_weighted_burden_wide$cumdiff_daly <- cumsum(data_weighted_burden_wide$diff_daly),
                  data_weighted_burden_wide$cumdiff_yll <- cumsum(data_weighted_burden_wide$diff_yll),
                  data_weighted_burden_wide$cumdiff_yld <- cumsum(data_weighted_burden_wide$diff_yld))

  data_weighted_burden_wide_collapse <- data_weighted_burden_wide |>
    dplyr::group_by(data_weighted_burden_wide$time) |>
    dplyr::summarise(data_weighted_burden_wide$diff_daly_mean <- mean(data_weighted_burden_wide$diff_daly),
                     data_weighted_burden_wide$diff_daly_min <- min(data_weighted_burden_wide$diff_daly),
                     data_weighted_burden_wide$diff_daly_max <- max(data_weighted_burden_wide$diff_daly),
                     data_weighted_burden_wide$diff_yll_mean <- mean(data_weighted_burden_wide$diff_yll),
                     data_weighted_burden_wide$diff_yll_min <- min(data_weighted_burden_wide$diff_yll),
                     data_weighted_burden_wide$diff_yll_max <- max(data_weighted_burden_wide$diff_yll),
                     data_weighted_burden_wide$diff_yld_mean <- mean(data_weighted_burden_wide$diff_yld),
                     data_weighted_burden_wide$diff_yld_min <- min(data_weighted_burden_wide$diff_yld),
                     data_weighted_burden_wide$diff_yld_max <- max(data_weighted_burden_wide$diff_yld),
                     data_weighted_burden_wide$cumdiff_daly_mean <- mean(data_weighted_burden_wide$cumdiff_daly),
                     data_weighted_burden_wide$cumdiff_daly_min <- min(data_weighted_burden_wide$cumdiff_daly),
                     data_weighted_burden_wide$cumdiff_daly_max <- max(data_weighted_burden_wide$cumdiff_daly),
                     data_weighted_burden_wide$cumdiff_yll_mean <- mean(data_weighted_burden_wide$cumdiff_yll),
                     data_weighted_burden_wide$cumdiff_yll_min <- min(data_weighted_burden_wide$cumdiff_yll),
                     data_weighted_burden_wide$cumdiff_yll_max <- max(data_weighted_burden_wide$cumdiff_yll),
                     data_weighted_burden_wide$cumdiff_yld_mean <- mean(data_weighted_burden_wide$cumdiff_yld),
                     data_weighted_burden_wide$cumdiff_yld_min <- min(data_weighted_burden_wide$cumdiff_yld),
                     data_weighted_burden_wide$cumdiff_yld_max <- max(data_weighted_burden_wide$cumdiff_yld))

  return(data_weighted_burden_wide_collapse)
}

#' Perform data smoothing
#'
#' This function performs data smoothing for burden of disease, when necessary. For instance, with only a few simulations, there can be positive values in difference in burden of disease.
#'
#' @param data_weighted_burden A data frame containing weighted values for burden of disease.
#' @return A data frame with spline smoothing applied for burden of disease.
#' @export
gen_data_weighted_burden_spline <- function(data_weighted_burden) {

  ## This function is data smoothing
  ## It is applied manually now in India project due to abnormal positive values in diff_daly or cumdiff_daly

  ## Only keep those 0 or negative values

  ## Notes for India project: Delete years 27,30,32-33 for ps3-low; Delete years 2028 for ps4-low

  data_weighted_burden_mean <- data_weighted_burden |>
    dplyr::filter(data_weighted_burden$cumdiff_daly_mean <= 0)

  data_weighted_burden_min <- data_weighted_burden |>
    dplyr::filter(data_weighted_burden$cumdiff_daly_min <= 0)

  ## Notes for India project: Delete years 29, 31 for ps2-high; Delete 37-38 for ps3-low; Delete 33-34 for ps4-middle; Delete 36-38 for ps4-low

  data_weighted_burden_max <- data_weighted_burden |>
    dplyr::filter(data_weighted_burden$cumdiff_daly_max <= 0)

  ## New data frame
  data_weighted_burden_spline <- data.frame(time = seq(min(data_weighted_burden$time),
                                                       max(data_weighted_burden$time),
                                                       length.out = 34))

  ## Fit spline and predict
  spline_fit <- splines::interpSpline(data_weighted_burden_mean$time, data_weighted_burden_mean$cumdiff_daly_mean)
  data_weighted_burden_spline$cumdiff_daly_mean <- stats::predict(spline_fit, data_weighted_burden_spline$time)$y

  spline_fit_min <- splines::interpSpline(data_weighted_burden_min$time, data_weighted_burden_min$cumdiff_daly_min)
  data_weighted_burden_spline$cumdiff_daly_min <- stats::predict(spline_fit_min, data_weighted_burden_spline$time)$y

  ## Use smooth.spline for ps4-low
  spline_fit_max <- splines::interpSpline(data_weighted_burden_max$time, data_weighted_burden_max$cumdiff_daly_max)
  data_weighted_burden_spline$cumdiff_daly_max <- stats::predict(spline_fit_max, data_weighted_burden_spline$time)$y

  ## Keep 0 values in the first two years, before policy implementation
  data_weighted_burden_spline$cumdiff_daly_mean <- ifelse(data_weighted_burden_spline$time<2024, 0, data_weighted_burden_spline$cumdiff_daly_mean)
  data_weighted_burden_spline$cumdiff_daly_min <- ifelse(data_weighted_burden_spline$time<2024, 0, data_weighted_burden_spline$cumdiff_daly_min)
  data_weighted_burden_spline$cumdiff_daly_max <- ifelse(data_weighted_burden_spline$time<2024, 0, data_weighted_burden_spline$cumdiff_daly_max)

  return(data_weighted_burden_spline)
}

#' Calculate Life Expectancy
#'
#' This function calculates life expectancy for various age and groups.
#'
#' @param data_mean A data frame containing mean values for various metrics.
#' @return A data frame with life expectancy.
#' @export
gen_data_le <- function(data_mean) {
  data_le <- data_mean[,c(data_mean$source,
                          data_mean$time,
                          data_mean$gender,
                          data_mean$age,
                          data_mean$count,
                          data_mean$deaths,
                          data_mean$migrations)]
  data_le$timediff <- data_le$time - 2023
  # calculate the population considering both dead and alive
  # count is survivors, deaths = dead pop/count
  data_le$count_both <- data_le$count*data_le$deaths+data_le$count
  # calculate life expectancy px at each age
  data_le <- data_le[order(data_le$source,
                           data_le$timediff,
                           -data_le$age),]
  data_le <- data_le |>
    dplyr::group_by(data_le$source,
                    data_le$timediff) |>
    dplyr::mutate(tx = cumsum(data_le$count),
                  px = data_le$tx/data_le$count)
  ## calculate period life expectancy as weighted sum of life expectancy at each age ##
  data_ple <- data_le |>
    dplyr::group_by(data_le$source,
                    data_le$timediff) |>
    dplyr::summarise(ple = stats::weighted.mean(data_le$px,data_le$count_both))

  ## calculate difference between baseline and intervention ##
  data_ple_wide <- tidyr::pivot_wider(data_ple,
                               names_from=data_ple$source,
                               id_cols = data_ple$timediff,
                               values_from = c(data_ple$ple))
  data_ple_wide$diff <- data_ple_wide$intervention - data_ple_wide$baseline

  return(data_ple_wide)
}
