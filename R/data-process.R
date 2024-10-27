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
  colnames(data) <- gsub("^mean_", "", colnames(data)) # Clean the column names by removing 'mean_'
  weight_column <- rlang::sym(config$weight)
  data_weighted <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(config$grouping_vars))) |>
    dplyr::mutate(
      prevalence_stroke = prevalence_intracerebralhemorrhage +
        prevalence_ischemicstroke +
        prevalence_subarachnoidhemorrhage,
      incidence_stroke = incidence_intracerebralhemorrhage +
        incidence_ischemicstroke +
        incidence_subarachnoidhemorrhage
    ) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(config$weighted_vars),
        ~ weighted.mean(.x, !!weight_column, na.rm = TRUE),
        .names = "weighted_{.col}"
      ),
      dplyr::across(
        dplyr::all_of(config$prevalence_disease),
        ~ weighted.mean(.x, !!weight_column, na.rm = TRUE),
        .names = "wprev_{stringr::str_sub(.col, 12)}"
      ),
      dplyr::across(
        dplyr::all_of(config$prevalence_disease),
        ~ sum(.x * !!weight_column, na.rm = TRUE),
        .names = "prevcase_{stringr::str_sub(.col, 12)}"
      ),
      dplyr::across(
        dplyr::all_of(config$incidence_disease),
        ~ sum(.x * !!weight_column, na.rm = TRUE),
        .names = "totalcase_{stringr::str_sub(.col, 11)}"
      ),
      dplyr::across(
        dplyr::all_of(config$burden),
        ~ sum(.x * !!weight_column, na.rm = TRUE),
        .names = "total_{.col}"
      )
    )

  data_weighted <- data_weighted |>
    dplyr::rename_with(
      ~ stringr::str_replace_all(.x, c(
        "deaths" = "death",
        "disability_weight" = "disabilityweight",
        "obese_weight" = "obesity",
        "over_weight" = "overweight",
        "ischemicheartdisease" = "ihd",
        "chronickidneydisease" = "ckd"
      )),
      dplyr::matches("^(weighted_|wprev_|prevcase_|totalcase_)")
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
  data_weighted_rf <- dplyr::select(
    data_weighted,
    config$names_from,
    config$id_cols,
    config$weighted_rf
  )

  data_weighted_rf_wide <- tidyr::pivot_wider(data_weighted_rf,
    names_from = config$names_from,
    id_cols = config$id_cols,
    values_from = config$weighted_rf
  )

  data_weighted_rf_wide <- data_weighted_rf_wide |>
    dplyr::mutate(
      !!!stats::setNames(
        lapply(config$rf, function(rf) {
          data_weighted_rf_wide[[paste0("weighted_", rf, "_intervention")]] - data_weighted_rf_wide[[paste0("weighted_", rf, "_baseline")]]
        }),
        paste0("diff_", config$rf)
      )
    ) |>
    dplyr::rename(diff_ei = diff_energyintake)

  data_weighted_rf_wide_collapse <- data_weighted_rf_wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(config$group))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(config$summary_columns_rf),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          min = ~ min(.x, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

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
  config <- load_config("default")
  data_weighted_ds <- dplyr::select(
    data_weighted,
    config$names_from,
    config$id_cols,
    config$weighted_ds
  )

  data_weighted_ds_wide <- tidyr::pivot_wider(data_weighted_ds,
                                              names_from = config$names_from,
                                              id_cols = config$id_cols,
                                              values_from = config$weighted_ds
  )

  data_weighted_ds_wide <- data_weighted_ds_wide |>
    dplyr::mutate(
      !!!stats::setNames(
        lapply(config$disease, function(ds) {
          100 * (data_weighted_ds_wide[[paste0("totalcase_", ds, "_intervention")]] - data_weighted_ds_wide[[paste0("totalcase_", ds, "_baseline")]])
        }),
        paste0("diff_inc_", config$disease)
      )
    ) |>
    dplyr::rename(diff_inc_db = diff_inc_diabetes)

  data_weighted_ds_wide <- data_weighted_ds_wide |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(config$group_ds))) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(config$summary_columns_ds),
        cumsum,
        .names = "cum{.col}"
        ))

  data_weighted_ds_wide_collapse <- data_weighted_ds_wide |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(config$group))) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(config$summary_columns_ds_cum),
        list(mean = ~ mean(.x, na.rm = TRUE),
             min = ~ min(.x, na.rm = TRUE),
             max = ~ max(.x, na.rm = TRUE)),
        .names = "{stringr::str_sub(.col, 4)}_{.fn}"
      ),
      .groups = "drop"
    )

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
  config <- load_config("default")
  data_weighted_burden <- dplyr::select(
    data_weighted,
    config$names_from,
    config$id_cols,
    config$weighted_burden
  )

  data_weighted_burden_wide <- tidyr::pivot_wider(data_weighted_burden,
    names_from = config$names_from,
    id_cols = config$id_cols,
    values_from = config$weighted_burden
  )

  for (burden in config$burden) {
    intervention <- paste0("total_", burden, "_intervention")
    baseline <- paste0("total_", burden, "_baseline")
    diff <- paste0("diff_", burden)
    data_weighted_burden_wide <- data_weighted_burden_wide |>
      dplyr::mutate(data_weighted_burden_wide[[diff]] <- (data_weighted_burden_wide[[intervention]] - data_weighted_burden_wide[[baseline]]) / 1000)
  }

  for (burden in config$summary_columns_burden) {
    data_weighted_burden_wide <- data_weighted_burden_wide |>
      dplyr::group_by(config$group_ds) |>
      dplyr::mutate(data_weighted_burden_wide[[paste0("cum", burden)]] <- cumsum(data_weighted_burden_wide[[burden]])) |>
      dplyr::ungroup()
  }

  data_weighted_burden_wide_collapse <- data_weighted_burden_wide |>
    dplyr::group_by(config$group) |>
    dplyr::summarise(dplyr::across(
      config$summary_columns_burden_cum,
      list(
        mean = ~ mean(.),
        min = ~ min(.),
        max = ~ max(.)
      )
    ))

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
    length.out = 34
  ))

  ## Fit spline and predict
  spline_fit <- splines::interpSpline(data_weighted_burden_mean$time, data_weighted_burden_mean$cumdiff_daly_mean)
  data_weighted_burden_spline$cumdiff_daly_mean <- stats::predict(spline_fit, data_weighted_burden_spline$time)$y

  spline_fit_min <- splines::interpSpline(data_weighted_burden_min$time, data_weighted_burden_min$cumdiff_daly_min)
  data_weighted_burden_spline$cumdiff_daly_min <- stats::predict(spline_fit_min, data_weighted_burden_spline$time)$y

  ## Use smooth.spline for ps4-low
  spline_fit_max <- splines::interpSpline(data_weighted_burden_max$time, data_weighted_burden_max$cumdiff_daly_max)
  data_weighted_burden_spline$cumdiff_daly_max <- stats::predict(spline_fit_max, data_weighted_burden_spline$time)$y

  ## Keep 0 values in the first two years, before policy implementation
  data_weighted_burden_spline$cumdiff_daly_mean <- ifelse(data_weighted_burden_spline$time < 2024, 0, data_weighted_burden_spline$cumdiff_daly_mean)
  data_weighted_burden_spline$cumdiff_daly_min <- ifelse(data_weighted_burden_spline$time < 2024, 0, data_weighted_burden_spline$cumdiff_daly_min)
  data_weighted_burden_spline$cumdiff_daly_max <- ifelse(data_weighted_burden_spline$time < 2024, 0, data_weighted_burden_spline$cumdiff_daly_max)

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
  data_le <- data_mean[, c(
    data_mean$source,
    data_mean$time,
    data_mean$gender,
    data_mean$age,
    data_mean$count,
    data_mean$deaths,
    data_mean$migrations
  )]
  data_le$timediff <- data_le$time - 2023
  # calculate the population considering both dead and alive
  # count is survivors, deaths = dead pop/count
  data_le$count_both <- data_le$count * data_le$deaths + data_le$count
  # calculate life expectancy px at each age
  data_le <- data_le[order(
    data_le$source,
    data_le$timediff,
    -data_le$age
  ), ]
  data_le <- data_le |>
    dplyr::group_by(
      data_le$source,
      data_le$timediff
    ) |>
    dplyr::mutate(
      tx = cumsum(data_le$count),
      px = data_le$tx / data_le$count
    )
  ## calculate period life expectancy as weighted sum of life expectancy at each age ##
  data_ple <- data_le |>
    dplyr::group_by(
      data_le$source,
      data_le$timediff
    ) |>
    dplyr::summarise(ple = stats::weighted.mean(data_le$px, data_le$count_both))

  ## calculate difference between baseline and intervention ##
  data_ple_wide <- tidyr::pivot_wider(data_ple,
    names_from = data_ple$source,
    id_cols = data_ple$timediff,
    values_from = c(data_ple$ple)
  )
  data_ple_wide$diff <- data_ple_wide$intervention - data_ple_wide$baseline

  return(data_ple_wide)
}

# Define global variables to suppress notes
utils::globalVariables(c(
  "incidence_intracerebralhemorrhage", "incidence_ischemicstroke",
  "incidence_subarachnoidhemorrhage", "prevalence_intracerebralhemorrhage",
  "prevalence_ischemicstroke", "prevalence_subarachnoidhemorrhage",
  "diff_energyintake", "diff_inc_diabetes"
))
