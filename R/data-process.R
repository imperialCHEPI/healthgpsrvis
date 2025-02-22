#' Data Processing
#'
#' This file contains a set of functions designed to work together for
#' processing the data. Below is a description of how to use these functions
#' in sequence.
#'
#' ## Step-by-Step Usage:
#'
#' 1. **Read the data**: This function reads the data from the location
#' specified `data <-  readRDS("data.rds")`.
#'
#' 1. **`gen_data_weighted`**: Calculates weighted mean values for various
#' metrics over years `data_weighted <- gen_data_weighted(data)`.
#'
#' 1. **`gen_data_weighted_rf`**: Calculates the differences between
#' intervention and baseline values for risk factors
#' `data_weighted_rf_wide_collapse <- gen_data_weighted_rf(data_weighted)`.
#'
#' 1. **`gen_data_weighted_ds`**: Calculates the differences between
#' intervention and baseline values for incidences
#' `data_weighted_ds_wide_collapse <- gen_data_weighted_ds(data_weighted)`.
#'
#' 1. **`gen_data_weighted_burden`**: Calculates the differences between
#' intervention and baseline values for burden of disease
#' `data_weighted_bd_wide_collapse <- gen_data_weighted_burden(data_weighted)`.
#'
#' 1. **`gen_data_weighted_bd_spline`**: Performs data smoothing for burden
#' of disease, when necessary. For instance, with only a few simulations, there
#' can be positive values in difference in burden of disease
#' `data_weighted_burden_spline <- gen_data_weighted_bd_spline(data_weighted_bd_wide_collapse)`.
#'
#'
#' ## Examples
#' ```r
#' # Example of using all functions together
#' data <- readRDS("data.rds")
#' data_weighted <- gen_data_weighted(data)
#' data_weighted_rf_wide_collapse <- gen_data_weighted_rf(data_weighted)
#' data_weighted_ds_wide_collapse <- gen_data_weighted_ds(data_weighted)
#' data_weighted_bd_wide_collapse <- gen_data_weighted_burden(data_weighted)
#' data_weighted_burden_spline <- gen_data_weighted_bd_spline(
#' sdata_weighted_bd_wide_collapse)
#' ```
#'
#' @name DataProcessing
NULL

#' Generate Weighted Mean Values Over Years
#'
#' This function calculates weighted mean values for various metrics over years.
#'
#' @param data A data frame containing values for various metrics.
#' @param configname The name of the configuration file to use (e.g., "default", "development", "production", "testing").
#' @return A data frame with weighted values for various metrics over years.
#' @export
gen_data_weighted <- function(data, configname = "default") {

  print("Loading the config file...")
  config <- load_config(configname)
  print("Processing the data...")
  colnames(data) <- gsub("^mean_", "", colnames(data)) # Clean the column names
  ## by removing 'mean_'
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
  print("Data processing complete.")
  return(data_weighted)
}

#' Calculate Differences for Risk Factors
#'
#' This function calculates the differences between intervention and baseline
#' values for risk factors.
#'
#' @param data_weighted A data frame containing weighted mean values for various
#'  metrics.
#' @param configname The name of the configuration file to use (e.g., "default", "development", "production", "testing").
#' @return A data frame with differences between intervention and baseline
#' values for risk factors.
#' @export
gen_data_weighted_rf <- function(data_weighted, configname = "default") {
  print("Loading the config file...")
  config <- load_config(configname)
  print("Processing the data...")
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
          data_weighted_rf_wide[[paste0("weighted_", rf, "_intervention")]] -
            data_weighted_rf_wide[[paste0("weighted_", rf, "_baseline")]]
        }),
        paste0("diff_", config$rf)
      )
    )

  if ("diff_energyintake" %in% colnames(data_weighted_rf_wide)) {
    data_weighted_rf_wide <- data_weighted_rf_wide |>
      dplyr::rename(
        diff_ei = diff_energyintake
      )
  }

  data_weighted_rf_wide_collapse <- data_weighted_rf_wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(config$group))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_rf),
                                ~ mean(.x, na.rm = TRUE),
                                .names = "{.col}_mean")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_rf),
                                ~ t.test(.x)$conf.int[1],
                                .names = "{.col}_ci_low")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_rf),
                                ~ t.test(.x)$conf.int[2],
                                .names = "{.col}_ci_high"))
  print("Data processing complete.")
  return(data_weighted_rf_wide_collapse)
}

#' Calculate Differences for Incidences
#'
#' This function calculates the differences between intervention and baseline
#' values for incidences.
#'
#' @param data_weighted A data frame containing weighted mean values for various
#'  metrics.
#' @param configname The name of the configuration file to use (e.g., "default", "development", "production", "testing").
#' @return A data frame with differences between intervention and baseline
#' values for incidences.
#' @export
gen_data_weighted_ds_diff <- function(data_weighted, configname = "default") {
  print("Loading the config file...")
  config <- load_config(configname)
  print("Processing the data...")
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

  data_weighted_ds_wide_diff <- data_weighted_ds_wide |>
    dplyr::mutate(
      !!!stats::setNames(
        lapply(config$disease, function(ds) {
          100 * (data_weighted_ds_wide[[paste0("totalcase_",
                                               ds,
                                               "_intervention")]] -
                   data_weighted_ds_wide[[paste0("totalcase_",
                                                 ds,
                                                 "_baseline")]])
        }),
        paste0("diff_inc_", config$disease)
      )
    )

  if ("diff_inc_diabetes" %in% colnames(data_weighted_ds_wide_diff)) {
    data_weighted_ds_wide_diff <- data_weighted_ds_wide_diff |>
      dplyr::rename(diff_inc_db = diff_inc_diabetes)
  }
  print("Data processing complete.")
  return(data_weighted_ds_wide_diff)
}

#' Calculate Cumulative Differences for Incidences
#'
#' This function calculates the cumulative differences between intervention and
#' baseline values for incidences.
#'
#' @param data_weighted A data frame containing weighted mean values for various
#'  metrics.
#' @param configname The name of the configuration file to use (e.g., "default", "development", "production", "testing").
#' @return A data frame with differences between intervention and baseline
#' values for incidences.
#' @export
gen_data_weighted_ds_cumdiff <- function(data_weighted, configname = "default") {
  print("Loading the config file...")
  config <- load_config(configname)
  print("Processing the data...")
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
          100 * (data_weighted_ds_wide[[paste0("totalcase_",
                                               ds,
                                               "_intervention")]] -
                   data_weighted_ds_wide[[paste0("totalcase_",
                                                 ds,
                                                 "_baseline")]])
        }),
        paste0("diff_inc_", config$disease)
      )
    )

  if ("diff_inc_diabetes" %in% colnames(data_weighted_ds_wide)) {
    data_weighted_ds_wide <- data_weighted_ds_wide |>
      dplyr::rename(diff_inc_db = diff_inc_diabetes)
  }

  data_weighted_ds_wide <- data_weighted_ds_wide |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(config$group_ds)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(config$summary_columns_ds),
        cumsum,
        .names = "cum{.col}"
      )
    )

  data_weighted_ds_wide_collapse <- data_weighted_ds_wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(config$group))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_ds_cum),
                                ~ mean(.x, na.rm = TRUE),
                                .names = "{.col}_mean")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_ds_cum),
                                ~ t.test(.x)$conf.int[1],
                                .names = "{.col}_ci_low")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_ds_cum),
                                ~ t.test(.x)$conf.int[2],
                                .names = "{.col}_ci_high"))
  print("Data processing complete.")
  return(data_weighted_ds_wide_collapse)
}

#' Calculate Differences for Burden of Disease
#'
#' This function calculates the differences between intervention and baseline
#' values for burden of disease.
#'
#' @param data_weighted A data frame containing weighted mean values for various
#'  metrics.
#' @param configname The name of the configuration file to use (e.g., "default", "development", "production", "testing").
#' @return A data frame with differences between intervention and baseline
#' values for burden of disease.
#' @export
gen_data_weighted_burden <- function(data_weighted, configname = "default") {
  print("Loading the config file...")
  config <- load_config(configname)
  print("Processing the data...")
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

  data_weighted_burden_wide <- data_weighted_burden_wide |>
    dplyr::mutate(
      !!!stats::setNames(
        lapply(config$burden, function(value) {
          (data_weighted_burden_wide[[paste0("total_",
                                             value,
                                             "_intervention")]] -
             data_weighted_burden_wide[[paste0("total_",
                                               value,
                                               "_baseline")]]) / 1000
        }),
        paste0("diff_", config$burden)
      )
    )

  data_weighted_burden_wide <- data_weighted_burden_wide |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(config$group_ds)
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(config$summary_columns_burden),
        cumsum,
        .names = "cum{.col}"
      )
    )

  data_weighted_bd_wide_collapse <- data_weighted_burden_wide |>
    dplyr::group_by(dplyr::across(dplyr::all_of(config$group))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_burden_cum),
                                ~ mean(.x, na.rm = TRUE),
                                .names = "{.col}_mean")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_burden_cum),
                                ~ t.test(.x)$conf.int[1],
                                .names = "{.col}_ci_low")) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(config$summary_columns_burden_cum),
                                ~ t.test(.x)$conf.int[2],
                                .names = "{.col}_ci_high"))
  print("Data processing complete.")
  return(data_weighted_bd_wide_collapse)
}

#' Perform data smoothing
#'
#' This function performs data smoothing for burden of disease, when necessary.
#' For instance, with only a few simulations, there can be positive values in
#' difference in burden of disease.
#'
#' @param data_weighted_bd_wide_collapse A data frame containing weighted
#' values for burden of disease.
#' @param configname The name of the configuration file to use (e.g., "default", "development", "production", "testing").
#' @return A data frame with spline smoothing applied for burden of disease.
#' @export
gen_data_weighted_bd_spline <- function(data_weighted_bd_wide_collapse, configname = "default") {
  print("Loading the config file...")
  config <- load_config(configname)
  print("Processing the data...")
  config_file_path <- system.file("config",
                                  "config.yml",
                                  package = "healthgpsrvis")
  burden_spline <- config::get(
    value = "burden_spline",
    file = config_file_path,
    use_parent = FALSE
  )

  ## This function is data smoothing
  ## It is applied manually now in India project due to abnormal positive values
  ## in diff_daly or cumdiff_daly

  ## Only keep those 0 or negative values

  ## Notes for India project: Delete years 27,30,32-33 for ps3-low; Delete years
  ## 2028 for ps4-low

  data_weighted_burden_mean <- data_weighted_bd_wide_collapse |>
    dplyr::filter(!!rlang::sym(burden_spline[[1]]$burden_mean) <= 0)

  data_weighted_burden_ci_low <- data_weighted_bd_wide_collapse |>
    dplyr::filter(!!rlang::sym(burden_spline[[2]]$burden_ci_low) <= 0)

  ## Notes for India project: Delete years 29, 31 for ps2-high; Delete 37-38 for
  ## ps3-low; Delete 33-34 for ps4-middle; Delete 36-38 for ps4-low

  data_weighted_burden_ci_high <- data_weighted_bd_wide_collapse |>
    dplyr::filter(!!rlang::sym(burden_spline[[3]]$burden_ci_high) <= 0)

  ## New data frame
  data_weighted_burden_spline <- data.frame(time = seq(
    min(data_weighted_bd_wide_collapse[config$group]),
    max(data_weighted_bd_wide_collapse[config$group]),
    length.out = 34
  ))

  ## Fit spline and predict
  spline_fit_mean <- splines::interpSpline(
    as.numeric(unlist(data_weighted_burden_mean[config$group])),
    as.numeric(unlist(data_weighted_burden_mean[burden_spline[[1]]$burden_mean])
    )
  )
  data_weighted_burden_spline[burden_spline[[1]]$burden_mean] <- stats::predict(
    spline_fit_mean,
    as.numeric(unlist(data_weighted_burden_spline[config$group]))
  )$y

  spline_fit_ci_low <- splines::interpSpline(
    as.numeric(unlist(data_weighted_burden_ci_low[config$group])),
    as.numeric(unlist(data_weighted_burden_ci_low[burden_spline[[2]]$burden_ci_low]))
  )
  data_weighted_burden_spline[burden_spline[[2]]$burden_ci_low] <- stats::predict(
    spline_fit_ci_low,
    as.numeric(unlist(data_weighted_burden_spline[config$group]))
  )$y

  ## Use smooth.spline for ps4-low
  spline_fit_ci_high <- splines::interpSpline(
    as.numeric(unlist(data_weighted_burden_ci_high[config$group])),
    as.numeric(unlist(data_weighted_burden_ci_high[burden_spline[[3]]$burden_ci_high]))
  )
  data_weighted_burden_spline[burden_spline[[3]]$burden_ci_high] <- stats::predict(
    spline_fit_ci_high,
    as.numeric(unlist(data_weighted_burden_spline[config$group]))
  )$y

  ## Keep 0 values in the first two years, before policy implementation
  group <- config$group
  burden_spline <- unlist(config$burden_spline)

  for (burden_sp in burden_spline) {
    data_weighted_burden_spline[[burden_sp]] <- ifelse(
      data_weighted_burden_spline[[group]] < 2024,
      0,
      data_weighted_burden_spline[[burden_sp]]
    )
  }
  print(colnames(data_weighted_burden_spline))
  print("Data processing complete.")
  return(data_weighted_burden_spline)
}

# Define global variables to suppress notes
utils::globalVariables(c(
  "incidence_intracerebralhemorrhage", "incidence_ischemicstroke",
  "incidence_subarachnoidhemorrhage", "prevalence_intracerebralhemorrhage",
  "prevalence_ischemicstroke", "prevalence_subarachnoidhemorrhage",
  "diff_energyintake", "diff_inc_diabetes"
))
