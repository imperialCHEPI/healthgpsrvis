#' Generate Mean Values for Various Metrics
#'
#' This function calculates mean values for various metrics grouped by source, time, gender, and age.
#'
#' @param data A data frame containing the raw data.
#' @return A data frame with mean values for various metrics.
#' @export
gen_data_mean <- function(data) {
  # Show the first few rows of the data (or use utils:: somewhere else in the function to clear the R CMD check notes)
  print(utils::head(data))

  # Calculate mean values grouped by "source", "time", "gender", and "age"
  data_mean <- data |>
    dplyr::group_by(data$source, data$time, data$gender, data$age) |>
    dplyr::summarise(count = mean(data$count),
              income = mean(data$income),
              sector = mean(data$sector),
              carbohydrate = mean(data$carbohydrate, na.rm = TRUE),
              fat = mean(data$fat, na.rm = TRUE),
              protein = mean(data$protein, na.rm = TRUE),
              sodium = mean(data$sodium, na.rm = TRUE),
              energyintake = mean(data$energyintake, na.rm = TRUE),
              physicalactivity = mean(data$physicalactivity, na.rm = TRUE),
              bmi = mean(data$bmi, na.rm = TRUE),
              height = mean(data$height, na.rm = TRUE),
              weight = mean(data$weight, na.rm = TRUE),
              prevalence_ischemicheartdisease = mean(data$prevalence_ischemicheartdisease, na.rm = TRUE),
              incidence_ischemicheartdisease = mean(data$incidence_ischemicheartdisease, na.rm = TRUE),
              prevalence_diabetes = mean(data$prevalence_diabetes, na.rm = TRUE),
              incidence_diabetes = mean(data$incidence_diabetes, na.rm = TRUE),
              prevalence_intracerebralhemorrhage = mean(data$prevalence_intracerebralhemorrhage, na.rm = TRUE),
              incidence_intracerebralhemorrhage = mean(data$incidence_intracerebralhemorrhage, na.rm = TRUE),
              prevalence_ischemicstroke = mean(data$prevalence_ischemicstroke, na.rm = TRUE),
              incidence_ischemicstroke = mean(data$incidence_ischemicstroke, na.rm = TRUE),
              prevalence_asthma = mean(data$prevalence_asthma, na.rm = TRUE),
              incidence_asthma = mean(data$incidence_asthma, na.rm = TRUE),
              #prevalence_stomachcancer = mean(data$prevalence_stomachcancer, na.rm = TRUE),
              #incidence_stomachcancer = mean(data$incidence_stomachcancer, na.rm = TRUE),
              prevalence_subarachnoidhemorrhage = mean(data$prevalence_subarachnoidhemorrhage, na.rm = TRUE),
              incidence_subarachnoidhemorrhage = mean(data$incidence_subarachnoidhemorrhage, na.rm = TRUE),
              prevalence_stroke = prevalence_intracerebralhemorrhage + prevalence_ischemicstroke + prevalence_subarachnoidhemorrhage,
              incidence_stroke = incidence_intracerebralhemorrhage + incidence_ischemicstroke + incidence_subarachnoidhemorrhage,
              prevalence_ckd = mean(data$prevalence_chronickidneydisease, na.rm = TRUE),
              incidence_ckd = mean(data$incidence_chronickidneydisease, na.rm = TRUE),
              disability_weight = mean(data$disability_weight, na.rm = TRUE),
              deaths = mean(data$deaths, na.rm = TRUE),
              migrations = mean(data$migrations, na.rm = TRUE),
              normal_weight = mean(data$normal_weight, na.rm = TRUE),
              over_weight = mean(data$over_weight, na.rm = TRUE),
              obese_weight = mean(data$obese_weight, na.rm = TRUE),
              above_weight = mean(data$above_weight, na.rm = TRUE),
              yll = mean(data$yll, na.rm = TRUE),
              yld = mean(data$yld, na.rm = TRUE),
              daly = mean(data$daly, na.rm = TRUE))

  return(data_mean)
}

# Define global variables to suppress notes
utils::globalVariables(c("incidence_intracerebralhemorrhage", "incidence_ischemicstroke",
"incidence_subarachnoidhemorrhage", "prevalence_intracerebralhemorrhage",
"prevalence_ischemicstroke", "prevalence_subarachnoidhemorrhage"))

#' Generate Weighted Mean Values Over Years
#'
#' This function calculates weighted mean values for various metrics over years.
#'
#' @param data A data frame containing values for various metrics.
#' @return A data frame with weighted values for various metrics over years.
#' @export
gen_data_weighted <- function(data) {
  data_weighted <- data |>
    dplyr::group_by(data$source,
                    data$time,
                    data$simID) |>
    dplyr::mutate(data$prevalence_stroke <- data$prevalence_intracerebralhemorrhage +
                                           data$prevalence_ischemicstroke +
                                           data$prevalence_subarachnoidhemorrhage,
                  data$incidence_stroke <- data$incidence_intracerebralhemorrhage +
                                          data$incidence_ischemicstroke +
                                          data$incidence_subarachnoidhemorrhage) |>
    dplyr::summarise(weighted_income = stats::weighted.mean(data$income, data$count),
              weighted_sector = stats::weighted.mean(data$sector, data$count),
              weighted_sodium = stats::weighted.mean(data$sodium, data$count, na.rm = TRUE),
              weighted_carbohydarte = stats::weighted.mean(data$carbohydrate, data$count, na.rm = TRUE),
              weighted_fat = stats::weighted.mean(data$fat, data$count, na.rm = TRUE),
              weighted_protein = stats::weighted.mean(data$protein, data$count, na.rm = TRUE),
              weighted_energyintake = stats::weighted.mean(data$energyintake, data$count, na.rm = TRUE),
              weighted_physicalactivity = stats::weighted.mean(data$physicalactivity, data$count),
              weighted_bmi = stats::weighted.mean(data$bmi, data$count, na.rm = TRUE),
              weighted_height = stats::weighted.mean(data$height, data$count),
              weighted_weight = stats::weighted.mean(data$weight, data$count, na.rm = TRUE),
              weighted_overweight = stats::weighted.mean(data$over_weight, data$count),
              weighted_obesity = stats::weighted.mean(data$obese_weight, data$count),
              wprev_ihd = stats::weighted.mean(data$prevalence_ischemicheartdisease, data$count, na.rm = TRUE),
              wprev_diabetes = stats::weighted.mean(data$prevalence_diabetes, data$count, na.rm = TRUE),
              wprev_stroke = stats::weighted.mean(data$prevalence_stroke, data$count, na.rm = TRUE),
              wprev_asthma = stats::weighted.mean(data$prevalence_asthma, data$count, na.rm = TRUE),
              wprev_ckd = stats::weighted.mean(data$prevalence_chronickidneydisease, data$count, na.rm = TRUE),
              prevcase_ihd = sum(data$prevalence_ischemicheartdisease * data$count, na.rm = TRUE),
              prevcase_diabetes = sum(data$prevalence_diabetes * data$count, na.rm = TRUE),
              prevcase_stroke = sum(data$prevalence_stroke * data$count, na.rm = TRUE),
              prevcase_asthma = sum(data$prevalence_asthma * data$count, na.rm = TRUE),
              prevcase_ckd = sum(data$prevalence_chronickidneydisease * data$count, na.rm = TRUE),
              totalcase_ihd = sum(data$incidence_ischemicheartdisease * data$count, na.rm = TRUE),
              totalcase_diabetes = sum(data$incidence_diabetes * data$count, na.rm = TRUE),
              totalcase_stroke = sum(data$incidence_stroke * data$count, na.rm = TRUE),
              totalcase_asthma = sum(data$incidence_asthma * data$count, na.rm = TRUE),
              totalcase_ckd = sum(data$incidence_chronickidneydisease * data$count, na.rm = TRUE),
              weighted_disabilityweight = stats::weighted.mean(data$disability_weight, data$count),
              weighted_death = stats::weighted.mean(data$deaths, data$count),
              weighted_migrations = stats::weighted.mean(data$migrations, data$count),
              total_yll = sum(data$yll * data$count, na.rm = TRUE),
              total_yld = sum(data$yld * data$count, na.rm = TRUE),
              total_daly = sum(data$daly * data$count, na.rm = TRUE))

  return(data_weighted)
}

#' Calculate Differences for Various Metrics
#'
#' This function calculates the differences between intervention and baseline values for various metrics.
#'
#' @param data_weighted A data frame containing weighted mean values for various metrics.
#' @return A data frame with differences between intervention and baseline values for various metrics.
#' @export
gen_data_weighted_rf <- function(data_weighted) {
  data_weighted_rf <- dplyr::select(data_weighted,
                                         data_weighted$source,
                                         data_weighted$time,
                                         data_weighted$simID,
                                         data_weighted$weighted_sodium,
                                         data_weighted$weighted_energyintake,
                                         data_weighted$weighted_bmi,
                                         data_weighted$weighted_obesity)

  data_weighted_rf_wide <- tidyr::pivot_wider(data_weighted_rf,
                                            names_from = data_weighted_rf$source,
                                            id_cols = c(data_weighted_rf$time, data_weighted_rf$simID),
                                            values_from = c(data_weighted_rf$weighted_sodium,
                                                            data_weighted_rf$weighted_energyintake,
                                                            data_weighted_rf$weighted_bmi,
                                                            data_weighted_rf$weighted_obesity))
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
    dplyr::summarise(data_weighted_ds_wide$diff_inc_ihd_mean <- mean(data_weighted_ds_wide$cumdiff_inc_ihd),
                     data_weighted_ds_wide$diff_inc_ihd_min <- min(data_weighted_ds_wide$cumdiff_inc_ihd),
                     data_weighted_ds_wide$diff_inc_ihd_max <- max(data_weighted_ds_wide$cumdiff_inc_ihd),
                     data_weighted_ds_wide$diff_inc_db_mean <- mean(data_weighted_ds_wide$cumdiff_inc_db),
                     data_weighted_ds_wide$diff_inc_db_min <- min(data_weighted_ds_wide$cumdiff_inc_db),
                     data_weighted_ds_wide$diff_inc_db_max <- max(data_weighted_ds_wide$cumdiff_inc_db),
                     data_weighted_ds_wide$diff_inc_stroke_mean <- mean(data_weighted_ds_wide$cumdiff_inc_stroke),
                     data_weighted_ds_wide$diff_inc_stroke_min <- min(data_weighted_ds_wide$cumdiff_inc_stroke),
                     data_weighted_ds_wide$diff_inc_stroke_max <- max(data_weighted_ds_wide$cumdiff_inc_stroke),
                     data_weighted_ds_wide$diff_inc_asthma_mean <- mean(data_weighted_ds_wide$cumdiff_inc_asthma),
                     data_weighted_ds_wide$diff_inc_asthma_min <- min(data_weighted_ds_wide$cumdiff_inc_asthma),
                     data_weighted_ds_wide$diff_inc_asthma_max <- max(data_weighted_ds_wide$cumdiff_inc_asthma),
                     data_weighted_ds_wide$diff_inc_ckd_mean <- mean(data_weighted_ds_wide$cumdiff_inc_ckd),
                     data_weighted_ds_wide$diff_inc_ckd_min <- min(data_weighted_ds_wide$cumdiff_inc_ckd),
                     data_weighted_ds_wide$diff_inc_ckd_max <- max(data_weighted_ds_wide$cumdiff_inc_ckd))

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

  ## calculate difference btw baseline and intervention ##
  data_ple_wide <- tidyr::pivot_wider(data_ple,
                               names_from=data_ple$source,
                               id_cols = data_ple$timediff,
                               values_from = c(data_ple$ple))
  data_ple_wide$diff <- data_ple_wide$intervention - data_ple_wide$baseline

  return(data_ple_wide)
}
