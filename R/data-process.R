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
#' @param data_mean A data frame containing mean values for various metrics.
#' @return A data frame with weighted mean values for various metrics over years.
#' @export
gen_data_mean_weighted <- function(data_mean) {
  data_mean_weighted <- data_mean |>
    dplyr::group_by(data_mean$source, data_mean$time) |>
    dplyr::summarise(weighted_income =stats::weighted.mean(data_mean$income, data_mean$count),
              weighted_sector = stats::weighted.mean(data_mean$sector, data_mean$count),
              weighted_sodium = stats::weighted.mean(data_mean$sodium, data_mean$count, na.rm = TRUE),
              weighted_sodium_ci_low = stats::weighted.mean(data_mean$sodium_ci_low, data_mean$count, na.rm = TRUE),
              weighted_sodium_ci_high = stats::weighted.mean(data_mean$sodium_ci_high, data_mean$count, na.rm = TRUE),
              weighted_carbohydarte = stats::weighted.mean(data_mean$carbohydrate, data_mean$count, na.rm = TRUE),
              weighted_fat = stats::weighted.mean(data_mean$fat, data_mean$count, na.rm = TRUE),
              weighted_protein = stats::weighted.mean(data_mean$protein, data_mean$count, na.rm = TRUE),
              weighted_energyintake = stats::weighted.mean(data_mean$energyintake, data_mean$count, na.rm = TRUE),
              weighted_physicalactivity = stats::weighted.mean(data_mean$physicalactivity, data_mean$count),
              weighted_bmi = stats::weighted.mean(data_mean$bmi, data_mean$count, na.rm = TRUE),
              weighted_height = stats::weighted.mean(data_mean$height, data_mean$count),
              weighted_weight = stats::weighted.mean(data_mean$weight, data_mean$count),
              weighted_overweight = stats::weighted.mean(data_mean$over_weight, data_mean$count),
              weighted_obesity = stats::weighted.mean(data_mean$obese_weight, data_mean$count),
              wprev_ihd = stats::weighted.mean(data_mean$prevalence_ischemicheartdisease, data_mean$count),
              wprev_diabetes = stats::weighted.mean(data_mean$prevalence_diabetes, data_mean$count),
              wprev_stroke = stats::weighted.mean(data_mean$prevalence_stroke, data_mean$count),
              wprev_asthma = stats::weighted.mean(data_mean$prevalence_asthma, data_mean$count),
              wprev_ckd = stats::weighted.mean(data_mean$prevalence_ckd, data_mean$count),
              prevcase_ihd = sum(data_mean$prevalence_ischemicheartdisease * data_mean$count),
              prevcase_diabetes = sum(data_mean$prevalence_diabetes * data_mean$count),
              prevcase_stroke = sum(data_mean$prevalence_stroke * data_mean$count),
              prevcase_asthma = sum(data_mean$prevalence_asthma * data_mean$count),
              prevcase_ckd = sum(data_mean$prevalence_ckd * data_mean$count),
              totalcase_ihd = sum(data_mean$incidence_ischemicheartdisease * data_mean$count),
              totalcase_diabetes = sum(data_mean$incidence_diabetes * data_mean$count),
              totalcase_stroke = sum(data_mean$incidence_stroke * data_mean$count),
              totalcase_asthma = sum(data_mean$incidence_asthma * data_mean$count),
              totalcase_ckd = sum(data_mean$incidence_ckd * data_mean$count),
              weighted_disabilityweight = stats::weighted.mean(data_mean$disability_weight, data_mean$count),
              weighted_death = stats::weighted.mean(data_mean$deaths, data_mean$count),
              weighted_migrations = stats::weighted.mean(data_mean$migrations, data_mean$count),
              total_yll = sum(data_mean$yll * data_mean$count),
              total_yld = sum(data_mean$yld * data_mean$count),
              total_daly = sum(data_mean$daly * data_mean$count))

  return(data_mean_weighted)
}

#' Calculate Differences for Various Metrics
#'
#' This function calculates the differences between intervention and baseline values for various metrics.
#'
#' @param data_mean_weighted A data frame containing weighted mean values for various metrics.
#' @return A data frame with differences between intervention and baseline values for various metrics.
#' @export
gen_data_mean_weighted_rf_wide <- function(data_mean_weighted) {
  data_mean_weighted_rf <- dplyr::select(data_mean_weighted,
                                         data_mean_weighted$source,
                                         data_mean_weighted$timediff,
                                         data_mean_weighted$weighted_sodium,
                                         data_mean_weighted$weighted_carbohydarte,
                                         data_mean_weighted$weighted_fat,
                                         data_mean_weighted$weighted_protein,
                                         data_mean_weighted$weighted_energyintake,
                                         data_mean_weighted$weighted_bmi,
                                         data_mean_weighted$weighted_height,
                                         data_mean_weighted$weighted_weight,
                                         data_mean_weighted$weighted_obesity)

  data_mean_weighted_rf_wide <- tidyr::pivot_wider(data_mean_weighted_rf,
                                            names_from = data_mean_weighted_rf$source,
                                            id_cols = data_mean_weighted_rf$timediff,
                                            values_from = c(data_mean_weighted_rf$weighted_sodium,
                                                            data_mean_weighted_rf$weighted_carbohydarte,
                                                            data_mean_weighted_rf$weighted_fat,
                                                            data_mean_weighted_rf$weighted_protein,
                                                            data_mean_weighted_rf$weighted_energyintake,
                                                            data_mean_weighted_rf$weighted_bmi,
                                                            data_mean_weighted_rf$weighted_height,
                                                            data_mean_weighted_rf$weighted_weight,
                                                            data_mean_weighted_rf$weighted_obesity))
  data_mean_weighted_rf_wide <- data_mean_weighted_rf_wide |>
    dplyr::mutate(data_mean_weighted_rf_wide$diff_sodium <- data_mean_weighted_rf_wide$weighted_sodium_intervention - data_mean_weighted_rf_wide$weighted_sodium_baseline,
                  data_mean_weighted_rf_wide$diff_ei <- data_mean_weighted_rf_wide$weighted_energyintake_intervention - data_mean_weighted_rf_wide$weighted_energyintake_baseline,
                  data_mean_weighted_rf_wide$diff_bmi <- data_mean_weighted_rf_wide$weighted_bmi_intervention - data_mean_weighted_rf_wide$weighted_bmi_baseline,
                  data_mean_weighted_rf_wide$diff_obesity <- data_mean_weighted_rf_wide$weighted_obesity_intervention - data_mean_weighted_rf_wide$weighted_obesity_baseline)

  return(data_mean_weighted_rf_wide)
}

#' Calculate Differences for Incidences
#'
#' This function calculates the differences between intervention and baseline values for incidences.
#'
#' @param data_mean_weighted A data frame containing weighted mean values for various metrics.
#' @return A data frame with differences between intervention and baseline values for incidences.
#' @export
gen_data_mean_weighted_inc_wide <- function(data_mean_weighted) {
  data_mean_weighted_inc <- dplyr::select(data_mean_weighted,
                                          data_mean_weighted$source,
                                          data_mean_weighted$timediff,
                                          data_mean_weighted$totalcase_ihd,
                                          data_mean_weighted$totalcase_diabetes,
                                          data_mean_weighted$totalcase_stroke,
                                          data_mean_weighted$totalcase_asthma,
                                          #data_mean_weighted$totalcase_stomachcancer,
                                          data_mean_weighted$totalcase_ckd)

  data_mean_weighted_inc_wide <- tidyr::pivot_wider(data_mean_weighted_inc,
                                             names_from = data_mean_weighted_inc$source,
                                             id_cols = data_mean_weighted_inc$timediff,
                                             values_from = c(data_mean_weighted_inc$totalcase_ihd,
                                                             data_mean_weighted_inc$totalcase_diabetes,
                                                             data_mean_weighted_inc$totalcase_stroke,
                                                             data_mean_weighted_inc$totalcase_asthma,
                                                             #data_mean_weighted_inc$totalcase_stomachcancer,
                                                             data_mean_weighted_inc$totalcase_ckd))

  data_mean_weighted_inc_wide$diff_ihd <- 100*(data_mean_weighted_inc_wide$totalcase_ihd_intervention - data_mean_weighted_inc_wide$totalcase_ihd_baseline)
  data_mean_weighted_inc_wide$diff_diabetes <- 100*(data_mean_weighted_inc_wide$totalcase_diabetes_intervention - data_mean_weighted_inc_wide$totalcase_diabetes_baseline)
  data_mean_weighted_inc_wide$diff_stroke <- 100*(data_mean_weighted_inc_wide$totalcase_stroke_intervention - data_mean_weighted_inc_wide$totalcase_stroke_baseline)
  data_mean_weighted_inc_wide$diff_asthma <- 100*(data_mean_weighted_inc_wide$totalcase_asthma_intervention - data_mean_weighted_inc_wide$totalcase_asthma_baseline)
  #data_mean_weighted_inc_wide$diff_stomachcancer <- 100*(data_mean_weighted_inc_wide$totalcase_stomachcancer_intervention - data_mean_weighted_inc_wide$totalcase_stomachcancer_baseline)
  data_mean_weighted_inc_wide$diff_ckd <- 100*(data_mean_weighted_inc_wide$totalcase_ckd_intervention - data_mean_weighted_inc_wide$totalcase_ckd_baseline)

  data_mean_weighted_inc_wide$cumdiff_ihd <- cumsum(data_mean_weighted_inc_wide$diff_ihd)
  data_mean_weighted_inc_wide$cumdiff_diabetes <- cumsum(data_mean_weighted_inc_wide$diff_diabetes)
  data_mean_weighted_inc_wide$cumdiff_stroke <- cumsum(data_mean_weighted_inc_wide$diff_stroke)
  data_mean_weighted_inc_wide$cumdiff_asthma <- cumsum(data_mean_weighted_inc_wide$diff_asthma)
  #data_mean_weighted_inc_wide$cumdiff_stomachcancer <- cumsum(data_mean_weighted_inc_wide$diff_stomachcancer)
  data_mean_weighted_inc_wide$cumdiff_ckd <- cumsum(data_mean_weighted_inc_wide$diff_ckd)

  return(data_mean_weighted_inc_wide)
}

#' Calculate Differences for Burden of Disease
#'
#' This function calculates the differences between intervention and baseline values for burden of disease.
#'
#' @param data_mean_weighted A data frame containing weighted mean values for various metrics.
#' @return A data frame with differences between intervention and baseline values for burden of disease.
#' @export
gen_data_mean_weighted_burden_wide <- function(data_mean_weighted) {
  data_mean_weighted_burden <- dplyr::select(data_mean_weighted,
                                             data_mean_weighted$source,
                                             data_mean_weighted$timediff,
                                             data_mean_weighted$total_yll,
                                             data_mean_weighted$total_yld,
                                             data_mean_weighted$total_daly)

  data_mean_weighted_burden_wide <- tidyr::pivot_wider(data_mean_weighted_burden,
                                                names_from = data_mean_weighted_burden$source,
                                                id_cols = data_mean_weighted_burden$timediff,
                                                values_from = c(data_mean_weighted_burden$total_yll,
                                                                data_mean_weighted_burden$total_yld,
                                                                data_mean_weighted_burden$total_daly))

  data_mean_weighted_burden_wide$diff_yll <- (data_mean_weighted_burden_wide$total_yll_intervention - data_mean_weighted_burden_wide$total_yll_baseline)/1000
  data_mean_weighted_burden_wide$diff_yld <- (data_mean_weighted_burden_wide$total_yld_intervention - data_mean_weighted_burden_wide$total_yld_baseline)/1000
  data_mean_weighted_burden_wide$diff_daly <- (data_mean_weighted_burden_wide$total_daly_intervention - data_mean_weighted_burden_wide$total_daly_baseline)/1000
  data_mean_weighted_burden_wide$cumdiff_daly <- cumsum(data_mean_weighted_burden_wide$diff_daly)

  return(data_mean_weighted_burden_wide)
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
