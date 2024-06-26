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
              daly = mean(data$daly, na.rm = TRUE)) |>
    dplyr::mutate(sodium_ci_low = stats::t.test(data$sodium)$conf.int[1],
           sodium_ci_high = stats::t.test(data$sodium)$conf.int[2])

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
              total_daly = sum(data_mean$daly * data_mean$count)) |>
    dplyr::mutate(timediff = data_mean$time - 2023)

  return(data_mean_weighted)
}
