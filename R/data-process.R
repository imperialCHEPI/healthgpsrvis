#' Generate Mean Values for Various Metrics
#'
#' This function calculates mean values for various metrics grouped by source, time, gender, and age.
#'
#' @param data A data frame containing the raw data.
#' @return A data frame with mean values for various metrics.
#' @export
gen_data_mean <- function(data) {
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
