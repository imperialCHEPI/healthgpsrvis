#' Generate Mean Values for Various Metrics
#'
#' This function calculates mean values for various metrics grouped by source, time, gender, and age.
#'
#' @param data A data frame containing the raw data.
#' @return A data frame with mean values for various metrics.
#' @export
gen_data_mean <- function(data) {
  data_mean <- data |>
    dplyr::group_by(source, time, gender, age) |>
    dplyr::summarise(count = mean(count),
              income = mean(income),
              sector = mean(sector),
              carbohydrate = mean(carbohydrate, na.rm = TRUE),
              fat = mean(fat, na.rm = TRUE),
              protein = mean(protein, na.rm = TRUE),
              sodium = mean(sodium, na.rm = TRUE),
              energyintake = mean(energyintake, na.rm = TRUE),
              physicalactivity = mean(physicalactivity, na.rm = TRUE),
              bmi = mean(bmi, na.rm = TRUE),
              height = mean(height, na.rm = TRUE),
              weight = mean(weight, na.rm = TRUE),
              prevalence_ischemicheartdisease = mean(prevalence_ischemicheartdisease, na.rm = TRUE),
              incidence_ischemicheartdisease = mean(incidence_ischemicheartdisease, na.rm = TRUE),
              prevalence_diabetes = mean(prevalence_diabetes, na.rm = TRUE),
              incidence_diabetes = mean(incidence_diabetes, na.rm = TRUE),
              prevalence_intracerebralhemorrhage = mean(prevalence_intracerebralhemorrhage, na.rm = TRUE),
              incidence_intracerebralhemorrhage = mean(incidence_intracerebralhemorrhage, na.rm = TRUE),
              prevalence_ischemicstroke = mean(prevalence_ischemicstroke, na.rm = TRUE),
              incidence_ischemicstroke = mean(incidence_ischemicstroke, na.rm = TRUE),
              prevalence_asthma = mean(prevalence_asthma, na.rm = TRUE),
              incidence_asthma = mean(incidence_asthma, na.rm = TRUE),
              #prevalence_stomachcancer = mean(prevalence_stomachcancer, na.rm = TRUE),
              #incidence_stomachcancer = mean(incidence_stomachcancer, na.rm = TRUE),
              prevalence_subarachnoidhemorrhage = mean(prevalence_subarachnoidhemorrhage, na.rm = TRUE),
              incidence_subarachnoidhemorrhage = mean(incidence_subarachnoidhemorrhage, na.rm = TRUE),
              prevalence_stroke = prevalence_intracerebralhemorrhage + prevalence_ischemicstroke + prevalence_subarachnoidhemorrhage,
              incidence_stroke = incidence_intracerebralhemorrhage + incidence_ischemicstroke + incidence_subarachnoidhemorrhage,
              prevalence_ckd = mean(prevalence_chronickidneydisease, na.rm = TRUE),
              incidence_ckd = mean(incidence_chronickidneydisease, na.rm = TRUE),
              disability_weight = mean(disability_weight, na.rm = TRUE),
              deaths = mean(deaths, na.rm = TRUE),
              migrations = mean(migrations, na.rm = TRUE),
              normal_weight = mean(normal_weight, na.rm = TRUE),
              over_weight = mean(over_weight, na.rm = TRUE),
              obese_weight = mean(obese_weight, na.rm = TRUE),
              above_weight = mean(above_weight, na.rm = TRUE),
              yll = mean(yll, na.rm = TRUE),
              yld = mean(yld, na.rm = TRUE),
              daly = mean(daly, na.rm = TRUE)) |>
    dplyr::mutate(sodium_ci_low = stats::t.test(sodium)$conf.int[1],
           sodium_ci_high = stats::t.test(sodium)$conf.int[2])

  return(data_mean)
}
