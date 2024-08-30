# Testing riskfactors() function
test_that("riskfactors function works correctly", {
  # Create sample data
  data_mean_weighted <- data.frame(
    time = rep(seq(2020, 2055, by = 5), 3),
    weighted_bmi = runif(24, 25, 38),
    weighted_energyintake = runif(24, 1700, 2750),
    weighted_fat = runif(24, 38, 120),
    weighted_obesity = runif(24, 0.1, 0.7),
    weighted_protein = runif(24, 46, 210),
    weighted_sodium = runif(24, 874, 2768),
    source = rep(c("Source_1", "Source_2", "Source_3"), each = 8)
  )

  # Test for valid input
  plot_bmi <- riskfactors("bmi", data_mean_weighted)
  expect_s3_class(plot_bmi, "ggplot")
  expect_equal(plot_bmi$labels$title, "BMI")
  expect_equal(plot_bmi$labels$y, "BMI (weighted)")

  plot_ei <- riskfactors("ei", data_mean_weighted)
  expect_s3_class(plot_ei, "ggplot")
  expect_equal(plot_ei$labels$title, "EI")
  expect_equal(plot_ei$labels$y, "Energy intake (weighted)")

  plot_sodium <- riskfactors("sodium", data_mean_weighted)
  expect_s3_class(plot_sodium, "ggplot")
  expect_equal(plot_sodium$labels$title, "SODIUM")
  expect_equal(plot_sodium$labels$y, "Sodium (weighted)")

  # Test for invalid input
  expect_error(riskfactors("invalid_riskft", data_mean_weighted),
               "Invalid risk factor. Choose from: 'bmi', 'ei', 'fat', 'obese', 'protein', 'sodium'.")
})

# Testing riskfactors_diff() function
test_that("riskfactors_diff function works correctly", {
  # Create sample data
  data_weighted_rf_wide_collapse <- data.frame(
    time = seq(-9, 21, by = 1),
    income = c(rep("low",9), rep("middle", 16), rep("high", 6)),
    diff_bmi_mean = runif(31, -3, 0),
    diff_bmi_max = runif(31, -5, 0),
    diff_bmi_min = runif(31, -1, 0),
    diff_ei_mean = runif(31, -200, 0),
    diff_ei_max = runif(31, -300, 0),
    diff_ei_min = runif(31, -100, 0),
    diff_obesity_mean = runif(31, -0.3, 0),
    diff_obesity_max = runif(31, -0.5, 0),
    diff_obesity_min = runif(31, -0.1, 0),
    diff_sodium_mean = runif(31, -700, 0),
    diff_sodium_max = runif(31, -1000, 0),
    diff_sodium_min = runif(31, -300, 0)
  )

  # Test for valid input
  plot_bmi <- riskfactors_diff("bmi", data_weighted_rf_wide_collapse, scale_y_continuous_limits = c(-0.43,0), scale_y_continuous_breaks = c(-0.43,-0.21,0), scale_y_continuous_labels = c(-0.43,-0.21,0))
  expect_s3_class(plot_bmi, "ggplot")
  expect_equal(plot_bmi$labels$title, "Reduction in BMI by income class")
  expect_equal(plot_bmi$labels$y, "BMI")

  plot_ei <- riskfactors_diff("ei", data_weighted_rf_wide_collapse, scale_y_continuous_limits = c(-38.3,0), scale_y_continuous_breaks = c(-38.3,-19.2,0), scale_y_continuous_labels = c(-38.3,-19.2,0))
  expect_s3_class(plot_ei, "ggplot")
  expect_equal(plot_ei$labels$title, "Reduction in energy intake (kcal) by income class")
  expect_equal(plot_ei$labels$y, "Energy")

  plot_obesity <- riskfactors_diff("obesity", data_weighted_rf_wide_collapse, scale_y_continuous_limits = c(-0.0135,0), scale_y_continuous_breaks = c(-0.0135,-0.00675,0), scale_y_continuous_labels = c("-1.35%","-0.675%","0"))
  expect_s3_class(plot_obesity, "ggplot")
  expect_equal(plot_obesity$labels$title, "Reduction in obesity prevalence by income class")
  expect_equal(plot_obesity$labels$y, "Obesity")

  plot_sodium <- riskfactors_diff("sodium", data_weighted_rf_wide_collapse, scale_y_continuous_limits = c(-64.6,0), scale_y_continuous_breaks = c(-64.6,-32.3,0), scale_y_continuous_labels = c(-64.6,-32.3,0))
  expect_s3_class(plot_sodium, "ggplot")
  expect_equal(plot_sodium$labels$title, "Reduction in sodium (mg) by income class")
  expect_equal(plot_sodium$labels$y, "Sodium")

  # Test for invalid input
  expect_error(riskfactors_diff("invalid_riskft_diff", data_weighted_rf_wide_collapse),
               "Invalid risk factor difference. Choose from: 'bmi', 'ei', 'obesity', 'sodium'.")
})

# Testing inc_diff() function
test_that("inc_diff function works correctly", {
  # Create sample data
  data_mean_weighted_inc_wide <- data.frame(
    timediff = seq(-9, 21, by = 1),
    diff_asthma = runif(31, -3, 0),
    diff_ckd = runif(31, -200, 0),
    diff_diabetes = runif(31, -0.3, 0),
    diff_ischemia = runif(31, -700, 0),
    diff_stroke = runif(31, -79, 0)
  )

  # Test for valid input
  plot_ckd <- inc_diff("ckd", data_mean_weighted_inc_wide)
  expect_s3_class(plot_ckd, "ggplot")
  expect_equal(plot_ckd$labels$title, "Chronic kidney disease - Reduction in incidence number")
  expect_equal(plot_ckd$labels$y, "CKD incidence")

  plot_ihd <- inc_diff("ischemia", data_mean_weighted_inc_wide)
  expect_s3_class(plot_ihd, "ggplot")
  expect_equal(plot_ihd$labels$title, "Ischemic heart disease - Reduction in incidence number")
  expect_equal(plot_ihd$labels$y, "Ischemia incidence")

  plot_stroke <- inc_diff("stroke", data_mean_weighted_inc_wide)
  expect_s3_class(plot_stroke, "ggplot")
  expect_equal(plot_stroke$labels$title, "Stroke - Reduction in incidence number")
  expect_equal(plot_stroke$labels$y, "Stroke incidence")

  # Test for invalid input
  expect_error(inc_diff("invalid_inc", data_mean_weighted_inc_wide),
               "Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'.")
})

# Testing inc_cum() function
test_that("inc_cum function works correctly", {
  # Create sample data
  data_mean_weighted_inc_wide <- data.frame(
    timediff = seq(-9, 21, by = 1),
    cumdiff_asthma = runif(31, -3, 0),
    cumdiff_ckd = runif(31, -200, 0),
    cumdiff_diabetes = runif(31, -0.3, 0),
    cumdiff_ihd = runif(31, -700, 0),
    cumdiff_stroke = runif(31, -79, 0)
  )

  # Test for valid input
  plot_ckd <- inc_cum("ckd", data_mean_weighted_inc_wide)
  expect_s3_class(plot_ckd, "ggplot")
  expect_equal(plot_ckd$labels$title, "Chronic kidney disease - Cumulative reduction in incidence number")
  expect_equal(plot_ckd$labels$y, "CKD incidence")

  plot_ihd <- inc_cum("ischemia", data_mean_weighted_inc_wide)
  expect_s3_class(plot_ihd, "ggplot")
  expect_equal(plot_ihd$labels$title, "Ischemic heart disease - Cumulative reduction in incidence number")
  expect_equal(plot_ihd$labels$y, "Ischemia incidence")

  plot_stroke <- inc_cum("stroke", data_mean_weighted_inc_wide)
  expect_s3_class(plot_stroke, "ggplot")
  expect_equal(plot_stroke$labels$title, "Stroke - Cumulative reduction in incidence number")
  expect_equal(plot_stroke$labels$y, "Stroke incidence")

  # Test for invalid input
  expect_error(inc_cum("invalid_inc_cum", data_mean_weighted_inc_wide),
               "Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'.")
})

# Testing burden_disease() function
test_that("burden_disease function works correctly", {
  # Create sample data
  data_mean_weighted_burden_wide <- data.frame(
    timediff = seq(-9, 21, by = 1),
    diff_daly = runif(31, -3, 0),
    cumdiff_daly = runif(31, -200, 0),
    diff_yld = runif(31, -0.3, 0),
    diff_yll = runif(31, -700, 0)
  )

  # Test for valid input
  plot_daly <- burden_disease("daly", data_mean_weighted_burden_wide)
  expect_s3_class(plot_daly, "ggplot")
  expect_equal(plot_daly$labels$title, "Reduction in DALY")
  expect_equal(plot_daly$labels$y, "DALY")

  plot_yll <- burden_disease("yll", data_mean_weighted_burden_wide)
  expect_s3_class(plot_yll, "ggplot")
  expect_equal(plot_yll$labels$title, "Reduction in YLL")
  expect_equal(plot_yll$labels$y, "YLL")

  plot_dalycum <- burden_disease("dalycum", data_mean_weighted_burden_wide)
  expect_s3_class(plot_dalycum, "ggplot")
  expect_equal(plot_dalycum$labels$title, "Cumulative reduction in DALY under intervention")
  expect_equal(plot_dalycum$labels$y, "DALY")

  # Test for invalid input
  expect_error(burden_disease("invalid_burden_disease", data_mean_weighted_burden_wide),
               "Invalid burden of disease. Choose from: 'daly', 'dalycum', 'yld', 'yll'.")
})

# Testing life_exp() function
test_that("life_exp function works correctly", {
  # Create sample data
  data_ple_wide <- data.frame(
    timediff = seq(-9, 21, by = 1),
    diff = runif(31, -3, 0)
  )

  # Test for valid input
  plot_le <- life_exp(data_ple_wide$diff, data_ple_wide)
  expect_s3_class(plot_le, "ggplot")
  expect_equal(plot_le$labels$title, "Increase in life expectancy under intervention")
})

# Testing combine_plots() function
test_that("combine_plots function works correctly", {
  # Create sample metrics
  metrics <- list(
    risk_factors = c("bmi", "ei"),
    burden_disease = c("dalycum", "yld"))

  # Create sample data
  data_mean_weighted <- data.frame(
    time = rep(seq(2020, 2055, by = 5), 3),
    weighted_bmi = runif(24, 25, 38),
    weighted_energyintake = runif(24, 1700, 2750),
    weighted_fat = runif(24, 38, 120),
    weighted_obesity = runif(24, 0.1, 0.7),
    weighted_protein = runif(24, 46, 210),
    weighted_sodium = runif(24, 874, 2768),
    source = rep(c("Source_1", "Source_2", "Source_3"), each = 8))

  data_mean_weighted_burden_wide <- data.frame(
    timediff = seq(-9, 21, by = 1),
    diff_daly = runif(31, -3, 0),
    cumdiff_daly = runif(31, -200, 0),
    diff_yld = runif(31, -0.3, 0),
    diff_yll = runif(31, -700, 0)
  )

  # Create output file
  output_file <- tempfile(fileext = ".pdf")

  # Create the combined plot
  plot_combine <- combine_plots(metrics=metrics,
                                data_mean_weighted = data_mean_weighted,
                                data_mean_weighted_burden_wide = data_mean_weighted_burden_wide,
                                output_file = output_file)

  # Test that the output file is created
  expect_true(file.exists(output_file))

  # Test that the output file is not empty
  expect_gt(file.size(output_file), 0)

  # Clean up
  unlink(output_file)
})
