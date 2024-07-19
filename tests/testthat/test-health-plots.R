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
  data_mean_weighted_rf_wide <- data.frame(
    timediff = seq(-9, 21, by = 1),
    diff_bmi = runif(31, -3, 0),
    diff_ei = runif(31, -200, 0),
    diff_obesity = runif(31, -0.3, 0),
    diff_sodium = runif(31, -700, 0)
  )

  # Test for valid input
  plot_ei <- riskfactors_diff("ei", data_mean_weighted_rf_wide)
  expect_s3_class(plot_ei, "ggplot")
  expect_equal(plot_ei$labels$title, "Reduction in energy intake (kcal) under intervention")
  expect_equal(plot_ei$labels$y, "Energy")

  plot_obesity <- riskfactors_diff("obesity", data_mean_weighted_rf_wide)
  expect_s3_class(plot_obesity, "ggplot")
  expect_equal(plot_obesity$labels$title, "Reduction in obesity prevalence under intervention")
  expect_equal(plot_obesity$labels$y, "Obesity")

  # Test for invalid input
  expect_error(riskfactors_diff("invalid_riskft_diff", data_mean_weighted_rf_wide),
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

