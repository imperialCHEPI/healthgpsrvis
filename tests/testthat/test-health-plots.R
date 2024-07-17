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


