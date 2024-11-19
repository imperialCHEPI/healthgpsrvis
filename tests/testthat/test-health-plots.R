# Testing riskfactors() function
test_that("riskfactors function works correctly", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data)

  # Test for valid input
  plot_bmi <- riskfactors("bmi", data_weighted)
  expect_s3_class(plot_bmi, "ggplot")
  expect_equal(plot_bmi$labels$title, "BMI")
  expect_equal(plot_bmi$labels$y, "BMI (weighted)")

  plot_ei <- riskfactors("energyintake", data_weighted)
  expect_s3_class(plot_ei, "ggplot")
  expect_equal(plot_ei$labels$title, "ENERGYINTAKE")
  expect_equal(plot_ei$labels$y, "Energy intake (weighted)")

  plot_sodium <- riskfactors("sodium", data_weighted)
  expect_s3_class(plot_sodium, "ggplot")
  expect_equal(plot_sodium$labels$title, "SODIUM")
  expect_equal(plot_sodium$labels$y, "Sodium (weighted)")

  # Test for invalid input
  expect_error(
    riskfactors("invalid_riskft", data_weighted),
    "Invalid risk factor. Choose from: 'bmi', 'energyintake', 'fat', 'obesity', 'protein', 'sodium'."
  )
})

# Testing riskfactors_diff() function
test_that("riskfactors_diff function works correctly", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data)

  # Generate the weighted data for the risk factors
  data_weighted_rf_wide_collapse <- gen_data_weighted_rf(data_weighted)

  # Test for valid input
  plot_bmi <- riskfactors_diff("bmi",
    data_weighted_rf_wide_collapse,
    scale_y_continuous_limits = c(-0.43, 0),
    scale_y_continuous_breaks = c(-0.43, -0.21, 0),
    scale_y_continuous_labels = c(-0.43, -0.21, 0)
  )
  expect_s3_class(plot_bmi, "ggplot")
  expect_equal(plot_bmi$labels$title, "Reduction in BMI")
  expect_equal(plot_bmi$labels$y, "BMI")

  plot_ei <- riskfactors_diff("ei",
    data_weighted_rf_wide_collapse,
    scale_y_continuous_limits = c(-38.3, 0),
    scale_y_continuous_breaks = c(-38.3, -19.2, 0),
    scale_y_continuous_labels = c(-38.3, -19.2, 0)
  )
  expect_s3_class(plot_ei, "ggplot")
  expect_equal(plot_ei$labels$title, "Reduction in energy intake (kcal)")
  expect_equal(plot_ei$labels$y, "Energy")

  plot_obesity <- riskfactors_diff("obesity",
    data_weighted_rf_wide_collapse,
    scale_y_continuous_limits = c(-0.0135, 0),
    scale_y_continuous_breaks = c(-0.0135, -0.00675, 0),
    scale_y_continuous_labels = c("-1.35%", "-0.675%", "0")
  )
  expect_s3_class(plot_obesity, "ggplot")
  expect_equal(plot_obesity$labels$title, "Reduction in obesity prevalence")
  expect_equal(plot_obesity$labels$y, "Obesity")

  plot_sodium <- riskfactors_diff("sodium",
    data_weighted_rf_wide_collapse,
    scale_y_continuous_limits = c(-64.6, 0),
    scale_y_continuous_breaks = c(-64.6, -32.3, 0),
    scale_y_continuous_labels = c(-64.6, -32.3, 0)
  )
  expect_s3_class(plot_sodium, "ggplot")
  expect_equal(plot_sodium$labels$title, "Reduction in sodium (mg)")
  expect_equal(plot_sodium$labels$y, "Sodium")

  # Test for invalid input
  expect_error(
    riskfactors_diff("invalid_riskft_diff", data_weighted_rf_wide_collapse),
    "Invalid risk factor difference. Choose from: 'bmi', 'ei', 'obesity', 'sodium'."
  )
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
  expect_error(
    inc_diff("invalid_inc", data_mean_weighted_inc_wide),
    "Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'."
  )
})

# Testing inc_cum() function
test_that("inc_cum function works correctly", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data)

  # Generate the weighted data for the risk factors
  data_weighted_ds_wide_collapse <- gen_data_weighted_ds(data_weighted)

  # Test for valid input
  plot_asthma <- inc_cum("asthma",
    data_weighted_ds_wide_collapse,
    scale_y_continuous_limits = c(-4769000, 0),
    scale_y_continuous_breaks = c(-4769000, -4292000, -3815000, -3338000, -2861000, -2384000, -1908000, -1431000, -954000, -477000, 0),
    scale_y_continuous_labels = scales::comma(c(-4769000, -4292000, -3815000, -3338000, -2861000, -2384000, -1908000, -1431000, -954000, -477000, 0))
  )
  expect_s3_class(plot_asthma, "ggplot")
  expect_equal(plot_asthma$labels$title, "Asthma - Cumulative reduction")
  expect_equal(plot_asthma$labels$y, "Asthma incidence")

  plot_ckd <- inc_cum("ckd",
    data_weighted_ds_wide_collapse,
    scale_y_continuous_limits = c(-2098000, 0),
    scale_y_continuous_breaks = c(-2098000, -1888000, -1679000, -1469000, -1259000, -1049000, -839000, -629000, -420000, -210000, 0),
    scale_y_continuous_labels = scales::comma(c(-2098000, -1888000, -1679000, -1469000, -1259000, -1049000, -839000, -629000, -420000, -210000, 0))
  )
  expect_s3_class(plot_ckd, "ggplot")
  expect_equal(plot_ckd$labels$title, "Chronic kidney disease - Cumulative reduction")
  expect_equal(plot_ckd$labels$y, "CKD incidence")

  plot_db <- inc_cum("diabetes",
    data_weighted_ds_wide_collapse,
    scale_y_continuous_limits = c(-4424000, 0),
    scale_y_continuous_breaks = c(-4424000, -4084000, -3743000, -3403000, -3063000, -2722000, -2382000, -2042000, -1701000, -1361000, -1021000, -681000, -340000, 0),
    scale_y_continuous_labels = scales::comma(c(-4424000, -4084000, -3743000, -3403000, -3063000, -2722000, -2382000, -2042000, -1701000, -1361000, -1021000, -681000, -340000, 0))
  )
  expect_s3_class(plot_db, "ggplot")
  expect_equal(plot_db$labels$title, "Diabetes - Cumulative reduction")
  expect_equal(plot_db$labels$y, "Diabetes incidence")

  plot_ihd <- inc_cum("ischemia",
    data_weighted_ds_wide_collapse,
    scale_y_continuous_limits = c(-6455000, 0),
    scale_y_continuous_breaks = c(-6455000, -5810000, -5164000, -4519000, -3873000, -3228000, -2582000, -1937000, -1291000, -646000, 0),
    scale_y_continuous_labels = scales::comma(c(-6455000, -5810000, -5164000, -4519000, -3873000, -3228000, -2582000, -1937000, -1291000, -646000, 0))
  )
  expect_s3_class(plot_ihd, "ggplot")
  expect_equal(plot_ihd$labels$title, "Ischemic heart disease - Cumulative reduction")
  expect_equal(plot_ihd$labels$y, "Ischemia incidence")

  plot_stroke <- inc_cum("stroke",
    data_weighted_ds_wide_collapse,
    scale_y_continuous_limits = c(-1198000, 0),
    scale_y_continuous_breaks = c(-1198000, -1078000, -959000, -839000, -719000, -599000, -479000, -359000, -240000, -120000, 0),
    scale_y_continuous_labels = scales::comma(c(-1198000, -1078000, -959000, -839000, -719000, -599000, -479000, -359000, -240000, -120000, 0))
  )
  expect_s3_class(plot_stroke, "ggplot")
  expect_equal(plot_stroke$labels$title, "Stroke - Cumulative reduction")
  expect_equal(plot_stroke$labels$y, "Stroke incidence")

  # Test for invalid input
  expect_error(
    inc_cum("invalid_inc_cum", data_weighted_ds_wide_collapse),
    "Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'."
  )
})

# Testing burden_disease() function
test_that("burden_disease function works correctly", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data)

  # Generate the weighted data for the risk factors (using 'bd' instead of
  # 'burden' to keep lintr happy)
  data_weighted_bd_wide_collapse <- gen_data_weighted_burden(data_weighted)

  # Test for valid input
  plot_daly <- burden_disease("daly", data_weighted_bd_wide_collapse)
  expect_s3_class(plot_daly, "ggplot")
  expect_equal(plot_daly$labels$title, "Reduction in DALYs")
  expect_equal(plot_daly$labels$y, "DALYs")

  plot_dalycum <- burden_disease("dalycum",
    data_weighted_bd_wide_collapse,
    scale_y_continuous_limits = c(-41917000, 0),
    scale_y_continuous_breaks = c(-41917000, -33534000, -25150000, -16767000, -8383000, 0),
    scale_y_continuous_labels = scales::comma(c(-41917000, -33534000, -25150000, -16767000, -8383000, 0))
  )
  expect_s3_class(plot_dalycum, "ggplot")
  expect_equal(plot_dalycum$labels$title, "Cumulative reduction in DALYs")
  expect_equal(plot_dalycum$labels$y, "DALYs")

  plot_yld <- burden_disease("yld", data_weighted_bd_wide_collapse)
  expect_s3_class(plot_yld, "ggplot")
  expect_equal(plot_yld$labels$title, "Reduction in YLDs")
  expect_equal(plot_yld$labels$y, "YLDs")

  plot_yll <- burden_disease("yll", data_weighted_bd_wide_collapse)
  expect_s3_class(plot_yll, "ggplot")
  expect_equal(plot_yll$labels$title, "Reduction in YLLs")
  expect_equal(plot_yll$labels$y, "YLLs")

  # Test for invalid input
  expect_error(
    burden_disease("invalid_burden_disease", data_weighted_bd_wide_collapse),
    "Invalid burden of disease. Choose from: 'daly', 'dalycum', 'yld', 'yll'."
  )
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
    risk_factors = c("bmi", "energyintake")
  )
  # burden_disease = c("dalycum", "yld"))

  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data)

  # Generate the weighted data for the risk factors (using 'bd' instead of
  # 'burden' to keep lintr happy)
  data_weighted_bd_wide_collapse <- gen_data_weighted_burden(data_weighted)

  # Create output file
  output_file <- tempfile(fileext = ".pdf")

  # Create the combined plot
  plot_combine <- combine_plots(
    metrics = metrics,
    data_weighted = data_weighted,
    data_weighted_bd_wide_collapse = data_weighted_bd_wide_collapse,
    output_file = output_file
  )

  # Test that the output file is created
  expect_true(file.exists(output_file))

  # Test that the output file is not empty
  expect_gt(file.size(output_file), 0)

  # Clean up
  unlink(output_file)
})
