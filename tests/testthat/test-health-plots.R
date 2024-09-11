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
  plot_bmi <- riskfactors_diff("bmi",
                               data_weighted_rf_wide_collapse,
                               scale_y_continuous_limits = c(-0.43,0),
                               scale_y_continuous_breaks = c(-0.43,-0.21,0),
                               scale_y_continuous_labels = c(-0.43,-0.21,0))
  expect_s3_class(plot_bmi, "ggplot")
  expect_equal(plot_bmi$labels$title, "Reduction in BMI by income class")
  expect_equal(plot_bmi$labels$y, "BMI")

  plot_ei <- riskfactors_diff("ei",
                              data_weighted_rf_wide_collapse,
                              scale_y_continuous_limits = c(-38.3,0),
                              scale_y_continuous_breaks = c(-38.3,-19.2,0),
                              scale_y_continuous_labels = c(-38.3,-19.2,0))
  expect_s3_class(plot_ei, "ggplot")
  expect_equal(plot_ei$labels$title, "Reduction in energy intake (kcal) by income class")
  expect_equal(plot_ei$labels$y, "Energy")

  plot_obesity <- riskfactors_diff("obesity",
                                   data_weighted_rf_wide_collapse,
                                   scale_y_continuous_limits = c(-0.0135,0),
                                   scale_y_continuous_breaks = c(-0.0135,-0.00675,0),
                                   scale_y_continuous_labels = c("-1.35%","-0.675%","0"))
  expect_s3_class(plot_obesity, "ggplot")
  expect_equal(plot_obesity$labels$title, "Reduction in obesity prevalence by income class")
  expect_equal(plot_obesity$labels$y, "Obesity")

  plot_sodium <- riskfactors_diff("sodium",
                                  data_weighted_rf_wide_collapse,
                                  scale_y_continuous_limits = c(-64.6,0),
                                  scale_y_continuous_breaks = c(-64.6,-32.3,0),
                                  scale_y_continuous_labels = c(-64.6,-32.3,0))
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
  data_weighted_ds_wide_collapse <- data.frame(
    time = seq(-9, 21, by = 1),
    income = c(rep("low",9), rep("middle", 16), rep("high", 6)),
    diff_inc_asthma_mean = runif(31, -3, 0),
    diff_inc_asthma_max = runif(31, -5, 0),
    diff_inc_asthma_min = runif(31, -1, 0),
    diff_inc_ckd_mean = runif(31, -200, 0),
    diff_inc_ckd_max = runif(31, -300, 0),
    diff_inc_ckd_min = runif(31, -100, 0),
    diff_inc_db_mean = runif(31, -0.3, 0),
    diff_inc_db_max = runif(31, -0.5, 0),
    diff_inc_db_min = runif(31, -0.1, 0),
    diff_inc_ihd_mean = runif(31, -700, 0),
    diff_inc_ihd_max = runif(31, -1000, 0),
    diff_inc_ihd_min = runif(31, -300, 0),
    diff_inc_stroke_mean = runif(31, -79, 0),
    diff_inc_stroke_max = runif(31, -100, 0),
    diff_inc_stroke_min = runif(31, -50, 0)
  )

  # Test for valid input
  plot_asthma <- inc_cum("asthma",
                         data_weighted_ds_wide_collapse,
                         scale_y_continuous_limits = c(-4769000,0),
                         scale_y_continuous_breaks = c(-4769000,-4292000,-3815000,-3338000,-2861000,-2384000,-1908000,-1431000,-954000,-477000,0),
                         scale_y_continuous_labels = scales::comma(c(-4769000,-4292000,-3815000,-3338000,-2861000,-2384000,-1908000,-1431000,-954000,-477000,0)))
  expect_s3_class(plot_asthma, "ggplot")
  expect_equal(plot_asthma$labels$title, "Asthma - Cumulative reduction by income class")
  expect_equal(plot_asthma$labels$y, "Asthma incidence")

  plot_ckd <- inc_cum("ckd",
                      data_weighted_ds_wide_collapse,
                      scale_y_continuous_limits = c(-2098000,0),
                      scale_y_continuous_breaks = c(-2098000,-1888000,-1679000,-1469000,-1259000,-1049000,-839000,-629000,-420000,-210000,0),
                      scale_y_continuous_labels = scales::comma(c(-2098000,-1888000,-1679000,-1469000,-1259000,-1049000,-839000,-629000,-420000,-210000,0)))
  expect_s3_class(plot_ckd, "ggplot")
  expect_equal(plot_ckd$labels$title, "Chronic kidney disease - Cumulative reduction by income class")
  expect_equal(plot_ckd$labels$y, "CKD incidence")

  plot_db <- inc_cum("diabetes",
                     data_weighted_ds_wide_collapse,
                     scale_y_continuous_limits = c(-4424000,0),
                     scale_y_continuous_breaks = c(-4424000,-4084000,-3743000,-3403000,-3063000,-2722000,-2382000,-2042000,-1701000,-1361000,-1021000,-681000,-340000,0),
                     scale_y_continuous_labels = scales::comma(c(-4424000,-4084000,-3743000,-3403000,-3063000,-2722000,-2382000,-2042000,-1701000,-1361000,-1021000,-681000,-340000,0)))
  expect_s3_class(plot_db, "ggplot")
  expect_equal(plot_db$labels$title, "Diabetes - Cumulative reduction by income class")
  expect_equal(plot_db$labels$y, "Diabetes incidence")

  plot_ihd <- inc_cum("ischemia",
                      data_weighted_ds_wide_collapse,
                      scale_y_continuous_limits = c(-6455000,0),
                      scale_y_continuous_breaks = c(-6455000,-5810000,-5164000,-4519000,-3873000,-3228000,-2582000,-1937000,-1291000,-646000,0),
                      scale_y_continuous_labels = scales::comma(c(-6455000,-5810000,-5164000,-4519000,-3873000,-3228000,-2582000,-1937000,-1291000,-646000,0)))
  expect_s3_class(plot_ihd, "ggplot")
  expect_equal(plot_ihd$labels$title, "Ischemic heart disease - Cumulative reduction by income class")
  expect_equal(plot_ihd$labels$y, "Ischemia incidence")

  plot_stroke <- inc_cum("stroke",
                         data_weighted_ds_wide_collapse,
                         scale_y_continuous_limits = c(-1198000,0),
                         scale_y_continuous_breaks = c(-1198000,-1078000,-959000,-839000,-719000,-599000,-479000,-359000,-240000,-120000,0),
                         scale_y_continuous_labels = scales::comma(c(-1198000,-1078000,-959000,-839000,-719000,-599000,-479000,-359000,-240000,-120000,0)))
  expect_s3_class(plot_stroke, "ggplot")
  expect_equal(plot_stroke$labels$title, "Stroke - Cumulative reduction by income class")
  expect_equal(plot_stroke$labels$y, "Stroke incidence")

  # Test for invalid input
  expect_error(inc_cum("invalid_inc_cum", data_weighted_ds_wide_collapse),
               "Invalid incidence. Choose from: 'asthma', 'ckd', 'diabetes', 'ischemia', 'stroke'.")
})

# Testing burden_disease() function
test_that("burden_disease function works correctly", {
  # Create sample data
  data_weighted_burden_wide_collapse <- data.frame(
    time = seq(-9, 21, by = 1),
    income = c(rep("low",9), rep("middle", 16), rep("high", 6)),
    diff_daly_mean = runif(31, -3, 0),
    diff_daly_max = runif(31, -5, 0),
    diff_daly_min = runif(31, -1, 0),
    cumdiff_daly_mean = runif(31, -200, 0),
    cumdiff_daly_max = runif(31, -300, 0),
    cumdiff_daly_min = runif(31, -100, 0),
    diff_yld_mean = runif(31, -0.3, 0),
    diff_yld_max = runif(31, -0.5, 0),
    diff_yld_min = runif(31, -0.1, 0),
    diff_yll_mean = runif(31, -700, 0),
    diff_yll_max = runif(31, -1000, 0),
    diff_yll_min = runif(31, -300, 0)
  )

  # Test for valid input
  plot_daly <- burden_disease("daly", data_weighted_burden_wide_collapse)
  expect_s3_class(plot_daly, "ggplot")
  expect_equal(plot_daly$labels$title, "Reduction in DALYs by income class")
  expect_equal(plot_daly$labels$y, "DALYs")

  plot_dalycum <- burden_disease("dalycum",
                                 data_weighted_burden_wide_collapse,
                                 scale_y_continuous_limits = c(-41917000,0),
                                 scale_y_continuous_breaks = c(-41917000,-33534000,-25150000,-16767000,-8383000,0),
                                 scale_y_continuous_labels = scales::comma(c(-41917000,-33534000,-25150000,-16767000,-8383000,0)))
  expect_s3_class(plot_dalycum, "ggplot")
  expect_equal(plot_dalycum$labels$title, "Cumulative reduction in DALYs by income class")
  expect_equal(plot_dalycum$labels$y, "DALYs")

  plot_yld <- burden_disease("yld", data_weighted_burden_wide_collapse)
  expect_s3_class(plot_yld, "ggplot")
  expect_equal(plot_yld$labels$title, "Reduction in YLDs by income class")
  expect_equal(plot_yld$labels$y, "YLDs")

  plot_yll <- burden_disease("yll", data_weighted_burden_wide_collapse)
  expect_s3_class(plot_yll, "ggplot")
  expect_equal(plot_yll$labels$title, "Reduction in YLLs by income class")
  expect_equal(plot_yll$labels$y, "YLLs")

  # Test for invalid input
  expect_error(burden_disease("invalid_burden_disease", data_weighted_burden_wide_collapse),
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
    risk_factors = c("bmi", "ei"))
    #burden_disease = c("dalycum", "yld"))

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

  data_weighted_burden_wide_collapse <- data.frame(
    time = seq(-9, 21, by = 1),
    income = c(rep("low",9), rep("middle", 16), rep("high", 6)),
    diff_daly_mean = runif(31, -3, 0),
    diff_daly_max = runif(31, -5, 0),
    diff_daly_min = runif(31, -1, 0),
    cumdiff_daly_mean = runif(31, -200, 0),
    cumdiff_daly_max = runif(31, -300, 0),
    cumdiff_daly_min = runif(31, -100, 0),
    diff_yld_mean = runif(31, -0.3, 0),
    diff_yld_max = runif(31, -0.5, 0),
    diff_yld_min = runif(31, -0.1, 0),
    diff_yll_mean = runif(31, -700, 0),
    diff_yll_max = runif(31, -1000, 0),
    diff_yll_min = runif(31, -300, 0)
  )

  # Create output file
  output_file <- tempfile(fileext = ".pdf")

  # Create the combined plot
  plot_combine <- combine_plots(metrics=metrics,
                                data_mean_weighted = data_mean_weighted,
                                data_weighted_burden_wide_collapse = data_weighted_burden_wide_collapse,
                                output_file = output_file)

  # Test that the output file is created
  expect_true(file.exists(output_file))

  # Test that the output file is not empty
  expect_gt(file.size(output_file), 0)

  # Clean up
  unlink(output_file)
})
