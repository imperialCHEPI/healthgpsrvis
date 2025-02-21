# Testing .rds file
test_that("rds file is not empty", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Check if the .rds file is not empty
  expect_gt(length(data), 0)
})

# Testing gen_data_weighted() function
test_that("Columns in the generated dataframe", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data, configname = "default")

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted), 37)

  # Check if the data has the expected column names
  expect_equal(colnames(data_weighted), c(
    "source", "time", "simID",
    "weighted_income", "weighted_sector",
    "weighted_sodium",
    "weighted_carbohydrate",
    "weighted_fat", "weighted_protein",
    "weighted_energyintake",
    "weighted_physicalactivity",
    "weighted_bmi", "weighted_height",
    "weighted_weight",
    "weighted_overweight",
    "weighted_obesity",
    "weighted_disabilityweight",
    "weighted_death",
    "weighted_migrations", "wprev_ihd",
    "wprev_diabetes", "wprev_stroke",
    "wprev_asthma", "wprev_ckd",
    "prevcase_ihd", "prevcase_diabetes",
    "prevcase_stroke", "prevcase_asthma",
    "prevcase_ckd", "totalcase_ihd",
    "totalcase_diabetes",
    "totalcase_stroke",
    "totalcase_asthma", "totalcase_ckd",
    "total_yll", "total_yld",
    "total_daly"
  ))
})

# Testing gen_data_weighted_rf() function
test_that("Columns in the generated dataframe", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data, configname = "default")

  # Generate the weighted data for the risk factors
  data_weighted_rf_wide_collapse <- gen_data_weighted_rf(data_weighted,
                                                         configname = "default")

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted_rf_wide_collapse), 26)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_rf_wide_collapse),
    c(
      "time", "simID", "weighted_sodium_baseline",
      "weighted_sodium_intervention", "weighted_energyintake_baseline",
      "weighted_energyintake_intervention", "weighted_bmi_baseline",
      "weighted_bmi_intervention", "weighted_obesity_baseline",
      "weighted_obesity_intervention", "diff_sodium", "diff_ei", "diff_bmi",
      "diff_obesity", "diff_sodium_mean", "diff_ei_mean", "diff_bmi_mean",
      "diff_obesity_mean", "diff_sodium_ci_low", "diff_ei_ci_low",
      "diff_bmi_ci_low", "diff_obesity_ci_low", "diff_sodium_ci_high",
      "diff_ei_ci_high", "diff_bmi_ci_high", "diff_obesity_ci_high"
    )
  )
})

# Testing gen_data_weighted_ds_diff() function
test_that("Columns in the generated dataframe", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data, configname = "default")

  # Generate the weighted data for the risk factors
  data_weighted_ds_wide_diff <- gen_data_weighted_ds_diff(data_weighted,
                                                          configname = "default")

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted_ds_wide_diff), 17)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_ds_wide_diff),
    c(
      "time", "simID", "totalcase_ihd_baseline", "totalcase_ihd_intervention",
      "totalcase_diabetes_baseline", "totalcase_diabetes_intervention",
      "totalcase_stroke_baseline", "totalcase_stroke_intervention",
      "totalcase_asthma_baseline", "totalcase_asthma_intervention",
      "totalcase_ckd_baseline", "totalcase_ckd_intervention", "diff_inc_ihd",
      "diff_inc_db", "diff_inc_stroke", "diff_inc_asthma", "diff_inc_ckd"
    )
  )
})

# Testing gen_data_weighted_ds_cumdiff() function
test_that("Columns in the generated dataframe", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data, configname = "default")

  # Generate the weighted data for the risk factors
  data_weighted_ds_wide_collapse <- gen_data_weighted_ds_cumdiff(data_weighted,
                                                                 configname = "default")

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted_ds_wide_collapse), 37)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_ds_wide_collapse),
    c(
      "time", "simID", "totalcase_ihd_baseline", "totalcase_ihd_intervention",
      "totalcase_diabetes_baseline", "totalcase_diabetes_intervention",
      "totalcase_stroke_baseline", "totalcase_stroke_intervention",
      "totalcase_asthma_baseline", "totalcase_asthma_intervention",
      "totalcase_ckd_baseline", "totalcase_ckd_intervention", "diff_inc_ihd",
      "diff_inc_db", "diff_inc_stroke", "diff_inc_asthma", "diff_inc_ckd",
      "cumdiff_inc_ihd", "cumdiff_inc_db", "cumdiff_inc_stroke",
      "cumdiff_inc_asthma", "cumdiff_inc_ckd", "cumdiff_inc_ihd_mean",
      "cumdiff_inc_db_mean", "cumdiff_inc_stroke_mean",
      "cumdiff_inc_asthma_mean", "cumdiff_inc_ckd_mean",
      "cumdiff_inc_ihd_ci_low", "cumdiff_inc_db_ci_low",
      "cumdiff_inc_stroke_ci_low", "cumdiff_inc_asthma_ci_low",
      "cumdiff_inc_ckd_ci_low", "cumdiff_inc_ihd_ci_high",
      "cumdiff_inc_db_ci_high", "cumdiff_inc_stroke_ci_high",
      "cumdiff_inc_asthma_ci_high", "cumdiff_inc_ckd_ci_high"
    )
  )
})

# Testing gen_data_weighted_burden() function
test_that("Columns in the generated dataframe", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data, configname = "default")

  # Generate the weighted data for the risk factors (using 'bd' instead of
  # 'burden' to keep lintr happy)
  data_weighted_bd_wide_collapse <- gen_data_weighted_burden(data_weighted,
                                                             configname = "default")

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted_bd_wide_collapse), 32)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_bd_wide_collapse),
    c(
      "time", "simID", "total_yll_baseline", "total_yll_intervention",
      "total_yld_baseline", "total_yld_intervention", "total_daly_baseline",
      "total_daly_intervention", "diff_yll", "diff_yld", "diff_daly",
      "cumdiff_daly", "cumdiff_yll", "cumdiff_yld", "diff_daly_mean",
      "diff_yll_mean", "diff_yld_mean", "cumdiff_daly_mean", "cumdiff_yll_mean",
      "cumdiff_yld_mean", "diff_daly_ci_low", "diff_yll_ci_low",
      "diff_yld_ci_low", "cumdiff_daly_ci_low", "cumdiff_yll_ci_low",
      "cumdiff_yld_ci_low", "diff_daly_ci_high", "diff_yll_ci_high",
      "diff_yld_ci_high", "cumdiff_daly_ci_high", "cumdiff_yll_ci_high",
      "cumdiff_yld_ci_high"
    )
  )
})

# Testing gen_data_weighted_bd_spline() function
test_that("Columns in the generated dataframe", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Generate the weighted data
  data_weighted <- gen_data_weighted(data, configname = "default")

  # Generate the weighted data for the risk factors (using 'bd' instead of
  # 'burden' to keep lintr happy)
  data_weighted_bd_wide_collapse <- gen_data_weighted_burden(data_weighted, configname = "default")

  # Generate a data frame with spline smoothing applied for burden of disease
  data_weighted_burden_spline <- gen_data_weighted_bd_spline(
    data_weighted_bd_wide_collapse
  )

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted_burden_spline), 4)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_burden_spline),
    c("time", "cumdiff_daly_mean", "cumdiff_daly_ci_low", "cumdiff_daly_ci_high")
  )
})
