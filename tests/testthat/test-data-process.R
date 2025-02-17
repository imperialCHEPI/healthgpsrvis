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
  expect_equal(ncol(data_weighted_rf_wide_collapse), 13)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_rf_wide_collapse),
    c(
      "time", "diff_sodium_mean", "diff_sodium_min",
      "diff_sodium_max", "diff_ei_mean", "diff_ei_min",
      "diff_ei_max", "diff_bmi_mean", "diff_bmi_min", "diff_bmi_max",
      "diff_obesity_mean", "diff_obesity_min", "diff_obesity_max"
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
  expect_equal(ncol(data_weighted_ds_wide_collapse), 16)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_ds_wide_collapse),
    c(
      "time", "cumdiff_inc_ihd_mean", "cumdiff_inc_ihd_min",
      "cumdiff_inc_ihd_max", "cumdiff_inc_db_mean", "cumdiff_inc_db_min",
      "cumdiff_inc_db_max", "cumdiff_inc_stroke_mean", "cumdiff_inc_stroke_min",
      "cumdiff_inc_stroke_max", "cumdiff_inc_asthma_mean",
      "cumdiff_inc_asthma_min", "cumdiff_inc_asthma_max",
      "cumdiff_inc_ckd_mean", "cumdiff_inc_ckd_min", "cumdiff_inc_ckd_max"
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
  expect_equal(ncol(data_weighted_bd_wide_collapse), 19)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_bd_wide_collapse),
    c(
      "time", "diff_daly_mean", "diff_daly_min", "diff_daly_max",
      "diff_yll_mean", "diff_yll_min", "diff_yll_max", "diff_yld_mean",
      "diff_yld_min", "diff_yld_max", "cumdiff_daly_mean", "cumdiff_daly_min",
      "cumdiff_daly_max", "cumdiff_yll_mean", "cumdiff_yll_min",
      "cumdiff_yll_max", "cumdiff_yld_mean", "cumdiff_yld_min",
      "cumdiff_yld_max"
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
  data_weighted_bd_wide_collapse <- gen_data_weighted_burden(data_weighted,
                                                             configname = "default")

  # Generate a data frame with spline smoothing applied for burden of disease
  data_weighted_burden_spline <- gen_data_weighted_bd_spline(
    data_weighted_bd_wide_collapse
  )

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted_burden_spline), 4)

  # Check if the data has the expected column names
  expect_equal(
    colnames(data_weighted_burden_spline),
    c("time", "cumdiff_daly_mean", "cumdiff_daly_min", "cumdiff_daly_max")
  )
})

# Testing gen_data_le() function
