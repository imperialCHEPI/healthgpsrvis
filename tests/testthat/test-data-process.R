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
  data_weighted <- gen_data_weighted(data)

  # Check if the data has the expected number of columns
  expect_equal(ncol(data_weighted), 37)

  # Check if the data has the expected column names
  expect_equal(colnames(data_weighted), c("source", "time", "simID", "weighted_income",
                                 "weighted_sector", "weighted_sodium",
                                 "weighted_carbohydrate", "weighted_fat",
                                 "weighted_protein", "weighted_energyintake",
                                 "weighted_physicalactivity", "weighted_bmi",
                                 "weighted_height", "weighted_weight",
                                 "weighted_overweight", "weighted_obesity",
                                 "weighted_disabilityweight", "weighted_death",
                                 "weighted_migrations", "wprev_ihd",
                                 "wprev_diabetes", "wprev_stroke",
                                 "wprev_asthma", "wprev_ckd", "prevcase_ihd",
                                 "prevcase_diabetes", "prevcase_stroke",
                                 "prevcase_asthma", "prevcase_ckd",
                                 "totalcase_ihd", "totalcase_diabetes",
                                 "totalcase_stroke", "totalcase_asthma",
                                 "totalcase_ckd", "total_yll", "total_yld",
                                 "total_daly"))
})

# Testing gen_data_weighted_rf() function

# Testing gen_data_weighted_ds() function

# Testing gen_data_weighted_burden() function
