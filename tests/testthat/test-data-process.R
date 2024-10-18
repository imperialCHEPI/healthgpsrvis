# Testing .rds file
test_that("rds file is not empty", {
  # Get the path to the .rds file
  filepath <- testthat::test_path("testdata", "data_ps3_reformulation")

  # Read the .rds file
  data <- readRDS(filepath)

  # Check if the .rds file is not empty
  expect_gt(length(data), 0)
})


# Testing gen_data_weighted_rf() function
test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# Testing gen_data_weighted_ds() function

# Testing gen_data_weighted_burden() function
