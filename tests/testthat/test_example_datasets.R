# tests/testthat/test_example_datasets.R
# TDD: verify example datasets can be loaded into the system

datasets_dir <- test_path("datasets")

test_that("CCD dataset loads into the system", {
  path <- file.path(datasets_dir, "ccd.csv")
  expect_true(file.exists(path))

  df <- read.csv(path, stringsAsFactors = FALSE)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)

  df <- stamp_row_ids(df)
  expect_true(ROW_ID_COL %in% names(df))
  expect_equal(df[[ROW_ID_COL]], seq_len(nrow(df)))
})
