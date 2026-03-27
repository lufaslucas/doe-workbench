# tests/testthat/test_example_datasets.R
# TDD: verify example datasets can be loaded into the system

library(testthat)

pkg_root <- normalizePath(file.path(dirname(dirname(getwd()))))
if (!dir.exists(file.path(pkg_root, "R"))) pkg_root <- getwd()

source(file.path(pkg_root, "R", "config.R"), local = FALSE)
source(file.path(pkg_root, "R", "utils.R"),  local = FALSE)

datasets_dir <- file.path(pkg_root, "tests", "datasets")

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
