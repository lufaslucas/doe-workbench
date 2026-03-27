# tests/testthat/explore/test_needle_plot.R
# TDD: verify needle plot renders with example data

datasets_dir <- test_path("datasets")

test_that("needle plot builds from RCBD data", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  p <- build_needle_plot(
    df        = df,
    resp_col  = "Yield",
    fac_order = c("A", "B"),
    dcol      = "#404040"
  )

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)

  pl <- ggplotly(p)
  expect_s3_class(pl, "plotly")
})

test_that("faceted needle plot does not duplicate data across panels", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  p <- build_needle_plot(
    df        = df,
    resp_col  = "Yield",
    fac_order = c("A", "B"),
    dcol      = "#404040",
    facet_var = "Block"
  )

  built <- ggplot_build(p)

  # The individual points layer (layer 5: last geom_point before annotate)
  # should have exactly nrow(df) points — not nrow(df) * n_facets
  # Layers: 1=hline, 2=group needles, 3=diamond markers, 4=individual needles, 5=points, 6=annotate
  point_layer <- built$data[[5]]
  expect_equal(nrow(point_layer), nrow(df))
})

test_that("sort by response reorders treatment levels", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  p_fac <- build_needle_plot(df = df, resp_col = "Yield", fac_order = c("A", "B"),
                              dcol = "#404040", sort_mode = "factors")
  p_resp <- build_needle_plot(df = df, resp_col = "Yield", fac_order = c("A", "B"),
                               dcol = "#404040", sort_mode = "response")

  levels_fac  <- levels(p_fac$data$.trt_id)
  levels_resp <- levels(p_resp$data$.trt_id)

  # Both should have the same set of levels, but in different order
  expect_setequal(levels_fac, levels_resp)
  expect_false(identical(levels_fac, levels_resp))
})

test_that("categorical colour vector renders", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  p <- build_needle_plot(df = df, resp_col = "Yield", fac_order = c("A", "B"),
                          dcol = "#404040", colour_vec = df$Block,
                          colour_label = "Block")

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
  pl <- ggplotly(p)
  expect_s3_class(pl, "plotly")
})

test_that("continuous colour vector renders", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  p <- build_needle_plot(df = df, resp_col = "Yield", fac_order = c("A", "B"),
                          dcol = "#404040", colour_vec = df$Temp_C,
                          colour_label = "Temp_C")

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
  pl <- ggplotly(p)
  expect_s3_class(pl, "plotly")
})

test_that("single factor works", {
  df <- read.csv(file.path(datasets_dir, "unbalanced.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  p <- build_needle_plot(df = df, resp_col = "Response", fac_order = "Treatment",
                          dcol = "#404040")

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
  pl <- ggplotly(p)
  expect_s3_class(pl, "plotly")
})

test_that("zero-variance response does not error", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)
  df$Yield <- 50  # all identical

  p <- build_needle_plot(df = df, resp_col = "Yield", fac_order = c("A", "B"),
                          dcol = "#404040")

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
})

test_that("needle plot with 3+ factors does not crash (discrete scale)", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  # Include Block as a factor — reproduces the "discrete value on continuous scale" crash
  p <- build_needle_plot(df = df, resp_col = "Yield",
                          fac_order = c("A", "B", "Block"),
                          dcol = "#404040")

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)

  # Also test ggplotly conversion doesn't crash
  pl <- ggplotly(p)
  expect_s3_class(pl, "plotly")
})

test_that("needle plot with facet + 3 factors does not crash", {
  df <- read.csv(file.path(datasets_dir, "rcbd.csv"), stringsAsFactors = FALSE)
  df <- stamp_row_ids(df)

  p <- build_needle_plot(df = df, resp_col = "Yield",
                          fac_order = c("A", "B", "Block"),
                          dcol = "#404040",
                          facet_var = "Block")

  expect_s3_class(p, "ggplot")
  built <- ggplot_build(p)
  expect_true(length(built$data) > 0)
  pl <- ggplotly(p)
  expect_s3_class(pl, "plotly")
})
