library(testthat)

pkg_root <- normalizePath(file.path(dirname(dirname(getwd()))))
if (!dir.exists(file.path(pkg_root, "R"))) pkg_root <- getwd()
app_dir <- file.path(pkg_root, "inst", "app")

can_run_shinytest2 <- function() {
  tryCatch({
    requireNamespace("shinytest2", quietly = TRUE)
  }, error = function(e) FALSE)
}

click_tab <- function(app, tab_text) {
  app$run_js(sprintf(
    "var tabs = document.querySelectorAll('.navbar-nav .nav-link');
     for (var t of tabs) { if (t.textContent.trim() === '%s') { t.click(); break; } }",
    tab_text
  ))
  Sys.sleep(1)
}

click_id <- function(app, id) {
  app$run_js(sprintf(
    "var el = document.getElementById('%s'); if (el) el.click();",
    id
  ))
  Sys.sleep(1)
}

set_inputs_named <- function(app, values) {
  do.call(app$set_inputs, values)
}

new_app_or_skip <- function(name) {
  tryCatch(
    shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = name,
      height = 900,
      width = 1400,
      load_timeout = 30000,
      timeout = 30000
    ),
    error = function(e) skip(paste("shinytest2 app launch unavailable:", e$message))
  )
}

test_that("reset clears visible Models spec inputs back to defaults", {
  if (!can_run_shinytest2()) skip("shinytest2 is unavailable in this environment")

  app <- new_app_or_skip("model-state-reset")
  on.exit(app$stop(), add = TRUE)

  click_tab(app, "Models")

  set_inputs_named(app, setNames(list("mpg ~ wt + hp"), "models-custom_formula"))
  set_inputs_named(app, setNames(list(4), "models-max_way"))
  set_inputs_named(app, setNames(list(TRUE), "models-append_formulas"))
  set_inputs_named(app, setNames(list(FALSE), "models-include_blocks"))

  expect_equal(app$get_value(input = "models-custom_formula"), "mpg ~ wt + hp")
  expect_equal(app$get_value(input = "models-max_way"), 4)
  expect_identical(app$get_value(input = "models-append_formulas"), TRUE)
  expect_identical(app$get_value(input = "models-include_blocks"), FALSE)

  click_id(app, "reset_btn")
  click_id(app, "reset_confirm_btn")
  click_tab(app, "Models")

  expect_equal(app$get_value(input = "models-custom_formula"), "")
  expect_equal(app$get_value(input = "models-max_way"), 2)
  expect_identical(app$get_value(input = "models-append_formulas"), FALSE)
  expect_identical(app$get_value(input = "models-include_blocks"), TRUE)
})

test_that("Design send-to-Models updates visible Models custom formula", {
  if (!can_run_shinytest2()) skip("shinytest2 is unavailable in this environment")

  app <- new_app_or_skip("design-to-models")
  on.exit(app$stop(), add = TRUE)

  click_tab(app, "Data")
  set_inputs_named(app, setNames(list("rcbd"), "data_upload-example_choice"))
  click_id(app, "data_upload-load_example")

  click_tab(app, "Design")
  set_inputs_named(app, setNames(list("A + B + A:B + Block"), "design-alias_full_formula"))
  set_inputs_named(app, setNames(list("A + B + A:B + Block + Temp_C"), "design-alias_check_formula"))

  # Wait for the alias push controls to render after the formula update.
  Sys.sleep(1.5)
  click_id(app, "design-alias_push_removed")
  click_tab(app, "Models")

  expect_equal(app$get_value(input = "models-custom_formula"), "Yield ~ A + B + A:B + Block")
})

test_that("Models active response selector does not retain stale value after reset", {
  if (!can_run_shinytest2()) skip("shinytest2 is unavailable in this environment")

  app <- new_app_or_skip("model-response-reset")
  on.exit(app$stop(), add = TRUE)

  click_tab(app, "Data")
  set_inputs_named(app, setNames(list("rcbd"), "data_upload-example_choice"))
  click_id(app, "data_upload-load_example")

  click_tab(app, "Models")
  set_inputs_named(app, setNames(list("Purity"), "models-active_response"))
  expect_equal(app$get_value(input = "models-active_response"), "Purity")

  click_id(app, "reset_btn")
  click_id(app, "reset_confirm_btn")
  click_tab(app, "Models")

  # Reset resolves to the first available response for the current dataset.
  expect_false(identical(app$get_value(input = "models-active_response"), "Purity"))
  expect_equal(app$get_value(input = "models-active_response"), "Yield")
})
