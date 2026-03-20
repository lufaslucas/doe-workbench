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

test_that("reset clears visible Design spec inputs back to defaults", {
  if (!can_run_shinytest2()) {
    skip("shinytest2 is unavailable in this environment")
  }

  app <- tryCatch(
    shinytest2::AppDriver$new(
      app_dir = app_dir,
      name = "design-state-reset",
      height = 900,
      width = 1400,
      load_timeout = 30000,
      timeout = 30000
    ),
    error = function(e) {
      skip(paste("shinytest2 app launch unavailable:", e$message))
    }
  )
  on.exit(app$stop(), add = TRUE)

  click_tab(app, "Design")

  set_inputs_named(app, setNames(list("A + B + A:B"), "design-alias_full_formula"))
  set_inputs_named(app, setNames(list("A + B + A:B + Block"), "design-alias_check_formula"))
  set_inputs_named(app, setNames(list(0.91), "design-alias_threshold"))

  expect_equal(app$get_value(input = "design-alias_full_formula"), "A + B + A:B")
  expect_equal(app$get_value(input = "design-alias_check_formula"), "A + B + A:B + Block")
  expect_equal(app$get_value(input = "design-alias_threshold"), 0.91)

  click_id(app, "reset_btn")
  click_id(app, "reset_confirm_btn")
  click_tab(app, "Design")

  expect_equal(app$get_value(input = "design-alias_full_formula"), "")
  expect_equal(app$get_value(input = "design-alias_check_formula"), "")
  expect_equal(app$get_value(input = "design-alias_threshold"), 0.99)
})
