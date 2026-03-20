library(testthat)

pkg_root <- normalizePath(file.path(dirname(dirname(getwd()))))
r_dir    <- file.path(pkg_root, "R")
if (!dir.exists(r_dir)) { pkg_root <- getwd(); r_dir <- file.path(pkg_root, "R") }

# Source config.R first (defines constants used by app_state.R and utils.R)
tryCatch(source(file.path(r_dir, "config.R"), local = FALSE), error = function(e) NULL)
for (f in sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))) {
  if (grepl("^mod_|^ui_helpers|^config\\.R$", basename(f))) next
  tryCatch(source(f, local = FALSE), error = function(e) NULL)
}

test_that("make_default_rv includes phase-one state fields with expected defaults", {
  defs <- make_default_rv()

  expect_true(all(c(
    "formulas", "formula_gen", "models", "mc_results",
    "mc_on", "mc_alpha", "mc_terms", "mc_methods",
    "formula_aliases", "alias_labels", "inestimable_terms",
    "pending_alias_resolution", "report_items", "selected_obs"
  ) %in% names(defs)))

  expect_equal(defs$formulas, character(0))
  expect_equal(defs$formula_gen, 0L)
  expect_equal(defs$models, list())
  expect_equal(defs$mc_results, list())
  expect_false(defs$mc_on)
  expect_equal(defs$mc_alpha, ALPHA_DEFAULT)
  expect_equal(defs$mc_terms, character(0))
  expect_equal(defs$mc_methods, character(0))
  expect_equal(defs$formula_aliases, list())
  expect_equal(defs$alias_labels, list())
  expect_equal(defs$inestimable_terms, character(0))
  expect_null(defs$pending_alias_resolution)
  expect_equal(defs$report_items, list())
  expect_null(defs$selected_obs)
})

test_that("clear_formula_state resets formula and alias state while bumping generation", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  rv$formulas <- c("Y ~ A", "Y ~ A + B")
  rv$formula_gen <- 5L
  rv$formula_aliases <- list("Y ~ A + B" = data.frame(Term_1 = "A", Term_2 = "B"))
  rv$alias_labels <- list(A = "A + B")
  rv$inestimable_terms <- "A:B"
  rv$pending_alias_resolution <- list(formulas = c("Y ~ A + B"))
  rv$skip_auto_formula <- TRUE

  shiny::isolate(clear_formula_state(rv))

  expect_equal(shiny::isolate(rv$formulas), character(0))
  expect_equal(shiny::isolate(rv$formula_gen), 6L)
  expect_equal(shiny::isolate(rv$formula_aliases), list())
  expect_equal(shiny::isolate(rv$alias_labels), list())
  expect_equal(shiny::isolate(rv$inestimable_terms), character(0))
  expect_null(shiny::isolate(rv$pending_alias_resolution))
  expect_false(shiny::isolate(rv$skip_auto_formula))
})

test_that("make_default_rv includes Design spec fields with expected defaults", {
  defs <- make_default_rv()

  expect_true(all(c(
    "design_model_formula", "design_alias_formula", "alias_threshold"
  ) %in% names(defs)))

  expect_equal(defs$design_model_formula, "")
  expect_equal(defs$design_alias_formula, "")
  expect_equal(defs$alias_threshold, ALIAS_CORR_THRESH)
})

test_that("set_design_model_formula trims whitespace and handles NULL", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_design_model_formula(rv, "  A + B  "))
  expect_equal(shiny::isolate(rv$design_model_formula), "A + B")

  shiny::isolate(set_design_model_formula(rv, NULL))
  expect_equal(shiny::isolate(rv$design_model_formula), "")
})

test_that("set_design_alias_formula trims whitespace and handles NULL", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_design_alias_formula(rv, " A:B + C:D "))
  expect_equal(shiny::isolate(rv$design_alias_formula), "A:B + C:D")

  shiny::isolate(set_design_alias_formula(rv, NULL))
  expect_equal(shiny::isolate(rv$design_alias_formula), "")
})

test_that("set_alias_threshold validates and defaults invalid values", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_alias_threshold(rv, 0.95))
  expect_equal(shiny::isolate(rv$alias_threshold), 0.95)

  shiny::isolate(set_alias_threshold(rv, -0.5))
  expect_equal(shiny::isolate(rv$alias_threshold), ALIAS_CORR_THRESH)

  shiny::isolate(set_alias_threshold(rv, 1.5))
  expect_equal(shiny::isolate(rv$alias_threshold), ALIAS_CORR_THRESH)

  suppressWarnings(shiny::isolate(set_alias_threshold(rv, "invalid")))
  expect_equal(shiny::isolate(rv$alias_threshold), ALIAS_CORR_THRESH)
})

test_that("set_design_spec sets multiple fields at once", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_design_spec(rv,
    model_formula = "A + B + A:B",
    alias_formula = "A + B + C + A:B + A:C + B:C",
    threshold = 0.90))

  expect_equal(shiny::isolate(rv$design_model_formula), "A + B + A:B")
  expect_equal(shiny::isolate(rv$design_alias_formula), "A + B + C + A:B + A:C + B:C")
  expect_equal(shiny::isolate(rv$alias_threshold), 0.90)
})

test_that("reset clears Design spec to defaults via make_default_rv", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate({
    set_design_spec(rv,
      model_formula = "A + B",
      alias_formula = "A + B + A:B",
      threshold = 0.80)
  })

  # Simulate full reset
  defs <- make_default_rv()
  shiny::isolate({
    for (nm in names(defs)) rv[[nm]] <- defs[[nm]]
  })

  expect_equal(shiny::isolate(rv$design_model_formula), "")
  expect_equal(shiny::isolate(rv$design_alias_formula), "")
  expect_equal(shiny::isolate(rv$alias_threshold), ALIAS_CORR_THRESH)
})

test_that("backward-compatible load uses defaults for missing Design spec fields", {
  # Simulate an old save file without Design spec fields
  old_state <- list(
    data = data.frame(Y = 1:3, A = c("a","b","a")),
    roles = list(Y = "Response", A = "Factor"),
    formulas = c("Y ~ A" = "Y ~ A")
  )

  rv <- do.call(shiny::reactiveValues, make_default_rv())
  shiny::isolate({
    rv$design_model_formula <- old_state$design_model_formula %||% ""
    rv$design_alias_formula <- old_state$design_alias_formula %||% ""
    rv$alias_threshold      <- old_state$alias_threshold %||% ALIAS_CORR_THRESH
  })

  expect_equal(shiny::isolate(rv$design_model_formula), "")
  expect_equal(shiny::isolate(rv$design_alias_formula), "")
  expect_equal(shiny::isolate(rv$alias_threshold), ALIAS_CORR_THRESH)
})

test_that("make_default_rv includes Model spec fields with expected defaults", {
  defs <- make_default_rv()

  expect_true(all(c(
    "model_active_response", "model_custom_formula", "model_max_way",
    "model_poly_degree", "model_include_covariates", "model_formula_covariates",
    "model_max_covariates_per_formula", "model_include_cov_fac",
    "model_include_blocks", "model_include_block_fac", "model_append_formulas"
  ) %in% names(defs)))

  expect_null(defs$model_active_response)
  expect_equal(defs$model_custom_formula, "")
  expect_equal(defs$model_max_way, MAX_WAY_DEFAULT)
  expect_equal(defs$model_poly_degree, POLY_DEGREE_DEFAULT)
  expect_true(defs$model_include_covariates)
  expect_equal(defs$model_formula_covariates, character(0))
  expect_equal(defs$model_max_covariates_per_formula, 1L)
  expect_false(defs$model_include_cov_fac)
  expect_true(defs$model_include_blocks)
  expect_false(defs$model_include_block_fac)
  expect_false(defs$model_append_formulas)
})

test_that("set_model_custom_formula trims whitespace and handles NULL", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_model_custom_formula(rv, "  Y ~ A + B  "))
  expect_equal(shiny::isolate(rv$model_custom_formula), "Y ~ A + B")

  shiny::isolate(set_model_custom_formula(rv, NULL))
  expect_equal(shiny::isolate(rv$model_custom_formula), "")
})

test_that("set_model_max_way clamps to valid range", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_model_max_way(rv, 3))
  expect_equal(shiny::isolate(rv$model_max_way), 3L)

  shiny::isolate(set_model_max_way(rv, 0))
  expect_equal(shiny::isolate(rv$model_max_way), 1L)

  shiny::isolate(set_model_max_way(rv, 99))
  expect_equal(shiny::isolate(rv$model_max_way), MAX_WAY_LIMIT)

  shiny::isolate(set_model_max_way(rv, NULL))
  expect_equal(shiny::isolate(rv$model_max_way), MAX_WAY_DEFAULT)
})

test_that("set_model_poly_degree clamps to valid range", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_model_poly_degree(rv, 4))
  expect_equal(shiny::isolate(rv$model_poly_degree), 4L)

  shiny::isolate(set_model_poly_degree(rv, 0))
  expect_equal(shiny::isolate(rv$model_poly_degree), 1L)

  shiny::isolate(set_model_poly_degree(rv, 99))
  expect_equal(shiny::isolate(rv$model_poly_degree), POLY_DEGREE_LIMIT)

  shiny::isolate(set_model_poly_degree(rv, NULL))
  expect_equal(shiny::isolate(rv$model_poly_degree), POLY_DEGREE_DEFAULT)
})

test_that("set_model_max_covariates_per_formula clamps to >= 1", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_model_max_covariates_per_formula(rv, 3))
  expect_equal(shiny::isolate(rv$model_max_covariates_per_formula), 3L)

  shiny::isolate(set_model_max_covariates_per_formula(rv, 0))
  expect_equal(shiny::isolate(rv$model_max_covariates_per_formula), 1L)

  shiny::isolate(set_model_max_covariates_per_formula(rv, NULL))
  expect_equal(shiny::isolate(rv$model_max_covariates_per_formula), 1L)
})

test_that("set_model_builder_spec bulk-sets only non-NULL args", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate(set_model_builder_spec(rv,
    max_way = 4,
    include_blocks = FALSE,
    custom_formula = " Y ~ X "
  ))

  expect_equal(shiny::isolate(rv$model_max_way), 4L)
  expect_false(shiny::isolate(rv$model_include_blocks))
  expect_equal(shiny::isolate(rv$model_custom_formula), "Y ~ X")
  # Untouched fields keep defaults
  expect_equal(shiny::isolate(rv$model_poly_degree), POLY_DEGREE_DEFAULT)
  expect_true(shiny::isolate(rv$model_include_covariates))
})

test_that("clear_model_spec resets spec fields to defaults", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  shiny::isolate({
    rv$model_custom_formula <- "Y ~ A + B"
    rv$model_max_way <- 4L
    rv$model_include_blocks <- FALSE
    rv$model_append_formulas <- TRUE
  })

  shiny::isolate(clear_model_spec(rv))

  expect_null(shiny::isolate(rv$model_active_response))
  expect_equal(shiny::isolate(rv$model_custom_formula), "")
  expect_equal(shiny::isolate(rv$model_max_way), MAX_WAY_DEFAULT)
  expect_true(shiny::isolate(rv$model_include_blocks))
  expect_false(shiny::isolate(rv$model_append_formulas))
})

test_that("backward-compatible load uses defaults for missing Model spec fields", {
  old_state <- list(
    data = data.frame(Y = 1:3, A = c("a","b","a")),
    roles = list(Y = "Response", A = "Factor"),
    formulas = c("Y ~ A" = "Y ~ A")
  )

  rv <- do.call(shiny::reactiveValues, make_default_rv())
  shiny::isolate({
    rv$model_active_response            <- old_state$model_active_response
    rv$model_custom_formula             <- old_state$model_custom_formula %||% ""
    rv$model_max_way                    <- old_state$model_max_way %||% MAX_WAY_DEFAULT
    rv$model_poly_degree                <- old_state$model_poly_degree %||% POLY_DEGREE_DEFAULT
    rv$model_include_covariates         <- old_state$model_include_covariates %||% TRUE
    rv$model_formula_covariates         <- old_state$model_formula_covariates %||% character(0)
    rv$model_max_covariates_per_formula <- old_state$model_max_covariates_per_formula %||% 1L
    rv$model_include_cov_fac            <- old_state$model_include_cov_fac %||% FALSE
    rv$model_include_blocks             <- old_state$model_include_blocks %||% TRUE
    rv$model_include_block_fac          <- old_state$model_include_block_fac %||% FALSE
    rv$model_append_formulas            <- old_state$model_append_formulas %||% FALSE
  })

  expect_null(shiny::isolate(rv$model_active_response))
  expect_equal(shiny::isolate(rv$model_custom_formula), "")
  expect_equal(shiny::isolate(rv$model_max_way), MAX_WAY_DEFAULT)
  expect_equal(shiny::isolate(rv$model_poly_degree), POLY_DEGREE_DEFAULT)
  expect_true(shiny::isolate(rv$model_include_covariates))
  expect_equal(shiny::isolate(rv$model_formula_covariates), character(0))
  expect_equal(shiny::isolate(rv$model_max_covariates_per_formula), 1L)
  expect_false(shiny::isolate(rv$model_include_cov_fac))
  expect_true(shiny::isolate(rv$model_include_blocks))
  expect_false(shiny::isolate(rv$model_include_block_fac))
  expect_false(shiny::isolate(rv$model_append_formulas))
})

test_that("clear_model_state resets fitted outputs and MC settings to defaults", {
  rv <- do.call(shiny::reactiveValues, make_default_rv())

  rv$models <- list("Y ~ A" = lm(mpg ~ wt, data = mtcars))
  rv$mc_results <- list(example = data.frame(term = "A"))
  rv$mc_on <- TRUE
  rv$mc_alpha <- 0.10
  rv$mc_terms <- c("A", "A:B")
  rv$mc_methods <- c("tukey", "dunnett")
  rv$vif_df <- data.frame(Term = "wt", M1 = 1.2)
  rv$prune_notes <- list("Y ~ A" = "note")
  rv$model_notes <- list("Y ~ A" = list(warnings = "warn"))
  rv$model_errors <- list("Y ~ A + B" = "singular")
  rv$excluded_obs <- list("Y ~ A" = 3L)

  shiny::isolate(clear_model_state(rv))

  expect_equal(shiny::isolate(rv$models), list())
  expect_equal(shiny::isolate(rv$mc_results), list())
  expect_false(shiny::isolate(rv$mc_on))
  expect_equal(shiny::isolate(rv$mc_alpha), ALPHA_DEFAULT)
  expect_equal(shiny::isolate(rv$mc_terms), character(0))
  expect_equal(shiny::isolate(rv$mc_methods), character(0))
  expect_equal(shiny::isolate(rv$vif_df), data.frame())
  expect_equal(shiny::isolate(rv$prune_notes), list())
  expect_equal(shiny::isolate(rv$model_notes), list())
  expect_equal(shiny::isolate(rv$model_errors), list())
  expect_equal(shiny::isolate(rv$excluded_obs), list())
})
