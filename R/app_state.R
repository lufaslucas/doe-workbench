# R/app_state.R — State management helpers
# Centralised setters and clearers for rv (reactiveValues) fields.
# Extracted from utils.R + new Model spec setters.

# ---------------------------------------------------------------------------
# State-clearing helpers
# Centralised routines so modules don't need to know every field name.
# ---------------------------------------------------------------------------

#' Clear all formula-related state on rv.
#' Call when formulas are regenerated or data/roles change.
clear_formula_state <- function(rv) {
  defs <- make_default_rv()
  rv$formulas                 <- defs$formulas
  rv$formula_gen              <- rv$formula_gen + 1L
  rv$formula_aliases          <- defs$formula_aliases
  rv$alias_labels             <- defs$alias_labels
  rv$inestimable_terms        <- defs$inestimable_terms
  rv$pending_alias_resolution <- defs$pending_alias_resolution
  rv$skip_auto_formula        <- defs$skip_auto_formula
  invisible(rv)
}

#' Clear all fitted-model state on rv (including MC settings).
#' Call when models need to be invalidated (e.g., formulas changed).
clear_model_state <- function(rv) {
  defs <- make_default_rv()
  rv$models       <- defs$models
  rv$mc_results   <- defs$mc_results
  rv$mc_on        <- defs$mc_on
  rv$mc_alpha     <- defs$mc_alpha
  rv$mc_terms     <- defs$mc_terms
  rv$mc_methods   <- defs$mc_methods
  rv$vif_df       <- defs$vif_df
  rv$prune_notes  <- defs$prune_notes
  rv$model_notes  <- defs$model_notes
  rv$model_errors <- defs$model_errors
  rv$excluded_obs <- defs$excluded_obs
  invisible(rv)
}

# ---------------------------------------------------------------------------
# Design spec setters — centralise normalisation for Design formula state
# ---------------------------------------------------------------------------
set_design_model_formula <- function(rv, value) {
  rv$design_model_formula <- trimws(value %||% "")
  invisible(rv)
}

set_design_alias_formula <- function(rv, value) {
  rv$design_alias_formula <- trimws(value %||% "")
  invisible(rv)
}

set_alias_threshold <- function(rv, value) {
  v <- as.numeric(value)
  if (is.na(v) || v <= 0 || v > 1) v <- ALIAS_CORR_THRESH
  rv$alias_threshold <- v
  invisible(rv)
}

set_design_spec <- function(rv, model_formula = NULL, alias_formula = NULL,
                            threshold = NULL) {
  if (!is.null(model_formula)) set_design_model_formula(rv, model_formula)
  if (!is.null(alias_formula)) set_design_alias_formula(rv, alias_formula)
  if (!is.null(threshold))     set_alias_threshold(rv, threshold)
  invisible(rv)
}

# ---------------------------------------------------------------------------
# Model spec setters — centralise normalisation for Model builder state
# ---------------------------------------------------------------------------

#' Set the active response variable name.
set_model_active_response <- function(rv, value) {
  rv$model_active_response <- value %||% NULL
  invisible(rv)
}

#' Set the custom formula text (trimmed).
set_model_custom_formula <- function(rv, value) {
  rv$model_custom_formula <- trimws(value %||% "")
  invisible(rv)
}

#' Set max factor interaction order, clamped to 1..MAX_WAY_LIMIT.
set_model_max_way <- function(rv, value) {
  v <- as.integer(value %||% MAX_WAY_DEFAULT)
  if (is.na(v)) v <- MAX_WAY_DEFAULT
  v <- max(1L, min(v, MAX_WAY_LIMIT))
  rv$model_max_way <- v
  invisible(rv)
}

#' Set polynomial degree, clamped to 1..POLY_DEGREE_LIMIT.
set_model_poly_degree <- function(rv, value) {
  v <- as.integer(value %||% POLY_DEGREE_DEFAULT)
  if (is.na(v)) v <- POLY_DEGREE_DEFAULT
  v <- max(1L, min(v, POLY_DEGREE_LIMIT))
  rv$model_poly_degree <- v
  invisible(rv)
}

#' Set whether covariates are included in formula generation.
set_model_include_covariates <- function(rv, value) {
  rv$model_include_covariates <- isTRUE(value)
  invisible(rv)
}

#' Set the list of covariate column names to include.
set_model_formula_covariates <- function(rv, value) {
  rv$model_formula_covariates <- value %||% character(0)
  invisible(rv)
}

#' Set max covariates per formula, clamped to >= 1.
set_model_max_covariates_per_formula <- function(rv, value) {
  v <- as.integer(value %||% 1L)
  if (is.na(v) || v < 1L) v <- 1L
  rv$model_max_covariates_per_formula <- v
  invisible(rv)
}

#' Set whether covariate x factor interactions are included.
set_model_include_cov_fac <- function(rv, value) {
  rv$model_include_cov_fac <- isTRUE(value)
  invisible(rv)
}

#' Set whether blocks are included in formulas.
set_model_include_blocks <- function(rv, value) {
  rv$model_include_blocks <- isTRUE(value)
  invisible(rv)
}

#' Set whether block x factor interactions are included.
set_model_include_block_fac <- function(rv, value) {
  rv$model_include_block_fac <- isTRUE(value)
  invisible(rv)
}

#' Set whether formula generation appends to existing formulas.
set_model_append_formulas <- function(rv, value) {
  rv$model_append_formulas <- isTRUE(value)
  invisible(rv)
}

#' Bulk setter for model builder spec. Only non-NULL arguments are applied.
set_model_builder_spec <- function(rv,
                                   active_response = NULL,
                                   custom_formula = NULL,
                                   max_way = NULL,
                                   poly_degree = NULL,
                                   include_covariates = NULL,
                                   formula_covariates = NULL,
                                   max_covariates_per_formula = NULL,
                                   include_cov_fac = NULL,
                                   include_blocks = NULL,
                                   include_block_fac = NULL,
                                   append_formulas = NULL) {
  if (!is.null(active_response))            set_model_active_response(rv, active_response)
  if (!is.null(custom_formula))             set_model_custom_formula(rv, custom_formula)
  if (!is.null(max_way))                    set_model_max_way(rv, max_way)
  if (!is.null(poly_degree))                set_model_poly_degree(rv, poly_degree)
  if (!is.null(include_covariates))         set_model_include_covariates(rv, include_covariates)
  if (!is.null(formula_covariates))         set_model_formula_covariates(rv, formula_covariates)
  if (!is.null(max_covariates_per_formula)) set_model_max_covariates_per_formula(rv, max_covariates_per_formula)
  if (!is.null(include_cov_fac))            set_model_include_cov_fac(rv, include_cov_fac)
  if (!is.null(include_blocks))             set_model_include_blocks(rv, include_blocks)
  if (!is.null(include_block_fac))          set_model_include_block_fac(rv, include_block_fac)
  if (!is.null(append_formulas))            set_model_append_formulas(rv, append_formulas)
  invisible(rv)
}

#' Reset all model spec fields to defaults (does NOT clear analysis outputs).
clear_model_spec <- function(rv) {
  defs <- make_default_rv()
  rv$model_active_response            <- defs$model_active_response
  rv$model_custom_formula             <- defs$model_custom_formula
  rv$model_max_way                    <- defs$model_max_way
  rv$model_poly_degree                <- defs$model_poly_degree
  rv$model_include_covariates         <- defs$model_include_covariates
  rv$model_formula_covariates         <- defs$model_formula_covariates
  rv$model_max_covariates_per_formula <- defs$model_max_covariates_per_formula
  rv$model_include_cov_fac            <- defs$model_include_cov_fac
  rv$model_include_blocks             <- defs$model_include_blocks
  rv$model_include_block_fac          <- defs$model_include_block_fac
  rv$model_append_formulas            <- defs$model_append_formulas
  invisible(rv)
}
