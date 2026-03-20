# R/app_state.R — State management helpers
# Centralised setters, invalidation helpers, and action functions for rv
# (reactiveValues) fields. All upstream state changes should go through the
# named action functions defined here so invalidation is scoped and auditable.
#
# ═══════════════════════════════════════════════════════════════════════════
# INVALIDATION MATRIX
# ═══════════════════════════════════════════════════════════════════════════
#
# Upstream change          | Invalidates                                   | Action function
# ─────────────────────────┼───────────────────────────────────────────────┼──────────────────────────
# Data load / generate     | design, formulas, models, MC, UI selection   | apply_data_change()
# Role / type / transform  | (same cascade as data change)                | apply_role_change()
# Analysis mode switch     | (same cascade as role change)                | apply_role_change()
# Design spec change       | design outputs only (models NOT touched)     | apply_design_spec_change()
# Model spec change        | formulas, models, MC  (design NOT touched)   | apply_model_spec_change()
# Formula generation       | models, MC results  (spec & design preserved)| apply_generated_formulas()
# Model fitting            | MC results only  (formulas & spec preserved) | apply_fitted_models()
# MC config change         | MC results only  (models preserved)          | apply_mc_config_change()
#
# Each invalidation helper below is scoped to clear ONLY the fields it owns.
# Action functions compose these helpers to implement the rules above.
# ═══════════════════════════════════════════════════════════════════════════

# ---------------------------------------------------------------------------
# Ticket 1: Scoped invalidation helpers (composable building blocks)
# ---------------------------------------------------------------------------

#' Invalidate design-layer outputs (metadata, sim data, design formulas).
#' Does NOT touch model formulas or fitted results.
invalidate_design_outputs <- function(rv) {
  defs <- make_default_rv()
  rv$design_metadata      <- defs$design_metadata
  rv$design_model_formula <- defs$design_model_formula
  rv$design_alias_formula <- defs$design_alias_formula
  rv$alias_threshold      <- defs$alias_threshold
  rv$sim_data             <- defs$sim_data
  invisible(rv)
}

#' Invalidate generated formulas and their associated metadata.
#' Bumps formula_gen to signal downstream reactives.
invalidate_formula_outputs <- function(rv) {
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

#' Invalidate fitted models, VIF, diagnostics, prune notes, and outlier state.
#' Does NOT clear MC config (mc_on, mc_alpha, mc_terms, mc_methods).
invalidate_model_outputs <- function(rv) {
  defs <- make_default_rv()
  rv$models       <- defs$models
  rv$vif_df       <- defs$vif_df
  rv$prune_notes  <- defs$prune_notes
  rv$model_notes  <- defs$model_notes
  rv$model_errors <- defs$model_errors
  rv$excluded_obs <- defs$excluded_obs
  invisible(rv)
}

#' Invalidate multiple-comparisons results and config.
#' When full = TRUE (default), also resets MC config (on/alpha/terms/methods).
#' When full = FALSE, only clears computed MC results.
invalidate_mc_outputs <- function(rv, full = TRUE) {
  defs <- make_default_rv()
  rv$mc_results <- defs$mc_results
  if (full) {
    rv$mc_on      <- defs$mc_on
    rv$mc_alpha   <- defs$mc_alpha
    rv$mc_terms   <- defs$mc_terms
    rv$mc_methods <- defs$mc_methods
  }
  invisible(rv)
}

# ---------------------------------------------------------------------------
# Backward-compatible aliases (delegate to new helpers)
# ---------------------------------------------------------------------------

#' Clear all formula-related state on rv.
#' @seealso invalidate_formula_outputs
clear_formula_state <- function(rv) {
  invalidate_formula_outputs(rv)
}

#' Clear all fitted-model state on rv (including MC settings).
#' @seealso invalidate_model_outputs, invalidate_mc_outputs
clear_model_state <- function(rv) {
  invalidate_model_outputs(rv)
  invalidate_mc_outputs(rv, full = TRUE)
  invisible(rv)
}

# ---------------------------------------------------------------------------
# Ticket 2: Action functions for upstream state changes
# ---------------------------------------------------------------------------

#' Action: new data was loaded/generated.
#' Clears ALL downstream state (design, formulas, models, MC, UI selection).
#' Does NOT set rv$data/roles/col_types — caller handles that.
apply_data_change <- function(rv) {
  invalidate_design_outputs(rv)
  invalidate_formula_outputs(rv)
  invalidate_model_outputs(rv)
  invalidate_mc_outputs(rv, full = TRUE)
  rv$selected_obs <- NULL
  invisible(rv)
}

#' Action: roles, types, or transforms changed.
#' Same cascade as data change (everything downstream is stale).
#' @seealso apply_data_change
apply_role_change <- function(rv) {
  apply_data_change(rv)
}

#' Action: Design spec changed (formula/threshold edits within Design tab).
#' Only invalidates design-derived outputs; Models remain untouched.
apply_design_spec_change <- function(rv) {
  # Design formula changes do NOT propagate to Models unless explicitly pushed.
  # Only reset sim_data since the alias structure may have changed.
  rv$sim_data <- NULL
  invisible(rv)
}

#' Action: Model builder spec changed (response, max_way, etc.).
#' Invalidates generated formulas and everything downstream.
apply_model_spec_change <- function(rv) {
  invalidate_formula_outputs(rv)
  invalidate_model_outputs(rv)
  invalidate_mc_outputs(rv, full = TRUE)
  invisible(rv)
}

#' Action: formulas were generated (or re-generated).
#' Writes formulas + alias metadata atomically, then invalidates fitted state.
#' @param formulas Named character vector of formulas.
#' @param aliases List of alias data frames per formula.
#' @param alias_labels Named list of alias label mappings.
#' @param inestimable Character vector of inestimable terms.
apply_generated_formulas <- function(rv, formulas, aliases = list(),
                                     alias_labels = list(),
                                     inestimable = character()) {
  rv$formulas         <- formulas
  rv$formula_aliases  <- aliases
  rv$alias_labels     <- alias_labels
  rv$inestimable_terms <- inestimable
  rv$formula_gen      <- rv$formula_gen + 1L
  # Fitted models are now stale

  invalidate_model_outputs(rv)
  invalidate_mc_outputs(rv, full = FALSE)
  invisible(rv)
}

#' Action: models were fitted.
#' Writes fitted models + diagnostics atomically. Clears stale MC results.
#' @param models Named list of fitted model objects.
#' @param errors Named list of fitting errors.
#' @param notes Named list of model notes/warnings.
#' @param vif_df VIF summary data frame (or NULL to compute later).
apply_fitted_models <- function(rv, models, errors = list(),
                                notes = list(), vif_df = NULL) {
  rv$models       <- models
  rv$model_errors <- errors
  rv$model_notes  <- notes
  if (!is.null(vif_df)) rv$vif_df <- vif_df
  # Previous MC results are stale (different models)
  invalidate_mc_outputs(rv, full = FALSE)
  invisible(rv)
}

#' Action: MC config changed (but models are unchanged).
#' Only clears MC results; models and formulas remain intact.
apply_mc_config_change <- function(rv) {
  rv$mc_results <- make_default_rv()$mc_results
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
