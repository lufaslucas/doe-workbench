# R/utils.R — Shared utility functions
# Null-coalescing, theme, colour palette, label helpers

# ---------------------------------------------------------------------------
# Null-coalescing helper
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# stamp_row_ids()
# Add a stable ._row_id column to a data frame. Call after every rv$data assignment.
# ---------------------------------------------------------------------------
stamp_row_ids <- function(df) {
  df[[ROW_ID_COL]] <- seq_len(nrow(df))
  df
}

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
# Custom theme: theme_minimal with facet panel outlines
# ---------------------------------------------------------------------------
theme_app <- function(...) {
  theme_minimal(...) +
    theme(
      panel.border = element_rect(colour = "grey70", fill = NA, linewidth = 0.5),
      strip.background = element_rect(colour = "grey70", fill = "grey95", linewidth = 0.5)
    )
}

# ---------------------------------------------------------------------------
# plotly_title() — consistent sans-serif title for plotly layouts
# Usage: layout(title = plotly_title("My title"))
# ---------------------------------------------------------------------------
plotly_title <- function(text, size = 14) {
  list(text = text, font = list(family = "Arial, Helvetica, sans-serif", size = size))
}

# ---------------------------------------------------------------------------
# Utility: short model label
# ---------------------------------------------------------------------------
short_label <- function(label, maxchar = 40) {
  if (nchar(label) <= maxchar) return(label)
  paste0(substr(label, 1, maxchar - 3), "...")
}

# ---------------------------------------------------------------------------
# Utility: colour palette for model overlays
# ---------------------------------------------------------------------------
model_colours <- function(n) {
  palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
               "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
               "#bcbd22", "#17becf")
  rep_len(palette, n)
}

# ---------------------------------------------------------------------------
# Categorical colour palettes (with strategy for many levels)
# ---------------------------------------------------------------------------
# Shell brand palette — core colours extended with complementary hues
shell_cat_palette <- function(n) {
  # Core Shell brand colours
  core <- c("#DD1D21", "#FBCE07", "#404040", "#003B73",
            "#E87722", "#00A651", "#8C6A32", "#6CACE4")
  if (n <= length(core)) return(core[seq_len(n)])
  # For many levels: extend by interpolating around HSL wheel anchored on Shell hues
  extra_needed <- n - length(core)
  hues <- seq(15, 345, length.out = extra_needed + 1)[seq_len(extra_needed)]
  extra <- grDevices::hcl(h = hues, c = 65, l = 55)
  c(core, extra)
}

shell_muted_cat_palette <- function(n) {
  core <- c("#B8423A", "#D4AB2F", "#5C5C5C", "#1A5E8D",
            "#C06928", "#2E8B57", "#9E8564", "#8FAFCF")
  if (n <= length(core)) return(core[seq_len(n)])
  extra_needed <- n - length(core)
  hues <- seq(15, 345, length.out = extra_needed + 1)[seq_len(extra_needed)]
  extra <- grDevices::hcl(h = hues, c = 45, l = 50)
  c(core, extra)
}

default_cat_palette <- function(n) {
  # Tableau 10
  pal <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
           "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
           "#bcbd22", "#17becf")
  if (n <= length(pal)) return(pal[seq_len(n)])
  extra_needed <- n - length(pal)
  hues <- seq(0, 340, length.out = extra_needed + 1)[seq_len(extra_needed)]
  extra <- grDevices::hcl(h = hues, c = 60, l = 55)
  c(pal, extra)
}

get_cat_palette <- function(theme_name, n = 8) {
  switch(theme_name,
    "shell"       = shell_cat_palette(n),
    "shell_muted" = shell_muted_cat_palette(n),
    "default"     = default_cat_palette(n),
    "viridis"     = grDevices::hcl.colors(n, "viridis"),
    "set2"        = if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Set2")[seq_len(n)]
                    else c(RColorBrewer::brewer.pal(8, "Set2"),
                           grDevices::hcl(seq(15, 345, length.out = n - 7)[seq_len(n - 8)], 55, 65)),
    "dark2"       = if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Dark2")[seq_len(n)]
                    else c(RColorBrewer::brewer.pal(8, "Dark2"),
                           grDevices::hcl(seq(15, 345, length.out = n - 7)[seq_len(n - 8)], 65, 45)),
    default_cat_palette(n)
  )
}

# ---------------------------------------------------------------------------
# Continuous colour scale helpers
# ---------------------------------------------------------------------------
get_cont_colours <- function(theme_name, n = 256) {
  switch(theme_name,
    "shell_warm"  = grDevices::colorRampPalette(c("#FBCE07", "#E87722", "#DD1D21"))(n),
    "shell_cool"  = grDevices::colorRampPalette(c("#6CACE4", "#003B73", "#404040"))(n),
    "viridis"     = grDevices::hcl.colors(n, "viridis"),
    "inferno"     = grDevices::hcl.colors(n, "inferno"),
    "plasma"      = grDevices::hcl.colors(n, "plasma"),
    "bluered"     = grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(n),
    grDevices::hcl.colors(n, "viridis")
  )
}

# Convert continuous theme to plotly-compatible colorscale
cont_plotly_colorscale <- function(theme_name) {
  cols <- get_cont_colours(theme_name, 10)
  lapply(seq_along(cols), function(i) {
    list((i - 1) / (length(cols) - 1), cols[i])
  })
}

# ggplot2 scale functions for categorical themes
scale_colour_cat <- function(theme_name, ...) {
  function(n) get_cat_palette(theme_name, n)
}

# ---------------------------------------------------------------------------
# build_colour_choices() — unified colour-by dropdown construction
# Pass all_cols for a flat list (Explore), or factors/blocks/covariates for
# labelled groups (Results). treatment_label = NULL omits .treatment.
# ---------------------------------------------------------------------------
build_colour_choices <- function(factors = NULL, blocks = NULL, covariates = NULL,
                                 all_cols = NULL, treatment_label = NULL,
                                 include_treatment = TRUE) {
  ch <- c("None" = "none")
  if (include_treatment && !is.null(treatment_label))
    ch <- c(ch, setNames(".treatment", treatment_label))
  if (!is.null(all_cols)) {
    for (cn in all_cols) ch <- c(ch, setNames(cn, cn))
  } else {
    if (!is.null(factors))
      for (f in factors) ch <- c(ch, setNames(f, f))
    if (!is.null(blocks))
      for (b in blocks) ch <- c(ch, setNames(b, paste0(b, " (block)")))
    if (!is.null(covariates))
      for (cv in covariates) ch <- c(ch, setNames(cv, paste0(cv, " (cov)")))
  }
  ch
}

# ---------------------------------------------------------------------------
# Smart jitter: only jitter replicated x-groups
# ---------------------------------------------------------------------------
smart_jitter_width <- function(data, x_col, jitter_amount, only_replicated = TRUE) {
  if (jitter_amount <= 0) return(0)
  if (!only_replicated) return(jitter_amount)
  # Check if there's any replication at all
  counts <- table(data[[x_col]])
  if (all(counts <= 1)) return(0)
  jitter_amount
}

# Position function for replicated-only jitter
position_smart_jitter <- function(data, x_col, jitter_amount, only_replicated = TRUE) {
  if (jitter_amount <= 0 || (only_replicated && all(table(data[[x_col]]) <= 1))) {
    position_identity()
  } else {
    position_jitter(width = jitter_amount, height = 0)
  }
}

# ---------------------------------------------------------------------------
# format_pvalue()
# Format p-values for display: <0.0001 for tiny, conservative rounding near alpha
# ---------------------------------------------------------------------------
format_pvalue <- function(p, alpha = ALPHA_DEFAULT, digits = PVALUE_DIGITS) {
  vapply(p, function(pv) {
    if (is.na(pv)) return(NA_character_)
    # Tiny p-values that round to 0
    if (round(pv, digits) == 0) return(PVALUE_FLOOR_LABEL)
    # Conservative rounding near alpha boundary:
    # If true p > alpha but naive rounding would show exactly alpha, round UP
    rounded <- round(pv, digits)
    if (pv > alpha && rounded <= alpha) {
      # Ceiling to next unit at given precision
      unit <- 10^(-digits)
      return(formatC(ceiling(pv / unit) * unit, format = "f", digits = digits))
    }
    # If true p <= alpha but naive rounding would show > alpha, round DOWN
    if (pv <= alpha && rounded > alpha) {
      unit <- 10^(-digits)
      return(formatC(floor(pv / unit) * unit, format = "f", digits = digits))
    }
    formatC(rounded, format = "f", digits = digits)
  }, character(1))
}

# ---------------------------------------------------------------------------
# format_pvalue_dt()
# Pre-format p-value columns in a data.frame for DT display.
# Replaces numeric p-value cols with formatted character strings while
# keeping a hidden shadow column (prefixed ._p_) for styleInterval colouring.
# ---------------------------------------------------------------------------
format_pvalue_dt <- function(df, p_cols, alpha = ALPHA_DEFAULT, digits = PVALUE_DIGITS) {
  for (pc in p_cols) {
    if (!(pc %in% names(df))) next
    shadow <- paste0("._p_", pc)
    df[[shadow]] <- df[[pc]]  # Keep numeric copy for conditional styling
    df[[pc]] <- format_pvalue(df[[pc]], alpha = alpha, digits = digits)
  }
  df
}

# ---------------------------------------------------------------------------
# relabel_alias_terms()
# Replace term names with alias-aware display labels
# ---------------------------------------------------------------------------
relabel_alias_terms <- function(term_vec, alias_labels) {
  if (length(alias_labels) == 0) return(term_vec)
  sapply(term_vec, function(t) {
    if (t %in% names(alias_labels)) alias_labels[[t]] else t
  }, USE.NAMES = FALSE)
}

# ---------------------------------------------------------------------------
# colour_nests_in_group()
# Check if colour variable is constant within each line group.
# Returns TRUE if every unique group value has exactly one colour value
# (i.e. colour "nests" within group). Only meaningful for categorical colour.
# ---------------------------------------------------------------------------
colour_nests_in_group <- function(df, colour_col, group_col) {
  if (is.null(colour_col) || is.null(group_col)) return(FALSE)
  if (!(colour_col %in% names(df)) || !(group_col %in% names(df))) return(FALSE)
  if (is.numeric(df[[colour_col]])) return(FALSE)
  group_vals <- split(as.character(df[[colour_col]]), df[[group_col]])
  all(vapply(group_vals, function(v) length(unique(v)) == 1L, logical(1)))
}

# ---------------------------------------------------------------------------
# Linked Selection helpers
# ---------------------------------------------------------------------------

#' Per-row alpha vector based on selection state
#' @param row_ids Integer vector of ._row_id values for the data frame
#' @param selected_obs NULL (no selection) or integer vector of selected row_ids
#' @return Numeric vector of alpha values, same length as row_ids
selection_alpha <- function(row_ids, selected_obs,
                            alpha_normal   = SEL_ALPHA_NORMAL,
                            alpha_selected = SEL_ALPHA_SELECTED,
                            alpha_dimmed   = SEL_ALPHA_DIMMED) {
  if (is.null(selected_obs)) return(rep(alpha_normal, length(row_ids)))
  ifelse(row_ids %in% selected_obs, alpha_selected, alpha_dimmed)
}

#' Post-process a ggplotly object to dim unselected points
#' Iterates over plotly traces, finds scatter traces with keys, and sets
#' marker$opacity per-point. Returns pp unchanged if selected_obs is NULL.
#' @param pp Plotly object from ggplotly()
#' @param row_ids The ._row_id values in data-frame order
#' @param selected_obs NULL or integer vector of selected row_ids
# ---------------------------------------------------------------------------
# is_locked() — read-only guard for state-mutating observers
# Returns TRUE (+ notification) if session is locked; caller should return().
# ---------------------------------------------------------------------------
is_locked <- function(rv, action_label = "This action") {
  if (isTRUE(rv$read_only)) {
    showNotification(
      paste0(action_label, " is disabled in read-only mode. Unlock the session first."),
      type = "warning", duration = 4
    )
    TRUE
  } else {
    FALSE
  }
}

apply_selection_style <- function(pp, row_ids, selected_obs) {
  if (is.null(selected_obs)) return(pp)
  for (i in seq_along(pp$x$data)) {
    tr <- pp$x$data[[i]]
    # Only modify scatter traces that have keys (observation-level points)
    has_markers <- !is.null(tr$mode) && grepl("markers", tr$mode)
    has_keys    <- !is.null(tr$key)
    if (has_markers && has_keys) {
      keys <- as.integer(unlist(tr$key))
      tr_alphas <- selection_alpha(keys, selected_obs)
      pp$x$data[[i]]$marker$opacity <- tr_alphas
    }
  }
  pp
}
