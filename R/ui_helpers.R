# R/ui_helpers.R — UI helper functions (apply_facets, dt_table)

# ---------------------------------------------------------------------------
# apply_facets()
# Generic ggplot faceting helper. Always uses label_both labeller.
# Two calling conventions:
#   1) Single-variable mode:  apply_facets(p, facet_var = "x", mode = "row"|"col"|"wrap")
#   2) Multi-variable mode:   apply_facets(p, row_var = "a", col_var = "b") or
#                              apply_facets(p, wrap_var = "c")
# Extra args (...) forwarded to facet_* (e.g. scales = "free_x")
# ---------------------------------------------------------------------------
apply_facets <- function(p, facet_var = NULL, mode = "wrap",
                         row_var = NULL, col_var = NULL, wrap_var = NULL, ...) {
  lbl <- ggplot2::label_both

  # Single-variable mode
  if (!is.null(facet_var) && facet_var != "none") {
    if (mode == "row") {
      return(p + facet_grid(reformulate(".", facet_var), labeller = lbl, ...))
    } else if (mode == "col") {
      return(p + facet_grid(reformulate(facet_var, "."), labeller = lbl, ...))
    } else {
      return(p + facet_wrap(reformulate(facet_var), labeller = lbl, ...))
    }
  }

  # Multi-variable mode
  has_row  <- !is.null(row_var) && row_var != "none"
  has_col  <- !is.null(col_var) && col_var != "none"
  has_wrap <- !is.null(wrap_var) && wrap_var != "none"

  if (has_wrap) {
    p + facet_wrap(reformulate(wrap_var), labeller = lbl, ...)
  } else if (has_row && has_col) {
    p + facet_grid(reformulate(col_var, row_var), labeller = lbl, ...)
  } else if (has_row) {
    p + facet_wrap(reformulate(row_var), ncol = 1, labeller = lbl, ...)
  } else if (has_col) {
    p + facet_wrap(reformulate(col_var), nrow = 1, labeller = lbl, ...)
  } else {
    p
  }
}

# ---------------------------------------------------------------------------
# dt_table()
# DT::datatable wrapper with export buttons (Copy/CSV/Excel)
# ---------------------------------------------------------------------------
dt_table <- function(data, ..., options = list(), extensions = character(),
                     dom = "Bfrtip", filter = "none") {
  # Merge export buttons into options
  options$dom     <- dom
  options$buttons <- c("copy", "csv", "excel")
  if (is.null(options$scrollX)) options$scrollX <- TRUE
  DT::datatable(data, ..., extensions = c("Buttons", extensions),
                options = options, filter = filter)
}
