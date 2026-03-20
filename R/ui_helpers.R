# R/ui_helpers.R — UI helper functions (apply_facets, dt_table, plot_3d_scatter)

# ---------------------------------------------------------------------------
# plot_3d_scatter()
# Shared 3D scatter plot builder for design space and preview plots.
# Handles categorical axes (numeric conversion + tick labels), jitter,
# colour-by (continuous/categorical), and shape-by.
#
# Args:
#   df          — data.frame
#   x_col, y_col, z_col — column names for axes
#   colour_col  — column name for colour mapping (NULL or "none" to skip)
#   shape_col   — column name for shape mapping (NULL or "none" to skip)
#   cat_pal     — vector of hex colours for categorical colour scale
#   dcol        — default single colour (hex string)
#   cont_cs     — plotly continuous colour scale (e.g. list(c(0,"blue"), c(1,"red")))
#   colour_label — display label for colour legend (defaults to colour_col)
#   title       — plot title
#   jitter      — jitter amount for discrete axes (0 to disable)
#   marker_size — marker size
# ---------------------------------------------------------------------------
plot_3d_scatter <- function(df, x_col, y_col, z_col,
                            colour_col = NULL, shape_col = NULL,
                            cat_pal = NULL, dcol = "#404040",
                            cont_cs = NULL,
                            colour_label = colour_col,
                            title = "3D Design Space",
                            jitter = 0.08, marker_size = 5) {

  # Convert to numeric positions; track categorical axes for custom tick labels
  cat_axis   <- list(x = FALSE, y = FALSE, z = FALSE)
  axis_levels <- list(x = NULL, y = NULL, z = NULL)

  to_numeric <- function(vals, axis_key) {
    if (is.numeric(vals)) return(vals)
    fv <- as.factor(vals)
    cat_axis[[axis_key]] <<- TRUE
    axis_levels[[axis_key]] <<- levels(fv)
    as.numeric(fv)
  }
  x_vals <- to_numeric(df[[x_col]], "x")
  y_vals <- to_numeric(df[[y_col]], "y")
  z_vals <- to_numeric(df[[z_col]], "z")

  # Add jitter for discrete-looking columns
  if (jitter > 0) {
    if (length(unique(x_vals)) <= 10)
      x_vals <- x_vals + stats::runif(length(x_vals), -jitter, jitter)
    if (length(unique(y_vals)) <= 10)
      y_vals <- y_vals + stats::runif(length(y_vals), -jitter, jitter)
    if (length(unique(z_vals)) <= 10)
      z_vals <- z_vals + stats::runif(length(z_vals), -jitter, jitter)
  }

  hover_text <- paste0(x_col, " = ", df[[x_col]], "\n",
                       y_col, " = ", df[[y_col]], "\n",
                       z_col, " = ", df[[z_col]])

  # Resolve colour / shape
  has_colour <- !is.null(colour_col) && colour_col != "none" && colour_col %in% names(df)
  has_shape  <- !is.null(shape_col) && shape_col != "none" && shape_col %in% names(df)

  # Pre-compute shape symbols
  shape_symbols <- NULL
  if (has_shape) {
    sym_map <- c("circle", "square", "diamond", "cross", "x",
                 "triangle-up", "triangle-down", "star")
    sv <- as.factor(df[[shape_col]])
    shape_symbols <- sym_map[((as.integer(sv) - 1L) %% length(sym_map)) + 1L]
  }

  mk_base <- list(size = marker_size)
  if (!is.null(shape_symbols)) mk_base$symbol <- shape_symbols

  if (is.null(colour_label)) colour_label <- colour_col

  if (has_colour && is.numeric(df[[colour_col]])) {
    # Continuous colour
    cs <- if (!is.null(cont_cs)) cont_cs else "Viridis"
    mk <- c(mk_base, list(color = df[[colour_col]], colorscale = cs,
                           showscale = TRUE,
                           colorbar = list(title = colour_label)))
    p <- plotly::plot_ly(x = x_vals, y = y_vals, z = z_vals,
                         type = "scatter3d", mode = "markers",
                         marker = mk, text = hover_text, hoverinfo = "text")
  } else if (has_colour) {
    cv <- as.factor(df[[colour_col]])
    pal <- if (!is.null(cat_pal) && length(cat_pal) >= length(levels(cv))) {
      cat_pal[seq_along(levels(cv))]
    } else {
      tryCatch(RColorBrewer::brewer.pal(max(3, length(levels(cv))), "Set2"),
               error = function(e) rep("#404040", length(levels(cv))))
    }
    if (has_shape) {
      # Build per-group traces to align shapes with colour groups
      p <- plotly::plot_ly()
      for (gi in seq_along(levels(cv))) {
        g <- levels(cv)[gi]
        idx <- which(cv == g)
        p <- plotly::add_trace(p, x = x_vals[idx], y = y_vals[idx], z = z_vals[idx],
                               type = "scatter3d", mode = "markers",
                               marker = list(size = marker_size, color = pal[gi],
                                             symbol = shape_symbols[idx]),
                               text = hover_text[idx], hoverinfo = "text",
                               name = g)
      }
    } else {
      p <- plotly::plot_ly(x = x_vals, y = y_vals, z = z_vals,
                           color = cv, colors = pal,
                           type = "scatter3d", mode = "markers",
                           marker = mk_base,
                           text = hover_text, hoverinfo = "text")
    }
  } else {
    mk <- c(mk_base, list(color = dcol))
    p <- plotly::plot_ly(x = x_vals, y = y_vals, z = z_vals,
                         type = "scatter3d", mode = "markers",
                         marker = mk, text = hover_text, hoverinfo = "text")
  }

  # Build axis specs with custom tick labels for categorical axes
  make_axis <- function(ax_title, axis_key) {
    ax <- list(title = ax_title)
    if (cat_axis[[axis_key]]) {
      lvls <- axis_levels[[axis_key]]
      ax$tickvals <- seq_along(lvls)
      ax$ticktext <- lvls
      ax$tickmode <- "array"
    }
    ax
  }
  p %>% plotly::layout(
    title = title,
    scene = list(xaxis = make_axis(x_col, "x"),
                 yaxis = make_axis(y_col, "y"),
                 zaxis = make_axis(z_col, "z")),
    margin = list(t = 30))
}

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

# ---------------------------------------------------------------------------
# sim_term_labels()
# Generate coefficient-style labels for simulation effect inputs.
# For factors: "Factor [Level1]" (showing first k-1 levels under sum-to-zero).
# For interactions: "A:B" stays as-is.
# For numerics/polynomials: term name stays as-is.
# ---------------------------------------------------------------------------
sim_term_labels <- function(terms, data) {
  sapply(terms, function(t) {
    # Polynomial: keep as-is
    if (grepl("^I\\(", t)) return(t)
    # Interaction: keep as-is
    if (grepl(":", t)) return(t)
    # Single term — check if it's a factor in the data
    if (t %in% names(data)) {
      v <- data[[t]]
      if (is.factor(v) || is.character(v)) {
        levs <- if (is.factor(v)) levels(v) else sort(unique(v))
        if (length(levs) == 2) {
          # 2-level factor: show "Term [Level1 vs Level2]"
          return(paste0(t, " [", levs[1], " vs ", levs[2], "]"))
        } else if (length(levs) > 2) {
          # Multi-level: this term expands to multiple coefficients
          # but simulation uses single-column approach, so label with first level
          return(paste0(t, " (", length(levs), " levels)"))
        }
      }
    }
    t
  }, USE.NAMES = FALSE)
}
