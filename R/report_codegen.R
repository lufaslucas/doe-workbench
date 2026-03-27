# R/report_codegen.R — Code generation helpers for reproducible reports

# ---------------------------------------------------------------------------
# codegen_data_setup()
# Returns R code string that recreates the dataset + roles + transforms
# ---------------------------------------------------------------------------
codegen_data_setup <- function(rv) {
  lines <- character()
  lines <- c(lines, "# ── Data Setup ──")
  lines <- c(lines, "library(doe.workbench)")
  lines <- c(lines, "")

  # Dataset embedding (inline for small, CSV reference for large)
  df <- rv$data[, setdiff(names(rv$data), ROW_ID_COL), drop = FALSE]
  if (nrow(df) <= 200) {
    # Inline data.frame construction
    col_strs <- vapply(names(df), function(cn) {
      v <- df[[cn]]
      if (is.numeric(v)) {
        paste0("  ", cn, " = c(", paste(v, collapse = ", "), ")")
      } else {
        paste0("  ", cn, " = c(", paste0('"', as.character(v), '"', collapse = ", "), ")")
      }
    }, character(1))
    lines <- c(lines, paste0("df <- data.frame("))
    lines <- c(lines, paste(col_strs, collapse = ",\n"))
    lines <- c(lines, ")")
  } else {
    lines <- c(lines, '# df <- read.csv("your_data.csv", stringsAsFactors = FALSE)')
    lines <- c(lines, paste0("# Dataset has ", nrow(df), " rows x ", ncol(df), " columns"))
  }
  lines <- c(lines, "")

  # Roles
  cols <- setdiff(names(rv$data), ROW_ID_COL)
  role_strs <- vapply(cols, function(cn) {
    paste0('  "', cn, '" = "', rv$roles[[cn]] %||% "Ignore", '"')
  }, character(1))
  lines <- c(lines, paste0("roles <- list("))
  lines <- c(lines, paste(role_strs, collapse = ",\n"))
  lines <- c(lines, ")")
  lines <- c(lines, "")

  # Col types
  type_strs <- vapply(cols, function(cn) {
    paste0('  "', cn, '" = "', rv$col_types[[cn]] %||% "Numeric", '"')
  }, character(1))
  lines <- c(lines, paste0("col_types <- list("))
  lines <- c(lines, paste(type_strs, collapse = ",\n"))
  lines <- c(lines, ")")
  lines <- c(lines, "")

  # Transforms
  tr_cols <- names(rv$transforms)[vapply(rv$transforms, function(x) !is.null(x) && x != "none", logical(1))]
  if (length(tr_cols) > 0) {
    tr_strs <- vapply(tr_cols, function(cn) {
      paste0('  "', cn, '" = "', rv$transforms[[cn]], '"')
    }, character(1))
    lines <- c(lines, paste0("transforms <- list("))
    lines <- c(lines, paste(tr_strs, collapse = ",\n"))
    lines <- c(lines, ")")
  } else {
    lines <- c(lines, "transforms <- list()")
  }
  lines <- c(lines, "")

  # Coding values
  cv_cols <- names(rv$coding_values)[vapply(rv$coding_values, function(x) !is.null(x), logical(1))]
  if (length(cv_cols) > 0) {
    cv_strs <- vapply(cv_cols, function(cn) {
      cv <- rv$coding_values[[cn]]
      paste0('  "', cn, '" = list(low = ', cv$low, ', high = ', cv$high, ')')
    }, character(1))
    lines <- c(lines, paste0("coding_values <- list("))
    lines <- c(lines, paste(cv_strs, collapse = ",\n"))
    lines <- c(lines, ")")
  } else {
    lines <- c(lines, "coding_values <- list()")
  }

  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# codegen_model_fit()
# Returns R code to fit a specific model
# ---------------------------------------------------------------------------
codegen_model_fit <- function(formula_str) {
  paste0(
    '# ── Model Fitting ──\n',
    'formulas <- c("', formula_str, '")\n',
    'names(formulas) <- formulas\n',
    'result <- fit_models(formulas, df, col_types, transforms, coding_values)\n',
    'm <- result$models[[1]]\n',
    'summary(m)'
  )
}

# ---------------------------------------------------------------------------
# codegen_anova()
# Returns code for Type III ANOVA
# ---------------------------------------------------------------------------
codegen_anova <- function(formula_str) {
  paste0(
    codegen_model_fit(formula_str), '\n\n',
    '# ── ANOVA Type III ──\n',
    'car::Anova(m, type = 3)'
  )
}

# ---------------------------------------------------------------------------
# codegen_coef()
# Returns code for coefficient table
# ---------------------------------------------------------------------------
codegen_coef <- function(formula_str) {
  paste0(
    codegen_model_fit(formula_str), '\n\n',
    '# ── Coefficients ──\n',
    'coef_table(result$models, transforms, coding_values)'
  )
}

# ---------------------------------------------------------------------------
# codegen_mc()
# Returns code for multiple comparisons
# ---------------------------------------------------------------------------
codegen_mc <- function(formula_str, spec, method, alpha) {
  paste0(
    codegen_model_fit(formula_str), '\n\n',
    '# ── Multiple Comparisons ──\n',
    'mc <- run_mc(m, spec = "', spec, '", method = "', method,
    '", alpha = ', alpha, ')\n',
    'print(mc)'
  )
}

# ---------------------------------------------------------------------------
# codegen_header()
# Returns the report header with version info
# ---------------------------------------------------------------------------
codegen_header <- function() {
  sha <- tryCatch(
    system2("git", c("-C", system.file(package = "doe.workbench"),
                     "rev-parse", "--short", "HEAD"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) "unknown"
  )
  paste0(
    '# DoE Workbench — Reproducible Analysis\n',
    '# Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '\n',
    '# Package version: ', utils::packageVersion("doe.workbench"), '\n',
    '# Git commit: ', sha, '\n',
    '# \n',
    '# To reproduce: install doe.workbench at the above commit,\n',
    '# then run the code blocks below. Always reconfirm results\n',
    '# against the current package version.\n'
  )
}
