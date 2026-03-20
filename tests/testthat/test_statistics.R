# tests/testthat/test_statistics.R
# Unit tests for backend statistical functions (no Shiny context needed)

library(testthat)

# ---------------------------------------------------------------------------
# Load package R files
# ---------------------------------------------------------------------------
pkg_root <- normalizePath(file.path(dirname(dirname(getwd()))))
r_dir    <- file.path(pkg_root, "R")
if (!dir.exists(r_dir)) { pkg_root <- getwd(); r_dir <- file.path(pkg_root, "R") }

suppressPackageStartupMessages({
  library(car)
  library(emmeans)
  library(lmtest)
})

for (f in sort(list.files(r_dir, pattern = "\\.R$", full.names = TRUE))) {
  if (grepl("^mod_|^ui_helpers", basename(f))) next
  tryCatch(source(f, local = FALSE), error = function(e) NULL)
}


# ============================================================================
# format_pvalue()
# ============================================================================

test_that("format_pvalue returns correct floor label for tiny values", {
  expect_equal(format_pvalue(0),      PVALUE_FLOOR_LABEL)
  expect_equal(format_pvalue(1e-10),  PVALUE_FLOOR_LABEL)
  expect_equal(format_pvalue(1e-5),   PVALUE_FLOOR_LABEL)
  expect_equal(format_pvalue(0.00001), PVALUE_FLOOR_LABEL)
})

test_that("format_pvalue returns correct string for normal values", {
  expect_equal(format_pvalue(0.05),   "0.0500")
  expect_equal(format_pvalue(0.001),  "0.0010")
  expect_equal(format_pvalue(0.5),    "0.5000")
  expect_equal(format_pvalue(0.9999), "0.9999")
  expect_equal(format_pvalue(1.0),    "1.0000")
})

test_that("format_pvalue conservative rounding near alpha boundary", {
  # p > alpha but naive round would show alpha => round UP
  expect_equal(format_pvalue(0.05001), "0.0501")
  expect_equal(format_pvalue(0.05004), "0.0501")

  # p = 0.04999 is <= 0.05 (genuinely significant), rounds to 0.0500
  # This is correct — 0.0500 <= 0.05, so no "disagree" case
  expect_equal(format_pvalue(0.04999), "0.0500")

  # p < alpha but naive round would show > alpha => round DOWN
  # Need value where truth < 0.05 but round(x, 4) > 0.05
  # E.g., 0.049996 rounds to 0.0500 which is NOT > 0.05, so no trigger
  # Actually this case is very rare at 4dp — 0.04999x always rounds to 0.0500 <= 0.05
})

test_that("format_pvalue handles NA", {
  expect_true(is.na(format_pvalue(NA)))
})

test_that("format_pvalue is vectorised", {
  result <- format_pvalue(c(0.001, 0.05, 0.9, NA, 1e-10))
  expect_length(result, 5)
  expect_equal(result[1], "0.0010")
  expect_true(is.na(result[4]))
  expect_equal(result[5], PVALUE_FLOOR_LABEL)
})


# ============================================================================
# format_pvalue_dt()
# ============================================================================

test_that("format_pvalue_dt creates shadow columns", {
  df <- data.frame(
    Term = c("A", "B", "C"),
    p_val = c(0.001, 0.0500001, NA)
  )
  result <- format_pvalue_dt(df, "p_val")

  # Shadow column exists

  expect_true("._p_p_val" %in% names(result))
  # Shadow is numeric
  expect_true(is.numeric(result[["._p_p_val"]]))
  # Original is now character
  expect_true(is.character(result[["p_val"]]))
  # Values are correct
  expect_equal(result$p_val[1], "0.0010")
  expect_equal(result$p_val[2], "0.0501")  # conservative rounding
  expect_true(is.na(result$p_val[3]))
})

test_that("format_pvalue_dt skips missing columns", {
  df <- data.frame(Term = "A", other = 1)
  result <- format_pvalue_dt(df, "nonexistent")
  expect_equal(ncol(result), ncol(df))
})


# ============================================================================
# rmse_summary()
# ============================================================================

test_that("rmse_summary returns correct columns", {
  set.seed(1)
  df <- data.frame(x = 1:20, y = 1:20 + rnorm(20))
  m <- lm(y ~ x, data = df)
  rs <- rmse_summary(list("M1" = m))

  expected_cols <- c("Model", "RMSE", "R2", "Adj_R2", "AIC", "BIC",
                     "PRESS_RMSE", "PRESS_R2", "df_resid")
  expect_true(all(expected_cols %in% names(rs)))
  expect_equal(nrow(rs), 1L)
})

test_that("rmse_summary PRESS_R2 < R2", {
  set.seed(1)
  df <- data.frame(x = 1:20, y = 1:20 + rnorm(20))
  m <- lm(y ~ x, data = df)
  rs <- rmse_summary(list("M1" = m))

  expect_true(rs$PRESS_R2[1] < rs$R2[1])
  expect_true(rs$PRESS_RMSE[1] > rs$RMSE[1])
})

test_that("rmse_summary handles multiple models", {
  set.seed(1)
  df <- data.frame(x = 1:20, y = 1:20 + rnorm(20))
  m1 <- lm(y ~ x, data = df)
  m2 <- lm(y ~ 1, data = df)
  rs <- rmse_summary(list("Full" = m1, "Intercept" = m2))

  expect_equal(nrow(rs), 2L)
  expect_true(rs$R2[1] > rs$R2[2])
})


# ============================================================================
# type3_wide()
# ============================================================================

test_that("type3_wide handles single model correctly", {
  set.seed(42)
  df <- data.frame(
    A = factor(rep(c("L", "H"), each = 10)),
    B = factor(rep(c("X", "Y"), 10)),
    Y = rnorm(20, mean = rep(c(5, 10), each = 10))
  )
  m <- lm(Y ~ A + B + A:B, data = df, contrasts = list(A = contr.sum, B = contr.sum))
  wide <- type3_wide(list("M1" = m))

  expect_true(nrow(wide) >= 2)
  expect_true("term" %in% names(wide))
  f_cols <- grep("_F$", names(wide), value = TRUE)
  expect_length(f_cols, 1)
})

test_that("type3_wide handles multiple models with different terms", {
  set.seed(42)
  df <- data.frame(
    A = factor(rep(c("L", "H"), each = 10)),
    B = factor(rep(c("X", "Y"), 10)),
    Y = rnorm(20, mean = rep(c(5, 10), each = 10))
  )
  m1 <- lm(Y ~ A + B + A:B, data = df, contrasts = list(A = contr.sum, B = contr.sum))
  m2 <- lm(Y ~ A, data = df, contrasts = list(A = contr.sum))
  wide <- type3_wide(list("Full" = m1, "Reduced" = m2))

  # Both models should have F columns
  f_cols <- grep("_F$", names(wide), value = TRUE)
  expect_length(f_cols, 2)
  # Reduced model should have NA for B and A:B
  expect_true(any(is.na(wide[["Reduced_F"]])))
})

test_that("type3_wide skips saturated models", {
  # 4 observations, 4 parameters => 0 residual df
  df <- data.frame(
    A = factor(c("L", "H", "L", "H")),
    B = factor(c("X", "X", "Y", "Y")),
    Y = c(1, 2, 3, 4)
  )
  m <- lm(Y ~ A + B + A:B, data = df, contrasts = list(A = contr.sum, B = contr.sum))
  wide <- type3_wide(list("Saturated" = m))

  # Should return empty or skip
  expect_true(nrow(wide) == 0 || !any(grepl("_F$", names(wide))))
})


# ============================================================================
# classify_anova_terms()
# ============================================================================

test_that("classify_anova_terms assigns correct categories", {
  terms <- c("A", "B", "Block", "Temp", "A:B", "A:Block", "A:Temp",
             "I(Temp^2)", "Residuals")
  roles <- classify_anova_terms(
    terms,
    factor_cols = c("A", "B"),
    block_cols  = "Block",
    cov_cols    = "Temp"
  )

  expect_equal(roles[["A"]],        "Factor")
  expect_equal(roles[["B"]],        "Factor")
  expect_equal(roles[["Block"]],    "Block")
  expect_equal(roles[["Temp"]],     "Covariate")
  expect_equal(roles[["A:B"]],      "Factor_Interaction")
  expect_equal(roles[["A:Block"]],  "Block_Interaction")
  expect_equal(roles[["A:Temp"]],   "Covariate_Interaction")
  expect_equal(roles[["Residuals"]], "Residuals")
})


# ============================================================================
# vif_summary()
# ============================================================================

test_that("vif_summary returns wide data.frame", {
  set.seed(42)
  df <- data.frame(
    A = factor(rep(c("L", "H"), each = 10)),
    x = rnorm(20),
    Y = rnorm(20)
  )
  m <- lm(Y ~ A + x, data = df, contrasts = list(A = contr.sum))
  vs <- vif_summary(list("M1" = m))

  expect_true(nrow(vs) >= 2)
  expect_true("Term" %in% names(vs))
  expect_true("M1" %in% names(vs))
})

test_that("vif_summary returns empty for single-predictor models", {
  set.seed(42)
  df <- data.frame(A = factor(rep(c("L", "H"), each = 10)), Y = rnorm(20))
  m <- lm(Y ~ A, data = df, contrasts = list(A = contr.sum))
  vs <- vif_summary(list("M1" = m))

  expect_true(nrow(vs) == 0)
})


# ============================================================================
# run_mc() — Multiple Comparisons
# ============================================================================

test_that("run_mc Student method returns correct structure", {
  set.seed(42)
  df <- data.frame(
    Trt = factor(rep(LETTERS[1:3], each = 5)),
    Y   = c(rnorm(5, 5), rnorm(5, 8), rnorm(5, 12))
  )
  m  <- lm(Y ~ Trt, data = df, contrasts = list(Trt = contr.sum))
  mc <- run_mc(m, spec = "Trt", method = "student", alpha = 0.05)

  expect_equal(nrow(mc), 3L)
  expect_true("estimate" %in% names(mc))
  expect_true("SE" %in% names(mc))
  expect_true("p.value" %in% names(mc))
  expect_true("lower.CL" %in% names(mc))
  expect_true("upper.CL" %in% names(mc))
})

test_that("run_mc Tukey method adjusts p-values upward", {
  set.seed(42)
  df <- data.frame(
    Trt = factor(rep(LETTERS[1:3], each = 5)),
    Y   = c(rnorm(5, 5), rnorm(5, 8), rnorm(5, 12))
  )
  m  <- lm(Y ~ Trt, data = df, contrasts = list(Trt = contr.sum))
  mc_s <- run_mc(m, spec = "Trt", method = "student", alpha = 0.05)
  mc_t <- run_mc(m, spec = "Trt", method = "tukey",   alpha = 0.05)

  # Tukey p-values should be >= Student p-values (more conservative)
  expect_true(all(mc_t$p.value >= mc_s$p.value - 1e-10))
})

test_that("run_mc Dunnett returns k-1 comparisons", {
  set.seed(42)
  df <- data.frame(
    Trt = factor(rep(LETTERS[1:4], each = 5)),
    Y   = c(rnorm(5, 5), rnorm(5, 6), rnorm(5, 8), rnorm(5, 12))
  )
  m  <- lm(Y ~ Trt, data = df, contrasts = list(Trt = contr.sum))
  mc <- run_mc(m, spec = "Trt", method = "dunnett", control = "A", alpha = 0.05)

  expect_equal(nrow(mc), 3L)  # k-1 = 4-1 = 3
})

test_that("run_mc custom method with selected pairs", {
  set.seed(42)
  df <- data.frame(
    Trt = factor(rep(LETTERS[1:4], each = 5)),
    Y   = c(rnorm(5, 5), rnorm(5, 6), rnorm(5, 8), rnorm(5, 12))
  )
  m  <- lm(Y ~ Trt, data = df, contrasts = list(Trt = contr.sum))

  # Get all pairwise labels
  em <- emmeans::emmeans(m, "Trt")
  all_pairs <- emmeans::contrast(em, method = "pairwise", adjust = "none")
  all_df <- as.data.frame(summary(all_pairs))

  # Select only 2 specific pairs
  sel <- all_df$contrast[1:2]
  mc <- run_mc(m, spec = "Trt", method = "custom", selected_pairs = sel, alpha = 0.05)

  expect_equal(nrow(mc), 2L)
})


# ============================================================================
# backward_eliminate()
# ============================================================================

test_that("backward_eliminate removes non-significant terms", {
  set.seed(42)
  df <- data.frame(
    A = factor(rep(c("L", "H"), each = 15)),
    noise = rnorm(30),
    Y = c(rnorm(15, 5), rnorm(15, 10))
  )
  m <- lm(Y ~ A + noise, data = df, contrasts = list(A = contr.sum))
  reduced <- backward_eliminate(m, alpha = 0.05)

  remaining <- attr(terms(reduced), "term.labels")
  expect_true("A" %in% remaining)
  # noise should be removed (p >> 0.05)
  expect_false("noise" %in% remaining)
})

test_that("backward_eliminate keeps all terms when all significant", {
  set.seed(42)
  df <- data.frame(
    A = factor(rep(c("L", "H"), each = 15)),
    x = rnorm(30),
    Y = c(rnorm(15, 5), rnorm(15, 10)) + 3 * rnorm(30)  # x has effect implicitly
  )
  df$Y <- df$Y + 5 * as.numeric(df$A == "H") + 2 * df$x
  m <- lm(Y ~ A + x, data = df, contrasts = list(A = contr.sum))
  reduced <- backward_eliminate(m, alpha = 0.05)

  remaining <- attr(terms(reduced), "term.labels")
  expect_true("A" %in% remaining)
  expect_true("x" %in% remaining)
})


# ============================================================================
# run_diagnostics()
# ============================================================================

test_that("run_diagnostics returns valid structure", {
  set.seed(42)
  df <- data.frame(x = 1:30, y = 1:30 + rnorm(30, 0, 2))
  m <- lm(y ~ x, data = df)
  diags <- run_diagnostics(m)

  expect_true(is.list(diags))
  expect_true(all(c("normality", "heteroscedasticity", "outliers",
                     "influence", "overall") %in% names(diags)))

  # Each should have level and message
  for (nm in c("normality", "heteroscedasticity", "outliers", "influence", "overall")) {
    expect_true("level" %in% names(diags[[nm]]))
    expect_true("message" %in% names(diags[[nm]]))
    expect_true(diags[[nm]]$level %in% c("green", "amber", "red"))
  }
})


# ============================================================================
# get_lsmeans_df()
# ============================================================================

test_that("get_lsmeans_df returns correct LS means", {
  set.seed(42)
  df <- data.frame(
    Trt = factor(rep(c("A", "B", "C"), each = 5)),
    Y   = c(rnorm(5, 5), rnorm(5, 8), rnorm(5, 12))
  )
  m   <- lm(Y ~ Trt, data = df, contrasts = list(Trt = contr.sum))
  lsm <- get_lsmeans_df(m, "Trt", model_label = "Test")

  expect_true(!is.null(lsm))
  expect_equal(nrow(lsm), 3L)
  expect_true("emmean" %in% names(lsm))
  expect_true("model" %in% names(lsm))
  expect_equal(lsm$model[1], "Test")
})


# ============================================================================
# get_effects_df()
# ============================================================================

test_that("get_effects_df returns partial effects for covariate", {
  set.seed(42)
  df <- data.frame(
    A = factor(rep(c("L", "H"), each = 15)),
    x = rnorm(30, 10, 2),
    Y = rnorm(30)
  )
  df$Y <- 5 + 3 * df$x + rnorm(30)
  m <- lm(Y ~ A + x, data = df, contrasts = list(A = contr.sum))

  eff <- get_effects_df(m, "x", model_label = "Test")
  expect_true(!is.null(eff))
  expect_true(all(c("x", "fit", "lwr", "upr", "model") %in% names(eff)))
  expect_equal(nrow(eff), 60)  # default n_grid
})


# ============================================================================
# leverage_plot_data()
# ============================================================================

test_that("leverage_plot_data returns per-term data", {
  set.seed(42)
  df <- data.frame(
    A = factor(rep(c("L", "H"), each = 15)),
    x = rnorm(30),
    Y = rnorm(30, mean = rep(c(5, 10), each = 15))
  )
  m <- lm(Y ~ A + x, data = df, contrasts = list(A = contr.sum))
  lev <- leverage_plot_data(m)

  expect_true(is.list(lev))
  expect_true(length(lev) >= 1)
  # Each element should have x_residual, y_residual, term
  for (nm in names(lev)) {
    expect_true(all(c("x_residual", "y_residual", "term") %in% names(lev[[nm]])))
  }
})


# ============================================================================
# Utility Functions
# ============================================================================

test_that("build_colour_choices returns valid choices", {
  ch <- build_colour_choices(
    factors    = c("A", "B"),
    blocks     = c("Block1"),
    covariates = c("Temp"),
    treatment_label = "Treatment"
  )

  expect_true("none" %in% ch)
  expect_true(".treatment" %in% ch)
  expect_true("A" %in% ch)
  expect_true("B" %in% ch)
  expect_true("Block1" %in% ch)
  expect_true("Temp" %in% ch)
})

test_that("build_colour_choices with all_cols mode", {
  ch <- build_colour_choices(all_cols = c("X", "Y", "Z"))

  expect_true("none" %in% ch)
  expect_true("X" %in% ch)
  expect_true("Y" %in% ch)
})

test_that("short_label truncates long strings", {
  expect_equal(short_label("abc", 10), "abc")
  expect_equal(short_label("abcdefghijklmnop", 10), "abcdefg...")
})

test_that("model_colours returns n colours", {
  expect_length(model_colours(5), 5)
  expect_length(model_colours(15), 15)
})

test_that("stamp_row_ids adds row ID column", {
  df <- data.frame(x = 1:5, y = letters[1:5])
  result <- stamp_row_ids(df)
  expect_true(ROW_ID_COL %in% names(result))
  expect_equal(result[[ROW_ID_COL]], 1:5)
})

test_that("selection_alpha returns correct values", {
  # No selection
  alphas <- selection_alpha(1:5, NULL)
  expect_equal(alphas, rep(SEL_ALPHA_NORMAL, 5))

  # With selection
  alphas <- selection_alpha(1:5, c(2, 4))
  expect_equal(alphas[2], SEL_ALPHA_SELECTED)
  expect_equal(alphas[1], SEL_ALPHA_DIMMED)
})

test_that("colour_nests_in_group works correctly", {
  df <- data.frame(
    colour = c("red", "red", "blue", "blue"),
    group  = c("A", "A", "B", "B")
  )
  expect_true(colour_nests_in_group(df, "colour", "group"))

  df2 <- data.frame(
    colour = c("red", "blue", "red", "blue"),
    group  = c("A", "A", "B", "B")
  )
  expect_false(colour_nests_in_group(df2, "colour", "group"))
})

# ============================================================================
# fit_models() with level_labels
# ============================================================================

test_that("fit_models applies level_labels to factor levels", {
  df <- data.frame(
    y  = c(10, 20, 30, 40),
    x  = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  df[[ROW_ID_COL]] <- 1:4

  labels <- list(x = c("A" = "Alpha", "B" = "Beta"))
  result <- fit_models(
    c("y ~ x" = "y ~ x"), df,
    col_types = list(x = "Factor"),
    level_labels = labels
  )
  m <- result$models[["y ~ x"]]
  expect_false(is.null(m))
  mf <- model.frame(m)
  expect_equal(levels(mf$x), c("Alpha", "Beta"))
})

test_that("fit_models works without level_labels", {
  df <- data.frame(
    y  = c(10, 20, 30, 40),
    x  = c("A", "B", "A", "B"),
    stringsAsFactors = FALSE
  )
  df[[ROW_ID_COL]] <- 1:4

  result <- fit_models(
    c("y ~ x" = "y ~ x"), df,
    col_types = list(x = "Factor")
  )
  m <- result$models[["y ~ x"]]
  mf <- model.frame(m)
  expect_equal(levels(mf$x), c("A", "B"))
})
