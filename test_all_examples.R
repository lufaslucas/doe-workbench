#!/usr/bin/env Rscript
# test_all_examples.R — Exercise the full pipeline for every example dataset
# Sources global.R, builds each example, runs formulas → models → ANOVA → effects → residuals

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(car)
  library(emmeans)
  library(multcomp)
  library(DT)
})

setwd("/Users/dipesh/Documents/AI/DataAnalysisApp")
source("global.R")

cat("=== Test Script: All Example Datasets ===\n\n")
errors_found <- list()

# Helper: run a test block and capture errors
run_test <- function(name, expr) {
  result <- tryCatch({
    expr
    cat("  PASS:", name, "\n")
    TRUE
  }, error = function(e) {
    msg <- paste0("  FAIL: ", name, " — ", e$message)
    cat(msg, "\n")
    errors_found[[length(errors_found) + 1]] <<- list(dataset = current_dataset, test = name, error = e$message)
    FALSE
  })
  result
}

# ═══════════════════════════════════════════════════════════════════════════
# 1. RCBD Example
# ═══════════════════════════════════════════════════════════════════════════
current_dataset <- "RCBD"
cat("── 1. RCBD (2 factors, blocks, covariate) ──\n")

set.seed(42)
blocks_list <- lapply(1:4, function(b) {
  ord <- sample(4)
  data.frame(
    Block = paste0("B", b),
    A     = c("Low","High","Low","High")[ord],
    B     = c("Control","Control","Treatment","Treatment")[ord],
    stringsAsFactors = FALSE
  )
})
df_rcbd <- do.call(rbind, blocks_list)
df_rcbd$RunOrder <- seq_len(nrow(df_rcbd))
block_effect <- c(B1=0, B2=1.8, B3=-1.2, B4=0.5)
A_eff  <- ifelse(df_rcbd$A == "High", 8, 0)
B_eff  <- ifelse(df_rcbd$B == "Treatment", 5, 0)
AB_eff <- ifelse(df_rcbd$A == "High" & df_rcbd$B == "Treatment", 3, 0)
blk    <- block_effect[df_rcbd$Block]
df_rcbd$Temp_C <- round(22 + 0.15 * df_rcbd$RunOrder + rnorm(16, 0, 0.8), 1)
df_rcbd$Yield  <- round(50 + A_eff + B_eff + AB_eff + blk + 0.4*(df_rcbd$Temp_C-22) + rnorm(16,0,1.5), 2)
df_rcbd$Purity <- round(80 + 0.4*A_eff + 0.6*B_eff + 0.2*AB_eff + 0.3*blk - 0.2*(df_rcbd$Temp_C-22) + rnorm(16,0,1.2), 2)

col_types_rcbd <- list(Block="Factor", A="Factor", B="Factor", RunOrder="Numeric", Temp_C="Numeric", Yield="Numeric", Purity="Numeric")

# Build formulas (comparative mode)
run_test("build_formulas (main effects)", {
  f <- build_formulas("Yield", c("A","B"), covariates = c("Temp_C","RunOrder"), blocks = c("Block"), max_way = 2)
  stopifnot(length(f) > 0)
  cat("    Generated", length(f), "formulas\n")
})

# Fit models
rcbd_models <- NULL
run_test("fit_models", {
  f <- build_formulas("Yield", c("A","B"), covariates = c("Temp_C","RunOrder"), blocks = c("Block"), max_way = 2)
  rcbd_models <<- fit_models(f, df_rcbd, col_types = col_types_rcbd)
  stopifnot(length(rcbd_models) > 0)
  cat("    Fitted", length(rcbd_models), "models\n")
})

# Type III ANOVA
run_test("type3_wide ANOVA", {
  tr <- classify_anova_terms(
    unique(unlist(lapply(rcbd_models, function(m) rownames(car::Anova(m, type=3))))),
    c("A","B"), c("Block"), c("Temp_C","RunOrder"))
  w <- type3_wide(rcbd_models, tr)
  stopifnot(nrow(w) > 0)
  cat("    ANOVA table has", nrow(w), "terms\n")
})

# RMSE summary
run_test("rmse_summary", {
  s <- rmse_summary(rcbd_models)
  stopifnot(nrow(s) > 0)
  cat("    RMSE range:", range(s$RMSE), "\n")
})

# Coefficients
run_test("coef_table", {
  ct <- coef_table(rcbd_models)
  stopifnot(nrow(ct) > 0)
})

# VIF
run_test("vif_summary", {
  v <- vif_summary(rcbd_models)
  cat("    VIF table:", nrow(v), "rows\n")
})

# LS Means
run_test("get_lsmeans_df (A)", {
  em <- get_lsmeans_df(rcbd_models[[1]], "A", "Model1")
  stopifnot(!is.null(em), nrow(em) > 0)
  cat("    LS Means for A:", nrow(em), "rows\n")
})

run_test("get_lsmeans_df (A:B interaction)", {
  em <- get_lsmeans_df(rcbd_models[[1]], "A:B", "Model1")
  stopifnot(!is.null(em), nrow(em) > 0)
  cat("    LS Means for A:B:", nrow(em), "rows\n")
})

# Partial residuals (factor)
run_test("get_partial_residuals_factor", {
  pr <- get_partial_residuals_factor(rcbd_models[[1]], "A", "Model1")
  stopifnot(!is.null(pr), nrow(pr) > 0)
})

# Covariate effects
run_test("get_effects_df (Temp_C)", {
  eff <- get_effects_df(rcbd_models[[1]], "Temp_C", "Model1")
  if (is.null(eff)) {
    # May not be in first model; try others
    for (m in rcbd_models) {
      eff <- get_effects_df(m, "Temp_C", "test")
      if (!is.null(eff)) break
    }
  }
  stopifnot(!is.null(eff), nrow(eff) > 0)
})

# Partial residuals (covariate)
run_test("get_partial_residuals_covariate (Temp_C)", {
  pr <- NULL
  for (m in rcbd_models) {
    pr <- get_partial_residuals_covariate(m, "Temp_C", "test")
    if (!is.null(pr)) break
  }
  stopifnot(!is.null(pr), nrow(pr) > 0)
})

# Leverage plots
run_test("leverage_plot_data", {
  lev <- leverage_plot_data(rcbd_models[[1]])
  stopifnot(length(lev) > 0)
  cat("    Leverage plots for", length(lev), "terms\n")
})

# Multiple comparisons
run_test("run_mc (Tukey on A)", {
  mc <- run_mc(rcbd_models[[1]], "A", "tukey")
  stopifnot(nrow(mc) > 0)
})

run_test("run_mc (Dunnett on A)", {
  mc <- run_mc(rcbd_models[[1]], "A", "dunnett")
  stopifnot(nrow(mc) > 0)
})

# Backward elimination
run_test("backward_eliminate", {
  # Use full model (last one)
  full_m <- rcbd_models[[length(rcbd_models)]]
  pruned <- backward_eliminate(full_m, alpha = 0.05)
  stopifnot(!is.null(pruned))
  cat("    Pruned model terms:", paste(attr(terms(pruned), "term.labels"), collapse=", "), "\n")
})

# Residual diagnostics
run_test("residual diagnostics (rstandard, rstudent, cooks.distance)", {
  m <- rcbd_models[[1]]
  rs <- rstandard(m)
  rt <- rstudent(m)
  cd <- cooks.distance(m)
  stopifnot(length(rs) > 0, length(rt) > 0, length(cd) > 0)
})

# Alias structure
run_test("compute_aliases", {
  result <- compute_aliases(df_rcbd, c("A","B","A:B"), c("A","B","A:B","Block","Block:A","Block:B"), 0.99)
  stopifnot(!is.null(result), nrow(result) > 0)
})

# Power analysis
run_test("design_power", {
  pw <- design_power(df_rcbd, c("A","B"), sigma = 1, delta = 1, alpha = 0.05, max_order = 2)
  stopifnot(nrow(pw) > 0)
})

cat("\n")

# ═══════════════════════════════════════════════════════════════════════════
# 2. Mediator Trap
# ═══════════════════════════════════════════════════════════════════════════
current_dataset <- "Mediator Trap"
cat("── 2. Mediator Trap ──\n")

set.seed(99)
n_per <- 8
df_med <- data.frame(Catalyst = rep(c("A","B","C"), each = n_per), stringsAsFactors = FALSE)
cat_eff <- c(A = 10, B = 15, C = 20)
df_med$Viscosity <- round(cat_eff[df_med$Catalyst] + rnorm(nrow(df_med), 0, 1.5), 2)
df_med$Strength  <- round(30 + 2 * df_med$Viscosity + rnorm(nrow(df_med), 0, 2), 2)

col_types_med <- list(Catalyst="Factor", Viscosity="Numeric", Strength="Numeric")

run_test("build_formulas (Catalyst only)", {
  f <- build_formulas("Strength", c("Catalyst"))
  stopifnot(length(f) > 0)
})

run_test("build_formulas (Catalyst + Viscosity)", {
  f <- build_formulas("Strength", c("Catalyst"), covariates = c("Viscosity"))
  stopifnot(length(f) > 0)
})

med_models <- NULL
run_test("fit_models (compare with/without covariate)", {
  f1 <- c("Strength ~ Catalyst" = "Strength ~ Catalyst")
  f2 <- c("Strength ~ Catalyst + Viscosity" = "Strength ~ Catalyst + Viscosity")
  med_models <<- fit_models(c(f1, f2), df_med, col_types = col_types_med)
  stopifnot(length(med_models) == 2)
})

run_test("type3_wide ANOVA", {
  w <- type3_wide(med_models)
  stopifnot(nrow(w) > 0)
})

run_test("LS Means for Catalyst (both models)", {
  for (mn in names(med_models)) {
    em <- get_lsmeans_df(med_models[[mn]], "Catalyst", mn)
    stopifnot(!is.null(em), nrow(em) > 0)
  }
})

run_test("Covariate effect (Viscosity)", {
  eff <- get_effects_df(med_models[["Strength ~ Catalyst + Viscosity"]], "Viscosity", "M2")
  stopifnot(!is.null(eff), nrow(eff) > 0)
})

run_test("run_mc (Tukey on Catalyst)", {
  mc <- run_mc(med_models[[1]], "Catalyst", "tukey")
  stopifnot(nrow(mc) > 0)
})

cat("\n")

# ═══════════════════════════════════════════════════════════════════════════
# 3. Block-Covariate Collinearity
# ═══════════════════════════════════════════════════════════════════════════
current_dataset <- "Collinearity"
cat("── 3. Block-Covariate Collinearity ──\n")

set.seed(77)
plots      <- paste0("P", 1:4)
fert_lvls  <- c("Low","Med","High")
n_per_cell <- 3
df_col <- expand.grid(Plot = plots, Fertilizer = fert_lvls, Rep = seq_len(n_per_cell), stringsAsFactors = FALSE)
df_col$Rep <- NULL
df_col$RunOrder <- sample(nrow(df_col))
elevation  <- c(P1=100, P2=200, P3=300, P4=400)
df_col$SoilMoisture <- round(50 - 0.08 * elevation[df_col$Plot] + rnorm(nrow(df_col), 0, 3), 1)
fert_eff  <- c(Low=0, Med=6, High=12)
block_eff_c <- c(P1=0, P2=2, P3=4, P4=5)
df_col$Biomass <- round(40 + fert_eff[df_col$Fertilizer] + block_eff_c[df_col$Plot] + 0.5 * df_col$SoilMoisture + rnorm(nrow(df_col), 0, 2), 2)

col_types_col <- list(Plot="Factor", Fertilizer="Factor", RunOrder="Numeric", SoilMoisture="Numeric", Biomass="Numeric")

col_models <- NULL
run_test("fit_models (Block only, Covariate only, Both)", {
  f <- c(
    "Biomass ~ Fertilizer + Plot" = "Biomass ~ Fertilizer + Plot",
    "Biomass ~ Fertilizer + SoilMoisture" = "Biomass ~ Fertilizer + SoilMoisture",
    "Biomass ~ Fertilizer + Plot + SoilMoisture" = "Biomass ~ Fertilizer + Plot + SoilMoisture"
  )
  col_models <<- fit_models(f, df_col, col_types = col_types_col)
  stopifnot(length(col_models) == 3)
})

run_test("VIF shows collinearity", {
  v <- vif_summary(col_models)
  stopifnot(nrow(v) > 0)
  cat("    VIF table:\n")
  print(v)
})

run_test("type3_wide ANOVA", {
  w <- type3_wide(col_models)
  stopifnot(nrow(w) > 0)
})

run_test("LS Means for Fertilizer", {
  em <- get_lsmeans_df(col_models[[1]], "Fertilizer", "M1")
  stopifnot(!is.null(em), nrow(em) > 0)
})

cat("\n")

# ═══════════════════════════════════════════════════════════════════════════
# 4. 2^(4-1) Fractional Factorial (Regression mode)
# ═══════════════════════════════════════════════════════════════════════════
current_dataset <- "FracFact"
cat("── 4. Fractional Factorial (Regression mode) ──\n")

set.seed(123)
base <- expand.grid(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1))
base$D <- base$A * base$B * base$C
base$RunOrder <- sample(nrow(base))
base$Yield <- round(40 + 5*base$A + 3*base$B - 2*base$C + 1.5*base$D + 2*base$A*base$B + rnorm(8, 0, 0.8), 2)

col_types_ff <- list(A="Numeric", B="Numeric", C="Numeric", D="Numeric", RunOrder="Numeric", Yield="Numeric")
transforms_ff <- list(A="coding", B="coding", C="coding", D="coding", RunOrder="none", Yield="none")
coding_vals_ff <- list(
  A = list(low=-1, high=1), B = list(low=-1, high=1),
  C = list(low=-1, high=1), D = list(low=-1, high=1)
)

run_test("build_regression_formulas", {
  f <- build_regression_formulas("Yield", c("A","B","C","D"), max_way = 2, poly_degree = 2)
  stopifnot(length(f) > 0)
  cat("    Generated", length(f), "regression formulas\n")
})

ff_models <- NULL
run_test("fit_models (regression with coding)", {
  f <- build_regression_formulas("Yield", c("A","B","C","D"), max_way = 2, poly_degree = 1)
  ff_models <<- fit_models(f, base, col_types = col_types_ff,
                            transforms = transforms_ff, coding_values = coding_vals_ff)
  stopifnot(length(ff_models) > 0)
  cat("    Fitted", length(ff_models), "models\n")
})

run_test("type3_wide ANOVA (regression)", {
  w <- type3_wide(ff_models)
  stopifnot(nrow(w) > 0)
})

run_test("coef_table (regression)", {
  ct <- coef_table(ff_models)
  stopifnot(nrow(ct) > 0)
})

run_test("compute_aliases (fractional factorial)", {
  result <- compute_aliases(base, c("A","B","C","D","A:B","A:C","A:D","B:C","B:D","C:D"),
                            c("A","B","C","D","A:B","A:C","A:D","B:C","B:D","C:D","A:B:C","A:B:D","A:C:D","B:C:D"),
                            0.99)
  stopifnot(!is.null(result), nrow(result) > 0)
  cat("    Aliases found:", nrow(result), "\n")
})

run_test("effects_df for numeric factor A", {
  eff <- get_effects_df(ff_models[[1]], "A", "M1")
  stopifnot(!is.null(eff), nrow(eff) > 0)
})

run_test("partial_residuals_covariate for numeric factor A", {
  pr <- get_partial_residuals_covariate(ff_models[[1]], "A", "M1")
  stopifnot(!is.null(pr), nrow(pr) > 0)
})

run_test("backward_eliminate (regression)", {
  full_m <- ff_models[[length(ff_models)]]
  pruned <- backward_eliminate(full_m, alpha = 0.05)
  stopifnot(!is.null(pruned))
  cat("    Pruned terms:", paste(attr(terms(pruned), "term.labels"), collapse=", "), "\n")
})

cat("\n")

# ═══════════════════════════════════════════════════════════════════════════
# 5. CCD (Central Composite Design)
# ═══════════════════════════════════════════════════════════════════════════
current_dataset <- "CCD"
cat("── 5. CCD (Central Composite Design) ──\n")

set.seed(456)
fact <- expand.grid(x1 = c(-1, 1), x2 = c(-1, 1))
axial <- data.frame(x1 = c(-1, 1, 0, 0), x2 = c(0, 0, -1, 1))
centre <- data.frame(x1 = rep(0, 3), x2 = rep(0, 3))
df_ccd <- rbind(fact, axial, centre)
df_ccd$RunOrder <- sample(nrow(df_ccd))
df_ccd$Response <- round(80 + 4*df_ccd$x1 - 3*df_ccd$x2 + 2*df_ccd$x1*df_ccd$x2 - 5*df_ccd$x1^2 - 4*df_ccd$x2^2 + rnorm(nrow(df_ccd), 0, 0.6), 2)

col_types_ccd <- list(x1="Numeric", x2="Numeric", RunOrder="Numeric", Response="Numeric")
transforms_ccd <- list(x1="coding", x2="coding", RunOrder="none", Response="none")
coding_vals_ccd <- list(x1 = list(low=-1, high=1), x2 = list(low=-1, high=1))

run_test("build_regression_formulas (quadratic)", {
  f <- build_regression_formulas("Response", c("x1","x2"), max_way = 2, poly_degree = 2)
  stopifnot(length(f) > 0)
  cat("    Generated", length(f), "formulas\n")
  for (fn in names(f)) cat("      ", fn, "\n")
})

ccd_models <- NULL
run_test("fit_models (CCD full quadratic)", {
  f <- build_regression_formulas("Response", c("x1","x2"), max_way = 2, poly_degree = 2)
  ccd_models <<- fit_models(f, df_ccd, col_types = col_types_ccd,
                             transforms = transforms_ccd, coding_values = coding_vals_ccd)
  stopifnot(length(ccd_models) > 0)
  cat("    Fitted", length(ccd_models), "models\n")
})

run_test("type3_wide ANOVA (CCD)", {
  w <- type3_wide(ccd_models)
  stopifnot(nrow(w) > 0)
})

run_test("rmse_summary (CCD)", {
  s <- rmse_summary(ccd_models)
  stopifnot(nrow(s) > 0)
  cat("    Best R²:", max(s$R2), "\n")
})

run_test("coef_table (CCD)", {
  ct <- coef_table(ccd_models)
  stopifnot(nrow(ct) > 0)
})

run_test("effects_df for x1 (CCD)", {
  eff <- get_effects_df(ccd_models[[length(ccd_models)]], "x1", "Full")
  stopifnot(!is.null(eff), nrow(eff) > 0)
})

run_test("leverage_plot_data (CCD)", {
  lev <- leverage_plot_data(ccd_models[[length(ccd_models)]])
  stopifnot(length(lev) > 0)
  cat("    Leverage plots for", length(lev), "terms\n")
})

run_test("design_power (CCD)", {
  pw <- design_power(df_ccd, c("x1","x2"), sigma = 0.6, delta = 1, alpha = 0.05, max_order = 2)
  stopifnot(nrow(pw) > 0)
})

run_test("coded_design_matrix (CCD)", {
  coded <- coded_design_matrix(df_ccd, c("x1","x2"), transforms_ccd, coding_vals_ccd)
  stopifnot(nrow(coded) == nrow(df_ccd))
})

cat("\n")

# ═══════════════════════════════════════════════════════════════════════════
# Summary
# ═══════════════════════════════════════════════════════════════════════════
cat("═══════════════════════════════════════════\n")
if (length(errors_found) == 0) {
  cat("ALL TESTS PASSED ✓\n")
} else {
  cat(length(errors_found), "FAILURE(S) FOUND:\n\n")
  for (e in errors_found) {
    cat("  Dataset:", e$dataset, "\n")
    cat("  Test:   ", e$test, "\n")
    cat("  Error:  ", e$error, "\n\n")
  }
}
cat("═══════════════════════════════════════════\n")
