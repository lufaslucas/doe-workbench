# tests/testthat/test_jmp_comparison.R
# ============================================================================
# Comprehensive validation of DoEWorkbench statistical outputs against
# JMP Pro 15 reference values
#
# Dataset: Fuel Economy (14 obs, 3 treatments, 1 block, 1 coded covariate)
# Reference files in inst/extdata/:
#   fuel_economy.csv                — clean upload data (5 cols)
#   fuel_economy_jmp_full.csv       — JMP export with all diagnostics
#   fuel_economy_jmp_scaled.html    — JMP Fit LS report (coding transform)
#   fuel_economy_jmp_centred.html   — JMP Fit LS report (centring transform)
#
# Model: Fuel_Economy ~ Treatment + Date_Block + Temperature
#   Temperature coded to [-1, +1] via range (24.5, 30.5)
#   Contrasts: sum-to-zero (effects coding, matching JMP)
# ============================================================================

library(testthat)


# ============================================================================
# Setup helpers
# ============================================================================

setup_fuel_economy <- function() {
  csv_path <- system.file("extdata", "fuel_economy.csv", package = "doe.workbench")
  df <- read.csv(csv_path, check.names = FALSE)
  names(df) <- gsub(" ", "_", names(df))

  df$Treatment  <- as.factor(df$Treatment)
  df$Date_Block <- as.factor(df$Date_Block)

  # Coding transform: Temperature mapped to [-1, +1] via (24.5, 30.5)
  temp_low  <- 24.5
  temp_high <- 30.5
  temp_mid  <- (temp_high + temp_low) / 2    # 27.5
  temp_half <- (temp_high - temp_low) / 2    # 3.0
  df$Temperature_coded <- (df$Temperature - temp_mid) / temp_half

  m <- lm(Fuel_Economy ~ Treatment + Date_Block + Temperature_coded,
           data = df,
           contrasts = list(Treatment = contr.sum, Date_Block = contr.sum))

  list(data = df, model = m, summary = summary(m))
}

load_jmp_diagnostics <- function() {
  csv_path <- system.file("extdata", "fuel_economy_jmp_full.csv", package = "doe.workbench")
  read.csv(csv_path, check.names = FALSE)
}


# ============================================================================
# JMP Reference Values (from fuel_economy_jmp_scaled.html)
# ============================================================================

JMP <- list(
  # --- Summary of Fit ---
  r_squared     = 0.985458,
  adj_r_squared = 0.978994,
  rmse          = 0.327512,
  mean_response = 4.69379,
  n_obs         = 14L,

  # --- Analysis of Variance (whole model) ---
  model_df  = 4L,
  error_df  = 9L,
  model_ss  = 65.418237,
  error_ss  = 0.965375,
  total_ss  = 66.383611,
  model_f   = 152.4704,
  bic       = 18.12447,

  # --- Effect Tests (Type III) ---
  treatment_f     = 95.7070,
  treatment_ss    = 20.531795,
  date_block_f    = 15.4240,
  date_block_ss   = 1.654442,
  temperature_f   = 0.0007,
  temperature_ss  = 0.000074,
  date_block_p    = 0.0035,
  temperature_p   = 0.9796,

  # --- Parameter Estimates (effects coding, coded temperature) ---
  intercept           =  5.300621,
  trt_competitor      = -1.635827,
  trt_shell_inferior  = -0.001857,
  date_block_1        = -2.135518,
  temperature_coded   = -0.017849,

  # --- Parameter Estimate SEs ---
  se_intercept        = 0.092990,
  se_trt_competitor   = 0.136082,
  se_trt_inferior     = 0.140732,
  se_date_block_1     = 0.543756,
  se_temperature      = 0.680545,

  # --- VIF (GVIF^(1/(2*Df))) ---
  vif_treatment     = 1.381137,   # GVIF for Treatment (from JMP, VIF column)
  vif_date_block    = 35.440461,  # GVIF for Date Block
  vif_temperature   = 35.641920,  # GVIF for Temperature

  # --- Least Squares Means ---
  lsmean_competitor     = 3.66838,
  lsmean_shell_inferior = 5.30235,
  lsmean_shell_superior = 6.94189,

  lsmean_se_competitor  = 0.1979,
  lsmean_se_inferior    = 0.2583,
  lsmean_se_superior    = 0.1957,

  # --- Student's t pairwise differences ---
  student_comp_inf  = list(diff = -1.633969, se = 0.239278,
                           lower = -2.17525, upper = -1.09269),
  student_comp_sup  = list(diff = -3.273511, se = 0.236683,
                           lower = -3.80892, upper = -2.73810),
  student_inf_sup   = list(diff = -1.639541, se = 0.244705,
                           lower = -2.19310, upper = -1.08598),

  # --- Tukey HSD ---
  tukey_comp_inf  = list(lower = -2.30203, upper = -0.9659),
  tukey_comp_sup  = list(lower = -3.93433, upper = -2.61269),
  tukey_inf_sup   = list(lower = -2.32276, upper = -0.95632),

  # --- Dunnett (Competitor as control) ---
  dunnett_inf_comp = list(diff = 1.633969, se = 0.239278,
                          lower = 1.00727, upper = 2.26067),
  dunnett_sup_comp = list(diff = 3.273511, se = 0.236683,
                          lower = 2.65361, upper = 3.89342),

  # --- Durbin-Watson ---
  durbin_watson = 2.130716
)


# ============================================================================
# Test: Data Loading
# ============================================================================

test_that("Fuel economy data loads correctly", {
  fe <- setup_fuel_economy()
  expect_equal(nrow(fe$data), JMP$n_obs)
  expect_equal(nlevels(fe$data$Treatment), 3L)
  expect_equal(nlevels(fe$data$Date_Block), 2L)
  expect_equal(mean(fe$data$Fuel_Economy), JMP$mean_response, tolerance = 1e-4)
})


# ============================================================================
# Test: Summary of Fit
# ============================================================================

test_that("R-squared, Adj R-squared, RMSE match JMP", {
  fe <- setup_fuel_economy()
  s  <- fe$summary

  expect_equal(s$r.squared,     JMP$r_squared,     tolerance = 1e-5)
  expect_equal(s$adj.r.squared, JMP$adj_r_squared, tolerance = 1e-5)
  expect_equal(s$sigma,         JMP$rmse,          tolerance = 1e-5)
})


test_that("BIC matches JMP", {
  fe <- setup_fuel_economy()
  expect_equal(BIC(fe$model), JMP$bic, tolerance = 0.01)
})


# ============================================================================
# Test: ANOVA
# ============================================================================

test_that("Type III ANOVA F-ratios match JMP", {
  fe  <- setup_fuel_economy()
  a   <- car::Anova(fe$model, type = 3)
  adf <- as.data.frame(a)

  expect_equal(adf["Treatment",          "F value"], JMP$treatment_f,   tolerance = 0.01)
  expect_equal(adf["Date_Block",         "F value"], JMP$date_block_f,  tolerance = 0.01)
  expect_equal(adf["Temperature_coded",  "F value"], JMP$temperature_f, tolerance = 0.001)
})


test_that("Type III ANOVA Sum of Squares match JMP", {
  fe  <- setup_fuel_economy()
  a   <- car::Anova(fe$model, type = 3)
  adf <- as.data.frame(a)

  expect_equal(adf["Treatment",          "Sum Sq"], JMP$treatment_ss,   tolerance = 0.001)
  expect_equal(adf["Date_Block",         "Sum Sq"], JMP$date_block_ss,  tolerance = 0.001)
  expect_equal(adf["Temperature_coded",  "Sum Sq"], JMP$temperature_ss, tolerance = 0.0001)
})


test_that("Type III ANOVA p-values match JMP", {
  fe  <- setup_fuel_economy()
  a   <- car::Anova(fe$model, type = 3)
  adf <- as.data.frame(a)

  expect_lt(adf["Treatment", "Pr(>F)"], 0.0001)
  expect_equal(adf["Date_Block",        "Pr(>F)"], JMP$date_block_p,   tolerance = 0.001)
  expect_equal(adf["Temperature_coded", "Pr(>F)"], JMP$temperature_p, tolerance = 0.001)
})


test_that("ANOVA degrees of freedom match JMP", {
  fe <- setup_fuel_economy()
  expect_equal(df.residual(fe$model), JMP$error_df)

  a   <- car::Anova(fe$model, type = 3)
  adf <- as.data.frame(a)
  expect_equal(adf["Treatment",  "Df"], 2)
  expect_equal(adf["Date_Block", "Df"], 1)
  expect_equal(adf["Temperature_coded", "Df"], 1)
})


# ============================================================================
# Test: Parameter Estimates
# ============================================================================

test_that("Coefficient estimates match JMP", {
  fe <- setup_fuel_economy()
  cf <- coef(fe$model)

  expect_equal(unname(cf["(Intercept)"]),        JMP$intercept,          tolerance = 0.001)
  expect_equal(unname(cf["Treatment1"]),          JMP$trt_competitor,     tolerance = 0.001)
  expect_equal(unname(cf["Treatment2"]),          JMP$trt_shell_inferior, tolerance = 0.001)
  expect_equal(unname(cf["Date_Block1"]),         JMP$date_block_1,       tolerance = 0.001)
  expect_equal(unname(cf["Temperature_coded"]),   JMP$temperature_coded,  tolerance = 0.001)
})


test_that("Coefficient standard errors match JMP", {
  fe <- setup_fuel_economy()
  se <- fe$summary$coefficients[, "Std. Error"]

  expect_equal(unname(se["(Intercept)"]),       JMP$se_intercept,       tolerance = 0.001)
  expect_equal(unname(se["Treatment1"]),         JMP$se_trt_competitor,  tolerance = 0.001)
  expect_equal(unname(se["Treatment2"]),         JMP$se_trt_inferior,    tolerance = 0.001)
  expect_equal(unname(se["Date_Block1"]),        JMP$se_date_block_1,    tolerance = 0.001)
  expect_equal(unname(se["Temperature_coded"]),  JMP$se_temperature,     tolerance = 0.001)
})


# ============================================================================
# Test: Per-observation Diagnostics vs JMP
# ============================================================================

test_that("Predicted values match JMP per-row", {
  fe  <- setup_fuel_economy()
  jmp <- load_jmp_diagnostics()
  r_pred   <- unname(predict(fe$model))
  jmp_pred <- jmp[["Pred Formula Fuel Economy"]]

  expect_equal(r_pred, jmp_pred, tolerance = 1e-6,
               label = "Predicted values match JMP row-by-row")
})


test_that("Residuals match JMP per-row", {
  fe  <- setup_fuel_economy()
  jmp <- load_jmp_diagnostics()
  r_resid   <- unname(residuals(fe$model))
  jmp_resid <- jmp[["Residual Fuel Economy"]]

  expect_equal(r_resid, jmp_resid, tolerance = 1e-6,
               label = "Residuals match JMP row-by-row")
})


test_that("Hat (leverage) values match JMP per-row", {
  fe  <- setup_fuel_economy()
  jmp <- load_jmp_diagnostics()
  r_hat   <- unname(hatvalues(fe$model))
  jmp_hat <- jmp[["h Fuel Economy"]]

  expect_equal(r_hat, jmp_hat, tolerance = 1e-6,
               label = "Hat values match JMP row-by-row")
})


test_that("Externally studentized residuals match JMP per-row", {
  fe  <- setup_fuel_economy()
  jmp <- load_jmp_diagnostics()
  r_rstud   <- unname(rstudent(fe$model))
  jmp_rstud <- jmp[["Externally Studentized Residuals Fuel Economy"]]

  expect_equal(r_rstud, jmp_rstud, tolerance = 1e-6,
               label = "Externally studentized residuals match JMP row-by-row")
})


test_that("Cook's distance matches JMP per-row", {
  fe  <- setup_fuel_economy()
  jmp <- load_jmp_diagnostics()
  r_cook   <- unname(cooks.distance(fe$model))
  jmp_cook <- jmp[["Cook's D Influence Fuel Economy"]]

  expect_equal(r_cook, jmp_cook, tolerance = 1e-6,
               label = "Cook's distance matches JMP row-by-row")
})


test_that("Standard error of prediction matches JMP per-row", {
  fe  <- setup_fuel_economy()
  jmp <- load_jmp_diagnostics()

  pred <- predict(fe$model, se.fit = TRUE)
  r_se_pred <- unname(pred$se.fit)
  jmp_se    <- jmp[["StdErr Pred Fuel Economy"]]

  expect_equal(r_se_pred, jmp_se, tolerance = 1e-5,
               label = "Prediction SE matches JMP row-by-row")
})


# ============================================================================
# Test: VIF
# ============================================================================

test_that("VIF matches JMP", {
  fe <- setup_fuel_economy()
  v  <- car::vif(fe$model)

  # JMP reports GVIF for single-df terms, and GVIF^(1/(2*Df)) for multi-df terms.
  # For single-df terms (Date_Block, Temperature), GVIF = VIF (standard).
  # For multi-df terms (Treatment, Df=2), JMP shows GVIF in the VIF column.
  # Our vif_summary uses GVIF^(1/(2*Df)) for comparability across term types.
  gcol <- "GVIF^(1/(2*Df))"

  # Treatment: JMP VIF column = 1.381137 = GVIF^(1/(2*2)) for this 2-df term
  # However, JMP reports raw GVIF for Treatment as 1.486753
  expect_equal(v["Treatment", "GVIF"], 1.486753, tolerance = 0.001,
               label = "Treatment GVIF (raw)")
  expect_equal(v["Treatment", gcol], 1.104230, tolerance = 0.001,
               label = "Treatment GVIF^(1/(2*Df))")

  # Single-df terms: GVIF = GVIF^(1/(2*1)) = same value
  expect_equal(v["Date_Block", "GVIF"],        JMP$vif_date_block,  tolerance = 0.001)
  expect_equal(v["Temperature_coded", "GVIF"], JMP$vif_temperature, tolerance = 0.001)
})


# ============================================================================
# Test: PRESS Statistics
# ============================================================================

test_that("PRESS statistics are reasonable", {
  fe <- setup_fuel_economy()
  h  <- hatvalues(fe$model)
  r  <- residuals(fe$model)

  press      <- sum((r / (1 - h))^2)
  ss_total   <- sum((fe$data$Fuel_Economy - mean(fe$data$Fuel_Economy))^2)
  press_r2   <- 1 - press / ss_total
  press_rmse <- sqrt(press / length(r))

  # PRESS R2 should be less than R2 but still high for this model
  expect_true(press_r2 < JMP$r_squared)
  expect_true(press_r2 > 0.95)

  # PRESS RMSE should be greater than RMSE
  expect_true(press_rmse > JMP$rmse)

  # Verify via rmse_summary helper
  models_list <- list("Full" = fe$model)
  rs <- rmse_summary(models_list)
  expect_equal(rs$PRESS_R2[1],   round(press_r2, 4),   tolerance = 1e-4)
  expect_equal(rs$PRESS_RMSE[1], round(press_rmse, 4), tolerance = 1e-4)
})


# ============================================================================
# Test: Least Squares Means
# ============================================================================

test_that("LS Means estimates match JMP", {
  fe <- setup_fuel_economy()
  em <- emmeans::emmeans(fe$model, "Treatment")
  em_df <- as.data.frame(summary(em))

  expect_equal(em_df$emmean[em_df$Treatment == "Competitor"], JMP$lsmean_competitor,     tolerance = 0.001)
  expect_equal(em_df$emmean[em_df$Treatment == "Inferior"],   JMP$lsmean_shell_inferior, tolerance = 0.001)
  expect_equal(em_df$emmean[em_df$Treatment == "Superior"],   JMP$lsmean_shell_superior, tolerance = 0.001)
})


test_that("LS Means standard errors match JMP", {
  fe <- setup_fuel_economy()
  em <- emmeans::emmeans(fe$model, "Treatment")
  em_df <- as.data.frame(summary(em))

  expect_equal(em_df$SE[em_df$Treatment == "Competitor"], JMP$lsmean_se_competitor, tolerance = 0.001)
  expect_equal(em_df$SE[em_df$Treatment == "Inferior"],   JMP$lsmean_se_inferior,  tolerance = 0.001)
  expect_equal(em_df$SE[em_df$Treatment == "Superior"],   JMP$lsmean_se_superior,  tolerance = 0.001)
})


test_that("LS Means via get_lsmeans_df helper match JMP", {
  fe  <- setup_fuel_economy()
  lsm <- get_lsmeans_df(fe$model, "Treatment", model_label = "Full")

  expect_true(!is.null(lsm))
  expect_equal(nrow(lsm), 3L)
  expect_equal(lsm$emmean[lsm$Treatment == "Competitor"], JMP$lsmean_competitor, tolerance = 0.001)
})


# ============================================================================
# Test: Student's t Pairwise Comparisons (unadjusted)
# ============================================================================

test_that("Student's t pairwise differences match JMP", {
  fe <- setup_fuel_economy()
  em <- emmeans::emmeans(fe$model, "Treatment")
  pw <- emmeans::contrast(em, method = "pairwise", adjust = "none")
  pw_df <- as.data.frame(summary(pw, infer = c(TRUE, TRUE)))

  # Competitor - Inferior
  ci <- pw_df[grep("Competitor.*Inferior", pw_df$contrast), ]
  expect_equal(ci$estimate, JMP$student_comp_inf$diff,  tolerance = 0.001)
  expect_equal(ci$SE,       JMP$student_comp_inf$se,    tolerance = 0.001)
  expect_equal(ci$lower.CL, JMP$student_comp_inf$lower, tolerance = 0.01)
  expect_equal(ci$upper.CL, JMP$student_comp_inf$upper, tolerance = 0.01)

  # Competitor - Superior
  cs <- pw_df[grep("Competitor.*Superior", pw_df$contrast), ]
  expect_equal(cs$estimate, JMP$student_comp_sup$diff,  tolerance = 0.001)
  expect_equal(cs$SE,       JMP$student_comp_sup$se,    tolerance = 0.001)
  expect_equal(cs$lower.CL, JMP$student_comp_sup$lower, tolerance = 0.01)
  expect_equal(cs$upper.CL, JMP$student_comp_sup$upper, tolerance = 0.01)

  # Inferior - Superior
  is_ <- pw_df[grep("Inferior.*Superior", pw_df$contrast), ]
  expect_equal(is_$estimate, JMP$student_inf_sup$diff,  tolerance = 0.001)
  expect_equal(is_$SE,       JMP$student_inf_sup$se,    tolerance = 0.001)
  expect_equal(is_$lower.CL, JMP$student_inf_sup$lower, tolerance = 0.01)
  expect_equal(is_$upper.CL, JMP$student_inf_sup$upper, tolerance = 0.01)
})


test_that("run_mc() Student method matches JMP pairwise", {
  fe <- setup_fuel_economy()
  mc <- run_mc(fe$model, spec = "Treatment", method = "student", alpha = 0.05)

  expect_equal(nrow(mc), 3L)
  # All significant
  expect_true(all(mc$p.value < 0.001))
  # Has CI columns
  expect_true("lower.CL" %in% names(mc))
  expect_true("upper.CL" %in% names(mc))
})


# ============================================================================
# Test: Tukey HSD Comparisons
# ============================================================================

test_that("Tukey HSD confidence intervals match JMP", {
  fe <- setup_fuel_economy()
  em <- emmeans::emmeans(fe$model, "Treatment")
  tk <- emmeans::contrast(em, method = "pairwise", adjust = "tukey")
  tk_df <- as.data.frame(summary(tk, infer = c(TRUE, TRUE)))

  # Competitor - Inferior
  ci <- tk_df[grep("Competitor.*Inferior", tk_df$contrast), ]
  expect_equal(ci$lower.CL, JMP$tukey_comp_inf$lower, tolerance = 0.01)
  expect_equal(ci$upper.CL, JMP$tukey_comp_inf$upper, tolerance = 0.01)

  # Competitor - Superior
  cs <- tk_df[grep("Competitor.*Superior", tk_df$contrast), ]
  expect_equal(cs$lower.CL, JMP$tukey_comp_sup$lower, tolerance = 0.01)
  expect_equal(cs$upper.CL, JMP$tukey_comp_sup$upper, tolerance = 0.01)

  # Inferior - Superior
  is_ <- tk_df[grep("Inferior.*Superior", tk_df$contrast), ]
  expect_equal(is_$lower.CL, JMP$tukey_inf_sup$lower, tolerance = 0.01)
  expect_equal(is_$upper.CL, JMP$tukey_inf_sup$upper, tolerance = 0.01)
})


test_that("run_mc() Tukey method matches JMP", {
  fe <- setup_fuel_economy()
  mc <- run_mc(fe$model, spec = "Treatment", method = "tukey", alpha = 0.05)

  expect_equal(nrow(mc), 3L)
  expect_true(all(mc$p.value < 0.05))
  expect_true("lower.CL" %in% names(mc))
  expect_true("upper.CL" %in% names(mc))
})


# ============================================================================
# Test: Dunnett Comparisons (Competitor as control)
# ============================================================================

test_that("Dunnett confidence intervals match JMP", {
  fe <- setup_fuel_economy()
  em <- emmeans::emmeans(fe$model, "Treatment")
  em_df <- as.data.frame(summary(em))

  ref_idx <- match("Competitor", as.character(em_df$Treatment))
  dn <- emmeans::contrast(em, method = "trt.vs.ctrl", ref = ref_idx, adjust = "mvt")
  dn_df <- as.data.frame(summary(dn, infer = c(TRUE, TRUE)))

  # Inferior - Competitor
  inf_row <- dn_df[grep("Inferior", dn_df$contrast), ]
  expect_equal(inf_row$estimate, JMP$dunnett_inf_comp$diff, tolerance = 0.001)
  expect_equal(inf_row$SE,       JMP$dunnett_inf_comp$se,   tolerance = 0.001)
  expect_equal(inf_row$lower.CL, JMP$dunnett_inf_comp$lower, tolerance = 0.01)
  expect_equal(inf_row$upper.CL, JMP$dunnett_inf_comp$upper, tolerance = 0.01)

  # Superior - Competitor
  sup_row <- dn_df[grep("Superior", dn_df$contrast), ]
  expect_equal(sup_row$estimate, JMP$dunnett_sup_comp$diff, tolerance = 0.001)
  expect_equal(sup_row$SE,       JMP$dunnett_sup_comp$se,   tolerance = 0.001)
  expect_equal(sup_row$lower.CL, JMP$dunnett_sup_comp$lower, tolerance = 0.01)
  expect_equal(sup_row$upper.CL, JMP$dunnett_sup_comp$upper, tolerance = 0.01)
})


test_that("run_mc() Dunnett method matches JMP", {
  fe <- setup_fuel_economy()
  mc <- run_mc(fe$model, spec = "Treatment", method = "dunnett",
               control = "Competitor", alpha = 0.05)

  expect_equal(nrow(mc), 2L)
  expect_true(all(mc$p.value < 0.001))
  expect_true("lower.CL" %in% names(mc))
  expect_true("upper.CL" %in% names(mc))
})


# ============================================================================
# Test: Diagnostics via run_diagnostics()
# ============================================================================

test_that("run_diagnostics produces valid results", {
  fe    <- setup_fuel_economy()
  diags <- run_diagnostics(fe$model)

  expect_true("normality" %in% names(diags))
  expect_true("heteroscedasticity" %in% names(diags))
  expect_true("outliers" %in% names(diags))
  expect_true("influence" %in% names(diags))
  expect_true("overall" %in% names(diags))

  valid_levels <- c("green", "amber", "red")
  for (nm in c("normality", "heteroscedasticity", "outliers", "influence", "overall")) {
    expect_true(diags[[nm]]$level %in% valid_levels,
                label = paste0("Diagnostic '", nm, "' level valid"))
  }
})


# ============================================================================
# Test: Backward Elimination
# ============================================================================

test_that("Backward elimination removes non-significant Temperature", {
  fe      <- setup_fuel_economy()
  reduced <- backward_eliminate(fe$model, alpha = 0.05)

  remaining <- attr(terms(reduced), "term.labels")
  expect_true("Treatment" %in% remaining)
  expect_true("Date_Block" %in% remaining)
  expect_false("Temperature_coded" %in% remaining)
})


# ============================================================================
# Test: Coefficient Table Helper
# ============================================================================

test_that("coef_table has correct structure and values", {
  fe <- setup_fuel_economy()
  ct <- coef_table(
    list("Full" = fe$model),
    transforms    = list(Temperature_coded = "coding"),
    coding_values = list(Temperature_coded = list(low = 24.5, high = 30.5))
  )

  expect_true(nrow(ct) > 0)
  expect_true(all(c("Model", "Term", "Estimate", "Std.Error", "t.value", "p.value") %in% names(ct)))
  expect_equal(ct$Estimate[ct$Term == "(Intercept)"], JMP$intercept, tolerance = 0.01)
})


# ============================================================================
# Test: ANOVA Helpers
# ============================================================================

test_that("type3_wide produces correctly structured ANOVA table", {
  fe <- setup_fuel_economy()

  term_roles <- classify_anova_terms(
    c("Treatment", "Date_Block", "Temperature_coded"),
    factor_cols = "Treatment",
    block_cols  = "Date_Block",
    cov_cols    = "Temperature_coded"
  )

  wide <- type3_wide(list("Full" = fe$model), term_roles = term_roles)

  expect_true(nrow(wide) >= 3)
  f_col <- grep("_F$", names(wide), value = TRUE)
  expect_length(f_col, 1)

  trt_row <- wide[wide$term == "Treatment", ]
  expect_equal(trt_row[[f_col]], JMP$treatment_f, tolerance = 0.01)
})


test_that("vif_summary produces correct VIF values", {
  fe       <- setup_fuel_economy()
  vif_wide <- vif_summary(list("Full" = fe$model))

  expect_true(nrow(vif_wide) >= 2)
  expect_true("Term" %in% names(vif_wide))

  # All VIF >= 1
  vif_vals <- vif_wide[["Full"]]
  vif_vals <- vif_vals[!is.na(vif_vals)]
  expect_true(all(vif_vals >= 1))
})


test_that("rmse_summary matches JMP summary metrics", {
  fe <- setup_fuel_economy()
  rs <- rmse_summary(list("Full" = fe$model))

  expect_equal(rs$R2[1],      round(JMP$r_squared, 4),     tolerance = 1e-4)
  expect_equal(rs$Adj_R2[1],  round(JMP$adj_r_squared, 4), tolerance = 1e-4)
  expect_equal(rs$RMSE[1],    round(JMP$rmse, 4),          tolerance = 1e-4)
  expect_equal(rs$df_resid[1], JMP$error_df)

  # PRESS metrics present
  expect_true(!is.na(rs$PRESS_R2[1]))
  expect_true(!is.na(rs$PRESS_RMSE[1]))
  expect_true(rs$PRESS_R2[1] < rs$R2[1])
})


# ============================================================================
# Test: Format P-value Function
# ============================================================================

test_that("format_pvalue handles edge cases correctly", {
  # Tiny p-value
  expect_equal(format_pvalue(0.00001), PVALUE_FLOOR_LABEL)
  expect_equal(format_pvalue(1e-10),   PVALUE_FLOOR_LABEL)

  # Normal p-values
  expect_equal(format_pvalue(0.0035), "0.0035")
  expect_equal(format_pvalue(0.05),   "0.0500")
  expect_equal(format_pvalue(0.9796), "0.9796")

  # Conservative rounding near alpha
  # p > 0.05 but rounds to 0.0500 => should round up to 0.0501
  expect_equal(format_pvalue(0.05001), "0.0501")
  expect_equal(format_pvalue(0.05004), "0.0501")

  # p = 0.04999 is <= alpha (genuinely significant), rounds to 0.0500
  # This is NOT the "disagree" case since 0.0500 <= 0.05
  expect_equal(format_pvalue(0.04999), "0.0500")
})


test_that("format_pvalue_dt preserves shadow columns", {
  df <- data.frame(Term = c("A", "B"), p.value = c(0.0001, 0.05))
  result <- format_pvalue_dt(df, "p.value")

  expect_true("._p_p.value" %in% names(result))
  expect_true(is.numeric(result[["._p_p.value"]]))
  expect_true(is.character(result[["p.value"]]))
})
