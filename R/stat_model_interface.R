# R/stat_model_interface.R — Model abstraction layer
#
# Wrapper functions for model methods that behave differently across model
# classes (lm, lmer, glm). Currently all dispatch to lm; adding mixed model
# or GLM support later requires changes ONLY in this file.
#
# Methods that work via R's S3 generic dispatch (residuals, fitted, predict,
# coef, formula, model.frame, model.matrix, nobs, AIC, BIC, df.residual,
# summary, terms, emmeans) do NOT need wrapping — they already dispatch
# correctly for lm, lmerMod, and glm.

# ---------------------------------------------------------------------------
# model_fit()
# Fit a model given formula, data, and contrasts.
# type: "lm" (default), "lmer" or "glm" in future
# ---------------------------------------------------------------------------
model_fit <- function(formula, data, contrasts = NULL, type = "lm",
                      weights = NULL, ...) {
  switch(type,
    lm = {
      if (!is.null(weights)) {
        lm(formula, data = data, contrasts = contrasts, weights = weights, ...)
      } else {
        lm(formula, data = data, contrasts = contrasts, ...)
      }
    },
    stop("Model type '", type, "' not yet supported.")
  )
}

# ---------------------------------------------------------------------------
# model_refit()
# Refit a model on new data, preserving formula and contrasts.
# ---------------------------------------------------------------------------
model_refit <- function(model, data) {
  UseMethod("model_refit")
}

model_refit.lm <- function(model, data) {
  ctr <- model$contrasts
  if (length(ctr) == 0) ctr <- NULL
  # Preserve weights if the original model used them
  w <- model$weights
  if (!is.null(w) && length(w) == nrow(data)) {
    lm(formula(model), data = data, contrasts = ctr, weights = w)
  } else {
    lm(formula(model), data = data, contrasts = ctr)
  }
}

model_refit.default <- model_refit.lm

# ---------------------------------------------------------------------------
# model_anova()
# Compute Type III (or other) ANOVA table.
# For lm: car::Anova(model, type)
# For lmer: will need test.statistic = "F" + lmerTest
# ---------------------------------------------------------------------------
model_anova <- function(model, type = 3) {
  UseMethod("model_anova")
}

model_anova.lm <- function(model, type = 3) {
  car::Anova(model, type = type)
}

model_anova.default <- model_anova.lm

# ---------------------------------------------------------------------------
# model_studentized_residuals()
# Externally studentized residuals (leave-one-out).
# For lm: rstudent(model)
# For lmer: not directly available — will use scaled residuals
# ---------------------------------------------------------------------------
model_studentized_residuals <- function(model) {
  UseMethod("model_studentized_residuals")
}

model_studentized_residuals.lm <- function(model) {
  rstudent(model)
}

model_studentized_residuals.default <- model_studentized_residuals.lm

# ---------------------------------------------------------------------------
# model_standard_residuals()
# Internally standardized residuals.
# For lm: rstandard(model)
# ---------------------------------------------------------------------------
model_standard_residuals <- function(model) {
  UseMethod("model_standard_residuals")
}

model_standard_residuals.lm <- function(model) {
  rstandard(model)
}

model_standard_residuals.default <- model_standard_residuals.lm

# ---------------------------------------------------------------------------
# model_cooks_distance()
# Cook's distance for each observation.
# For lm: cooks.distance(model)
# For lmer: will use cooks.distance.estex or similar
# ---------------------------------------------------------------------------
model_cooks_distance <- function(model) {
  UseMethod("model_cooks_distance")
}

model_cooks_distance.lm <- function(model) {
  cooks.distance(model)
}

model_cooks_distance.default <- model_cooks_distance.lm

# ---------------------------------------------------------------------------
# model_hat_values()
# Leverage (hat) values for each observation.
# For lm: hatvalues(model)
# ---------------------------------------------------------------------------
model_hat_values <- function(model) {
  UseMethod("model_hat_values")
}

model_hat_values.lm <- function(model) {
  hatvalues(model)
}

model_hat_values.default <- model_hat_values.lm

# ---------------------------------------------------------------------------
# model_bp_test()
# Breusch-Pagan test for heteroscedasticity.
# For lm: lmtest::bptest(model)
# For lmer: not applicable (different variance structure)
# ---------------------------------------------------------------------------
model_bp_test <- function(model) {
  UseMethod("model_bp_test")
}

model_bp_test.lm <- function(model) {
  lmtest::bptest(model)
}

model_bp_test.default <- model_bp_test.lm

# ---------------------------------------------------------------------------
# model_vif()
# Variance inflation factors.
# For lm: car::vif(model)
# For lmer: will need special handling
# ---------------------------------------------------------------------------
model_vif <- function(model) {
  UseMethod("model_vif")
}

model_vif.lm <- function(model) {
  car::vif(model)
}

model_vif.default <- model_vif.lm

# ---------------------------------------------------------------------------
# model_influence()
# Influence measures (DFBETAS, Cook's D, hat values, etc.)
# For lm: influence.measures(model)
# ---------------------------------------------------------------------------
model_influence <- function(model) {
  UseMethod("model_influence")
}

model_influence.lm <- function(model) {
  influence.measures(model)
}

model_influence.default <- model_influence.lm
