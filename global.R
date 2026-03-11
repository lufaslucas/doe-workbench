# global.R — Package loading + shared helper functions

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(car)
  library(emmeans)
  library(multcomp)
  library(readxl)
  library(rmarkdown)
  library(flextable)
})

# Try loading officedown (optional, falls back to regular Word)
has_officedown <- requireNamespace("officedown", quietly = TRUE)

# ---------------------------------------------------------------------------
# Null-coalescing helper (must live in global.R, not server.R —
# Shiny 1.13 uses the last expression in server.R as the server function)
# ---------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---------------------------------------------------------------------------
# build_formulas()
# Returns a named character vector: label => formula string
# ---------------------------------------------------------------------------
build_formulas <- function(response, factors, covariates = character(0),
                            blocks = character(0), max_way = 2,
                            include_covariates = TRUE, include_blocks = TRUE,
                            include_block_fac = FALSE) {
  if (length(factors) == 0) return(character(0))
  result <- character(0)

  # Build factor terms progressively (main effects → 2FI → 3FI → ...)
  rhs <- factors
  f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
  result[f] <- f

  if (max_way >= 2 && length(factors) >= 2) {
    for (order in 2:min(max_way, length(factors))) {
      combos <- combn(factors, order, simplify = FALSE)
      interactions <- sapply(combos, function(x) paste(x, collapse = ":"))
      rhs <- c(rhs, interactions)
      f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
      result[f] <- f
    }
  }

  # + covariates on all models
  if (include_covariates && length(covariates) > 0) {
    cov_term <- paste(covariates, collapse = " + ")
    cov_result <- character(0)
    for (f in names(result)) {
      new_f <- paste0(f, " + ", cov_term)
      cov_result[new_f] <- new_f
    }
    result <- c(result, cov_result)
  }

  # + blocks on all models
  if (include_blocks && length(blocks) > 0) {
    blk_term <- paste(blocks, collapse = " + ")
    blk_result <- character(0)
    for (f in names(result)) {
      new_f <- paste0(f, " + ", blk_term)
      blk_result[new_f] <- new_f
    }
    result <- c(result, blk_result)

    # + block × factor interactions (optional)
    if (include_block_fac && length(factors) > 0) {
      blk_fac <- as.vector(outer(blocks, factors, function(b, f) paste0(b, ":", f)))
      blk_fac_term <- paste(blk_fac, collapse = " + ")
      blk_fac_result <- character(0)
      for (f in names(blk_result)) {
        new_f <- paste0(f, " + ", blk_fac_term)
        blk_fac_result[new_f] <- new_f
      }
      result <- c(result, blk_fac_result)
    }
  }

  result
}

# ---------------------------------------------------------------------------
# build_regression_formulas()
# For regression (MLR) mode: generates polynomial models
# Linear, Linear+2FI, Linear+Quadratic, Full Quadratic (linear+2FI+quadratic)
# ---------------------------------------------------------------------------
build_regression_formulas <- function(response, factors, covariates = character(0),
                                       blocks = character(0), max_way = 2,
                                       poly_degree = 2,
                                       include_covariates = TRUE,
                                       include_cov_fac = FALSE,
                                       include_blocks = TRUE,
                                       include_block_fac = FALSE) {
  if (length(factors) == 0 && length(covariates) == 0) return(character(0))
  result <- character(0)

  # Progressive formula building: each step adds terms to the previous
  rhs <- factors

  # 1. Main effects
  if (length(rhs) > 0) {
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  # 2. + factor interactions (up to max_way)
  if (max_way >= 2 && length(factors) >= 2) {
    for (order in 2:min(max_way, length(factors))) {
      combos <- combn(factors, order, simplify = FALSE)
      interactions <- sapply(combos, function(x) paste(x, collapse = ":"))
      rhs <- c(rhs, interactions)
    }
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  # 3. + polynomial terms (degree 2 = quadratic, degree 3 = cubic, etc.)
  if (poly_degree >= 2 && length(factors) > 0) {
    for (deg in 2:poly_degree) {
      poly_terms <- paste0("I(", factors, "^", deg, ")")
      rhs <- c(rhs, poly_terms)
    }
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  # 4. + covariates
  if (include_covariates && length(covariates) > 0) {
    rhs <- c(rhs, covariates)
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  # 5. + covariate × factor interactions
  if (include_cov_fac && length(covariates) > 0 && length(factors) > 0) {
    cov_fac <- as.vector(outer(covariates, factors, function(c, f) paste0(c, ":", f)))
    rhs <- c(rhs, cov_fac)
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  # 6. + blocks
  if (include_blocks && length(blocks) > 0) {
    rhs <- c(rhs, blocks)
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  # 7. + block × factor interactions
  if (include_block_fac && length(blocks) > 0 && length(factors) > 0) {
    blk_fac <- as.vector(outer(blocks, factors, function(b, f) paste0(b, ":", f)))
    rhs <- c(rhs, blk_fac)
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  result
}

# ---------------------------------------------------------------------------
# backward_eliminate()
# Remove terms with p > alpha one at a time (highest p first) using Type III SS
# Returns the pruned lm model
# ---------------------------------------------------------------------------
backward_eliminate <- function(model, alpha = 0.05) {
  # Extract contrasts from the original model so re-fitting works
  # (avoids 'ctr not found' when update() re-evaluates the original call)
  ctr <- model$contrasts
  mod_data <- model$model
  repeat {
    a <- tryCatch(car::Anova(model, type = 3), error = function(e) NULL)
    if (is.null(a)) break

    df <- as.data.frame(a)
    df$term <- rownames(df)
    names(df)[names(df) == "Pr(>F)"] <- "p_value"
    df <- df[!df$term %in% c("(Intercept)", "Residuals"), , drop = FALSE]
    if (nrow(df) == 0) break

    worst <- df[which.max(df$p_value), ]
    if (is.na(worst$p_value) || worst$p_value <= alpha) break

    current_terms <- attr(terms(model), "term.labels")
    new_terms <- setdiff(current_terms, worst$term)
    if (length(new_terms) == 0) break

    resp <- all.vars(formula(model))[1]
    new_f <- as.formula(paste(resp, "~", paste(new_terms, collapse = " + ")))
    # Filter contrasts to only factors present in the new formula
    new_vars <- all.vars(new_f)
    new_ctr <- ctr[intersect(names(ctr), new_vars)]
    if (length(new_ctr) == 0) new_ctr <- NULL
    model <- lm(new_f, data = mod_data, contrasts = new_ctr)
  }
  model
}

# Helper: generate all subsets of factor terms up to max_way interactions
generate_interaction_terms <- function(factors, max_way) {
  n <- length(factors)
  if (n == 0) return(list())
  max_way <- min(max_way, n)
  current_terms <- factors
  all_combos <- list(current_terms)
  if (max_way >= 2) {
    for (order in 2:max_way) {
      combos <- combn(factors, order, simplify = FALSE)
      interaction_terms <- sapply(combos, function(x) paste(x, collapse = ":"))
      current_terms <- c(current_terms, interaction_terms)
      all_combos <- c(all_combos, list(current_terms))
    }
  }
  all_combos
}

# ---------------------------------------------------------------------------
# fit_models()
# Returns named list of lm objects
# col_types: named list colname -> "Factor" | "Numeric"
# ---------------------------------------------------------------------------
fit_models <- function(formulas_vec, data, col_types = list(),
                       transforms = list(), coding_values = list()) {
  # Coerce columns according to declared types
  for (cn in names(col_types)) {
    if (!cn %in% names(data)) next
    if (col_types[[cn]] == "Factor") {
      data[[cn]] <- as.factor(data[[cn]])
    } else {
      data[[cn]] <- suppressWarnings(as.numeric(as.character(data[[cn]])))
    }
  }

  # Apply user-selected transforms to numeric columns
  for (cn in names(transforms)) {
    if (!cn %in% names(data) || !is.numeric(data[[cn]])) next
    tr <- transforms[[cn]]
    if (is.null(tr) || tr == "none") next
    if (tr == "centre") {
      data[[cn]] <- data[[cn]] - mean(data[[cn]], na.rm = TRUE)
    } else if (tr == "coding") {
      cv <- coding_values[[cn]]
      if (!is.null(cv)) {
        low  <- cv$low
        high <- cv$high
        if (!is.null(low) && !is.null(high) && high != low) {
          midpoint   <- (high + low) / 2
          half_range <- (high - low) / 2
          data[[cn]] <- (data[[cn]] - midpoint) / half_range
        }
      }
    }
  }

  # All factor columns in the data
  fac_cols <- names(data)[sapply(data, is.factor)]

  models <- list()
  for (label in names(formulas_vec)) {
    tryCatch({
      f <- as.formula(formulas_vec[[label]])
      # Only set contrasts for factors that appear in this formula
      f_vars <- all.vars(f)
      facs_in_formula <- intersect(fac_cols, f_vars)
      ctr <- setNames(rep(list("contr.sum"), length(facs_in_formula)),
                       facs_in_formula)
      models[[label]] <- lm(f, data = data, contrasts = ctr)
    }, error = function(e) {
      message("Error fitting model '", label, "': ", e$message)
    })
  }
  models
}

# ---------------------------------------------------------------------------
# coef_table()
# Extract model coefficients with proper labels for sum-to-zero contrasts.
# For factors with contr.sum, R names levels as Factor1, Factor2, ...
# We relabel to the actual factor level names (last level is derived).
# Returns data.frame: Model | Term | Estimate | Std.Error | t.value | p.value
# ---------------------------------------------------------------------------
coef_table <- function(models_list) {
  if (length(models_list) == 0) return(data.frame())
  rows <- list()
  for (mname in names(models_list)) {
    m <- models_list[[mname]]
    s <- summary(m)$coefficients
    cf <- as.data.frame(s)
    cf$raw_name <- rownames(cf)
    names(cf) <- c("Estimate", "Std.Error", "t.value", "p.value", "raw_name")

    # Build a mapping from R's internal coefficient names to readable names
    ctr <- m$contrasts
    mf  <- model.frame(m)
    label_map <- list()
    for (fac_name in names(ctr)) {
      if (identical(ctr[[fac_name]], "contr.sum") || identical(ctr[[fac_name]], contr.sum)) {
        levs <- levels(mf[[fac_name]])
        n_levs <- length(levs)
        for (k in seq_len(n_levs - 1)) {
          internal <- paste0(fac_name, k)
          label_map[[internal]] <- paste0(fac_name, " [", levs[k], "]")
        }
      }
    }

    cf$Term <- sapply(cf$raw_name, function(rn) {
      if (rn %in% names(label_map)) return(label_map[[rn]])
      # Handle interaction terms like FactorA1:FactorB2
      parts <- strsplit(rn, ":")[[1]]
      labelled <- sapply(parts, function(p) {
        if (p %in% names(label_map)) label_map[[p]] else p
      })
      paste(labelled, collapse = " : ")
    })

    cf$Model <- mname
    rows[[mname]] <- cf[, c("Model", "Term", "Estimate", "Std.Error", "t.value", "p.value")]
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# type3_wide()
# Returns data.frame: term | Model1_F | Model1_p | Model2_F | Model2_p | ...
# ---------------------------------------------------------------------------
type3_wide <- function(models_list, term_roles = NULL) {
  if (length(models_list) == 0) return(data.frame())

  all_anova <- lapply(names(models_list), function(mname) {
    m <- models_list[[mname]]
    tryCatch({
      a  <- car::Anova(m, type = 3)
      df <- as.data.frame(a)
      df$term <- rownames(df)
      names(df)[names(df) == "F value"] <- "F_value"
      names(df)[names(df) == "Pr(>F)"]  <- "p_value"
      df <- df[df$term != "(Intercept)", , drop = FALSE]
      df <- df[, c("term", "F_value", "p_value"), drop = FALSE]
      names(df)[2:3] <- c(paste0(mname, "_F"), paste0(mname, "_p"))
      df
    }, error = function(e) {
      message("ANOVA error for '", mname, "': ", e$message)
      NULL
    })
  })

  all_anova <- Filter(Negate(is.null), all_anova)
  if (length(all_anova) == 0) return(data.frame())

  wide <- Reduce(function(a, b) merge(a, b, by = "term", all = TRUE), all_anova)
  order_anova_terms(wide, term_roles)
}

# ---------------------------------------------------------------------------
# rmse_summary()
# Returns data.frame: Model | RMSE | R2 | AdjR2 | df_resid
# ---------------------------------------------------------------------------
rmse_summary <- function(models_list) {
  if (length(models_list) == 0) return(data.frame())
  data.frame(
    Model    = names(models_list),
    RMSE     = sapply(models_list, function(m) round(sigma(m), 4)),
    R2       = sapply(models_list, function(m) round(summary(m)$r.squared, 4)),
    Adj_R2   = sapply(models_list, function(m) round(summary(m)$adj.r.squared, 4)),
    df_resid = sapply(models_list, function(m) df.residual(m)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ---------------------------------------------------------------------------
# get_lsmeans_df()
# Returns data.frame from emmeans summary with model label column
# ---------------------------------------------------------------------------
get_lsmeans_df <- function(model, spec, model_label = "Model") {
  tryCatch({
    spec_list <- strsplit(spec, ":")[[1]]
    em  <- suppressMessages(emmeans::emmeans(model, specs = spec_list))
    df  <- as.data.frame(summary(em))
    df$model <- model_label
    df
  }, error = function(e) {
    message("emmeans error: ", e$message)
    NULL
  })
}

# ---------------------------------------------------------------------------
# get_effects_df()
# Partial effects for a continuous covariate: predicted response over a grid
# with all other predictors held at mean (numeric) or reference (factor)
# ---------------------------------------------------------------------------
get_effects_df <- function(model, covariate, model_label = "Model", n_grid = 60) {
  tryCatch({
    mf        <- model.frame(model)
    pred_vars <- names(mf)[-1]   # first column is the response

    if (!covariate %in% pred_vars) return(NULL)

    cov_seq <- seq(min(mf[[covariate]], na.rm = TRUE),
                   max(mf[[covariate]], na.rm = TRUE),
                   length.out = n_grid)

    newdata <- as.data.frame(lapply(pred_vars, function(cn) {
      if (cn == covariate) {
        cov_seq
      } else if (is.numeric(mf[[cn]])) {
        rep(mean(mf[[cn]], na.rm = TRUE), n_grid)
      } else {
        factor(rep(levels(mf[[cn]])[1], n_grid), levels = levels(mf[[cn]]))
      }
    }), check.names = FALSE)
    names(newdata) <- pred_vars

    pred <- predict(model, newdata = newdata, interval = "confidence")
    data.frame(
      x     = cov_seq,
      fit   = pred[, "fit"],
      lwr   = pred[, "lwr"],
      upr   = pred[, "upr"],
      model = model_label
    )
  }, error = function(e) {
    message("Effects error (", covariate, "): ", e$message)
    NULL
  })
}

# ---------------------------------------------------------------------------
# run_mc()
# Returns data.frame of pairwise/contrast results
# ---------------------------------------------------------------------------
run_mc <- function(model, spec, method = "tukey",
                   control = NULL, contrast_mat = NULL) {
  tryCatch({
    spec_list <- strsplit(spec, ":")[[1]]
    em <- suppressMessages(emmeans::emmeans(model, specs = spec_list))

    result <- switch(tolower(method),
      "tukey" = {
        pairs <- emmeans::contrast(em, method = "pairwise", adjust = "tukey")
        as.data.frame(summary(pairs))
      },
      "student" = {
        pairs <- emmeans::contrast(em, method = "pairwise", adjust = "none")
        as.data.frame(summary(pairs))
      },
      "dunnett" = {
        if (is.null(control)) {
          pairs <- emmeans::contrast(em, method = "trt.vs.ctrl", adjust = "dunnettx")
        } else {
          pairs <- emmeans::contrast(em, method = "trt.vs.ctrl",
                                     ref = which(levels(factor(model$model[[spec_list[1]]])) == control),
                                     adjust = "dunnettx")
        }
        as.data.frame(summary(pairs))
      },
      "maxt" = {
        pairs   <- emmeans::contrast(em, method = "pairwise")
        glht_r  <- multcomp::glht(model, emmeans::as.glht(pairs))
        sum_r   <- summary(glht_r, test = multcomp::adjusted("free"))
        data.frame(
          contrast  = names(sum_r$test$coefficients),
          estimate  = sum_r$test$coefficients,
          std.error = sum_r$test$sigma,
          t.ratio   = sum_r$test$tstat,
          p.value   = sum_r$test$pvalues
        )
      },
      "mvtnorm" = {
        if (!is.null(contrast_mat)) {
          K <- tryCatch(
            as.matrix(read.table(text = contrast_mat, header = FALSE)),
            error = function(e) NULL
          )
          if (!is.null(K)) {
            glht_r  <- multcomp::glht(model, linfct = K)
            sum_r   <- summary(glht_r)
            data.frame(
              contrast  = names(sum_r$test$coefficients),
              estimate  = sum_r$test$coefficients,
              std.error = sum_r$test$sigma,
              t.ratio   = sum_r$test$tstat,
              p.value   = sum_r$test$pvalues
            )
          } else data.frame()
        } else data.frame()
      },
      {
        pairs <- emmeans::contrast(em, method = "pairwise", adjust = "tukey")
        as.data.frame(summary(pairs))
      }
    )

    result$method <- method
    result$spec   <- spec
    result
  }, error = function(e) {
    message("MC error (", method, " on ", spec, "): ", e$message)
    data.frame()
  })
}

# ---------------------------------------------------------------------------
# Utility: short model label
# ---------------------------------------------------------------------------
short_label <- function(label, maxchar = 40) {
  if (nchar(label) <= maxchar) return(label)
  paste0(substr(label, 1, maxchar - 3), "...")
}

# ---------------------------------------------------------------------------
# Utility: colour palette
# ---------------------------------------------------------------------------
model_colours <- function(n) {
  palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728",
               "#9467bd", "#8c564b", "#e377c2", "#7f7f7f",
               "#bcbd22", "#17becf")
  rep_len(palette, n)
}

# ---------------------------------------------------------------------------
# order_anova_terms()
# Reorder ANOVA wide table: factors → blocks → covariates → Residuals
# term_roles: named vector  term_name -> "Factor"|"Block"|"Covariate"
# ---------------------------------------------------------------------------
order_anova_terms <- function(wide, term_roles = NULL) {
  if (is.null(term_roles) || nrow(wide) == 0) return(wide)
  role_order <- c("Factor" = 1, "Block" = 2, "Covariate" = 3, "Residuals" = 4)
  wide$`.sort` <- sapply(wide$term, function(t) {
    if (t == "Residuals") return(4)
    r <- term_roles[t]
    if (is.na(r)) return(3.5)
    role_order[r] %||% 3.5
  })
  wide <- wide[order(wide$`.sort`), , drop = FALSE]
  wide$`.sort` <- NULL
  rownames(wide) <- NULL
  wide
}

# ---------------------------------------------------------------------------
# classify_anova_terms()
# Given factor/block/covariate column names, classify each ANOVA term
# Returns named vector: term -> "Factor"|"Block"|"Covariate"
# Interaction terms: "Factor" if any component is a factor
# ---------------------------------------------------------------------------
classify_anova_terms <- function(terms, factor_cols, block_cols, cov_cols) {
  sapply(terms, function(t) {
    if (t == "Residuals") return("Residuals")
    parts <- strsplit(t, ":")[[1]]
    if (any(parts %in% factor_cols)) return("Factor")
    if (any(parts %in% block_cols))  return("Block")
    if (any(parts %in% cov_cols))    return("Covariate")
    "Covariate"
  }, USE.NAMES = TRUE)
}

# ---------------------------------------------------------------------------
# vif_summary()
# Compute VIF for each model; return wide data.frame: Term | Model1 | Model2 | ...
# Values are GVIF^(1/(2*Df)) which is comparable to sqrt(VIF) for single-df terms
# ---------------------------------------------------------------------------
vif_summary <- function(models_list) {
  if (length(models_list) == 0) return(data.frame())

  # Collect per-model VIF values in long form
  long_rows <- lapply(names(models_list), function(mname) {
    m <- models_list[[mname]]
    tryCatch({
      if (length(coef(m)) <= 2) return(NULL)
      has_interactions <- any(grepl(":", attr(terms(m), "term.labels")))
      v <- if (has_interactions) car::vif(m, type = "predictor") else car::vif(m)
      if (is.data.frame(v)) {
        # type = "predictor" returns a data.frame with GVIF^(1/(2*Df)) column
        gcol <- "GVIF^(1/(2*Df))"
        if (gcol %in% names(v)) {
          data.frame(
            Term  = rownames(v),
            VIF   = round(v[[gcol]], 2),
            Model = mname,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Term  = rownames(v),
            VIF   = round(v[[1]]^(1/(2*v[[2]])), 2),
            Model = mname,
            stringsAsFactors = FALSE
          )
        }
      } else if (is.matrix(v)) {
        data.frame(
          Term  = rownames(v),
          VIF   = round(v[, "GVIF^(1/(2*Df))"], 2),
          Model = mname,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          Term  = names(v),
          VIF   = round(sqrt(v), 2),
          Model = mname,
          stringsAsFactors = FALSE
        )
      }
    }, error = function(e) NULL)
  })

  long <- do.call(rbind, Filter(Negate(is.null), long_rows))
  if (is.null(long) || nrow(long) == 0) return(data.frame())

  # Pivot to wide: Term | Model1 | Model2 | ...
  all_terms  <- unique(long$Term)
  all_models <- unique(long$Model)
  wide <- data.frame(Term = all_terms, stringsAsFactors = FALSE)
  for (mn in all_models) {
    sub <- long[long$Model == mn, ]
    vals <- sub$VIF[match(all_terms, sub$Term)]
    wide[[mn]] <- vals
  }
  wide
}

# ---------------------------------------------------------------------------
# get_partial_residuals_factor()
# Adjusted data points for LS means: adjusted_y = emmean_group + residual
# Returns data.frame: factor columns + adjusted_y + model
# ---------------------------------------------------------------------------
get_partial_residuals_factor <- function(model, spec, model_label = "Model") {
  tryCatch({
    spec_list <- strsplit(spec, ":")[[1]]
    mf   <- model.frame(model)
    resp <- model.response(mf)
    fitt <- fitted(model)
    resids <- residuals(model)

    em  <- emmeans::emmeans(model, specs = spec_list)
    em_df <- as.data.frame(summary(em))

    # Build a key for each observation from the factor columns
    obs_keys <- apply(mf[, spec_list, drop = FALSE], 1, function(row) {
      paste(as.character(row), collapse = ":")
    })
    em_keys <- apply(em_df[, spec_list, drop = FALSE], 1, function(row) {
      paste(as.character(row), collapse = ":")
    })
    em_map <- setNames(em_df$emmean, em_keys)

    # adjusted_y = emmean for this group + residual
    adj_y <- em_map[obs_keys] + resids

    result <- mf[, spec_list, drop = FALSE]
    result$adjusted_y <- as.numeric(adj_y)
    result$model <- model_label
    result
  }, error = function(e) {
    message("Partial resid factor error: ", e$message)
    NULL
  })
}

# ---------------------------------------------------------------------------
# get_partial_residuals_covariate()
# Component + residual plot data: adjusted_y = beta*x + residual + intercept
# where "beta*x" is the partial contribution of the covariate
# Returns data.frame: x, adjusted_y, model
# ---------------------------------------------------------------------------
get_partial_residuals_covariate <- function(model, covariate, model_label = "Model") {
  tryCatch({
    mf   <- model.frame(model)
    if (!covariate %in% names(mf)) return(NULL)

    resids <- residuals(model)
    x_vals <- mf[[covariate]]

    # Partial residual = beta_cov * x + residual
    b      <- coef(model)
    # Find the coefficient for this covariate
    b_cov  <- b[covariate]
    if (is.na(b_cov)) return(NULL)

    intercept <- b["(Intercept)"]
    adj_y <- intercept + b_cov * x_vals + resids

    data.frame(
      x          = x_vals,
      adjusted_y = adj_y,
      model      = model_label
    )
  }, error = function(e) {
    message("Partial resid covariate error: ", e$message)
    NULL
  })
}

# ---------------------------------------------------------------------------
# leverage_plot_data()
# Compute added-variable (leverage) plot data for each term in a model.
# For each term: regress Y and the term on all other terms, plot residuals.
# Returns list of data.frames keyed by term name.
# ---------------------------------------------------------------------------
leverage_plot_data <- function(model) {
  tryCatch({
    mf <- model.frame(model)
    resp <- model.response(mf)
    term_labels <- attr(terms(model), "term.labels")
    if (length(term_labels) == 0) return(list())

    mm <- model.matrix(model)
    result <- list()
    for (tl in term_labels) {
      # Find columns in model.matrix belonging to this term
      assign_vec <- attr(mm, "assign")
      term_idx <- which(attr(terms(model), "term.labels") == tl)
      if (length(term_idx) == 0) next
      cols_this  <- which(assign_vec == term_idx)
      cols_other <- which(assign_vec != term_idx & assign_vec > 0)  # exclude intercept too

      if (length(cols_this) == 0) next

      X_other <- cbind(1, mm[, cols_other, drop = FALSE])  # intercept + other terms

      # Residuals of Y on others
      y_on_others <- lm.fit(X_other, resp)
      y_res <- resp - y_on_others$fitted.values

      if (length(cols_this) == 1) {
        # Single column term: straightforward AV plot
        x_term <- mm[, cols_this]
        x_on_others <- lm.fit(X_other, x_term)
        x_res <- x_term - x_on_others$fitted.values

        result[[tl]] <- data.frame(
          x_residual = x_res,
          y_residual = y_res,
          term = tl,
          stringsAsFactors = FALSE
        )
      } else {
        # Multi-column term (factor with multiple levels or interaction):
        # Use the fitted value from regressing Y on this term's columns alone
        # (after partialling out other terms)
        X_term <- mm[, cols_this, drop = FALSE]
        fit_term <- lm.fit(cbind(1, X_term), y_res)
        fitted_term <- fit_term$fitted.values

        result[[tl]] <- data.frame(
          x_residual = fitted_term,
          y_residual = y_res,
          term = tl,
          stringsAsFactors = FALSE
        )
      }
    }
    result
  }, error = function(e) {
    message("Leverage plot error: ", e$message)
    list()
  })
}

# ---------------------------------------------------------------------------
# compute_aliases()
# Detect confounded/aliased terms by comparing model matrix columns.
# full_terms:  terms in the model you plan to fit
# check_terms: larger set of terms to check for aliasing
# If check_terms is NULL, check_terms = full_terms (self-aliasing)
# Returns data.frame: Full_model_term | Aliased_with | Correlation
# ---------------------------------------------------------------------------
compute_aliases <- function(data, full_terms, check_terms = NULL,
                            threshold = 0.99) {
  if (length(full_terms) == 0)
    return(data.frame(Message = "No terms specified."))

  all_terms <- if (is.null(check_terms)) full_terms else unique(c(full_terms, check_terms))

  # Build numeric design columns for each term
  df <- data
  for (cn in names(df)) {
    if (!is.numeric(df[[cn]])) {
      df[[cn]] <- as.numeric(as.factor(df[[cn]]))
    }
    df[[cn]] <- df[[cn]] - mean(df[[cn]], na.rm = TRUE)
  }

  build_col <- function(term) {
    parts <- strsplit(term, ":")[[1]]
    if (!all(parts %in% names(df))) return(NULL)
    col_vals <- rep(1, nrow(df))
    for (p in parts) col_vals <- col_vals * df[[p]]
    col_vals
  }

  # Build columns for all terms
  X <- list()
  valid_terms <- character()
  for (t in all_terms) {
    cv <- build_col(t)
    if (!is.null(cv) && sd(cv) > 1e-10) {
      X[[t]] <- cv
      valid_terms <- c(valid_terms, t)
    }
  }
  if (length(X) < 2)
    return(data.frame(Message = "Not enough valid terms to check aliases."))

  Xmat <- do.call(cbind, X)
  Xmat <- scale(Xmat)
  cor_mat <- cor(Xmat)

  # Find all aliases: any pair with |cor| >= threshold
  # Label whether each term is in the full model or alias-only
  nc <- length(valid_terms)
  aliases <- list()
  if (nc >= 2) {
    for (i in 1:(nc - 1)) {
      for (j in (i + 1):nc) {
        r <- cor_mat[i, j]
        if (!is.na(r) && abs(r) >= threshold) {
          aliases <- c(aliases, list(data.frame(
            Term_1      = valid_terms[i],
            Term_2      = valid_terms[j],
            Correlation = round(r, 3),
            In_model    = if (valid_terms[i] %in% full_terms && valid_terms[j] %in% full_terms)
                            "Both in model"
                          else if (valid_terms[i] %in% full_terms || valid_terms[j] %in% full_terms)
                            "One in model"
                          else "Neither in model",
            stringsAsFactors = FALSE
          )))
        }
      }
    }
  }

  if (length(aliases) == 0)
    return(data.frame(Message = "No aliases detected at this threshold."))
  do.call(rbind, aliases)
}

# ---------------------------------------------------------------------------
# design_power()
# Compute power for each term based on the design matrix.
# delta = minimum detectable effect, sigma = error SD
# Returns data.frame: Term | df | SE | Power
# ---------------------------------------------------------------------------
design_power <- function(data, factors, sigma, delta, alpha = 0.05, max_order = 2) {
  if (length(factors) == 0 || sigma <= 0) return(data.frame())

  df <- data[, factors, drop = FALSE]
  for (col in factors) {
    if (!is.numeric(df[[col]])) {
      df[[col]] <- as.numeric(as.factor(df[[col]]))
    }
    df[[col]] <- df[[col]] - mean(df[[col]], na.rm = TRUE)
  }

  n <- nrow(df)
  all_terms <- character()
  for (order in seq_len(min(max_order, length(factors)))) {
    combos <- combn(factors, order, simplify = FALSE)
    all_terms <- c(all_terms, sapply(combos, paste, collapse = ":"))
  }

  p_total <- length(all_terms)
  df_error <- n - p_total - 1
  if (df_error <= 0)
    return(data.frame(Term = all_terms, SE = NA, Power = NA,
                      Note = "Insufficient residual df"))

  results <- lapply(all_terms, function(term) {
    parts <- strsplit(term, ":")[[1]]
    x <- rep(1, n)
    for (p_name in parts) x <- x * df[[p_name]]
    ss_x <- sum(x^2)
    if (ss_x == 0) return(data.frame(Term = term, SE = NA, Power = 0))
    se <- sigma / sqrt(ss_x)
    ncp <- (delta / se)^2
    f_crit <- qf(1 - alpha, 1, df_error)
    power  <- 1 - pf(f_crit, 1, df_error, ncp = ncp)
    data.frame(Term = term, SE = round(se, 4), Power = round(power, 4),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, results)
}

# ---------------------------------------------------------------------------
# coded_design_matrix()
# Apply coding transforms and return the coded design matrix
# ---------------------------------------------------------------------------
coded_design_matrix <- function(data, factors, transforms = list(),
                                 coding_values = list()) {
  df <- data[, factors, drop = FALSE]
  for (cn in factors) {
    if (!is.numeric(df[[cn]]))
      df[[cn]] <- suppressWarnings(as.numeric(as.character(df[[cn]])))
    tr <- transforms[[cn]]
    if (is.null(tr) || tr == "none") next
    if (tr == "centre") {
      df[[cn]] <- df[[cn]] - mean(df[[cn]], na.rm = TRUE)
    } else if (tr == "coding") {
      cv <- coding_values[[cn]]
      if (!is.null(cv)) {
        low  <- cv$low; high <- cv$high
        if (!is.null(low) && !is.null(high) && high != low) {
          df[[cn]] <- (df[[cn]] - (high + low) / 2) / ((high - low) / 2)
        }
      }
    }
  }
  df
}
