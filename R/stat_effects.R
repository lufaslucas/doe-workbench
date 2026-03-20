# R/stat_effects.R — LS means, partial effects, multiple comparisons, leverage

# ---------------------------------------------------------------------------
# get_lsmeans_df()
# Compute LS means (estimated marginal means) for a factor/interaction spec
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
                   control = NULL, selected_pairs = NULL,
                   alpha = 0.05) {
  tryCatch({
    spec_list <- strsplit(spec, ":")[[1]]
    em <- suppressMessages(emmeans::emmeans(model, specs = spec_list))

    # Helper: get emmean labels from summary (works for single & multi-factor)
    get_em_labels <- function(em_obj) {
      em_df <- as.data.frame(summary(em_obj))
      if (length(spec_list) > 1) {
        apply(em_df[, spec_list, drop = FALSE], 1, function(r) paste(trimws(r), collapse = " "))
      } else {
        as.character(em_df[[spec_list]])
      }
    }

    pairs <- switch(tolower(method),
      "tukey" = {
        emmeans::contrast(em, method = "pairwise", adjust = "tukey")
      },
      "student" = {
        emmeans::contrast(em, method = "pairwise", adjust = "none")
      },
      "dunnett" = {
        # Proper Dunnett: use trt.vs.ctrl with mvt adjustment
        # This uses the correct Dunnett distribution (k-1 comparisons vs control)
        # rather than filtering from all-pairwise mvt (k*(k-1)/2 comparisons)
        ctrl <- if (!is.null(control) && control != "") control else {
          em_labels <- get_em_labels(em)
          em_labels[1]
        }
        # Find reference index for the chosen control level
        em_labels <- get_em_labels(em)
        ref_idx <- match(ctrl, em_labels)
        if (is.na(ref_idx)) {
          # Try normalized match (comma → space) for multi-factor labels
          em_norm <- gsub(",\\s*", " ", em_labels)
          ref_idx <- match(ctrl, em_norm)
        }
        if (is.na(ref_idx)) ref_idx <- 1L
        dun <- emmeans::contrast(em, method = "trt.vs.ctrl", ref = ref_idx,
                                  adjust = "mvt")
        dun_df <- as.data.frame(summary(dun, infer = c(TRUE, TRUE),
                                         level = 1 - alpha))
        if (nrow(dun_df) == 0) return(data.frame())
        dun_df$method <- method
        dun_df$spec   <- spec
        return(dun_df)
      },
      "custom" = {
        if (!is.null(selected_pairs) && length(selected_pairs) > 0) {
          # Compute all pairwise to get the contrast coefficient matrix
          all_pairs <- emmeans::contrast(em, method = "pairwise", adjust = "none")
          all_df <- as.data.frame(summary(all_pairs))
          contrast_col <- if ("contrast" %in% names(all_df)) "contrast" else names(all_df)[1]
          all_labels <- gsub("\\s+", " ", trimws(all_df[[contrast_col]]))
          selected_norm <- gsub("\\s+", " ", trimws(selected_pairs))
          keep_idx <- which(all_labels %in% selected_norm)
          if (length(keep_idx) > 0) {
            # Extract contrast coefficient matrix and select subset
            # coef() returns data.frame: rows=levels, cols=label+contrasts
            all_coefs <- coef(all_pairs)
            label_col <- names(all_coefs)[1]
            coef_mat  <- as.matrix(all_coefs[, -1, drop = FALSE])  # drop label
            coef_mat_t <- t(coef_mat)  # now rows=contrasts, cols=levels
            sub_t <- coef_mat_t[keep_idx, , drop = FALSE]
            # Convert to list of named contrast vectors for emmeans
            contrast_list <- lapply(seq_len(nrow(sub_t)), function(i) {
              v <- sub_t[i, ]
              names(v) <- all_coefs[[label_col]]
              v
            })
            names(contrast_list) <- rownames(sub_t)
            # Re-run mvt adjustment on just the selected subset
            sub_pairs <- emmeans::contrast(em, method = contrast_list, adjust = "mvt")
            sub_df <- as.data.frame(summary(sub_pairs, infer = c(TRUE, TRUE), level = 1 - alpha))
            # Restore original contrast labels
            if (nrow(sub_df) > 0 && contrast_col %in% names(sub_df))
              sub_df[[contrast_col]] <- all_df[[contrast_col]][keep_idx]
            sub_df$method <- method
            sub_df$spec   <- spec
            return(sub_df)
          }
        }
        # Fall through: no selected pairs, compute all pairwise
        emmeans::contrast(em, method = "pairwise", adjust = "mvt")
      },
      {
        emmeans::contrast(em, method = "pairwise", adjust = "tukey")
      }
    )

    # Get results with confidence intervals
    result <- as.data.frame(summary(pairs, infer = c(TRUE, TRUE), level = 1 - alpha))

    if (nrow(result) == 0) return(data.frame())
    result$method <- method
    result$spec   <- spec
    result
  }, error = function(e) {
    message("MC error (", method, " on ", spec, "): ", e$message)
    data.frame()
  })
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
    n      <- length(x_vals)

    # Build newdata: observed x for this covariate, reference for everything else
    # This correctly handles quadratic terms, interactions, etc.
    pred_vars <- names(mf)[-1]
    newdata <- as.data.frame(lapply(pred_vars, function(cn) {
      if (cn == covariate) {
        x_vals
      } else if (is.numeric(mf[[cn]])) {
        rep(mean(mf[[cn]], na.rm = TRUE), n)
      } else {
        factor(rep(levels(mf[[cn]])[1], n), levels = levels(mf[[cn]]))
      }
    }), check.names = FALSE)
    names(newdata) <- pred_vars

    pred  <- predict(model, newdata = newdata)
    adj_y <- pred + resids

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
