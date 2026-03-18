# R/stat_anova.R — ANOVA, VIF, term classification & ordering

# ---------------------------------------------------------------------------
# type3_wide()
# Returns data.frame: term | Model1_F | Model1_p | Model1_SS | Model1_MS | Model1_df | ...
# ---------------------------------------------------------------------------
type3_wide <- function(models_list, term_roles = NULL, sort_mode = "category") {
  if (length(models_list) == 0) return(data.frame())

  all_anova <- lapply(names(models_list), function(mname) {
    m <- models_list[[mname]]

    # Skip saturated models (0 residual df) — no error variance for F-tests
    if (df.residual(m) == 0) {
      message("Skipping ANOVA for '", mname, "': saturated model (0 residual df).")
      return(NULL)
    }

    anova_from_model <- function(mod) {
      a  <- model_anova(mod, type = 3)
      df <- as.data.frame(a)
      df$term <- rownames(df)
      names(df)[names(df) == "Sum Sq"]  <- "SS"
      names(df)[names(df) == "Df"]      <- "df_val"
      names(df)[names(df) == "F value"] <- "F_value"
      names(df)[names(df) == "Pr(>F)"]  <- "p_value"
      df$MS <- ifelse(df$df_val > 0, df$SS / df$df_val, NA_real_)
      df <- df[df$term != "(Intercept)", , drop = FALSE]
      out <- df[, c("term", "F_value", "p_value", "SS", "MS", "df_val"), drop = FALSE]
      names(out)[2:6] <- c(paste0(mname, "_F"), paste0(mname, "_p"),
                            paste0(mname, "_SS"), paste0(mname, "_MS"),
                            paste0(mname, "_df"))
      out
    }
    tryCatch({
      anova_from_model(m)
    }, error = function(e) {
      # If ANOVA fails (likely aliased coefficients), try refitting without aliased terms
      cf <- coef(m)
      na_coefs <- names(cf)[is.na(cf)]
      na_coefs <- setdiff(na_coefs, "(Intercept)")
      if (length(na_coefs) == 0) {
        message("ANOVA error for '", mname, "': ", e$message)
        return(NULL)
      }
      # Map aliased coefficients back to term labels
      term_labels <- attr(terms(m), "term.labels")
      assign_vec <- m$assign
      # Find terms where ALL coefficients are NA
      aliased_terms <- character()
      for (i in seq_along(term_labels)) {
        coef_idx <- which(assign_vec == i)
        if (length(coef_idx) > 0 && all(is.na(cf[coef_idx + 1]))) {
          aliased_terms <- c(aliased_terms, term_labels[i])
        }
      }
      keep_terms <- setdiff(term_labels, aliased_terms)
      if (length(keep_terms) == 0) {
        message("ANOVA error for '", mname, "': all terms aliased")
        return(NULL)
      }
      # Refit without aliased terms
      resp <- all.vars(formula(m))[1]
      new_f <- as.formula(paste(resp, "~", paste(keep_terms, collapse = " + ")))
      m_reduced <- tryCatch(
        model_fit(new_f, data = m$model, contrasts = m$contrasts),
        error = function(e2) NULL
      )
      if (is.null(m_reduced)) return(NULL)
      tryCatch(anova_from_model(m_reduced), error = function(e2) NULL)
    })
  })

  all_anova <- Filter(Negate(is.null), all_anova)
  if (length(all_anova) == 0) return(data.frame())

  wide <- Reduce(function(a, b) merge(a, b, by = "term", all = TRUE), all_anova)
  order_anova_terms(wide, term_roles, sort_mode = sort_mode)
}

# ---------------------------------------------------------------------------
# rmse_summary()
# Returns data.frame: Model | RMSE | R2 | AdjR2 | AIC | BIC | PRESS_RMSE | df_resid
# ---------------------------------------------------------------------------
rmse_summary <- function(models_list) {
  if (length(models_list) == 0) return(data.frame())

  # PRESS RMSE = sqrt(PRESS / n) where PRESS = sum of (residual / (1 - hat_ii))^2
  calc_press_rmse <- function(m) {
    tryCatch({
      h <- model_hat_values(m)
      r <- residuals(m)
      press <- sum((r / (1 - h))^2)
      sqrt(press / length(r))
    }, error = function(e) NA_real_)
  }

  data.frame(
    Model      = names(models_list),
    RMSE       = sapply(models_list, function(m) round(sigma(m), 4)),
    R2         = sapply(models_list, function(m) round(summary(m)$r.squared, 4)),
    Adj_R2     = sapply(models_list, function(m) round(summary(m)$adj.r.squared, 4)),
    AIC        = sapply(models_list, function(m) round(AIC(m), 2)),
    BIC        = sapply(models_list, function(m) round(BIC(m), 2)),
    PRESS_RMSE = sapply(models_list, function(m) round(calc_press_rmse(m), 4)),
    df_resid   = sapply(models_list, function(m) df.residual(m)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ---------------------------------------------------------------------------
# order_anova_terms()
# Reorder ANOVA wide table by category or by significance
# ---------------------------------------------------------------------------
order_anova_terms <- function(wide, term_roles = NULL, sort_mode = "category") {
  if (nrow(wide) == 0) return(wide)

  if (sort_mode == "significance") {
    p_cols <- grep("_p$", names(wide), value = TRUE)
    if (length(p_cols) > 0) {
      wide$`.sort` <- apply(wide[, p_cols, drop = FALSE], 1, function(row) {
        vals <- suppressWarnings(as.numeric(row))
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0) return(Inf)
        min(vals)
      })
      wide$`.sort`[wide$term == "Residuals"] <- Inf
      wide <- wide[order(wide$`.sort`), , drop = FALSE]
      wide$`.sort` <- NULL
      rownames(wide) <- NULL
      return(wide)
    }
  }

  if (is.null(term_roles)) return(wide)
  role_order <- c(
    "Factor" = 1, "Factor_Polynomial" = 2, "Factor_Interaction" = 3,
    "Block" = 4, "Covariate" = 5,
    "Block_Interaction" = 6, "Covariate_Interaction" = 7,
    "Residuals" = 8
  )
  wide$`.sort` <- sapply(wide$term, function(t) {
    if (t == "Residuals") return(8)
    r <- term_roles[t]
    if (is.na(r)) return(5.5)
    role_order[r] %||% 5.5
  })
  wide <- wide[order(wide$`.sort`), , drop = FALSE]
  wide$`.sort` <- NULL
  rownames(wide) <- NULL
  wide
}

# ---------------------------------------------------------------------------
# classify_anova_terms()
# Returns named vector with 8 categories
# ---------------------------------------------------------------------------
classify_anova_terms <- function(terms, factor_cols, block_cols, cov_cols) {
  sapply(terms, function(t) {
    if (t == "Residuals") return("Residuals")

    # Polynomial term: I(var^n)
    is_poly <- grepl("^I\\(", t)
    if (is_poly) {
      base_var <- gsub("^I\\((.+)\\^[0-9]+\\)$", "\\1", t)
      if (base_var %in% factor_cols) return("Factor_Polynomial")
      if (base_var %in% cov_cols)    return("Covariate")
      return("Factor_Polynomial")
    }

    parts <- strsplit(t, ":")[[1]]
    is_interaction <- length(parts) > 1

    has_factor <- any(parts %in% factor_cols)
    has_block  <- any(parts %in% block_cols)
    has_cov    <- any(parts %in% cov_cols)

    if (!is_interaction) {
      if (has_factor) return("Factor")
      if (has_block)  return("Block")
      if (has_cov)    return("Covariate")
      return("Covariate")
    }

    # Interaction: classify by what it involves
    if (has_block)  return("Block_Interaction")
    if (has_cov)    return("Covariate_Interaction")
    if (has_factor) return("Factor_Interaction")
    return("Covariate_Interaction")
  }, USE.NAMES = TRUE)
}

# ---------------------------------------------------------------------------
# vif_summary()
# Compute VIF for each model; return wide data.frame
# Values are GVIF^(1/(2*Df)) which is comparable to sqrt(VIF)
# ---------------------------------------------------------------------------
vif_summary <- function(models_list) {
  if (length(models_list) == 0) return(data.frame())

  # Collect per-model VIF values in long form (all terms including interactions)
  long_rows <- lapply(names(models_list), function(mname) {
    m <- models_list[[mname]]
    tryCatch({
      if (length(coef(m)) <= 2) return(NULL)
      v <- model_vif(m)
      if (is.matrix(v)) {
        # Multi-df terms: matrix with GVIF, Df, GVIF^(1/(2*Df))
        gcol <- "GVIF^(1/(2*Df))"
        vif_vals <- if (gcol %in% colnames(v)) v[, gcol] else v[, 1]^(1/(2*v[, 2]))
        data.frame(Term = rownames(v), VIF = round(vif_vals, 2),
                   Model = mname, stringsAsFactors = FALSE)
      } else {
        # Single-df terms: named numeric vector
        data.frame(Term = names(v), VIF = round(v, 2),
                   Model = mname, stringsAsFactors = FALSE)
      }
    }, error = function(e) NULL)
  })

  long <- do.call(rbind, Filter(Negate(is.null), long_rows))
  if (is.null(long) || nrow(long) == 0) return(data.frame())

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
