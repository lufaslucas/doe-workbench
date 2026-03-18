# R/stat_models.R — Model fitting, backward elimination, coefficients

# ---------------------------------------------------------------------------
# fit_models()
# Returns named list of lm objects
# col_types: named list colname -> "Factor" | "Numeric"
# ---------------------------------------------------------------------------
fit_models <- function(formulas_vec, data, col_types = list(),
                       transforms = list(), coding_values = list()) {
  # Drop internal ID column before fitting
  data <- data[, setdiff(names(data), ROW_ID_COL), drop = FALSE]

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
  errors <- list()
  for (label in names(formulas_vec)) {
    tryCatch({
      f <- as.formula(formulas_vec[[label]])
      # Only set contrasts for factors that appear in this formula
      f_vars <- all.vars(f)
      facs_in_formula <- intersect(fac_cols, f_vars)
      ctr <- setNames(rep(list("contr.sum"), length(facs_in_formula)),
                       facs_in_formula)
      models[[label]] <- model_fit(f, data = data, contrasts = ctr)
    }, error = function(e) {
      errors[[label]] <<- e$message
    })
  }
  list(models = models, errors = errors)
}

# ---------------------------------------------------------------------------
# check_model_notes()
# Detect aliased coefficients, zero-df residuals, and inestimable terms.
# Returns named list: model_name -> list(aliased_terms, dropped_terms, warnings)
# ---------------------------------------------------------------------------
check_model_notes <- function(models_list) {
  notes <- list()
  for (mname in names(models_list)) {
    m <- models_list[[mname]]
    model_notes <- list(aliased = character(), warnings = character())

    cf <- coef(m)
    na_coefs <- names(cf)[is.na(cf)]
    na_coefs <- setdiff(na_coefs, "(Intercept)")

    if (length(na_coefs) > 0) {
      # Map NA coefficients back to term labels
      term_labels <- attr(terms(m), "term.labels")
      assign_vec <- m$assign
      aliased_terms <- character()
      for (i in seq_along(term_labels)) {
        coef_idx <- which(assign_vec == i)
        if (length(coef_idx) > 0 && all(is.na(cf[coef_idx + 1]))) {
          aliased_terms <- c(aliased_terms, term_labels[i])
        }
      }
      if (length(aliased_terms) > 0) {
        model_notes$aliased <- aliased_terms
        model_notes$warnings <- c(model_notes$warnings,
          paste0("Aliased (dropped): ", paste(aliased_terms, collapse = ", "),
                 ". Design cannot independently estimate these terms."))
      }
    }

    # Zero residual df
    if (df.residual(m) == 0) {
      model_notes$warnings <- c(model_notes$warnings,
        "Saturated model (0 residual df). Cannot estimate error variance or compute F-tests.")
    } else if (df.residual(m) <= 2) {
      model_notes$warnings <- c(model_notes$warnings,
        paste0("Very few residual df (", df.residual(m),
               "). F-tests have low power."))
    }

    # Quadratic terms in 2-level designs
    term_labels <- attr(terms(m), "term.labels")
    quad_terms <- grep("^I\\(", term_labels, value = TRUE)
    if (length(quad_terms) > 0) {
      for (qt in quad_terms) {
        base_var <- gsub("^I\\((.+)\\^[0-9]+\\)$", "\\1", qt)
        if (base_var %in% names(m$model)) {
          n_levels <- length(unique(m$model[[base_var]]))
          if (n_levels <= 2) {
            model_notes$warnings <- c(model_notes$warnings,
              paste0(qt, ": cannot estimate with only ", n_levels,
                     " unique level(s) of ", base_var, "."))
          }
        }
      }
    }

    if (length(model_notes$warnings) > 0) {
      notes[[mname]] <- model_notes
    }
  }
  notes
}

# ---------------------------------------------------------------------------
# backward_eliminate()
# Remove terms with p > alpha one at a time (highest p first) using Type III SS
# Returns the pruned lm model
# ---------------------------------------------------------------------------
backward_eliminate <- function(model, alpha = 0.05) {
  ctr <- model$contrasts
  mod_data <- model$model
  repeat {
    a <- tryCatch(model_anova(model, type = 3), error = function(e) NULL)
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
    new_vars <- all.vars(new_f)
    new_ctr <- ctr[intersect(names(ctr), new_vars)]
    if (length(new_ctr) == 0) new_ctr <- NULL
    model <- model_fit(new_f, data = mod_data, contrasts = new_ctr)
  }
  model
}

# ---------------------------------------------------------------------------
# coef_table()
# Extract model coefficients with proper labels for sum-to-zero contrasts.
# Returns data.frame: Model | Term | Estimate | Std.Error | t.value | p.value | Coding
# ---------------------------------------------------------------------------
coef_table <- function(models_list, transforms = list(), coding_values = list()) {
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

    # Add coding annotation to terms
    cf$Coding <- sapply(cf$raw_name, function(rn) {
      if (rn == "(Intercept)") return("")
      parts <- strsplit(rn, ":")[[1]]
      base_vars <- sapply(parts, function(p) {
        for (fn in names(label_map)) {
          if (p == fn) return(gsub("[0-9]+$", "", fn))
        }
        if (grepl("^I\\(", p)) {
          return(gsub("^I\\((.+)\\^[0-9]+\\)$", "\\1", p))
        }
        p
      })
      codings <- sapply(base_vars, function(bv) {
        tr <- transforms[[bv]]
        if (is.null(tr) || tr == "none") return(NULL)
        if (tr == "centre") return(paste0(bv, ": centred"))
        if (tr == "coding") {
          cv <- coding_values[[bv]]
          if (!is.null(cv))
            return(paste0(bv, ": coded [", cv$low, ",", cv$high, "]\u2192[\u22121,+1]"))
          return(paste0(bv, ": coded"))
        }
        NULL
      })
      codings <- Filter(Negate(is.null), codings)
      if (length(codings) == 0) return("")
      paste(unique(codings), collapse = "; ")
    })

    cf$Model <- mname
    rows[[mname]] <- cf[, c("Model", "Term", "Estimate", "Std.Error", "t.value", "p.value", "Coding")]
  }
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# run_diagnostics()
# Runs normality, heteroscedasticity, and outlier checks on a fitted lm
# Returns a list of diagnostic results with traffic-light levels
# ---------------------------------------------------------------------------
run_diagnostics <- function(model) {
  diags <- list()
  n <- length(residuals(model))

  # 1. Normality: Shapiro-Wilk test
  sw <- tryCatch({
    # Shapiro-Wilk requires 3 <= n <= 5000
    if (n >= 3 && n <= 5000) shapiro.test(residuals(model)) else NULL
  }, error = function(e) NULL)
  if (!is.null(sw)) {
    lvl <- if (sw$p.value >= 0.05) "green"
           else if (sw$p.value >= 0.01) "amber"
           else "red"
    diags$normality <- list(
      test = "Shapiro-Wilk",
      stat = round(sw$statistic, 4),
      p.value = signif(sw$p.value, 4),
      level = lvl,
      message = switch(lvl,
        green = "Residuals appear normally distributed.",
        amber = "Mild departure from normality detected.",
        red   = "Significant departure from normality.")
    )
  }

  # 2. Heteroscedasticity: Breusch-Pagan test
  bp <- tryCatch(model_bp_test(model), error = function(e) NULL)
  if (!is.null(bp)) {
    lvl <- if (bp$p.value >= 0.05) "green"
           else if (bp$p.value >= 0.01) "amber"
           else "red"
    diags$heteroscedasticity <- list(
      test = "Breusch-Pagan",
      stat = round(bp$statistic, 4),
      p.value = signif(bp$p.value, 4),
      level = lvl,
      message = switch(lvl,
        green = "Variance appears constant (homoscedastic).",
        amber = "Some evidence of non-constant variance.",
        red   = "Significant non-constant variance detected.")
    )
  }

  # 3. Outliers: count of |rstudent| > 2 and > 3
  rst <- tryCatch(model_studentized_residuals(model), error = function(e) numeric(0))
  if (length(rst) > 0) {
    n_warn <- sum(abs(rst) > 2)
    n_severe <- sum(abs(rst) > 3)
    lvl <- if (n_severe > 0) "red"
           else if (n_warn > 0) "amber"
           else "green"
    outlier_pts <- if (n_warn > 0) names(rst)[abs(rst) > 2] else character(0)
    diags$outliers <- list(
      test = "Studentized Residuals",
      n_moderate = n_warn,
      n_severe = n_severe,
      level = lvl,
      flagged = outlier_pts,
      message = switch(lvl,
        green = "No outlier points flagged.",
        amber = paste0(n_warn, " point(s) with |rstudent| > 2."),
        red   = paste0(n_severe, " point(s) with |rstudent| > 3."))
    )
  }

  # 4. Influential points: Cook's distance
  cd <- tryCatch(model_cooks_distance(model), error = function(e) numeric(0))
  if (length(cd) > 0) {
    threshold <- 4 / n
    n_infl <- sum(cd > threshold)
    lvl <- if (n_infl == 0) "green"
           else if (n_infl <= 2) "amber"
           else "red"
    infl_pts <- if (n_infl > 0) names(cd)[cd > threshold] else character(0)
    diags$influence <- list(
      test = "Cook's Distance",
      threshold = round(threshold, 4),
      n_influential = n_infl,
      level = lvl,
      flagged = infl_pts,
      message = switch(lvl,
        green = "No influential points detected.",
        amber = paste0(n_infl, " point(s) exceed Cook's D threshold (", round(threshold, 3), ")."),
        red   = paste0(n_infl, " point(s) exceed Cook's D threshold (", round(threshold, 3), ")."))
    )
  }

  # 5. Overall level (worst of individual levels)
  levels_map <- c(green = 1, amber = 2, red = 3)
  all_levels <- sapply(diags, function(d) levels_map[d$level])
  worst <- if (length(all_levels) > 0) names(which.max(all_levels)) else "green"
  # Map back
  overall <- c("1" = "green", "2" = "amber", "3" = "red")[as.character(max(all_levels, 1))]

  diags$overall <- list(
    level = overall,
    message = switch(overall,
      green = "All diagnostics look good.",
      amber = "Some diagnostics show mild concerns \u2014 review flagged items.",
      red   = "One or more diagnostics show significant issues \u2014 model assumptions may be violated.")
  )

  diags
}
