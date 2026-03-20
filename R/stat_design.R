# R/stat_design.R — Design analysis: aliases, power, coding, simulation

# ---------------------------------------------------------------------------
# detect_inestimable_terms()
# Check which terms in a formula are inestimable given the design data.
# Returns character vector of inestimable term labels.
# ---------------------------------------------------------------------------
detect_inestimable_terms <- function(formula_str, data) {
  f <- tryCatch(as.formula(formula_str), error = function(e) NULL)
  if (is.null(f)) return(character())

  resp <- all.vars(f)[1]
  if (!resp %in% names(data)) return(character())

  tryCatch({
    d <- data
    # Use dummy response if missing or all-NA (estimability doesn't depend on Y)
    if (all(is.na(d[[resp]]))) d[[resp]] <- rnorm(nrow(d))

    m <- lm(f, data = d)
    cf <- coef(m)
    na_coefs <- names(cf)[is.na(cf)]
    na_coefs <- setdiff(na_coefs, "(Intercept)")
    if (length(na_coefs) == 0) return(character())

    # Map coefficients back to term labels
    term_labels <- attr(terms(m), "term.labels")
    assign_vec <- m$assign

    inestimable <- character()
    for (i in seq_along(term_labels)) {
      coef_idx <- which(assign_vec == i)
      # Term is inestimable if ALL its coefficients are NA
      if (length(coef_idx) > 0 && all(is.na(cf[coef_idx + 1]))) {
        inestimable <- c(inestimable, term_labels[i])
      }
    }
    inestimable
  }, error = function(e) character())
}

# ---------------------------------------------------------------------------
# collapse_aliased_formulas()
# Given formulas and alias info, remove redundant aliased terms and build
# alias_labels mapping (e.g., "A:B" -> "A:B + C:D").
# Returns list(formulas, alias_labels).
# ---------------------------------------------------------------------------
collapse_aliased_formulas <- function(formulas, alias_map) {
  labels <- list()
  new_formulas <- character()

  for (f in formulas) {
    af <- alias_map[[f]]
    if (is.null(af) || nrow(af) == 0) {
      new_formulas <- c(new_formulas, setNames(f, names(formulas)[formulas == f]))
      next
    }

    # Parse formula terms
    parsed <- tryCatch(as.formula(f), error = function(e) NULL)
    if (is.null(parsed)) { new_formulas <- c(new_formulas, setNames(f, f)); next }
    tt <- terms(parsed)
    resp <- as.character(parsed[[2]])
    term_labels <- attr(tt, "term.labels")

    # Build alias groups via union-find
    groups <- as.list(term_labels)
    names(groups) <- term_labels
    find_root <- function(t) {
      while (groups[[t]][1] != t) t <- groups[[t]][1]
      t
    }
    for (i in seq_len(nrow(af))) {
      t1 <- af$Term_1[i]; t2 <- af$Term_2[i]
      if (!t1 %in% term_labels || !t2 %in% term_labels) next
      r1 <- find_root(t1); r2 <- find_root(t2)
      if (r1 != r2) groups[[r2]] <- c(r1)
    }

    # Collect alias groups: root -> all members
    alias_groups <- list()
    for (t in term_labels) {
      root <- find_root(t)
      alias_groups[[root]] <- unique(c(alias_groups[[root]], t))
    }

    # Build new term list: keep root, label it with all members
    keep_terms <- character()
    for (root in names(alias_groups)) {
      members <- alias_groups[[root]]
      keep_terms <- c(keep_terms, root)
      if (length(members) > 1) {
        labels[[root]] <- paste(members, collapse = " + ")
      }
    }

    new_f <- paste0(resp, " ~ ", paste(keep_terms, collapse = " + "))
    new_formulas <- c(new_formulas, setNames(new_f, new_f))
  }

  list(formulas = new_formulas, alias_labels = labels)
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
  # Also flag within-model collinearity at a lower threshold (|r| > 0.5)
  COLLINEARITY_THRESHOLD <- 0.5
  nc <- length(valid_terms)
  aliases <- list()
  if (nc >= 2) {
    for (i in 1:(nc - 1)) {
      for (j in (i + 1):nc) {
        r <- cor_mat[i, j]
        if (is.na(r)) next
        both_in <- valid_terms[i] %in% full_terms && valid_terms[j] %in% full_terms
        # Report at alias threshold OR within-model collinearity > 0.5
        if (abs(r) >= threshold || (both_in && abs(r) > COLLINEARITY_THRESHOLD)) {
          severity <- if (abs(r) >= threshold) "Aliased"
                      else if (abs(r) >= 0.8) "High collinearity"
                      else "Moderate collinearity"
          aliases <- c(aliases, list(data.frame(
            Term_1      = valid_terms[i],
            Term_2      = valid_terms[j],
            Correlation = round(r, 3),
            Severity    = severity,
            In_model    = if (both_in) "Both in model"
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
    return(data.frame(Message = "No aliases or collinearity detected."))
  result <- do.call(rbind, aliases)
  result[order(-abs(result$Correlation)), , drop = FALSE]
}

# ---------------------------------------------------------------------------
# design_summary()
# Build a plain-language design summary: df budget, estimability,
# confounded pairs, replication, and power notes.
# Returns a list of sections, each with title, level (green/amber/red), items.
# ---------------------------------------------------------------------------
design_summary <- function(data, full_terms, check_terms = NULL,
                           threshold = 0.99, col_types = list()) {
  sections <- list()
  n <- nrow(data)

  # 1. Degrees of freedom budget
  df_items <- list()
  df_items <- c(df_items, list(paste0("Total runs: ", n)))

  # Build model matrix to count model df
  tryCatch({
    df_work <- data
    for (cn in names(df_work)) {
      if (!is.numeric(df_work[[cn]])) {
        df_work[[cn]] <- as.numeric(as.factor(df_work[[cn]]))
        df_work[[cn]] <- df_work[[cn]] - mean(df_work[[cn]])
      }
    }
    f_str <- paste("~", paste(full_terms, collapse = " + "))
    mm <- model.matrix(as.formula(f_str), data = df_work)
    model_df <- ncol(mm) - 1  # exclude intercept
    resid_df <- n - model_df - 1
    df_items <- c(df_items, list(
      paste0("Model df: ", model_df, " (", length(full_terms), " terms)"),
      paste0("Residual df: ", resid_df)
    ))
    df_level <- if (resid_df == 0) "red"
                else if (resid_df <= 2) "amber"
                else "green"
    if (resid_df == 0)
      df_items <- c(df_items, list("Saturated model \u2014 no df for error estimation or F-tests."))
    else if (resid_df <= 2)
      df_items <- c(df_items, list(paste0("Very few residual df (", resid_df, "). F-tests will have low power.")))

    # Pure error df from replicates
    treatments <- apply(df_work[, setdiff(names(df_work), names(df_work)[sapply(df_work, function(x) all(x == x[1]))]), drop = FALSE], 1, paste, collapse = "_")
    rep_table <- table(treatments)
    n_reps <- sum(rep_table > 1)
    pe_df <- sum(rep_table - 1)
    if (pe_df > 0)
      df_items <- c(df_items, list(paste0("Pure error df: ", pe_df, " (from ", n_reps, " replicated treatment(s))")))
    else
      df_items <- c(df_items, list("No replicated treatments \u2014 cannot estimate pure error or lack-of-fit."))
  }, error = function(e) {
    df_level <- "amber"
    df_items <- c(df_items, list(paste0("Could not compute model matrix: ", e$message)))
  })
  sections$df <- list(title = "Degrees of Freedom Budget", level = df_level %||% "amber", items = df_items)

  # 2. Confounded pairs
  alias_result <- compute_aliases(data, full_terms, check_terms, threshold)
  conf_items <- list()
  conf_level <- "green"
  if ("Correlation" %in% names(alias_result)) {
    aliased <- alias_result[alias_result$Severity == "Aliased", , drop = FALSE]
    high_col <- alias_result[alias_result$Severity == "High collinearity", , drop = FALSE]
    mod_col <- alias_result[alias_result$Severity == "Moderate collinearity", , drop = FALSE]

    if (nrow(aliased) > 0) {
      conf_level <- "red"
      for (i in seq_len(nrow(aliased)))
        conf_items <- c(conf_items, list(paste0(
          aliased$Term_1[i], " and ", aliased$Term_2[i],
          " are fully confounded (r = ", aliased$Correlation[i], "). ",
          "Their effects cannot be separated.")))
    }
    if (nrow(high_col) > 0) {
      if (conf_level != "red") conf_level <- "amber"
      for (i in seq_len(nrow(high_col)))
        conf_items <- c(conf_items, list(paste0(
          high_col$Term_1[i], " and ", high_col$Term_2[i],
          " are highly correlated (r = ", high_col$Correlation[i], "). ",
          "Estimates will be unstable.")))
    }
    if (nrow(mod_col) > 0) {
      if (conf_level == "green") conf_level <- "amber"
      for (i in seq_len(min(nrow(mod_col), 5)))
        conf_items <- c(conf_items, list(paste0(
          mod_col$Term_1[i], " and ", mod_col$Term_2[i],
          " have moderate correlation (r = ", mod_col$Correlation[i], ").")))
      if (nrow(mod_col) > 5)
        conf_items <- c(conf_items, list(paste0("... and ", nrow(mod_col) - 5, " more moderately correlated pairs.")))
    }
  }
  if (length(conf_items) == 0) conf_items <- list("No confounding or collinearity detected among model terms.")
  sections$confounding <- list(title = "Confounding & Collinearity", level = conf_level, items = conf_items)

  # 3. Replication
  rep_items <- list()
  rep_level <- "green"
  tryCatch({
    # Count unique treatment combinations (factor columns only)
    fac_cols <- names(data)[!sapply(data, is.numeric)]
    if (length(fac_cols) > 0) {
      trt_combos <- apply(data[, fac_cols, drop = FALSE], 1, paste, collapse = "_")
      trt_table <- table(trt_combos)
      n_unique <- length(trt_table)
      n_replicated <- sum(trt_table > 1)
      rep_items <- c(rep_items, list(
        paste0(n_unique, " unique treatment combination(s) across ", n, " runs."),
        paste0(n_replicated, " treatment(s) replicated (", sum(trt_table > 1), " with reps).")
      ))
      if (n_replicated == 0) {
        rep_level <- "amber"
        rep_items <- c(rep_items, list("No replication \u2014 cannot estimate pure error."))
      }
      if (any(trt_table > 1) && max(trt_table) != min(trt_table[trt_table > 1])) {
        rep_items <- c(rep_items, list("Unequal replication across treatments."))
      }
    } else {
      rep_items <- c(rep_items, list("All columns are numeric \u2014 replication assessment not applicable for continuous designs."))
    }
  }, error = function(e) NULL)
  sections$replication <- list(title = "Replication", level = rep_level, items = rep_items)

  sections
}

# ---------------------------------------------------------------------------
# design_power()
# Compute power for each term based on the design matrix.
# delta = minimum detectable effect, sigma = error SD
# If `model_terms` is provided, use those terms directly.
# Otherwise generate from `factors` up to `max_order`.
# Returns data.frame: Term | df | SE | Power
# ---------------------------------------------------------------------------
design_power <- function(data, factors, sigma, delta, alpha = 0.05, max_order = 2,
                          model_terms = NULL) {
  if (sigma <= 0) return(data.frame())

  # Centre numeric columns for power calculation
  df <- data
  # Identify all columns referenced by model terms (factors, blocks, covariates)
  if (!is.null(model_terms) && length(model_terms) > 0) {
    raw_parts <- unique(unlist(lapply(model_terms, function(t) {
      if (grepl("^I\\(", t)) {
        inner <- gsub("^I\\((.+)\\)$", "\\1", t)
        return(gsub("\\^.*", "", inner))
      }
      strsplit(t, ":")[[1]]
    })))
    all_vars <- unique(c(factors, raw_parts))
  } else {
    all_vars <- factors
  }
  for (col in intersect(all_vars, names(df))) {
    if (!is.numeric(df[[col]])) {
      df[[col]] <- as.numeric(as.factor(df[[col]]))
    }
    df[[col]] <- df[[col]] - mean(df[[col]], na.rm = TRUE)
  }

  n <- nrow(df)

  # Determine terms: use model_terms if provided, else generate combinatorially
  if (!is.null(model_terms) && length(model_terms) > 0) {
    all_terms <- model_terms
  } else {
    if (length(factors) == 0) return(data.frame())
    all_terms <- character()
    for (order in seq_len(min(max_order, length(factors)))) {
      combos <- combn(factors, order, simplify = FALSE)
      all_terms <- c(all_terms, sapply(combos, paste, collapse = ":"))
    }
  }

  p_total <- length(all_terms)
  df_error <- n - p_total - 1
  if (df_error <= 0)
    return(data.frame(Term = all_terms, SE = NA, Power = NA,
                      Note = "Insufficient residual df"))

  # Build column for each term (interactions, polynomials)
  build_col <- function(term) {
    # Handle I(x^n) polynomial terms
    if (grepl("^I\\(", term)) {
      inner <- gsub("^I\\((.+)\\)$", "\\1", term)
      if (grepl("\\^", inner)) {
        pts <- strsplit(inner, "\\^")[[1]]
        var_name <- trimws(pts[1])
        pw <- as.numeric(trimws(pts[2]))
        if (var_name %in% names(df) && !is.na(pw)) return(df[[var_name]]^pw)
      }
      return(NULL)
    }
    parts <- strsplit(term, ":")[[1]]
    if (!all(parts %in% names(df))) return(NULL)
    x <- rep(1, n)
    for (p_name in parts) x <- x * df[[p_name]]
    x
  }

  results <- lapply(all_terms, function(term) {
    x <- build_col(term)
    if (is.null(x)) return(data.frame(Term = term, SE = NA, Power = NA_real_,
                                       stringsAsFactors = FALSE))
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

# ---------------------------------------------------------------------------
# compute_alias_matrix()
# Compute the alias matrix A = (X'X)^{-1} X' Z
# where X = model matrix columns, Z = alias-only columns.
# For each model term i, E[beta_hat_i] = beta_i + sum_j A_ij * gamma_j
# Returns list: model_terms, alias_terms, A (matrix), X_model, X_alias
# ---------------------------------------------------------------------------
compute_alias_matrix <- function(data, model_terms, alias_terms) {
  df <- data
  for (cn in names(df)) {
    if (!is.numeric(df[[cn]])) df[[cn]] <- as.numeric(as.factor(df[[cn]]))
    df[[cn]] <- df[[cn]] - mean(df[[cn]], na.rm = TRUE)
  }

  build_col <- function(term) {
    # Handle I(x^n) polynomial terms
    if (grepl("^I\\(", term)) {
      inner <- gsub("^I\\((.+)\\)$", "\\1", term)
      if (grepl("\\^", inner)) {
        parts <- strsplit(inner, "\\^")[[1]]
        var_name <- trimws(parts[1])
        power <- as.numeric(trimws(parts[2]))
        if (var_name %in% names(df) && !is.na(power)) return(df[[var_name]]^power)
      }
      return(NULL)
    }
    parts <- strsplit(term, ":")[[1]]
    if (!all(parts %in% names(df))) return(NULL)
    col_vals <- rep(1, nrow(df))
    for (p in parts) col_vals <- col_vals * df[[p]]
    col_vals
  }

  # Build model columns
  Xm_list <- list(); valid_model <- character()
  for (t in model_terms) {
    cv <- build_col(t)
    if (!is.null(cv) && sd(cv) > 1e-10) {
      Xm_list[[t]] <- cv; valid_model <- c(valid_model, t)
    }
  }
  if (length(valid_model) == 0) return(NULL)

  # Build alias-only columns
  alias_only <- setdiff(alias_terms, model_terms)
  Xa_list <- list(); valid_alias <- character()
  for (t in alias_only) {
    cv <- build_col(t)
    if (!is.null(cv) && sd(cv) > 1e-10) {
      Xa_list[[t]] <- cv; valid_alias <- c(valid_alias, t)
    }
  }

  Xm <- do.call(cbind, Xm_list)
  if (length(valid_alias) == 0) {
    return(list(model_terms = valid_model, alias_terms = character(0),
                A = matrix(nrow = length(valid_model), ncol = 0,
                           dimnames = list(valid_model, NULL)),
                X_model = Xm,
                X_alias = matrix(nrow = nrow(df), ncol = 0)))
  }

  Xa <- do.call(cbind, Xa_list)
  XtX_inv <- tryCatch(solve(crossprod(Xm)), error = function(e) NULL)
  if (is.null(XtX_inv)) return(NULL)

  A <- XtX_inv %*% crossprod(Xm, Xa)
  rownames(A) <- valid_model; colnames(A) <- valid_alias

  list(model_terms = valid_model, alias_terms = valid_alias,
       A = A, X_model = Xm, X_alias = Xa)
}

# ---------------------------------------------------------------------------
# compute_real_effects()
# Given model effects, alias effects, and alias matrix, compute biased estimates.
# Returns data.frame: Term | True_Coefficient | Confounding_Bias | Apparent_Effect | Contributors
# ---------------------------------------------------------------------------
compute_real_effects <- function(alias_info, model_effects, alias_effects = NULL) {
  model_terms <- alias_info$model_terms
  A <- alias_info$A

  true_eff <- sapply(model_terms, function(t) {
    v <- model_effects[[t]]
    if (is.null(v)) 0 else v
  })

  result <- data.frame(Term = model_terms, True_Coefficient = true_eff,
                        stringsAsFactors = FALSE)

  if (!is.null(alias_effects) && length(alias_info$alias_terms) > 0 && ncol(A) > 0) {
    gamma <- sapply(alias_info$alias_terms, function(t) {
      v <- alias_effects[[t]]
      if (is.null(v)) 0 else v
    })
    bias <- as.numeric(A %*% gamma)
    result$Confounding_Bias <- round(bias, 4)
    result$Apparent_Effect <- round(true_eff + bias, 4)
    result$Contributors <- sapply(seq_len(nrow(A)), function(i) {
      contributions <- A[i, ] * gamma
      nonzero <- which(abs(contributions) > 1e-6)
      if (length(nonzero) == 0) return("")
      paste(paste0(alias_info$alias_terms[nonzero], "(",
                   round(A[i, nonzero], 3), "\u00d7",
                   round(gamma[nonzero], 2), ")"),
            collapse = ", ")
    })
  } else {
    result$Confounding_Bias <- 0
    result$Apparent_Effect <- true_eff
    result$Contributors <- ""
  }
  result
}

# ---------------------------------------------------------------------------
# simulate_design_response()
# Simulate y = grand_mean + X_model * beta + X_alias * gamma + N(0, sigma)
# Returns numeric vector of simulated responses
# ---------------------------------------------------------------------------
simulate_design_response <- function(data, alias_info, model_effects,
                                      alias_effects = NULL, sigma = 1,
                                      grand_mean = 0) {
  n <- nrow(data)
  Xm <- alias_info$X_model
  beta <- sapply(alias_info$model_terms, function(t) {
    v <- model_effects[[t]]
    if (is.null(v)) 0 else v
  })
  y <- grand_mean + as.numeric(Xm %*% beta)

  if (!is.null(alias_effects) && ncol(alias_info$X_alias) > 0) {
    gamma <- sapply(alias_info$alias_terms, function(t) {
      v <- alias_effects[[t]]
      if (is.null(v)) 0 else v
    })
    y <- y + as.numeric(alias_info$X_alias %*% gamma)
  }

  y + rnorm(n, 0, sigma)
}
