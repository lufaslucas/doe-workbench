# R/stat_robustness.R — Robustness check functions (7 functions)

# ---------------------------------------------------------------------------
# robustness_lsmean_vs_limit()
# Test whether each factor level's LS mean is above/below a limit, across models
# ---------------------------------------------------------------------------
robustness_lsmean_vs_limit <- function(models, spec, limits, directions,
                                        alpha = 0.05) {
  spec_list <- strsplit(spec, ":")[[1]]
  rows <- list()

  for (mn in names(models)) {
    em <- tryCatch(
      suppressMessages(emmeans::emmeans(models[[mn]], specs = spec_list)),
      error = function(e) NULL)
    if (is.null(em)) next
    em_df <- as.data.frame(summary(em, level = 1 - alpha))

    level_keys <- if (length(spec_list) > 1) {
      apply(em_df[, spec_list, drop = FALSE], 1,
            function(r) paste(trimws(r), collapse = ":"))
    } else {
      as.character(em_df[[spec_list]])
    }

    for (i in seq_len(nrow(em_df))) {
      lk <- level_keys[i]
      lim <- limits[[lk]] %||% 0
      dir <- directions[[lk]] %||% ">"
      est <- em_df$emmean[i]

      test_result <- tryCatch({
        tt <- emmeans::test(em, null = lim, side = dir)
        as.data.frame(tt)[i, ]
      }, error = function(e) NULL)

      pval <- if (!is.null(test_result)) {
        p_col <- grep("p.value", names(test_result), value = TRUE)
        if (length(p_col) > 0) test_result[[p_col[1]]] else NA
      } else NA

      same_side <- if (dir == ">") est > lim else est < lim
      sig <- !is.na(pval) && pval < alpha

      rows[[length(rows) + 1]] <- data.frame(
        Level = lk, Model = mn,
        emmean = round(est, 4),
        lower.CL = round(em_df$lower.CL[i], 4),
        upper.CL = round(em_df$upper.CL[i], 4),
        Limit = lim, Direction = dir,
        SameSide = same_side, Significant = sig,
        p.value = if (!is.na(pval)) signif(pval, 4) else NA,
        stringsAsFactors = FALSE)
    }
  }

  if (length(rows) == 0) return(list(detail = data.frame(), summary = data.frame()))
  detail <- do.call(rbind, rows)

  summary_rows <- lapply(unique(detail$Level), function(lv) {
    sub <- detail[detail$Level == lv, , drop = FALSE]
    n_models <- nrow(sub)
    all_same <- all(sub$SameSide)
    all_sig  <- all(sub$Significant, na.rm = TRUE)
    none_sig <- all(!sub$Significant, na.rm = TRUE)
    data.frame(
      Level = lv, Direction = sub$Direction[1], Limit = sub$Limit[1],
      Dir_Verdict = if (all_same) "Robust" else "Fragile",
      Sig_Verdict = if (all_sig) "Robust significant"
                    else if (none_sig) "Robust non-significant"
                    else "Fragile",
      Models_Same_Side = sum(sub$SameSide),
      Models_Significant = sum(sub$Significant, na.rm = TRUE),
      Models_Tested = n_models,
      stringsAsFactors = FALSE)
  })
  list(detail = detail, summary = do.call(rbind, summary_rows))
}

# ---------------------------------------------------------------------------
# robustness_pairwise_order()
# Check whether LS mean ordering is consistent across models
# ---------------------------------------------------------------------------
robustness_pairwise_order <- function(models, spec) {
  spec_list <- strsplit(spec, ":")[[1]]
  all_em <- list()
  for (mn in names(models)) {
    df <- get_lsmeans_df(models[[mn]], spec, mn)
    if (!is.null(df) && nrow(df) > 0) all_em[[mn]] <- df
  }
  if (length(all_em) == 0)
    return(list(detail = data.frame(), summary = data.frame()))

  first_df <- all_em[[1]]
  level_keys <- if (length(spec_list) > 1) {
    apply(first_df[, spec_list, drop = FALSE], 1,
          function(r) paste(trimws(r), collapse = ":"))
  } else as.character(first_df[[spec_list]])
  levels_list <- unique(level_keys)

  if (length(levels_list) < 2)
    return(list(detail = data.frame(), summary = data.frame()))
  pairs <- combn(levels_list, 2, simplify = FALSE)

  # Build level -> emmean map per model
  model_maps <- lapply(all_em, function(df) {
    keys <- if (length(spec_list) > 1) {
      apply(df[, spec_list, drop = FALSE], 1,
            function(r) paste(trimws(r), collapse = ":"))
    } else as.character(df[[spec_list]])
    setNames(df$emmean, keys)
  })

  # Average across models for expected direction
  all_keys <- unique(unlist(lapply(model_maps, names)))
  avg_map <- sapply(all_keys, function(k) {
    vals <- sapply(model_maps, function(mm) if (k %in% names(mm)) mm[k] else NA)
    mean(vals, na.rm = TRUE)
  })

  detail_rows <- list()
  summary_rows <- list()
  for (pr in pairs) {
    a <- pr[1]; b <- pr[2]
    expected <- if (avg_map[a] >= avg_map[b]) paste(a, ">", b) else paste(b, ">", a)
    pair_label <- paste(a, "vs", b)
    consistent_count <- 0L; total_count <- 0L
    for (mn in names(model_maps)) {
      mm <- model_maps[[mn]]
      if (!(a %in% names(mm) && b %in% names(mm))) next
      total_count <- total_count + 1L
      obs_dir <- if (mm[a] >= mm[b]) paste(a, ">", b) else paste(b, ">", a)
      is_con <- (obs_dir == expected)
      if (is_con) consistent_count <- consistent_count + 1L
      detail_rows[[length(detail_rows) + 1]] <- data.frame(
        Pair = pair_label, ExpectedDir = expected, Model = mn,
        A_mean = round(mm[a], 4), B_mean = round(mm[b], 4),
        ObservedDir = obs_dir, Consistent = is_con,
        stringsAsFactors = FALSE)
    }
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      Pair = pair_label, ExpectedDir = expected,
      Consistent = consistent_count, Total = total_count,
      Verdict = if (consistent_count == total_count) "Robust" else "Fragile",
      stringsAsFactors = FALSE)
  }
  list(
    detail  = if (length(detail_rows) > 0) do.call(rbind, detail_rows) else data.frame(),
    summary = if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame())
}

# ---------------------------------------------------------------------------
# robustness_pairwise_significance()
# Check if pairwise significance conclusions are consistent across models
# ---------------------------------------------------------------------------
robustness_pairwise_significance <- function(models, spec,
                                              method = "tukey", alpha = 0.05) {
  detail_rows <- list()
  for (mn in names(models)) {
    mc_df <- run_mc(models[[mn]], spec, method = method, alpha = alpha)
    if (nrow(mc_df) == 0) next
    contrast_col <- if ("contrast" %in% names(mc_df)) "contrast" else names(mc_df)[1]
    p_col <- grep("p.value", names(mc_df), value = TRUE)
    if (length(p_col) == 0) next
    p_col <- p_col[1]
    for (i in seq_len(nrow(mc_df))) {
      pval <- mc_df[[p_col]][i]
      detail_rows[[length(detail_rows) + 1]] <- data.frame(
        Pair = mc_df[[contrast_col]][i], Model = mn,
        estimate = round(mc_df$estimate[i], 4),
        p.value = signif(pval, 4),
        Significant = pval < alpha,
        stringsAsFactors = FALSE)
    }
  }
  if (length(detail_rows) == 0) return(list(detail = data.frame(), summary = data.frame()))
  detail <- do.call(rbind, detail_rows)

  summary_rows <- lapply(unique(detail$Pair), function(pr) {
    sub <- detail[detail$Pair == pr, , drop = FALSE]
    all_sig  <- all(sub$Significant)
    none_sig <- all(!sub$Significant)
    data.frame(
      Pair = pr, Sig_Count = sum(sub$Significant), Total = nrow(sub),
      Verdict = if (all_sig) "Robust significant"
                else if (none_sig) "Robust non-significant"
                else "Fragile",
      stringsAsFactors = FALSE)
  })
  list(detail = detail, summary = do.call(rbind, summary_rows))
}

# ---------------------------------------------------------------------------
# covariate_mediation_check()
# Flag covariates that vary significantly by treatment group
# ---------------------------------------------------------------------------
covariate_mediation_check <- function(data, covariate_cols, factor_cols,
                                       alpha = 0.05, r2_thresh = 0.30) {
  if (length(covariate_cols) == 0 || length(factor_cols) == 0)
    return(data.frame())

  treatment <- if (length(factor_cols) == 1) {
    factor(data[[factor_cols]])
  } else {
    factor(apply(data[, factor_cols, drop = FALSE], 1,
                 function(r) paste(r, collapse = ":")))
  }

  rows <- lapply(covariate_cols, function(cv) {
    if (!cv %in% names(data) || !is.numeric(data[[cv]])) {
      return(data.frame(Covariate = cv, F_stat = NA, p.value = NA,
                        R2 = NA, Verdict = "Skipped",
                        stringsAsFactors = FALSE))
    }
    tryCatch({
      fit <- lm(data[[cv]] ~ treatment)
      af  <- anova(fit)
      f_val <- af$`F value`[1]
      p_val <- af$`Pr(>F)`[1]
      r2    <- summary(fit)$r.squared
      verdict <- if (p_val >= alpha) "OK"
                 else if (r2 < r2_thresh) "Caution"
                 else "Warning \u2014 likely mediator"
      data.frame(Covariate = cv, F_stat = round(f_val, 3),
                 p.value = signif(p_val, 4), R2 = round(r2, 3),
                 Verdict = verdict, stringsAsFactors = FALSE)
    }, error = function(e) {
      data.frame(Covariate = cv, F_stat = NA, p.value = NA,
                 R2 = NA, Verdict = paste("Error:", e$message),
                 stringsAsFactors = FALSE)
    })
  })
  do.call(rbind, rows)
}

# ---------------------------------------------------------------------------
# outlier_influence_check()
# DFBETAS + Cook's D diagnostics with iterative removal
# ---------------------------------------------------------------------------
outlier_influence_check <- function(model, spec, alpha = 0.05, max_iter = 5L) {
  n <- nobs(model)
  cook_thresh  <- COOK_D_THRESH_MULT / n
  dfbeta_thresh <- DFBETA_THRESH_MULT / sqrt(n)

  # Step 1: influence diagnostics on original model
  im <- tryCatch(model_influence(model), error = function(e) NULL)
  if (is.null(im)) return(list(influence_df = data.frame(),
                                removal_log = data.frame(),
                                conclusion_comparison = data.frame(),
                                pairwise_comparison = data.frame()))

  inf_mat <- im$infmat
  cook_col <- grep("cook.d", colnames(inf_mat), value = TRUE)
  dfb_cols <- grep("^dfb\\.", colnames(inf_mat), value = TRUE)

  cook_d <- if (length(cook_col) > 0) inf_mat[, cook_col] else model_cooks_distance(model)
  dfb_mat <- if (length(dfb_cols) > 0) abs(inf_mat[, dfb_cols, drop = FALSE]) else NULL

  max_dfbeta <- if (!is.null(dfb_mat)) apply(dfb_mat, 1, max) else rep(0, n)
  most_affected <- if (!is.null(dfb_mat)) {
    dfb_cols[apply(dfb_mat, 1, which.max)]
  } else rep(NA, n)
  most_affected <- gsub("^dfb\\.", "", most_affected)

  Flagged <- (cook_d > cook_thresh) | (max_dfbeta > dfbeta_thresh)

  # Include data values so users can identify runs
  mf <- model.frame(model)
  influence_df <- data.frame(
    Obs = seq_len(n),
    stringsAsFactors = FALSE)
  # Add all predictor/response columns for identification
  for (col_name in names(mf)) {
    vals <- mf[[col_name]]
    influence_df[[col_name]] <- if (is.numeric(vals)) round(vals, 3) else as.character(vals)
  }
  influence_df$Cook_D <- round(cook_d, 4)
  influence_df$max_DFBETA <- round(max_dfbeta, 4)
  influence_df$Most_Affected <- most_affected
  influence_df$Flagged <- Flagged
  # Sort flagged observations to top
  influence_df <- influence_df[order(-as.integer(Flagged), -cook_d), , drop = FALSE]

  # Step 2: iterative removal
  current_model <- model
  current_data <- model$model
  removal_log <- list()
  removed_obs <- integer(0)
  removed_data_rows <- list()

  for (iter in seq_len(max_iter)) {
    cd <- model_cooks_distance(current_model)
    n_cur <- length(cd)
    thresh_cur <- COOK_D_THRESH_MULT / n_cur
    if (all(cd <= thresh_cur, na.rm = TRUE)) break

    worst <- which.max(cd)
    # Capture the data values of the removed observation
    removed_row <- current_data[worst, , drop = FALSE]
    removed_vals <- paste(
      sapply(names(removed_row), function(cn) {
        v <- removed_row[[cn]]
        paste0(cn, "=", if (is.numeric(v)) round(v, 2) else as.character(v))
      }), collapse = ", ")

    removal_log[[iter]] <- data.frame(
      Iteration = iter, Run_Values = removed_vals,
      Cook_D = round(cd[worst], 4),
      Remaining_Flagged = sum(cd[-worst] > thresh_cur, na.rm = TRUE),
      stringsAsFactors = FALSE)
    removed_obs <- c(removed_obs, worst)
    removed_data_rows[[iter]] <- removed_row
    current_data <- current_data[-worst, , drop = FALSE]
    current_model <- tryCatch(
      model_refit(current_model, data = current_data),
      error = function(e) NULL)
    if (is.null(current_model)) break
  }

  removal_log_df <- if (length(removal_log) > 0) do.call(rbind, removal_log)
                    else data.frame(Iteration = integer(), Run_Values = character(),
                                     Cook_D = numeric(), Remaining_Flagged = integer())

  # Step 3: compare LS means (level-wise)
  spec_list <- strsplit(spec, ":")[[1]]
  conclusion_comparison <- data.frame()
  pairwise_comparison   <- data.frame()

  if (!is.null(current_model) && length(removed_obs) > 0) {
    orig_em_obj <- tryCatch(
      suppressMessages(emmeans::emmeans(model, specs = spec_list)),
      error = function(e) NULL)
    clean_em_obj <- tryCatch(
      suppressMessages(emmeans::emmeans(current_model, specs = spec_list)),
      error = function(e) NULL)

    if (!is.null(orig_em_obj) && !is.null(clean_em_obj)) {
      orig_em  <- as.data.frame(summary(orig_em_obj, level = 1 - alpha))
      clean_em <- as.data.frame(summary(clean_em_obj, level = 1 - alpha))

      orig_keys <- if (length(spec_list) > 1)
        apply(orig_em[, spec_list, drop = FALSE], 1,
              function(r) paste(trimws(r), collapse = ":"))
      else as.character(orig_em[[spec_list]])

      clean_keys <- if (length(spec_list) > 1)
        apply(clean_em[, spec_list, drop = FALSE], 1,
              function(r) paste(trimws(r), collapse = ":"))
      else as.character(clean_em[[spec_list]])

      common_keys <- intersect(orig_keys, clean_keys)
      rows <- lapply(common_keys, function(k) {
        oi <- which(orig_keys == k)[1]
        ci <- which(clean_keys == k)[1]
        data.frame(
          Level = k,
          Original_Estimate = round(orig_em$emmean[oi], 4),
          Clean_Estimate = round(clean_em$emmean[ci], 4),
          Change = round(clean_em$emmean[ci] - orig_em$emmean[oi], 4),
          Pct_Change = round((clean_em$emmean[ci] - orig_em$emmean[oi]) /
                              abs(orig_em$emmean[oi]) * 100, 1),
          stringsAsFactors = FALSE)
      })
      if (length(rows) > 0) conclusion_comparison <- do.call(rbind, rows)

      # Step 4: compare pairwise significance
      orig_pairs <- tryCatch({
        p <- emmeans::contrast(orig_em_obj, method = "pairwise", adjust = "tukey")
        as.data.frame(summary(p, level = 1 - alpha))
      }, error = function(e) NULL)

      clean_pairs <- tryCatch({
        p <- emmeans::contrast(clean_em_obj, method = "pairwise", adjust = "tukey")
        as.data.frame(summary(p, level = 1 - alpha))
      }, error = function(e) NULL)

      if (!is.null(orig_pairs) && !is.null(clean_pairs) &&
          nrow(orig_pairs) > 0 && nrow(clean_pairs) > 0) {
        common_contrasts <- intersect(orig_pairs$contrast, clean_pairs$contrast)
        pw_rows <- lapply(common_contrasts, function(ct) {
          oi <- which(orig_pairs$contrast == ct)[1]
          ci <- which(clean_pairs$contrast == ct)[1]
          orig_sig <- orig_pairs$p.value[oi] < alpha
          clean_sig <- clean_pairs$p.value[ci] < alpha
          orig_dir <- sign(orig_pairs$estimate[oi])
          clean_dir <- sign(clean_pairs$estimate[ci])
          data.frame(
            Contrast = ct,
            Original_Estimate = round(orig_pairs$estimate[oi], 4),
            Original_p = round(orig_pairs$p.value[oi], 4),
            Clean_Estimate = round(clean_pairs$estimate[ci], 4),
            Clean_p = round(clean_pairs$p.value[ci], 4),
            Direction_Changed = orig_dir != clean_dir,
            Significance_Changed = orig_sig != clean_sig,
            stringsAsFactors = FALSE)
        })
        if (length(pw_rows) > 0) pairwise_comparison <- do.call(rbind, pw_rows)
      }
    }
  }

  list(influence_df = influence_df, removal_log = removal_log_df,
       conclusion_comparison = conclusion_comparison,
       pairwise_comparison = pairwise_comparison,
       n_removed = length(removed_obs))
}

# ---------------------------------------------------------------------------
# robustness_coefficient_check()
# Compare coefficient sign, significance, and importance rank across models.
# Returns list(detail, sign_summary, sig_summary, rank_summary)
# ---------------------------------------------------------------------------
robustness_coefficient_check <- function(models, alpha = 0.05) {
  if (length(models) < 2) {
    return(list(detail = data.frame(Message = "Need at least 2 models."),
                sign_summary = data.frame(), sig_summary = data.frame(),
                rank_summary = data.frame()))
  }

  rows <- list()
  for (mn in names(models)) {
    m  <- models[[mn]]
    s  <- tryCatch(summary(m)$coefficients, error = function(e) NULL)
    if (is.null(s) || nrow(s) == 0) next
    cf <- as.data.frame(s)
    names(cf) <- c("Estimate", "Std.Error", "t.value", "p.value")
    cf$Term  <- rownames(cf)
    cf$Model <- mn

    # Standardised coefficient: estimate * sd(x) / sd(y)
    mf <- model.frame(m)
    resp_sd <- sd(mf[[1]], na.rm = TRUE)
    mm <- model.matrix(m)
    cf$Std_Coef <- sapply(cf$Term, function(tn) {
      if (tn == "(Intercept)") return(NA_real_)
      idx <- match(tn, colnames(mm))
      if (is.na(idx)) return(NA_real_)
      x_sd <- sd(mm[, idx], na.rm = TRUE)
      if (is.na(x_sd) || x_sd == 0 || resp_sd == 0) return(NA_real_)
      cf$Estimate[cf$Term == tn] * x_sd / resp_sd
    })

    cf$Sign <- ifelse(cf$Estimate > 0, "+", ifelse(cf$Estimate < 0, "-", "0"))
    cf$Significant <- cf$p.value < alpha
    cf$Abs_Std <- abs(cf$Std_Coef)

    # Rank by |standardised coef| within this model (excl intercept)
    non_int <- cf$Term != "(Intercept)" & !is.na(cf$Abs_Std)
    cf$Rank <- NA_integer_
    if (any(non_int))
      cf$Rank[non_int] <- rank(-cf$Abs_Std[non_int], ties.method = "min")

    rows[[mn]] <- cf[, c("Model", "Term", "Estimate", "Std.Error", "t.value",
                          "p.value", "Std_Coef", "Sign", "Significant", "Rank")]
  }

  if (length(rows) == 0)
    return(list(detail = data.frame(Message = "No valid coefficients."),
                sign_summary = data.frame(), sig_summary = data.frame(),
                rank_summary = data.frame()))

  detail <- do.call(rbind, rows)
  rownames(detail) <- NULL

  # Exclude intercept from summaries
  detail_no_int <- detail[detail$Term != "(Intercept)", , drop = FALSE]
  all_terms <- unique(detail_no_int$Term)

  # --- Sign stability ---
  sign_rows <- lapply(all_terms, function(tn) {
    sub <- detail_no_int[detail_no_int$Term == tn, , drop = FALSE]
    signs <- sub$Sign
    n_present <- nrow(sub)
    n_pos <- sum(signs == "+")
    n_neg <- sum(signs == "-")
    consistent <- (n_pos == n_present) || (n_neg == n_present)
    dominant_sign <- if (n_pos >= n_neg) "+" else "-"
    data.frame(Term = tn, Dominant_Sign = dominant_sign,
               Models_Positive = n_pos, Models_Negative = n_neg,
               Models_Present = n_present,
               Verdict = if (consistent) "Robust" else "Fragile",
               stringsAsFactors = FALSE)
  })
  sign_summary <- do.call(rbind, sign_rows)

  # --- Significance stability ---
  sig_rows <- lapply(all_terms, function(tn) {
    sub <- detail_no_int[detail_no_int$Term == tn, , drop = FALSE]
    n_present <- nrow(sub)
    n_sig     <- sum(sub$Significant, na.rm = TRUE)
    n_nonsig  <- n_present - n_sig
    verdict <- if (n_sig == n_present) "Robust significant"
               else if (n_nonsig == n_present) "Robust non-significant"
               else "Fragile"
    data.frame(Term = tn, Models_Significant = n_sig,
               Models_NonSignificant = n_nonsig,
               Models_Present = n_present,
               Verdict = verdict, stringsAsFactors = FALSE)
  })
  sig_summary <- do.call(rbind, sig_rows)

  # --- Rank stability ---
  rank_rows <- lapply(all_terms, function(tn) {
    sub <- detail_no_int[detail_no_int$Term == tn, , drop = FALSE]
    ranks <- sub$Rank[!is.na(sub$Rank)]
    n_present <- length(ranks)
    if (n_present == 0) {
      return(data.frame(Term = tn, Median_Rank = NA, Min_Rank = NA,
                        Max_Rank = NA, Rank_Range = NA,
                        Verdict = "N/A", stringsAsFactors = FALSE))
    }
    med_rank  <- median(ranks)
    min_rank  <- min(ranks)
    max_rank  <- max(ranks)
    rng       <- max_rank - min_rank
    verdict   <- if (rng == 0) "Robust" else if (rng <= 1) "Stable" else "Variable"
    data.frame(Term = tn, Median_Rank = med_rank, Min_Rank = min_rank,
               Max_Rank = max_rank, Rank_Range = rng,
               Verdict = verdict, stringsAsFactors = FALSE)
  })
  rank_summary <- do.call(rbind, rank_rows)

  list(detail = detail, sign_summary = sign_summary,
       sig_summary = sig_summary, rank_summary = rank_summary)
}

# ---------------------------------------------------------------------------
# model_selection_review()
# Audit a declared final model against all fitted models.
# Flags cherry-picking: significance that depends on model choice.
# ---------------------------------------------------------------------------
model_selection_review <- function(models, final_model_name, spec, method = "tukey",
                                   alpha = 0.05) {
  if (!final_model_name %in% names(models))
    return(list(verdict = list(), lsmean_audit = data.frame(),
                pairwise_audit = data.frame(), coef_audit = data.frame(),
                complexity = data.frame()))

  final_model <- models[[final_model_name]]
  other_models <- models[setdiff(names(models), final_model_name)]
  all_model_names <- names(models)
  n_models <- length(all_model_names)
  spec_list <- strsplit(spec, ":")[[1]]
  flags <- list()

  # --- LS Mean audit ---
  lsmean_rows <- list()
  for (mn in all_model_names) {
    em <- tryCatch(
      suppressMessages(emmeans::emmeans(models[[mn]], specs = spec_list)),
      error = function(e) NULL)
    if (is.null(em)) next
    em_df <- as.data.frame(summary(em, level = 1 - alpha))
    level_keys <- if (length(spec_list) > 1)
      apply(em_df[, spec_list, drop = FALSE], 1,
            function(r) paste(trimws(r), collapse = ":"))
    else as.character(em_df[[spec_list]])
    for (i in seq_along(level_keys)) {
      lsmean_rows[[length(lsmean_rows) + 1]] <- data.frame(
        Model = mn, Level = level_keys[i], emmean = em_df$emmean[i],
        Is_Final = (mn == final_model_name), stringsAsFactors = FALSE)
    }
  }
  lsmean_audit <- if (length(lsmean_rows) > 0) do.call(rbind, lsmean_rows) else data.frame()

  # --- Pairwise significance audit ---
  pw_rows <- list()
  for (mn in all_model_names) {
    mc_res <- tryCatch({
      em <- suppressMessages(emmeans::emmeans(models[[mn]], specs = spec_list))
      p  <- emmeans::contrast(em, method = "pairwise", adjust = method)
      as.data.frame(summary(p, level = 1 - alpha))
    }, error = function(e) NULL)
    if (is.null(mc_res) || nrow(mc_res) == 0) next
    for (i in seq_len(nrow(mc_res))) {
      pw_rows[[length(pw_rows) + 1]] <- data.frame(
        Model = mn, Contrast = mc_res$contrast[i],
        estimate = mc_res$estimate[i], p.value = mc_res$p.value[i],
        Significant = mc_res$p.value[i] < alpha,
        Is_Final = (mn == final_model_name), stringsAsFactors = FALSE)
    }
  }
  pairwise_audit <- if (length(pw_rows) > 0) do.call(rbind, pw_rows) else data.frame()

  # Flag: final model sig but others not
  if (nrow(pairwise_audit) > 0) {
    final_pw <- pairwise_audit[pairwise_audit$Is_Final, , drop = FALSE]
    other_pw <- pairwise_audit[!pairwise_audit$Is_Final, , drop = FALSE]
    for (ct in unique(final_pw$Contrast)) {
      f_sig <- final_pw$Significant[final_pw$Contrast == ct]
      if (length(f_sig) > 0 && any(f_sig)) {
        o_sigs <- other_pw$Significant[other_pw$Contrast == ct]
        n_other_sig <- sum(o_sigs, na.rm = TRUE)
        n_other <- length(o_sigs)
        if (n_other > 0 && n_other_sig / n_other < 0.5) {
          flags[[length(flags) + 1]] <- list(
            check = "Pairwise significance",
            detail = paste0(ct, ": final model significant, but only ",
                            n_other_sig, "/", n_other, " other models agree"),
            severity = if (n_other_sig == 0) "red" else "amber")
        }
      }
    }
  }

  # --- Coefficient audit ---
  coef_rows <- list()
  for (mn in all_model_names) {
    s <- tryCatch(summary(models[[mn]])$coefficients, error = function(e) NULL)
    if (is.null(s) || nrow(s) == 0) next
    cf <- as.data.frame(s)
    names(cf) <- c("Estimate", "Std.Error", "t.value", "p.value")
    cf$Term  <- rownames(cf)
    cf$Model <- mn
    cf$Sign  <- ifelse(cf$Estimate > 0, "+", ifelse(cf$Estimate < 0, "-", "0"))
    cf$Significant <- cf$p.value < alpha
    cf$Is_Final <- (mn == final_model_name)
    coef_rows[[mn]] <- cf[cf$Term != "(Intercept)",
                          c("Model", "Term", "Estimate", "p.value", "Sign",
                            "Significant", "Is_Final"), drop = FALSE]
  }
  coef_audit <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  rownames(coef_audit) <- NULL

  # Flag: final model has sig terms that others don't
  if (nrow(coef_audit) > 0) {
    final_cf <- coef_audit[coef_audit$Is_Final, , drop = FALSE]
    other_cf <- coef_audit[!coef_audit$Is_Final, , drop = FALSE]
    for (tn in unique(final_cf$Term[final_cf$Significant])) {
      o_sigs <- other_cf$Significant[other_cf$Term == tn]
      n_other_sig <- sum(o_sigs, na.rm = TRUE)
      n_other <- length(o_sigs)
      if (n_other > 0 && n_other_sig / n_other < 0.5) {
        flags[[length(flags) + 1]] <- list(
          check = "Coefficient significance",
          detail = paste0(tn, ": significant in final model, but only ",
                          n_other_sig, "/", n_other, " others"),
          severity = if (n_other_sig == 0) "red" else "amber")
      }
    }
    # Flag: sign flips in final model
    for (tn in unique(final_cf$Term)) {
      f_sign <- final_cf$Sign[final_cf$Term == tn]
      o_signs <- other_cf$Sign[other_cf$Term == tn]
      if (length(f_sign) > 0 && length(o_signs) > 0) {
        dominant <- names(sort(table(o_signs), decreasing = TRUE))[1]
        if (f_sign != dominant) {
          flags[[length(flags) + 1]] <- list(
            check = "Coefficient direction",
            detail = paste0(tn, ": sign is '", f_sign, "' in final model, but '",
                            dominant, "' in majority of others"),
            severity = "red")
        }
      }
    }
  }

  # --- Model complexity comparison ---
  complexity_rows <- lapply(all_model_names, function(mn) {
    m <- models[[mn]]
    s <- summary(m)
    n_terms <- length(coef(m)) - 1  # exclude intercept
    r2    <- s$r.squared
    adjr2 <- s$adj.r.squared
    # PRESS if available
    press_rmse <- tryCatch({
      e <- residuals(m)
      h <- model_hat_values(m)
      sqrt(mean((e / (1 - h))^2, na.rm = TRUE))
    }, error = function(e) NA)
    rmse <- s$sigma
    data.frame(Model = mn, N_Terms = n_terms, R2 = round(r2, 4),
               Adj_R2 = round(adjr2, 4), RMSE = round(rmse, 4),
               PRESS_RMSE = round(press_rmse, 4),
               Is_Final = (mn == final_model_name),
               stringsAsFactors = FALSE)
  })
  complexity <- do.call(rbind, complexity_rows)

  # Flag: final model is most complex
  if (nrow(complexity) > 1) {
    final_terms <- complexity$N_Terms[complexity$Is_Final]
    other_terms <- complexity$N_Terms[!complexity$Is_Final]
    if (length(final_terms) > 0 && all(final_terms > other_terms)) {
      flags[[length(flags) + 1]] <- list(
        check = "Model complexity",
        detail = paste0("Final model has the most terms (", final_terms,
                        ") \u2014 check for overfitting"),
        severity = "amber")
    }
    # Flag: PRESS/RMSE ratio
    final_press <- complexity$PRESS_RMSE[complexity$Is_Final]
    final_rmse  <- complexity$RMSE[complexity$Is_Final]
    if (length(final_press) > 0 && !is.na(final_press) && final_rmse > 0) {
      ratio <- final_press / final_rmse
      if (ratio > 1.3) {
        flags[[length(flags) + 1]] <- list(
          check = "Overfitting",
          detail = paste0("PRESS_RMSE / RMSE = ", round(ratio, 2),
                          " (>1.3) \u2014 model may be overfitting"),
          severity = "red")
      } else if (ratio > 1.1) {
        flags[[length(flags) + 1]] <- list(
          check = "Overfitting",
          detail = paste0("PRESS_RMSE / RMSE = ", round(ratio, 2),
                          " (>1.1) \u2014 mild overfitting risk"),
          severity = "amber")
      }
    }
  }

  # Overall verdict
  n_red   <- sum(sapply(flags, function(f) f$severity == "red"))
  n_amber <- sum(sapply(flags, function(f) f$severity == "amber"))
  overall <- if (n_red > 0) "Review needed \u2014 conclusions may depend on model choice"
             else if (n_amber > 0) "Proceed with caution \u2014 minor concerns"
             else "Conclusions appear robust across models"
  overall_colour <- if (n_red > 0) "red" else if (n_amber > 0) "amber" else "green"

  list(verdict = list(overall = overall, colour = overall_colour, flags = flags),
       lsmean_audit = lsmean_audit, pairwise_audit = pairwise_audit,
       coef_audit = coef_audit, complexity = complexity)
}
