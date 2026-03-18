# R/stat_formulas.R — Formula generation functions

# ---------------------------------------------------------------------------
# build_formulas()
# Returns a named character vector: label => formula string
# For ANOVA mode: progressive factor interactions + covariates + blocks
# ---------------------------------------------------------------------------
build_formulas <- function(response, factors, covariates = character(0),
                            blocks = character(0), max_way = 2,
                            include_covariates = TRUE, include_blocks = TRUE,
                            include_block_fac = FALSE,
                            max_covariates = NULL) {
  if (length(factors) == 0) return(character(0))
  result <- character(0)

  # Build factor terms progressively (main effects -> 2FI -> 3FI -> ...)
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

  # + covariates on all models (limited by max_covariates per formula)
  if (include_covariates && length(covariates) > 0) {
    mc <- if (!is.null(max_covariates)) min(max_covariates, length(covariates)) else length(covariates)
    if (mc >= length(covariates)) {
      cov_combos <- list(covariates)
    } else {
      cov_combos <- combn(covariates, mc, simplify = FALSE)
    }
    cov_result <- character(0)
    for (cc in cov_combos) {
      cov_term <- paste(cc, collapse = " + ")
      for (f in names(result)) {
        new_f <- paste0(f, " + ", cov_term)
        cov_result[new_f] <- new_f
      }
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

    # + block x factor interactions (optional)
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
# ---------------------------------------------------------------------------
build_regression_formulas <- function(response, factors, covariates = character(0),
                                       blocks = character(0), max_way = 2,
                                       poly_degree = 2,
                                       include_covariates = TRUE,
                                       include_cov_fac = FALSE,
                                       include_blocks = TRUE,
                                       include_block_fac = FALSE,
                                       max_covariates = NULL) {
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

  # 4. + covariates (limited by max_covariates per formula)
  if (include_covariates && length(covariates) > 0) {
    mc <- if (!is.null(max_covariates)) min(max_covariates, length(covariates)) else length(covariates)
    if (mc >= length(covariates)) {
      rhs <- c(rhs, covariates)
      f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
      result[f] <- f
    } else {
      cov_combos <- combn(covariates, mc, simplify = FALSE)
      for (cc in cov_combos) {
        rhs_c <- c(rhs, cc)
        f <- paste0(response, " ~ ", paste(rhs_c, collapse = " + "))
        result[f] <- f
      }
      # Update rhs with first combination for subsequent steps
      rhs <- c(rhs, cov_combos[[1]])
    }
  }

  # 5. + covariate x factor interactions
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

  # 7. + block x factor interactions
  if (include_block_fac && length(blocks) > 0 && length(factors) > 0) {
    blk_fac <- as.vector(outer(blocks, factors, function(b, f) paste0(b, ":", f)))
    rhs <- c(rhs, blk_fac)
    f <- paste0(response, " ~ ", paste(rhs, collapse = " + "))
    result[f] <- f
  }

  result
}

# ---------------------------------------------------------------------------
# generate_interaction_terms()
# All subsets of factor terms up to max_way interactions
# Returns list of character vectors, each progressively larger
# ---------------------------------------------------------------------------
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
# detect_formula_aliases()
# Check within a single formula's terms for aliased pairs given the design data.
# Returns data.frame(Term_1, Term_2, Correlation) or empty data.frame.
# ---------------------------------------------------------------------------
detect_formula_aliases <- function(data, formula_str, threshold = 0.99) {
  f <- tryCatch(as.formula(formula_str), error = function(e) NULL)
  if (is.null(f)) return(data.frame())

  tt <- tryCatch(terms(f), error = function(e) NULL)
  if (is.null(tt)) return(data.frame())
  term_labels <- attr(tt, "term.labels")
  if (length(term_labels) < 2) return(data.frame())

  result <- compute_aliases(data, full_terms = term_labels,
                            check_terms = NULL, threshold = threshold)
  if ("Message" %in% names(result)) return(data.frame())
  if ("In_model" %in% names(result)) {
    result <- result[result$In_model == "Both in model", , drop = FALSE]
  }
  if (nrow(result) == 0) return(data.frame())
  result[, c("Term_1", "Term_2", "Correlation"), drop = FALSE]
}
