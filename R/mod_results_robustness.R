# R/mod_results_robustness.R — Robustness sub-module (extracted from mod_results.R)
# Called from mod_results_server; shares the same Shiny namespace (no NS change).

results_robustness_server <- function(input, output, session, rv,
                                       active_models, mc_state,
                                       colour_theme, role_selectors) {
  ns <- session$ns

  # Unpack role selectors
  factors_       <- role_selectors$factors
  covariates     <- role_selectors$covariates
  all_covariates <- role_selectors$all_covariates

  # Local reactive state for each robustness sub-tab
  rv_robust_review    <- reactiveVal(NULL)
  rv_robust_lsmean    <- reactiveVal(NULL)
  rv_robust_order     <- reactiveVal(NULL)
  rv_robust_sig       <- reactiveVal(NULL)
  rv_robust_mediation <- reactiveVal(NULL)
  rv_robust_outlier   <- reactiveVal(NULL)
  rv_robust_coef      <- reactiveVal(NULL)

  # --- Model Review ---
  observeEvent(input$robust_review_run, {
    am <- active_models()
    req(length(am) >= 2, input$robust_final_model, input$robust_term)
    final_mn <- input$robust_final_model
    req(final_mn %in% names(am))
    spec   <- input$robust_term
    method <- input$robust_mc_method %||% "tukey"
    alpha  <- mc_state$mc_alpha() %||% 0.05
    withProgress(message = "Running Model Review ...", {
      res <- tryCatch(
        model_selection_review(am, final_mn, spec, method, alpha),
        error = function(e) { showNotification(e$message, type = "error"); NULL })
      rv_robust_review(res)
    })
  })

  output$robust_review_verdict_ui <- renderUI({
    res <- rv_robust_review()
    if (is.null(res)) return(p(class = "text-muted", "Press Run to audit."))
    v <- res$verdict
    bg <- switch(v$colour, green = ROBUST_GREEN, amber = ROBUST_AMBER,
                 red = ROBUST_RED, "white")
    flag_items <- lapply(v$flags, function(f) {
      icon_chr <- if (f$severity == "red") "\u26d4" else "\u26a0\ufe0f"
      tags$li(paste(icon_chr, f$check, "\u2014", f$detail))
    })
    tagList(
      div(style = paste0("padding:12px; border-radius:6px; background:", bg, ";"),
        h5(style = "margin:0;", v$overall),
        if (length(v$flags) > 0) tags$ul(flag_items)
        else p(class = "text-muted mb-0", "No concerns identified.")
      )
    )
  })

  output$robust_review_lsmean <- DT::renderDataTable({
    res <- rv_robust_review()
    if (is.null(res) || nrow(res$lsmean_audit) == 0)
      return(DT::datatable(data.frame(Message = "Press Run.")))
    d <- res$lsmean_audit
    d$emmean <- round(d$emmean, 3)
    dt <- dt_table(d, rownames = FALSE, options = list(pageLength = PAGE_LEN_DEFAULT))
    dt <- DT::formatStyle(dt, "Is_Final",
            backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#e3f2fd", "white")),
            fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal")))
    dt
  })

  output$robust_review_pairwise <- DT::renderDataTable({
    res <- rv_robust_review()
    if (is.null(res) || nrow(res$pairwise_audit) == 0)
      return(DT::datatable(data.frame(Message = "Press Run.")))
    d <- res$pairwise_audit
    d <- format_pvalue_dt(d, "p.value")
    shadow_idx <- which(names(d) == "._p_p.value") - 1
    dt <- dt_table(d, rownames = FALSE, options = list(pageLength = PAGE_LEN_DEFAULT,
                   columnDefs = list(list(visible = FALSE, targets = shadow_idx))))
    dt <- DT::formatRound(dt, "estimate", digits = 3)
    dt <- DT::formatStyle(dt, "Is_Final",
            backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#e3f2fd", "white")),
            fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal")))
    if ("Significant" %in% names(d)) {
      dt <- DT::formatStyle(dt, "Significant",
              backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(PVALUE_GREEN, "white")))
    }
    dt
  })

  output$robust_review_coefs <- DT::renderDataTable({
    res <- rv_robust_review()
    if (is.null(res) || nrow(res$coef_audit) == 0)
      return(DT::datatable(data.frame(Message = "Press Run.")))
    d <- res$coef_audit
    d <- format_pvalue_dt(d, "p.value")
    shadow_idx <- which(names(d) == "._p_p.value") - 1
    dt <- dt_table(d, rownames = FALSE, options = list(pageLength = PAGE_LEN_DEFAULT,
                   columnDefs = list(list(visible = FALSE, targets = shadow_idx))))
    dt <- DT::formatRound(dt, "Estimate", digits = 3)
    dt <- DT::formatStyle(dt, "Is_Final",
            backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#e3f2fd", "white")),
            fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal")))
    if ("Significant" %in% names(d)) {
      dt <- DT::formatStyle(dt, "Significant",
              backgroundColor = DT::styleEqual(c(TRUE, FALSE), c(PVALUE_GREEN, "white")))
    }
    dt
  })

  output$robust_review_complexity <- DT::renderDataTable({
    res <- rv_robust_review()
    if (is.null(res) || nrow(res$complexity) == 0)
      return(DT::datatable(data.frame(Message = "Press Run.")))
    d <- res$complexity
    dt <- dt_table(d, rownames = FALSE, options = list(pageLength = PAGE_LEN_PREVIEW))
    dt <- DT::formatStyle(dt, "Is_Final",
            backgroundColor = DT::styleEqual(c(TRUE, FALSE), c("#e3f2fd", "white")),
            fontWeight = DT::styleEqual(c(TRUE, FALSE), c("bold", "normal")))
    dt
  })

  # --- Dynamic per-level limit inputs ---
  rv_robust_levels <- reactiveVal(character(0))

  output$robust_level_inputs_ui <- renderUI({
    am <- active_models()
    req(length(am) > 0, input$robust_term)
    spec <- input$robust_term
    m1 <- am[[1]]
    em <- tryCatch(
      suppressMessages(emmeans::emmeans(m1, specs = strsplit(spec, ":")[[1]])),
      error = function(e) NULL)
    if (is.null(em)) {
      rv_robust_levels(character(0))
      return(p("Could not compute LS means for this term."))
    }
    em_df <- as.data.frame(summary(em))
    spec_parts <- strsplit(spec, ":")[[1]]
    if (length(spec_parts) > 1) {
      levels_vec <- apply(em_df[, spec_parts, drop = FALSE], 1,
                      function(r) paste(trimws(r), collapse = ":"))
    } else {
      levels_vec <- as.character(em_df[[spec_parts]])
    }
    rv_robust_levels(levels_vec)
    tagList(
      tags$hr(),
      tags$table(class = "table table-sm", style = "max-width:700px;",
        tags$thead(tags$tr(
          tags$th("Level"), tags$th("Limit"), tags$th("Direction"),
          tags$th("LS Mean (Model 1)")
        )),
        tags$tbody(
          lapply(seq_along(levels_vec), function(i) {
            lev <- levels_vec[i]
            lm_val <- round(em_df$emmean[i], 3)
            guess_dir <- if (lm_val >= input$robust_common_limit %||% 0) ">" else "<"
            tags$tr(
              tags$td(lev),
              tags$td(numericInput(
                ns(paste0("robust_limit_", i)), label = NULL,
                value = input$robust_common_limit %||% 0, width = "100px")),
              tags$td(selectInput(
                ns(paste0("robust_dir_", i)), label = NULL,
                choices = c(">" = ">", "<" = "<"),
                selected = guess_dir, width = "80px")),
              tags$td(lm_val)
            )
          })
        )
      )
    )
  })

  observeEvent(input$robust_apply_limit, {
    n <- length(rv_robust_levels())
    if (n == 0) return()
    val <- input$robust_common_limit %||% 0
    for (i in seq_len(n)) {
      updateNumericInput(session, paste0("robust_limit_", i), value = val)
    }
  })

  # --- LS Mean vs Limit ---
  observeEvent(input$robust_lsmean_run, {
    am   <- active_models()
    req(length(am) > 0, input$robust_term)
    spec <- input$robust_term
    keys <- rv_robust_levels()
    n    <- length(keys)
    req(n > 0)
    limits <- setNames(
      sapply(seq_len(n), function(i) input[[paste0("robust_limit_", i)]] %||% 0),
      keys)
    directions <- setNames(
      sapply(seq_len(n), function(i) input[[paste0("robust_dir_", i)]] %||% ">"),
      keys)
    alpha <- mc_state$mc_alpha() %||% 0.05
    withProgress(message = "Running LS Mean vs Limit ...", {
      res <- tryCatch(
        robustness_lsmean_vs_limit(am, spec, limits, directions, alpha),
        error = function(e) { showNotification(e$message, type = "error"); NULL })
      rv_robust_lsmean(res)
    })
  })

  output$robust_lsmean_summary <- DT::renderDataTable({
    res <- rv_robust_lsmean()
    if (is.null(res)) return(DT::datatable(data.frame(Message = "Press Run.")))
    dt <- dt_table(res$summary, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    if ("Dir_Verdict" %in% names(res$summary)) {
      dt <- DT::formatStyle(dt, "Dir_Verdict",
              backgroundColor = DT::styleEqual(
                c("Robust", "Fragile"),
                c(ROBUST_GREEN, ROBUST_RED)))
    }
    if ("Sig_Verdict" %in% names(res$summary)) {
      dt <- DT::formatStyle(dt, "Sig_Verdict",
              backgroundColor = DT::styleEqual(
                c("Robust significant", "Robust non-significant", "Fragile"),
                c(ROBUST_GREEN, ROBUST_GREEN, ROBUST_RED)))
    }
    dt
  })

  output$robust_lsmean_detail <- DT::renderDataTable({
    res <- rv_robust_lsmean()
    if (is.null(res)) return(DT::datatable(data.frame(Message = "Press Run.")))
    num_cols <- intersect(c("emmean", "Limit", "p.value"), names(res$detail))
    dt <- dt_table(res$detail, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DETAIL))
    for (nc in setdiff(num_cols, "p.value"))
      dt <- DT::formatRound(dt, nc, digits = 3)
    if ("p.value" %in% names(res$detail)) {
      d_lsm <- res$detail
      d_lsm <- format_pvalue_dt(d_lsm, "p.value")
      # Rebuild DT with formatted p-values
      shadow_idx <- which(names(d_lsm) == "._p_p.value") - 1
      dt <- dt_table(d_lsm, rownames = FALSE,
                     options = list(pageLength = PAGE_LEN_DETAIL,
                                   columnDefs = list(list(visible = FALSE, targets = shadow_idx))))
      for (nc in setdiff(num_cols, "p.value"))
        dt <- DT::formatRound(dt, nc, digits = 3)
    }
    dt
  })

  # --- Pairwise Ordering ---
  observeEvent(input$robust_order_run, {
    am <- active_models()
    req(length(am) > 0, input$robust_term)
    withProgress(message = "Running Pairwise Ordering ...", {
      res <- tryCatch(
        robustness_pairwise_order(am, input$robust_term),
        error = function(e) { showNotification(e$message, type = "error"); NULL })
      rv_robust_order(res)
    })
  })

  output$robust_order_summary <- DT::renderDataTable({
    res <- rv_robust_order()
    if (is.null(res)) return(DT::datatable(data.frame(Message = "Press Run.")))
    dt <- dt_table(res$summary, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    if ("Verdict" %in% names(res$summary)) {
      dt <- DT::formatStyle(dt, "Verdict",
              backgroundColor = DT::styleEqual(
                c("Robust", "Fragile"),
                c(ROBUST_GREEN, ROBUST_RED)))
    }
    dt
  })

  output$robust_order_detail <- DT::renderDataTable({
    res <- rv_robust_order()
    if (is.null(res)) return(DT::datatable(data.frame(Message = "Press Run.")))
    num_cols <- intersect(c("A_mean", "B_mean"), names(res$detail))
    dt <- dt_table(res$detail, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DETAIL))
    for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 3)
    dt
  })

  # --- Pairwise Significance ---
  observeEvent(input$robust_sig_run, {
    am <- active_models()
    req(length(am) > 0, input$robust_term)
    method <- input$robust_mc_method %||% "tukey"
    alpha  <- mc_state$mc_alpha() %||% 0.05
    withProgress(message = "Running Pairwise Significance ...", {
      res <- tryCatch(
        robustness_pairwise_significance(am, input$robust_term, method, alpha),
        error = function(e) { showNotification(e$message, type = "error"); NULL })
      rv_robust_sig(res)
    })
  })

  output$robust_sig_summary <- DT::renderDataTable({
    res <- rv_robust_sig()
    if (is.null(res)) return(DT::datatable(data.frame(Message = "Press Run.")))
    dt <- dt_table(res$summary, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    if ("Verdict" %in% names(res$summary)) {
      dt <- DT::formatStyle(dt, "Verdict",
              backgroundColor = DT::styleEqual(
                c("Robust significant", "Robust non-significant", "Fragile"),
                c(ROBUST_GREEN, ROBUST_GREEN, ROBUST_RED)))
    }
    dt
  })

  output$robust_sig_detail <- DT::renderDataTable({
    res <- rv_robust_sig()
    if (is.null(res)) return(DT::datatable(data.frame(Message = "Press Run.")))
    num_cols <- intersect(c("estimate", "p.value"), names(res$detail))
    dt <- dt_table(res$detail, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DETAIL))
    for (nc in setdiff(num_cols, "p.value"))
      dt <- DT::formatRound(dt, nc, digits = 3)
    if ("p.value" %in% names(res$detail)) {
      alpha <- mc_state$mc_alpha() %||% 0.05
      d_sig <- res$detail
      d_sig <- format_pvalue_dt(d_sig, "p.value", alpha = alpha)
      shadow_idx <- which(names(d_sig) == "._p_p.value") - 1
      dt <- dt_table(d_sig, rownames = FALSE,
                     options = list(pageLength = PAGE_LEN_DETAIL,
                                   columnDefs = list(list(visible = FALSE, targets = shadow_idx))))
      for (nc in setdiff(num_cols, "p.value"))
        dt <- DT::formatRound(dt, nc, digits = 3)
      dt <- DT::formatStyle(dt, "p.value",
              backgroundColor = DT::styleInterval(alpha, c(PVALUE_GREEN, "white")),
              valueColumns = "._p_p.value")
    }
    dt
  })

  # --- Covariate Mediation ---
  observeEvent(input$robust_mediation_run, {
    req(length(rv$models) > 0)
    d        <- rv$data
    cov_cols <- covariates()
    fac_cols <- factors_()
    req(length(cov_cols) > 0)
    alpha    <- mc_state$mc_alpha() %||% 0.05
    r2_thresh <- input$robust_r2_thresh %||% MEDIATION_R2_THRESH
    withProgress(message = "Running Covariate Mediation Check ...", {
      res <- tryCatch(
        covariate_mediation_check(d, cov_cols, fac_cols, alpha, r2_thresh),
        error = function(e) { showNotification(e$message, type = "error"); NULL })
      rv_robust_mediation(res)
    })
  })

  output$robust_mediation_table <- DT::renderDataTable({
    res <- rv_robust_mediation()
    if (is.null(res))
      return(DT::datatable(data.frame(
        Message = "Press Run. Requires at least one covariate.")))
    num_cols <- intersect(c("F_stat", "R2"), names(res))
    if ("p.value" %in% names(res))
      res <- format_pvalue_dt(res, "p.value")
    shadow_idx <- if ("._p_p.value" %in% names(res)) which(names(res) == "._p_p.value") - 1 else integer(0)
    hide_defs <- if (length(shadow_idx) > 0) list(list(visible = FALSE, targets = shadow_idx)) else list()
    dt <- dt_table(res, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT, columnDefs = hide_defs))
    for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 3)
    if ("Verdict" %in% names(res)) {
      dt <- DT::formatStyle(dt, "Verdict",
              backgroundColor = DT::styleEqual(
                c("OK", "Caution", "Warning \u2014 likely mediator"),
                c(ROBUST_GREEN, ROBUST_AMBER, ROBUST_RED)))
    }
    dt
  })

  # --- Outlier Influence ---
  observeEvent(input$robust_outlier_run, {
    req(input$robust_outlier_model, length(rv$models) > 0, input$robust_term)
    mname <- input$robust_outlier_model
    req(mname %in% names(rv$models))
    model    <- rv$models[[mname]]
    spec     <- input$robust_term
    alpha    <- mc_state$mc_alpha() %||% 0.05
    max_iter <- as.integer(input$robust_outlier_maxiter %||% OUTLIER_MAX_ITER)
    withProgress(message = "Running Outlier Influence Check ...", {
      res <- tryCatch(
        outlier_influence_check(model, spec, alpha, max_iter),
        error = function(e) { showNotification(e$message, type = "error"); NULL })
      rv_robust_outlier(res)
    })
  })

  output$robust_influence_table <- DT::renderDataTable({
    res <- rv_robust_outlier()
    if (is.null(res)) return(DT::datatable(data.frame(Message = "Press Run.")))
    dt <- dt_table(res$influence_df, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DETAIL))
    num_cols <- intersect(c("Cook_D", "max_DFBETA"), names(res$influence_df))
    for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 4)
    if ("Flagged" %in% names(res$influence_df)) {
      dt <- DT::formatStyle(dt, "Flagged",
              backgroundColor = DT::styleEqual(
                c(TRUE, FALSE), c(ROBUST_AMBER, "white")))
    }
    dt
  })

  output$robust_removal_log <- DT::renderDataTable({
    res <- rv_robust_outlier()
    if (is.null(res) || is.null(res$removal_log) || nrow(res$removal_log) == 0)
      return(DT::datatable(data.frame(
        Message = "No observations removed (none exceeded thresholds).")))
    dt <- dt_table(res$removal_log, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_PREVIEW))
    num_cols <- intersect(c("Cook_D"), names(res$removal_log))
    for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 4)
    dt
  })

  output$robust_conclusion_cmp <- DT::renderDataTable({
    res <- rv_robust_outlier()
    if (is.null(res) || is.null(res$conclusion_comparison) ||
        nrow(res$conclusion_comparison) == 0)
      return(DT::datatable(data.frame(
        Message = "No comparison available (no observations removed).")))
    dt <- dt_table(res$conclusion_comparison, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    num_cols <- intersect(c("Original_Estimate", "Clean_Estimate", "Change", "Pct_Change"),
                          names(res$conclusion_comparison))
    for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 3)
    dt
  })

  output$robust_pairwise_cmp <- DT::renderDataTable({
    res <- rv_robust_outlier()
    if (is.null(res) || is.null(res$pairwise_comparison) ||
        nrow(res$pairwise_comparison) == 0)
      return(DT::datatable(data.frame(
        Message = "No pairwise comparison available.")))
    dt <- dt_table(res$pairwise_comparison, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    d_pw <- res$pairwise_comparison
    num_cols <- intersect(c("Original_Estimate", "Clean_Estimate"), names(d_pw))
    p_cols <- intersect(c("Original_p", "Clean_p"), names(d_pw))
    if (length(p_cols) > 0) d_pw <- format_pvalue_dt(d_pw, p_cols)
    shadow_p_cols <- paste0("._p_", p_cols)
    shadow_p_idx <- which(names(d_pw) %in% shadow_p_cols) - 1
    hide_defs <- if (length(shadow_p_idx) > 0) list(list(visible = FALSE, targets = shadow_p_idx)) else list()
    dt <- dt_table(d_pw, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT, columnDefs = hide_defs))
    for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 3)
    if ("Direction_Changed" %in% names(res$pairwise_comparison)) {
      dt <- DT::formatStyle(dt, "Direction_Changed",
              backgroundColor = DT::styleEqual(
                c(TRUE, FALSE), c(ROBUST_RED, ROBUST_GREEN)))
    }
    if ("Significance_Changed" %in% names(res$pairwise_comparison)) {
      dt <- DT::formatStyle(dt, "Significance_Changed",
              backgroundColor = DT::styleEqual(
                c(TRUE, FALSE), c(ROBUST_RED, ROBUST_GREEN)))
    }
    dt
  })

  # --- Coefficient Robustness ---
  observeEvent(input$robust_coef_run, {
    am <- active_models()
    req(length(am) >= 2)
    alpha <- mc_state$mc_alpha() %||% 0.05
    withProgress(message = "Running Coefficient Robustness ...", {
      res <- tryCatch(
        robustness_coefficient_check(am, alpha),
        error = function(e) { showNotification(e$message, type = "error"); NULL })
      rv_robust_coef(res)
    })
  })

  output$robust_coef_sign <- DT::renderDataTable({
    res <- rv_robust_coef()
    if (is.null(res) || nrow(res$sign_summary) == 0)
      return(DT::datatable(data.frame(Message = "Press Run (needs 2+ models).")))
    dt <- dt_table(res$sign_summary, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    if ("Verdict" %in% names(res$sign_summary)) {
      dt <- DT::formatStyle(dt, "Verdict",
              backgroundColor = DT::styleEqual(
                c("Robust", "Fragile"),
                c(ROBUST_GREEN, ROBUST_RED)))
    }
    dt
  })

  output$robust_coef_sig <- DT::renderDataTable({
    res <- rv_robust_coef()
    if (is.null(res) || nrow(res$sig_summary) == 0)
      return(DT::datatable(data.frame(Message = "Press Run (needs 2+ models).")))
    dt <- dt_table(res$sig_summary, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    if ("Verdict" %in% names(res$sig_summary)) {
      dt <- DT::formatStyle(dt, "Verdict",
              backgroundColor = DT::styleEqual(
                c("Robust significant", "Robust non-significant", "Fragile"),
                c(ROBUST_GREEN, ROBUST_GREEN, ROBUST_RED)))
    }
    dt
  })

  output$robust_coef_rank <- DT::renderDataTable({
    res <- rv_robust_coef()
    if (is.null(res) || nrow(res$rank_summary) == 0)
      return(DT::datatable(data.frame(Message = "Press Run (needs 2+ models).")))
    dt <- dt_table(res$rank_summary, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DEFAULT))
    if ("Verdict" %in% names(res$rank_summary)) {
      dt <- DT::formatStyle(dt, "Verdict",
              backgroundColor = DT::styleEqual(
                c("Robust", "Stable", "Variable", "N/A"),
                c(ROBUST_GREEN, ROBUST_GREEN, ROBUST_RED, "white")))
    }
    dt
  })

  output$robust_coef_detail <- DT::renderDataTable({
    res <- rv_robust_coef()
    if (is.null(res) || is.null(res$detail) || nrow(res$detail) == 0)
      return(DT::datatable(data.frame(Message = "Press Run (needs 2+ models).")))
    d <- res$detail
    num_cols <- intersect(c("Estimate", "Std.Error", "t.value", "Std_Coef"),
                          names(d))
    dt <- dt_table(d, rownames = FALSE,
                   options = list(pageLength = PAGE_LEN_DETAIL))
    if ("p.value" %in% names(d)) {
      alpha <- mc_state$mc_alpha() %||% 0.05
      d <- format_pvalue_dt(d, "p.value", alpha = alpha)
      shadow_idx <- which(names(d) == "._p_p.value") - 1
      dt <- dt_table(d, rownames = FALSE,
                     options = list(pageLength = PAGE_LEN_DETAIL,
                                   columnDefs = list(list(visible = FALSE, targets = shadow_idx))))
      for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 3)
      dt <- DT::formatStyle(dt, "p.value",
              backgroundColor = DT::styleInterval(alpha, c(PVALUE_GREEN, "white")),
              valueColumns = "._p_p.value")
    } else {
      for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 3)
    }
    dt
  })
}
