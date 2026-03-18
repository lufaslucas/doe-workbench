# R/mod_results_residuals.R — Residuals sub-module (extracted from mod_results.R)
# Called from mod_results_server; shares the same Shiny namespace (no NS change).

results_residuals_server <- function(input, output, session, rv,
                                      active_models, update_model_checkbox,
                                      colour_theme, role_selectors) {
  ns <- session$ns

  # Unpack colour theme
  default_col        <- colour_theme$default_col
  cat_scale_colour   <- colour_theme$cat_scale_colour
  cat_scale_fill     <- colour_theme$cat_scale_fill
  cont_scale_colour  <- colour_theme$cont_scale_colour

  # Unpack role selectors
  factors_       <- role_selectors$factors
  run_orders     <- role_selectors$run_orders
  all_covariates <- role_selectors$all_covariates

  # ── Core Residual Data Frame ─────────────────────────────────────────

  resid_df <- reactive({
    req(input$resid_model, length(rv$models) > 0)
    mname <- input$resid_model
    req(mname %in% names(rv$models))
    m  <- rv$models[[mname]]
    mf <- model.frame(m)
    n <- length(fitted(m))
    row_ids <- if (ROW_ID_COL %in% names(rv$data)) rv$data[[ROW_ID_COL]][seq_len(n)] else seq_len(n)
    df <- data.frame(
      index         = seq_along(fitted(m)),
      row_id        = row_ids,
      fitted_vals   = fitted(m),
      actual        = model.response(mf),
      resid_raw     = residuals(m),
      resid_std     = model_standard_residuals(m),
      resid_ext     = model_studentized_residuals(m),
      cooks         = model_cooks_distance(m)
    )
    ro <- run_orders()
    if (length(ro) > 0 && ro[1] %in% names(rv$data)) {
      df$run_order <- rv$data[[ro[1]]][seq_len(n)]
    }
    df$outlier_flag <- ifelse(abs(df$resid_ext) > 3, "severe",
                       ifelse(abs(df$resid_ext) > 2, "moderate", "ok"))
    cook_thresh <- 4 / n
    df$influential <- df$cooks > cook_thresh
    df$label <- ifelse(df$outlier_flag != "ok" | df$influential,
                       as.character(df$row_id), "")
    resp_name <- names(mf)[1]
    hover_parts <- paste0(
      resp_name, ": ", round(df$actual, 3),
      "\nFitted: ", round(df$fitted_vals, 3),
      "\nResidual: ", round(df$resid_raw, 4),
      "\nStudentised: ", round(df$resid_ext, 3),
      "\nCook's D: ", signif(df$cooks, 3),
      "\nObs #: ", df$row_id
    )
    if (!is.null(df$run_order))
      hover_parts <- paste0(hover_parts, "\nRun Order: ", df$run_order)
    df$hover <- hover_parts
    df
  })

  resid_colour_vec <- reactive({
    df_d <- resid_df()
    cb <- input$resid_colour_by
    if (is.null(cb) || cb == "none") return(NULL)
    m     <- rv$models[[input$resid_model]]
    n     <- nrow(df_d)
    raw   <- rv$data[seq_len(n), , drop = FALSE]
    if (cb == ".treatment") {
      facs <- factors_()
      facs_in <- intersect(facs, names(raw))
      if (length(facs_in) == 0) return(NULL)
      as.factor(apply(raw[, facs_in, drop = FALSE], 1,
                      function(r) paste(r, collapse = ":")))
    } else if (cb %in% names(raw)) {
      as.factor(raw[[cb]])
    } else {
      NULL
    }
  })

  # ── Standard 2x2 Diagnostics ────────────────────────────────────────

  output$resid_standard <- renderPlotly({
    df_d <- resid_df()
    df_d$sqrt_abs_ext <- sqrt(abs(df_d$resid_ext))
    df_d$theo_q       <- qqnorm(df_d$resid_std, plot.it=FALSE)$x
    cv <- resid_colour_vec()
    has_colour <- !is.null(cv)
    if (has_colour) df_d$colour_by <- cv
    dcol <- default_col()

    if (has_colour) {
      is_cont <- is.numeric(cv)
      col_scale <- if (is_cont) cont_scale_colour()() else cat_scale_colour()()
      p1 <- ggplot(df_d, aes(x=fitted_vals, y=resid_raw, colour=colour_by, text=hover,
                              key=row_id)) +
        geom_point(alpha=0.6) + col_scale
      p2 <- ggplot(df_d, aes(x=theo_q, y=resid_std, colour=colour_by, text=hover,
                              key=row_id)) +
        geom_point(alpha=0.6) + col_scale
      p3 <- ggplot(df_d, aes(x=fitted_vals, y=sqrt_abs_ext, colour=colour_by, text=hover,
                              key=row_id)) +
        geom_point(alpha=0.6) + col_scale
    } else {
      p1 <- ggplot(df_d, aes(x=fitted_vals, y=resid_raw, text=hover, key=row_id)) +
        geom_point(alpha=0.6, colour=dcol)
      p2 <- ggplot(df_d, aes(x=theo_q, y=resid_std, text=hover, key=row_id)) +
        geom_point(alpha=0.6, colour=dcol)
      p3 <- ggplot(df_d, aes(x=fitted_vals, y=sqrt_abs_ext, text=hover, key=row_id)) +
        geom_point(alpha=0.6, colour=dcol)
    }

    df_flag <- df_d[df_d$label != "", , drop = FALSE]

    p1 <- p1 +
      geom_hline(yintercept=0, linetype="dashed", colour="red") +
      labs(x="Fitted values", y="Residuals") +
      theme_app() + theme(legend.position="none")
    if (nrow(df_d) >= 10)
      p1 <- tryCatch(p1 + geom_smooth(se=FALSE, colour="blue", linewidth=0.8, method="loess"),
                      error = function(e) p1)
    if (nrow(df_flag) > 0)
      p1 <- p1 + geom_text(data=df_flag, aes(x=fitted_vals, y=resid_raw, label=label),
                            size=3, vjust=-0.8, colour="#dc3545", inherit.aes=FALSE)

    p2 <- p2 +
      geom_abline(slope=1, intercept=0, colour="red", linetype="dashed") +
      labs(x="Theoretical quantiles", y="Standardised residuals") +
      theme_app() + theme(legend.position="none")
    if (nrow(df_flag) > 0)
      p2 <- p2 + geom_text(data=df_flag, aes(x=theo_q, y=resid_std, label=label),
                            size=3, vjust=-0.8, colour="#dc3545", inherit.aes=FALSE)

    p3 <- p3 +
      labs(x="Fitted values", y="\u221a|Ext. studentised resid.|") +
      theme_app() + theme(legend.position="none")
    if (nrow(df_d) >= 10)
      p3 <- tryCatch(p3 + geom_smooth(se=FALSE, colour="blue", linewidth=0.8, method="loess"),
                      error = function(e) p3)
    if (nrow(df_flag) > 0)
      p3 <- p3 + geom_text(data=df_flag, aes(x=fitted_vals, y=sqrt_abs_ext, label=label),
                            size=3, vjust=-0.8, colour="#dc3545", inherit.aes=FALSE)

    cook_thresh <- 4 / nrow(df_d)
    df_cook_flag <- df_d[df_d$influential, , drop = FALSE]
    p4 <- ggplot(df_d, aes(x=index, y=cooks, text=hover)) +
      geom_col(fill=dcol, alpha=0.7) +
      geom_hline(yintercept=cook_thresh, linetype="dashed", colour="red") +
      labs(x="Observation index", y="Cook's D") +
      theme_app()
    if (nrow(df_cook_flag) > 0)
      p4 <- p4 + geom_text(data=df_cook_flag, aes(x=index, y=cooks, label=label),
                            size=3, vjust=-0.8, colour="#dc3545", inherit.aes=FALSE)

    sel <- rv$selected_obs
    pp1 <- ggplotly(p1, tooltip="text", source=SEL_SOURCE) %>%
             apply_selection_style(df_d$row_id, sel)
    pp2 <- ggplotly(p2, tooltip="text", source=SEL_SOURCE) %>%
             apply_selection_style(df_d$row_id, sel)
    pp3 <- ggplotly(p3, tooltip="text", source=SEL_SOURCE) %>%
             apply_selection_style(df_d$row_id, sel)
    pp4 <- ggplotly(p4, tooltip="text")

    afont <- list(family = "Arial, Helvetica, sans-serif", size = 12)
    subplot(pp1, pp2, pp3, pp4,
            nrows=2, titleX=TRUE, titleY=TRUE, margin=0.09) %>%
      layout(
        title  = plotly_title(paste("Residual Diagnostics \u2014", input$resid_model)),
        margin = list(t = 70, l = 60),
        annotations = list(
          list(text = "Residuals vs Fitted",          x = 0.20, y = 1.03, xref = "paper", yref = "paper", showarrow = FALSE, font = afont),
          list(text = "Normal Q\u2013Q (standardised)", x = 0.78, y = 1.03, xref = "paper", yref = "paper", showarrow = FALSE, font = afont),
          list(text = "Scale-Location (studentised)",  x = 0.20, y = 0.46, xref = "paper", yref = "paper", showarrow = FALSE, font = afont),
          list(text = "Cook\u2019s Distance",          x = 0.78, y = 0.46, xref = "paper", yref = "paper", showarrow = FALSE, font = afont)
        )
      )
  })

  # ── Residuals vs Run Order ──────────────────────────────────────────

  output$resid_vs_runorder_ui <- renderUI({
    ro <- run_orders()
    if (length(ro) == 0 || !any(ro %in% names(rv$data %||% data.frame()))) {
      p("No Run Order column assigned. Set a column's role to 'Run Order' in the Assign Roles tab.",
        class="text-muted")
    } else {
      plotlyOutput(ns("resid_vs_runorder_plot"), height="400px")
    }
  })

  output$resid_vs_runorder_plot <- renderPlotly({
    df_d   <- resid_df()
    cv <- resid_colour_vec()
    has_colour <- !is.null(cv)
    if (has_colour) df_d$colour_by <- cv
    ro_col <- run_orders()
    req(length(ro_col) > 0, ro_col[1] %in% names(rv$data))
    ro_col <- ro_col[1]
    df_d$run_order <- rv$data[[ro_col]][seq_len(nrow(df_d))]
    dcol <- default_col()
    if (has_colour) {
      is_cont <- is.numeric(cv)
      col_scale <- if (is_cont) cont_scale_colour()() else cat_scale_colour()()
      p <- ggplot(df_d, aes(x=run_order, y=resid_raw, colour=colour_by, text=hover,
                             key=row_id)) +
        geom_point(alpha=0.6) + col_scale
    } else {
      p <- ggplot(df_d, aes(x=run_order, y=resid_raw, text=hover, key=row_id)) +
        geom_point(alpha=0.6, colour=dcol)
    }
    p <- p +
      geom_hline(yintercept=0, linetype="dashed", colour="red") +
      geom_smooth(method="loess", se=FALSE, colour="grey50", linewidth=0.8) +
      labs(title=paste("Residuals vs Run Order \u2014", input$resid_model),
           x=ro_col, y="Residuals", colour=NULL) +
      theme_app()
    ggplotly(p, tooltip="text", source=SEL_SOURCE) %>%
      apply_selection_style(df_d$row_id, rv$selected_obs)
  })

  # ── Residual Effect Summary Helper ──────────────────────────────────

  resid_effect_summary <- function(resids, x_vals) {
    parts <- character()
    if (is.numeric(x_vals)) {
      ct <- tryCatch(cor.test(x_vals, resids), error = function(e) NULL)
      if (!is.null(ct)) {
        if (ct$p.value < 0.05) parts <- c(parts, paste0("Mean trend (p=", signif(ct$p.value, 2), ")"))
      }
      bp <- tryCatch({
        s <- summary(lm(abs(resids) ~ x_vals))
        p_val <- pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail=FALSE)
        if (p_val < 0.05) paste0("Variance trend (p=", signif(p_val, 2), ")") else NULL
      }, error = function(e) NULL)
      if (!is.null(bp)) parts <- c(parts, bp)
    } else {
      grp <- as.factor(x_vals)
      if (nlevels(grp) >= 2 && nlevels(grp) < length(resids)) {
        aov_p <- tryCatch({
          s <- summary(aov(resids ~ grp))
          s[[1]]$`Pr(>F)`[1]
        }, error = function(e) NA)
        if (!is.na(aov_p) && aov_p < 0.05)
          parts <- c(parts, paste0("Mean differs by group (p=", signif(aov_p, 2), ")"))
        lev_p <- tryCatch({
          abs_dev <- abs(resids - ave(resids, grp, FUN = median))
          s <- summary(aov(abs_dev ~ grp))
          s[[1]]$`Pr(>F)`[1]
        }, error = function(e) NA)
        if (!is.na(lev_p) && lev_p < 0.05)
          parts <- c(parts, paste0("Variance differs (p=", signif(lev_p, 2), ")"))
      }
    }
    if (length(parts) == 0) return(NULL)
    paste(parts, collapse = " | ")
  }

  # ── Residuals vs Model Terms ────────────────────────────────────────

  output$resid_vs_terms_ui <- renderUI({
    req(input$resid_model, rv$models[[input$resid_model]])
    m         <- rv$models[[input$resid_model]]
    pred_vars <- names(model.frame(m))[-1]
    tagList(lapply(seq_along(pred_vars), function(i) {
      plotlyOutput(ns(paste0("resid_term_", i)), height="320px")
    }))
  })

  observe({
    req(input$resid_model, rv$models[[input$resid_model]])
    m         <- rv$models[[input$resid_model]]
    mf        <- model.frame(m)
    pred_vars <- names(mf)[-1]
    resids    <- residuals(m)
    for (i in seq_along(pred_vars)) {
      local({
        idx <- i; vn <- pred_vars[idx]
        output[[paste0("resid_term_", idx)]] <- renderPlotly({
          cv <- resid_colour_vec()
          cb <- input$resid_colour_by
          use_colour <- !is.null(cv) && !identical(cb, vn)
          dcol <- default_col()
          rdf <- resid_df()
          df_t <- data.frame(x=mf[[vn]], resid=resids, hover=rdf$hover,
                             row_id=rdf$row_id)
          if (use_colour) df_t$colour_by <- cv
          if (use_colour) {
            is_cont_c <- is.numeric(cv)
            col_sc <- if (is_cont_c) cont_scale_colour()() else cat_scale_colour()()
            p <- ggplot(df_t, aes(x=x, y=resid, colour=colour_by, text=hover,
                                  key=row_id)) +
              geom_point(alpha=0.6) + col_sc
          } else {
            p <- ggplot(df_t, aes(x=x, y=resid, text=hover, key=row_id)) +
              geom_point(alpha=0.6, colour=dcol)
          }
          eff_sub <- resid_effect_summary(resids, mf[[vn]])
          p <- p +
            geom_hline(yintercept=0, linetype="dashed", colour="red") +
            labs(title=paste("Residuals vs", vn), x=vn, y="Residuals", colour=NULL,
                 subtitle=eff_sub) +
            theme_app()
          if (is.numeric(mf[[vn]]) && nrow(df_t) >= 10)
            p <- tryCatch(p + geom_smooth(method="loess", se=FALSE, colour="grey50", linewidth=0.8),
                          error = function(e) p)
          ggplotly(p, tooltip="text", source=SEL_SOURCE) %>%
            apply_selection_style(df_t$row_id, rv$selected_obs)
        })
      })
    }
  })

  # ── Residuals vs Omitted Terms ──────────────────────────────────────

  output$resid_vs_omitted_ui <- renderUI({
    req(input$resid_model, rv$models[[input$resid_model]], rv$data)
    m           <- rv$models[[input$resid_model]]
    in_model    <- names(model.frame(m))
    all_cols    <- names(rv$data)
    omitted     <- setdiff(all_cols, in_model)
    omitted     <- omitted[sapply(omitted, function(cn) {
      v <- rv$data[[cn]]
      is.numeric(v) || is.factor(v) || is.character(v)
    })]
    # Exclude internal columns
    omitted <- setdiff(omitted, ROW_ID_COL)
    if (length(omitted) == 0) {
      return(p("All data columns are included in this model.", class="text-muted"))
    }
    tagList(lapply(seq_along(omitted), function(i) {
      plotlyOutput(ns(paste0("resid_omit_", i)), height="320px")
    }))
  })

  observe({
    req(input$resid_model, rv$models[[input$resid_model]], rv$data)
    m        <- rv$models[[input$resid_model]]
    in_model <- names(model.frame(m))
    omitted  <- setdiff(names(rv$data), in_model)
    omitted  <- omitted[sapply(omitted, function(cn) {
      v <- rv$data[[cn]]
      is.numeric(v) || is.factor(v) || is.character(v)
    })]
    omitted <- setdiff(omitted, ROW_ID_COL)
    for (i in seq_along(omitted)) {
      local({
        idx <- i; cn <- omitted[idx]
        output[[paste0("resid_omit_", idx)]] <- renderPlotly({
          req(cn %in% names(rv$data))
          df_r <- resid_df()
          cv <- resid_colour_vec()
          cb <- input$resid_colour_by
          use_colour <- !is.null(cv) && !identical(cb, cn)
          n <- nrow(df_r)
          m2 <- rv$models[[input$resid_model]]
          row_idx <- as.integer(names(fitted(m2)))
          if (is.null(row_idx) || any(is.na(row_idx))) row_idx <- seq_len(n)
          x_vals <- rv$data[[cn]][row_idx]
          df_o   <- data.frame(x=x_vals, resid=df_r$resid_raw, hover=df_r$hover,
                               row_id=df_r$row_id)
          if (use_colour && length(cv) == n) df_o$colour_by <- cv
          use_colour <- use_colour && "colour_by" %in% names(df_o)
          dcol <- default_col()
          if (use_colour) {
            is_cont_c <- is.numeric(cv)
            col_sc <- if (is_cont_c) cont_scale_colour()() else cat_scale_colour()()
            p <- ggplot(df_o, aes(x=x, y=resid, colour=colour_by, text=hover,
                                  key=row_id)) +
              geom_point(alpha=0.6) + col_sc
          } else {
            p <- ggplot(df_o, aes(x=x, y=resid, text=hover, key=row_id)) +
              geom_point(alpha=0.6, colour=dcol)
          }
          eff_sub <- resid_effect_summary(df_r$resid_raw, x_vals)
          p <- p +
            geom_hline(yintercept=0, linetype="dashed", colour="red") +
            labs(title=paste("Residuals vs", cn, "(not in model)"),
                 subtitle=eff_sub,
                 x=cn, y="Residuals", colour=NULL) +
            theme_app()
          if (is.numeric(x_vals) && nrow(df_o) >= 10)
            p <- tryCatch(p + geom_smooth(method="loess", se=FALSE, colour="grey50", linewidth=0.8),
                          error = function(e) p)
          ggplotly(p, tooltip="text", source=SEL_SOURCE) %>%
            apply_selection_style(df_o$row_id, rv$selected_obs)
        })
      })
    }
  })

  # ── Outlier Table & Refit ──────────────────────────────────────────

  output$outlier_table <- DT::renderDataTable({
    df_d <- resid_df()
    flagged <- df_d[df_d$outlier_flag != "ok" | df_d$influential, , drop = FALSE]
    if (nrow(flagged) == 0) {
      return(DT::datatable(
        data.frame(Message = "No outliers or influential points detected."),
        options = list(dom = "t"), rownames = FALSE
      ))
    }
    m <- rv$models[[input$resid_model]]
    mf <- model.frame(m)
    resp_name <- names(mf)[1]
    show_df <- data.frame(
      Obs       = flagged$row_id,
      Response  = round(flagged$actual, 3),
      Fitted    = round(flagged$fitted_vals, 3),
      Residual  = round(flagged$resid_raw, 4),
      Studentised = round(flagged$resid_ext, 3),
      CooksD    = signif(flagged$cooks, 3),
      Flag      = ifelse(flagged$outlier_flag == "severe", "Severe outlier",
                   ifelse(flagged$outlier_flag == "moderate", "Moderate outlier",
                   ifelse(flagged$influential, "Influential", "")))
    )
    names(show_df)[2] <- resp_name
    sel_ids <- rv$selected_obs %||% integer(0)
    sel_rows <- which(show_df$Obs %in% sel_ids)
    dt <- dt_table(show_df, rownames = FALSE, selection = "multiple",
             options = list(pageLength = 50), dom = "Bt") %>%
      DT::formatStyle("Flag", backgroundColor = DT::styleEqual(
        c("Severe outlier", "Moderate outlier", "Influential"),
        c("#ffebee", "#fff8e1", "#fff3e0")
      ))
    # Highlight selected observations — target whole row
    matched <- sel_ids[sel_ids %in% show_df$Obs]
    if (length(matched) > 0) {
      matched_chr <- as.character(matched)
      dt <- dt %>% DT::formatStyle(
        "Obs",
        target = "row",
        backgroundColor = DT::styleEqual(
          matched_chr, rep("#e3f2fd", length(matched_chr))
        ),
        fontWeight = DT::styleEqual(
          matched_chr, rep("bold", length(matched_chr))
        )
      )
    }
    dt
  })

  output$excluded_info_ui <- renderUI({
    mname <- input$resid_model
    excl <- rv$excluded_obs[[mname]]
    if (length(excl) > 0) {
      div(class = "alert alert-info p-2",
        tags$b("Currently excluded:"),
        br(),
        paste("Obs #", paste(sort(excl), collapse = ", ")),
        br(),
        tags$small(class = "text-muted",
          paste0("(", length(excl), " observation(s) removed from this model)"))
      )
    }
  })

  observeEvent(input$refit_no_outliers, {
    if (is_locked(rv, "Refit model")) return()
    req(input$resid_model, length(rv$models) > 0)
    mname <- input$resid_model
    m <- rv$models[[mname]]
    df_d <- resid_df()
    flagged <- df_d[df_d$outlier_flag != "ok" | df_d$influential, , drop = FALSE]
    req(nrow(flagged) > 0)

    sel <- input$outlier_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select rows in the outlier table first.", type = "warning")
      return()
    }
    obs_to_exclude <- flagged$row_id[sel]

    orig_data <- model.frame(m)
    keep_rows <- setdiff(seq_len(nrow(orig_data)), flagged$index[sel])
    new_data <- orig_data[keep_rows, , drop = FALSE]

    new_model <- tryCatch(
      model_refit(m, data = new_data),
      error = function(e) { showNotification(paste("Refit error:", e$message), type = "error"); NULL }
    )
    if (is.null(new_model)) return()

    new_label <- paste0(deparse(formula(m)), " [excl obs ", paste(sort(obs_to_exclude), collapse=","), "]")
    rv$models[[new_label]] <- new_model
    rv$excluded_obs[[new_label]] <- obs_to_exclude
    rv$formulas <- c(rv$formulas, setNames(new_label, new_label))

    model_names <- names(rv$models)
    update_model_checkbox(model_names)
    updateSelectInput(session, "resid_model", choices = model_names, selected = new_label)
    rv$vif_df <- vif_summary(rv$models)

    showNotification(
      paste0("Refitted model excluding ", length(obs_to_exclude), " observation(s)."),
      type = "message", duration = 5
    )
  })

  # ── Diagnostics Panel ──────────────────────────────────────────────

  output$diagnostics_ui <- renderUI({
    req(input$resid_model, rv$models[[input$resid_model]])
    m <- rv$models[[input$resid_model]]
    diags <- run_diagnostics(m)

    colour_map <- c(green = "#28a745", amber = "#ffc107", red = "#dc3545")
    icon_map   <- c(green = "check-circle", amber = "exclamation-triangle", red = "times-circle")

    make_row <- function(label, d) {
      bg <- if (d$level == "green") "#e8f5e9"
            else if (d$level == "amber") "#fff8e1"
            else "#ffebee"
      fluidRow(
        style = paste0("background:", bg, "; border-radius:6px; padding:8px 12px; margin-bottom:6px;"),
        column(1, icon(icon_map[d$level], style = paste0("color:", colour_map[d$level], "; font-size:1.4em;"))),
        column(3, tags$b(label)),
        column(4, d$message),
        column(4,
          if (!is.null(d$test)) {
            stat_text <- if (!is.null(d$p.value))
              paste0(d$test, " = ", d$stat, ", p = ", d$p.value)
            else if (!is.null(d$n_influential))
              paste0(d$n_influential, " pts > ", d$threshold)
            else if (!is.null(d$n_moderate))
              paste0(d$n_moderate, " moderate, ", d$n_severe, " severe")
            else ""
            tags$small(class = "text-muted", stat_text)
          }
        )
      )
    }

    overall_bg <- if (diags$overall$level == "green") "#e8f5e9"
                  else if (diags$overall$level == "amber") "#fff8e1"
                  else "#ffebee"

    tagList(
      div(style = paste0("background:", overall_bg, "; border-radius:8px; padding:12px 16px; margin-bottom:16px; border:2px solid ", colour_map[diags$overall$level], ";"),
        h5(icon(icon_map[diags$overall$level], style = paste0("color:", colour_map[diags$overall$level])),
           " Overall: ", diags$overall$message)
      ),
      if (!is.null(diags$normality))          make_row("Normality",          diags$normality),
      if (!is.null(diags$heteroscedasticity)) make_row("Constant Variance",  diags$heteroscedasticity),
      if (!is.null(diags$outliers))           make_row("Outliers",           diags$outliers),
      if (!is.null(diags$influence))          make_row("Influential Points", diags$influence)
    )
  })
}
