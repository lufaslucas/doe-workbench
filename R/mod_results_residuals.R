# R/mod_results_residuals.R — Residuals sub-module (extracted from mod_results.R)
# Called from mod_results_server; shares the same Shiny namespace (no NS change).

results_residuals_server <- function(input, output, session, rv,
                                      active_models, update_model_checkbox,
                                      colour_theme, role_selectors,
                                      shared_reactives = NULL) {
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

  # Unpack shared reactives for treatment shape
  treatment_      <- if (!is.null(shared_reactives)) shared_reactives$treatment else reactive(NULL)
  treatment_label_ <- if (!is.null(shared_reactives)) shared_reactives$treatment_label else reactive("Treatment")
  analysis_mode_  <- if (!is.null(shared_reactives)) shared_reactives$analysis_mode else reactive("comparative")

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
      cooks         = model_cooks_distance(m),
      hat_val       = model_hat_values(m)
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

  # Treatment shape: only in comparative mode with multiple factors
  resid_treatment <- reactive({
    mode <- analysis_mode_()
    if (!identical(mode, "comparative")) return(NULL)
    trt <- treatment_()
    if (is.null(trt)) return(NULL)
    df_d <- resid_df()
    n <- nrow(df_d)
    if (length(trt) < n) return(NULL)
    as.factor(trt[seq_len(n)])
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

    # Treatment shape in comparative mode
    trt <- resid_treatment()
    has_shape <- !is.null(trt) && length(trt) == nrow(df_d)
    if (has_shape) df_d$.treatment <- trt
    trt_lab <- if (has_shape) treatment_label_() else "Treatment"

    if (has_colour && has_shape) {
      is_cont <- is.numeric(cv)
      col_scale <- if (is_cont) cont_scale_colour()() else cat_scale_colour()()
      p1 <- ggplot(df_d, aes(x=fitted_vals, y=resid_raw, colour=colour_by,
                              shape=.treatment, text=hover, key=row_id)) +
        geom_point(alpha=0.6) + col_scale + labs(shape=trt_lab)
      p2 <- ggplot(df_d, aes(x=theo_q, y=resid_std, colour=colour_by,
                              shape=.treatment, text=hover, key=row_id)) +
        geom_point(alpha=0.6) + col_scale + labs(shape=trt_lab)
      p3 <- ggplot(df_d, aes(x=fitted_vals, y=sqrt_abs_ext, colour=colour_by,
                              shape=.treatment, text=hover, key=row_id)) +
        geom_point(alpha=0.6) + col_scale + labs(shape=trt_lab)
    } else if (has_colour) {
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
    } else if (has_shape) {
      p1 <- ggplot(df_d, aes(x=fitted_vals, y=resid_raw, shape=.treatment,
                              text=hover, key=row_id)) +
        geom_point(alpha=0.6, colour=dcol) + labs(shape=trt_lab)
      p2 <- ggplot(df_d, aes(x=theo_q, y=resid_std, shape=.treatment,
                              text=hover, key=row_id)) +
        geom_point(alpha=0.6, colour=dcol) + labs(shape=trt_lab)
      p3 <- ggplot(df_d, aes(x=fitted_vals, y=sqrt_abs_ext, shape=.treatment,
                              text=hover, key=row_id)) +
        geom_point(alpha=0.6, colour=dcol) + labs(shape=trt_lab)
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
             plotly::event_register("plotly_selected") %>%
             plotly::event_register("plotly_click") %>%
             plotly::event_register("plotly_deselect") %>%
             apply_selection_style(df_d$row_id, sel)
    pp2 <- ggplotly(p2, tooltip="text", source=SEL_SOURCE) %>%
             plotly::event_register("plotly_selected") %>%
             plotly::event_register("plotly_click") %>%
             plotly::event_register("plotly_deselect") %>%
             apply_selection_style(df_d$row_id, sel)
    pp3 <- ggplotly(p3, tooltip="text", source=SEL_SOURCE) %>%
             plotly::event_register("plotly_selected") %>%
             plotly::event_register("plotly_click") %>%
             plotly::event_register("plotly_deselect") %>%
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
          trt <- resid_treatment()
          has_shape <- !is.null(trt) && length(trt) == nrow(df_t)
          if (has_shape) df_t$.treatment <- trt
          trt_lab <- if (has_shape) treatment_label_() else "Treatment"
          if (use_colour && has_shape) {
            is_cont_c <- is.numeric(cv)
            col_sc <- if (is_cont_c) cont_scale_colour()() else cat_scale_colour()()
            p <- ggplot(df_t, aes(x=x, y=resid, colour=colour_by, shape=.treatment,
                                  text=hover, key=row_id)) +
              geom_point(alpha=0.6) + col_sc + labs(shape=trt_lab)
          } else if (use_colour) {
            is_cont_c <- is.numeric(cv)
            col_sc <- if (is_cont_c) cont_scale_colour()() else cat_scale_colour()()
            p <- ggplot(df_t, aes(x=x, y=resid, colour=colour_by, text=hover,
                                  key=row_id)) +
              geom_point(alpha=0.6) + col_sc
          } else if (has_shape) {
            p <- ggplot(df_t, aes(x=x, y=resid, shape=.treatment, text=hover,
                                  key=row_id)) +
              geom_point(alpha=0.6, colour=dcol) + labs(shape=trt_lab)
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
            plotly::event_register("plotly_selected") %>%
            plotly::event_register("plotly_click") %>%
            plotly::event_register("plotly_deselect") %>%
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
          trt <- resid_treatment()
          has_shape <- !is.null(trt) && length(trt) == nrow(df_o)
          if (has_shape) df_o$.treatment <- trt
          trt_lab <- if (has_shape) treatment_label_() else "Treatment"
          if (use_colour && has_shape) {
            is_cont_c <- is.numeric(cv)
            col_sc <- if (is_cont_c) cont_scale_colour()() else cat_scale_colour()()
            p <- ggplot(df_o, aes(x=x, y=resid, colour=colour_by, shape=.treatment,
                                  text=hover, key=row_id)) +
              geom_point(alpha=0.6) + col_sc + labs(shape=trt_lab)
          } else if (use_colour) {
            is_cont_c <- is.numeric(cv)
            col_sc <- if (is_cont_c) cont_scale_colour()() else cat_scale_colour()()
            p <- ggplot(df_o, aes(x=x, y=resid, colour=colour_by, text=hover,
                                  key=row_id)) +
              geom_point(alpha=0.6) + col_sc
          } else if (has_shape) {
            p <- ggplot(df_o, aes(x=x, y=resid, shape=.treatment, text=hover,
                                  key=row_id)) +
              geom_point(alpha=0.6, colour=dcol) + labs(shape=trt_lab)
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
            plotly::event_register("plotly_selected") %>%
            plotly::event_register("plotly_click") %>%
            plotly::event_register("plotly_deselect") %>%
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
      Int_Student = round(flagged$resid_std, 3),
      Ext_Student = round(flagged$resid_ext, 3),
      CooksD    = signif(flagged$cooks, 3),
      Leverage  = round(flagged$hat_val, 3),
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

  # ── Outlier table row selection → linked plot highlight ────────────
  observeEvent(input$outlier_table_rows_selected, {
    sel <- input$outlier_table_rows_selected
    df_d <- resid_df()
    flagged <- df_d[df_d$outlier_flag != "ok" | df_d$influential, , drop = FALSE]
    if (length(sel) == 0 || nrow(flagged) == 0) {
      rv$selected_obs <- NULL
      return()
    }
    rv$selected_obs <- as.integer(flagged$row_id[sel])
  }, ignoreNULL = TRUE)

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

    base_formula <- deparse(formula(m))
    # Create a display label with excluded obs info (for UI only)
    # The formula stored in rv$formulas must be a valid R formula string
    excl_suffix <- paste0(" #excl", paste(sort(obs_to_exclude), collapse=","))
    new_label <- paste0(base_formula, excl_suffix)
    rv$models[[new_label]] <- new_model
    rv$excluded_obs[[new_label]] <- obs_to_exclude
    # Store the CLEAN formula (without exclusion label) so it can be re-parsed
    rv$formulas <- c(rv$formulas, setNames(base_formula, new_label))

    model_names <- names(rv$models)
    update_model_checkbox(model_names)
    updateSelectInput(session, "resid_model", choices = model_names, selected = new_label)
    rv$vif_df <- vif_summary(rv$models)

    # Invalidate stale MC results for the original model
    if (length(rv$mc_results) > 0) {
      stale_keys <- grep(paste0("^", gsub("([.+*?^${}()|\\[\\]])", "\\\\\\1", mname), "__"),
                         names(rv$mc_results), value = TRUE)
      for (sk in stale_keys) rv$mc_results[[sk]] <- NULL
    }

    showNotification(
      paste0("Refitted model excluding ", length(obs_to_exclude),
             " observation(s). Multiple comparison results may need recalculating."),
      type = "message", duration = 6
    )
  })

  # ── Iterative Outlier Removal ────────────────────────────────────

  observeEvent(input$iter_remove_outliers, {
    if (is_locked(rv, "Iterative removal")) return()
    req(input$resid_model, length(rv$models) > 0)
    mname <- input$resid_model
    m <- rv$models[[mname]]
    thresh <- input$iter_outlier_thresh %||% 3
    resid_fn <- if (identical(input$iter_outlier_type, "int")) model_standard_residuals
                else model_studentized_residuals

    orig_data <- model.frame(m)
    current_model <- m
    current_data <- orig_data
    all_excluded <- integer(0)
    iteration <- 0
    max_iter <- 10  # safety cap
    new_models <- list()

    while (iteration < max_iter) {
      iteration <- iteration + 1
      rst <- tryCatch(resid_fn(current_model), error = function(e) numeric(0))
      if (length(rst) == 0) break
      worst_idx <- which.max(abs(rst))
      if (abs(rst[worst_idx]) <= thresh) break

      # Get the row_id of the excluded observation
      n_cur <- nrow(current_data)
      row_ids <- if (ROW_ID_COL %in% names(rv$data)) {
        rv$data[[ROW_ID_COL]][as.integer(rownames(current_data))]
      } else as.integer(rownames(current_data))
      excl_id <- row_ids[worst_idx]
      all_excluded <- c(all_excluded, excl_id)

      # Remove and refit
      current_data <- current_data[-worst_idx, , drop = FALSE]
      current_model <- tryCatch(
        model_refit(m, data = current_data),
        error = function(e) NULL)
      if (is.null(current_model)) break

      # Store intermediate model
      base_formula <- deparse(formula(m))
      label <- paste0(base_formula, " #iter", iteration,
                      "_excl", paste(sort(all_excluded), collapse = ","))
      new_models[[label]] <- current_model
    }

    if (length(new_models) == 0) {
      showNotification("No outliers exceed the threshold.", type = "message")
      return()
    }

    # Add all intermediate + final models
    base_formula <- deparse(formula(m))
    for (i in seq_along(new_models)) {
      label <- names(new_models)[i]
      rv$models[[label]] <- new_models[[label]]
      rv$excluded_obs[[label]] <- all_excluded[seq_len(i)]
      # Store clean formula for re-parsing
      rv$formulas <- c(rv$formulas, setNames(base_formula, label))
    }

    model_names <- names(rv$models)
    update_model_checkbox(model_names)
    final_label <- names(new_models)[length(new_models)]
    updateSelectInput(session, "resid_model", choices = model_names, selected = final_label)
    rv$vif_df <- vif_summary(rv$models)

    showNotification(
      paste0("Iterative removal: ", iteration, " iteration(s), ",
             length(all_excluded), " obs removed. ",
             length(new_models), " model(s) added."),
      type = "message", duration = 8
    )
  })

  # ── Studentised Residuals vs Cook's D Scatter Plot ───────────────

  output$outlier_influence_plot <- renderPlotly({
    df_d <- resid_df()
    req(nrow(df_d) > 0)
    n <- nrow(df_d)
    cook_thresh <- 4 / n
    dcol <- default_col()

    # Choose residual type
    resid_type <- input$outlier_resid_type %||% "ext"
    if (resid_type == "int") {
      df_d$student <- df_d$resid_std
      y_lab <- "Internally Studentised Residual"
    } else {
      df_d$student <- df_d$resid_ext
      y_lab <- "Externally Studentised Residual"
    }

    # Classification zones
    df_d$zone <- ifelse(abs(df_d$student) > 3 & df_d$cooks > cook_thresh, "Outlier + Influential",
                  ifelse(abs(df_d$student) > 2 & df_d$cooks > cook_thresh, "Moderate + Influential",
                  ifelse(abs(df_d$student) > 3, "Outlier (not influential)",
                  ifelse(abs(df_d$student) > 2, "Moderate outlier",
                  ifelse(df_d$cooks > cook_thresh, "Influential (not outlier)", "OK")))))

    zone_cols <- c("Outlier + Influential" = "#dc3545",
                   "Moderate + Influential" = "#fd7e14",
                   "Outlier (not influential)" = "#e83e8c",
                   "Moderate outlier" = "#ffc107",
                   "Influential (not outlier)" = "#6f42c1",
                   "OK" = dcol)

    # Treatment shape in comparative mode
    trt <- resid_treatment()
    has_shape <- !is.null(trt) && length(trt) == nrow(df_d)
    if (has_shape) df_d$.treatment <- trt
    trt_lab <- if (has_shape) treatment_label_() else "Treatment"

    p <- if (has_shape) {
      ggplot(df_d, aes(x = cooks, y = student, colour = zone, shape = .treatment,
                       text = hover, key = row_id))
    } else {
      ggplot(df_d, aes(x = cooks, y = student, colour = zone, text = hover, key = row_id))
    }
    p <- p +
      # Shaded boundary regions
      annotate("rect", xmin = cook_thresh, xmax = Inf, ymin = -Inf, ymax = Inf,
               fill = "#6f42c1", alpha = 0.04) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2, ymax = Inf,
               fill = "#ffc107", alpha = 0.04) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -2,
               fill = "#ffc107", alpha = 0.04) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 3, ymax = Inf,
               fill = "#dc3545", alpha = 0.06) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = -3,
               fill = "#dc3545", alpha = 0.06) +
      geom_point(alpha = 0.7, size = 2.5) +
      geom_vline(xintercept = cook_thresh, linetype = "dashed", colour = "#6f42c1", alpha = 0.6) +
      geom_hline(yintercept = c(-2, 2), linetype = "dashed", colour = "#ffc107", alpha = 0.6) +
      geom_hline(yintercept = c(-3, 3), linetype = "dashed", colour = "#dc3545", alpha = 0.6) +
      scale_colour_manual(values = zone_cols, name = "Classification") +
      labs(x = paste0("Cook's Distance (threshold = ", round(cook_thresh, 3), ")"),
           y = y_lab,
           title = paste(y_lab, "vs Cook's Distance"),
           shape = if (has_shape) trt_lab else ggplot2::waiver()) +
      theme_app() + theme(legend.position = "right")

    # Label flagged points
    df_label <- df_d[df_d$zone != "OK", , drop = FALSE]
    if (nrow(df_label) > 0)
      p <- p + geom_text(data = df_label, aes(label = as.character(row_id)),
                          size = 3, vjust = -0.8, show.legend = FALSE)

    sel <- rv$selected_obs
    ggplotly(p, tooltip = "text", source = SEL_SOURCE) %>%
      plotly::event_register("plotly_selected") %>%
      plotly::event_register("plotly_click") %>%
      plotly::event_register("plotly_deselect") %>%
      apply_selection_style(df_d$row_id, sel)
  })

  # ── Residual Pattern Analysis ─────────────────────────────────────

  output$resid_pattern_ui <- renderUI({
    req(input$resid_model, rv$models[[input$resid_model]])
    m <- rv$models[[input$resid_model]]
    mf <- model.frame(m)
    resids <- residuals(m)
    pred_vars <- names(mf)[-1]
    n <- length(resids)
    p_model <- length(coef(m))

    findings <- list()

    # 1. Check residuals vs each model term for systematic patterns
    for (vn in pred_vars) {
      x_vals <- mf[[vn]]
      if (is.numeric(x_vals)) {
        # Test for quadratic pattern (missing polynomial)
        quad_p <- tryCatch({
          s <- summary(lm(resids ~ x_vals + I(x_vals^2)))
          coefs <- s$coefficients
          if (nrow(coefs) >= 3) coefs[3, 4] else NA
        }, error = function(e) NA)
        if (!is.na(quad_p) && quad_p < 0.05)
          findings <- c(findings, list(list(
            level = "amber", term = vn, type = "missing_quad",
            msg = paste0("Quadratic pattern in residuals vs ", vn,
                         " (p=", signif(quad_p, 2), ") \u2014 consider adding ", vn, "\u00b2"))))

        # Test for unequal variance (heteroscedasticity vs term)
        bp_p <- tryCatch({
          s <- summary(lm(abs(resids) ~ x_vals))
          pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)
        }, error = function(e) NA)
        if (!is.na(bp_p) && bp_p < 0.05)
          findings <- c(findings, list(list(
            level = "amber", term = vn, type = "het_var",
            msg = paste0("Variance changes with ", vn,
                         " (p=", signif(bp_p, 2), ") \u2014 possible heteroscedasticity"))))
      } else {
        # Factor: test for different group variances
        grp <- as.factor(x_vals)
        if (nlevels(grp) >= 2) {
          lev_p <- tryCatch({
            abs_dev <- abs(resids - ave(resids, grp, FUN = median))
            s <- summary(aov(abs_dev ~ grp))
            s[[1]]$`Pr(>F)`[1]
          }, error = function(e) NA)
          if (!is.na(lev_p) && lev_p < 0.05)
            findings <- c(findings, list(list(
              level = "amber", term = vn, type = "het_var",
              msg = paste0("Variance differs across ", vn, " levels",
                           " (p=", signif(lev_p, 2), ") \u2014 unequal variance"))))
        }
      }
    }

    # 2. Check residuals vs omitted variables for missing terms
    all_cols <- names(rv$data)
    in_model <- names(mf)
    omitted <- setdiff(all_cols, in_model)
    omitted <- setdiff(omitted, ROW_ID_COL)
    omitted <- omitted[sapply(omitted, function(cn) {
      v <- rv$data[[cn]]
      is.numeric(v) || is.factor(v) || is.character(v)
    })]

    for (cn in omitted) {
      row_idx <- as.integer(names(fitted(m)))
      if (is.null(row_idx) || any(is.na(row_idx))) row_idx <- seq_len(n)
      x_vals <- rv$data[[cn]][row_idx]
      if (is.numeric(x_vals)) {
        cor_p <- tryCatch(cor.test(x_vals, resids)$p.value, error = function(e) NA)
        if (!is.na(cor_p) && cor_p < 0.05)
          findings <- c(findings, list(list(
            level = "red", term = cn, type = "missing_term",
            msg = paste0("Residuals correlate with omitted variable ", cn,
                         " (p=", signif(cor_p, 2), ") \u2014 consider adding to model"))))
      } else {
        grp <- as.factor(x_vals)
        if (nlevels(grp) >= 2 && nlevels(grp) < n) {
          aov_p <- tryCatch({
            s <- summary(aov(resids ~ grp))
            s[[1]]$`Pr(>F)`[1]
          }, error = function(e) NA)
          if (!is.na(aov_p) && aov_p < 0.05)
            findings <- c(findings, list(list(
              level = "red", term = cn, type = "missing_term",
              msg = paste0("Residuals differ by omitted factor ", cn,
                           " (p=", signif(aov_p, 2), ") \u2014 consider adding to model"))))
        }
      }
    }

    # 3. Check for missing interactions among model terms
    numeric_terms <- pred_vars[sapply(pred_vars, function(v) is.numeric(mf[[v]]))]
    if (length(numeric_terms) >= 2) {
      term_labels <- attr(terms(m), "term.labels")
      for (i in seq_along(numeric_terms)) {
        for (j in seq(i + 1, length(numeric_terms))) {
          if (j > length(numeric_terms)) break
          v1 <- numeric_terms[i]; v2 <- numeric_terms[j]
          int_term <- paste0(v1, ":", v2)
          if (int_term %in% term_labels || paste0(v2, ":", v1) %in% term_labels) next
          int_p <- tryCatch({
            s <- summary(lm(resids ~ mf[[v1]] * mf[[v2]]))
            coefs <- s$coefficients
            if (nrow(coefs) >= 4) coefs[4, 4] else NA
          }, error = function(e) NA)
          if (!is.na(int_p) && int_p < 0.01)  # stricter threshold for interactions
            findings <- c(findings, list(list(
              level = "amber", term = int_term, type = "missing_int",
              msg = paste0("Possible missing ", v1, "\u00d7", v2, " interaction",
                           " (p=", signif(int_p, 2), ")"))))
        }
      }
    }

    # Render findings
    if (length(findings) == 0) {
      return(div(class = "alert alert-success p-2",
        icon("check-circle"), " No systematic patterns detected in residuals."))
    }

    colour_map <- c(green = "#28a745", amber = "#ffc107", red = "#dc3545")
    icon_map   <- c(green = "check-circle", amber = "exclamation-triangle", red = "times-circle")

    tagList(lapply(findings, function(f) {
      bg <- if (f$level == "red") "#ffebee" else "#fff8e1"
      ic <- icon_map[f$level]
      cl <- colour_map[f$level]
      div(style = paste0("background:", bg, "; border-radius:6px; padding:8px 12px; margin-bottom:4px;"),
        icon(ic, style = paste0("color:", cl, ";")),
        tags$b(f$term), " \u2014 ", f$msg
      )
    }))
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
      if (!is.null(diags$influence))          make_row("Influential Points", diags$influence),
      if (!is.null(diags$influence) && diags$influence$level != "green")
        tags$small(class = "text-muted", style = "margin-left: 12px;",
          icon("info-circle"),
          " In screening/optimisation DoEs, influential points are expected ",
          "\u2014 they are often the extreme design points that drive the model. ",
          "Focus on outliers (large residuals) and assumption violations above.")
    )
  })
}
