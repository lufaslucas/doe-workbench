# R/mod_report.R — Report tab module (UI + server)

# ── UI ───────────────────────────────────────────────────────────────────────
mod_report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
        wellPanel(
          h4("Standard Report"),
          uiOutput(ns("report_summary")),
          hr(),
          radioButtons(ns("report_format"), "Output format",
                       choices = c("Word (.docx)" = "word", "PDF" = "pdf",
                                   "HTML" = "html"),
                       selected = "word"),
          downloadButton(ns("download_report"), "Download Report",
                         class = "btn-primary w-100 mt-2")
        )
      ),
      column(6,
        wellPanel(
          h5("Standard report includes:"),
          tags$ul(
            tags$li("Raw data preview"),
            tags$li("Column role and type assignments"),
            tags$li("ANOVA Type III table (selected models)"),
            tags$li("Model fit summary (RMSE, R\u00b2, Adj-R\u00b2)"),
            tags$li("Collinearity (VIF) table"),
            tags$li("Effects plots (LS means + covariate effects)"),
            tags$li("Multiple comparison results (if enabled)"),
            tags$li("Residual diagnostic plots (all selected models)")
          )
        )
      )
    ),
    hr(),
    p(class = "text-muted small",
      icon("info-circle"),
      " Custom report builder coming soon \u2014 add annotations to any plot or table in-situ and build a custom analysis narrative.")
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
mod_report_server <- function(id, rv, role_selectors, active_models,
                              mc_state) {
  moduleServer(id, function(input, output, session) {

    # Convenience aliases for role selectors
    factors_       <- role_selectors$factors_
    blocks         <- role_selectors$blocks
    all_covariates <- role_selectors$all_covariates

    # ── Report summary ─────────────────────────────────────────────────────
    output$report_summary <- renderUI({
      tagList(
        p(strong(length(rv$models)), "model(s) fitted,",
          strong(length(active_models())), "selected for report.")
      )
    })

    # ── Standard report download ───────────────────────────────────────────
    output$download_report <- downloadHandler(
      filename = function() {
        ext <- switch(input$report_format, word = "docx", pdf = "pdf", html = "html")
        paste0("ModelComparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
      },
      content = function(file) {
        req(rv$data, length(active_models()) > 0)

        all_terms_raw <- unique(unlist(lapply(active_models(), function(m) {
          a <- tryCatch(model_anova(m, type=3), error=function(e) NULL)
          if (!is.null(a)) rownames(a)[rownames(a) != "(Intercept)"] else character(0)
        })))
        factor_cols  <- factors_()
        # Only compute LS means for categorical factor terms (type = Factor)
        cat_factor_terms <- all_terms_raw[sapply(all_terms_raw, function(t) {
          parts <- strsplit(t, ":")[[1]]
          all(parts %in% factor_cols) &&
            all(sapply(parts, function(cn) (rv$col_types[[cn]] %||% "Factor") == "Factor"))
        })]
        # Numeric factor terms go in effects_list as covariates
        num_factor_terms <- setdiff(
          all_terms_raw[sapply(all_terms_raw, function(t) all(strsplit(t,":")[[1]] %in% factor_cols))],
          cat_factor_terms)

        lsmeans_list <- list()
        for (spec in cat_factor_terms) {
          ls_rows <- lapply(names(active_models()), function(mn) {
            get_lsmeans_df(active_models()[[mn]], spec, mn)
          })
          lsmeans_list[[spec]] <- bind_rows(Filter(Negate(is.null), ls_rows))
        }

        # Effects data for covariates + numeric factor terms
        cov_cols <- unique(c(all_covariates(), num_factor_terms))
        effects_list <- list()
        for (cov in cov_cols) {
          eff_rows <- lapply(names(active_models()), function(mn) {
            m <- active_models()[[mn]]
            mf <- tryCatch(model.frame(m), error=function(e) NULL)
            if (is.null(mf) || !cov %in% names(mf)) return(NULL)
            get_effects_df(m, cov, mn)
          })
          combined <- bind_rows(Filter(Negate(is.null), eff_rows))
          if (nrow(combined) > 0) effects_list[[cov]] <- combined
        }

        # Term roles for ANOVA ordering
        tr <- classify_anova_terms(all_terms_raw, factors_(), blocks(), all_covariates())

        # Build reproducible code
        repro_code <- tryCatch({
          header <- codegen_header()
          data_setup <- codegen_data_setup(rv)
          model_code <- vapply(names(active_models()), function(mn) {
            f <- rv$formulas[rv$formulas == mn]
            if (length(f) == 0) f <- mn  # formula is the label
            codegen_anova(f[1])
          }, character(1))
          paste(c(header, "", data_setup, "", model_code), collapse = "\n\n")
        }, error = function(e) "")

        params_list <- list(
          raw_data        = head(rv$data, 50),
          roles           = rv$roles,
          col_types       = rv$col_types,
          selected_models = names(active_models()),
          anova_wide      = type3_wide(active_models(), tr),
          rmse_df         = rmse_summary(active_models()),
          vif_df          = rv$vif_df,
          lsmeans_list    = lsmeans_list,
          effects_list    = effects_list,
          mc_results_list = rv$mc_results,
          mc_on           = isTRUE(mc_state$mc_on()),
          mc_terms        = mc_state$mc_terms(),
          mc_methods      = mc_state$mc_methods(),
          repro_code      = repro_code
        )

        fmt <- switch(input$report_format,
          word = {
            if (has_officedown) officedown::rdocx_document()
            else rmarkdown::word_document()
          },
          pdf = {
            if (!tinytex::is_tinytex() && Sys.which("xelatex") == "") {
              showNotification("Installing TinyTeX for PDF\u2026", type="message", duration=120)
              tinytex::install_tinytex()
            }
            engine <- if (Sys.which("xelatex") != "" || tinytex::is_tinytex()) "xelatex" else "pdflatex"
            rmarkdown::pdf_document(latex_engine=engine)
          },
          html = rmarkdown::html_document(
            toc            = TRUE,
            toc_depth      = 3,
            toc_float      = TRUE,
            theme          = "flatly",
            self_contained = TRUE
          )
        )

        withProgress(message="Rendering report\u2026", value=0.3, {
          tryCatch({
            rmarkdown::render(
              input         = system.file("rmd", "report_template.Rmd", package = "doe.workbench"),
              output_format = fmt,
              output_file   = file,
              params        = params_list,
              envir         = new.env(parent=globalenv()),
              quiet         = TRUE
            )
            incProgress(0.7)
          }, error = function(e) {
            showNotification(paste("Report error:", e$message), type="error", duration=10)
          })
        })
      }
    )

    # ── Custom Report Builder (server logic ready; UI placeholder in mod_report_ui) ──

    observeEvent(input$report_add_item, {
      item_type <- input$report_add_type
      comment   <- input$report_item_comment %||% ""
      timestamp <- format(Sys.time(), "%H:%M:%S")

      title <- switch(item_type,
        anova      = "ANOVA Table (Type III)",
        rmse       = "Model Fit Summary",
        vif        = "Collinearity (VIF)",
        coef       = "Coefficients",
        effects    = paste0("Effects Plot (", input$effects_term %||% "?", ")"),
        mc         = "Multiple Comparisons",
        resid      = paste0("Residual Diagnostics (", input$resid_model %||% "?", ")"),
        diag_flags = paste0("Diagnostic Flags (", input$resid_model %||% "?", ")"),
        balance    = "Design Balance Checks",
        item_type
      )

      new_item <- list(
        id        = paste0("ri_", as.integer(Sys.time()), "_", length(rv$report_items) + 1),
        type      = item_type,
        title     = title,
        comment   = comment,
        timestamp = timestamp,
        model     = input$resid_model %||% NULL,
        term      = input$effects_term %||% NULL
      )

      rv$report_items <- c(rv$report_items, list(new_item))
      updateTextInput(session, "report_item_comment", value = "")
      showNotification(paste0("Added: ", title), type = "message", duration = 3)
    })

    observeEvent(input$report_clear_items, {
      rv$report_items <- list()
      showNotification("Report builder cleared.", type = "message", duration = 3)
    })

    # Remove individual items
    observe({
      lapply(seq_along(rv$report_items), function(i) {
        btn_id <- paste0("report_remove_", rv$report_items[[i]]$id)
        observeEvent(input[[btn_id]], {
          rv$report_items <- rv$report_items[-i]
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    output$report_builder_items <- renderUI({
      items <- rv$report_items
      if (length(items) == 0) {
        return(p(class = "text-muted", "No items added yet. Use the controls above to build your report."))
      }
      ns <- session$ns
      tagList(lapply(seq_along(items), function(i) {
        item <- items[[i]]
        div(style = "background: #f8f9fa; border: 1px solid #dee2e6; border-radius: 6px; padding: 10px 14px; margin-bottom: 8px;",
          fluidRow(
            column(1, tags$b(paste0(i, "."))),
            column(4, tags$b(item$title),
                      tags$small(class = "text-muted d-block", item$timestamp)),
            column(5, if (nchar(item$comment) > 0) tags$em(item$comment) else tags$span(class = "text-muted", "No comment")),
            column(2,
              actionButton(ns(paste0("report_remove_", item$id)), NULL,
                           icon = icon("times"), class = "btn-sm btn-outline-danger"))
          )
        )
      }))
    })

    # Download custom report as HTML
    output$download_custom_report <- downloadHandler(
      filename = function() {
        paste0("Custom_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      },
      content = function(file) {
        items <- rv$report_items
        req(length(items) > 0)

        html_parts <- c(
          "<!DOCTYPE html><html><head><meta charset='utf-8'>",
          "<title>Custom Analysis Report</title>",
          "<style>body{font-family:sans-serif;max-width:900px;margin:40px auto;padding:0 20px;}",
          "table{border-collapse:collapse;width:100%;margin:10px 0;} th,td{border:1px solid #ddd;padding:6px 10px;text-align:left;}",
          "th{background:#f5f5f5;} .comment{background:#f0f8ff;padding:8px 12px;border-left:3px solid #007bff;margin:8px 0;font-style:italic;}",
          ".section{margin:24px 0;padding:16px;border:1px solid #eee;border-radius:6px;}",
          "h1{color:#333;} h2{color:#555;border-bottom:1px solid #eee;padding-bottom:6px;}</style></head><body>",
          paste0("<h1>Custom Analysis Report</h1><p>Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "</p><hr>")
        )

        for (item in items) {
          html_parts <- c(html_parts, "<div class='section'>",
                           paste0("<h2>", htmltools::htmlEscape(item$title), "</h2>"))

          content_html <- tryCatch({
            switch(item$type,
              anova = {
                req(length(rv$models) > 0)
                tbl <- type3_wide(rv$models, col_types = rv$col_types)
                knitr::kable(tbl, format = "html", digits = 4)
              },
              rmse = {
                req(length(rv$models) > 0)
                tbl <- rmse_summary(rv$models)
                knitr::kable(tbl, format = "html", digits = 4)
              },
              vif = {
                req(nrow(rv$vif_df) > 0)
                knitr::kable(rv$vif_df, format = "html", digits = 3)
              },
              coef = {
                req(length(rv$models) > 0)
                tbl <- coef_table(rv$models, col_types = rv$col_types,
                                  transforms = rv$transforms,
                                  coding_values = rv$coding_values)
                knitr::kable(tbl, format = "html", digits = 4)
              },
              diag_flags = {
                m <- rv$models[[item$model %||% names(rv$models)[1]]]
                if (!is.null(m)) {
                  d <- run_diagnostics(m)
                  paste0("<ul>",
                    paste0("<li><b>Normality:</b> ", d$normality$message, " (", d$normality$level, ")</li>"),
                    paste0("<li><b>Variance:</b> ", d$heteroscedasticity$message, " (", d$heteroscedasticity$level, ")</li>"),
                    paste0("<li><b>Outliers:</b> ", d$outliers$message, " (", d$outliers$level, ")</li>"),
                    paste0("<li><b>Influence:</b> ", d$influence$message, " (", d$influence$level, ")</li>"),
                    paste0("<li><b>Overall:</b> ", d$overall$message, " (", d$overall$level, ")</li>"),
                    "</ul>")
                } else "<p>No model available.</p>"
              },
              "<p>(Content available in app)</p>"
            )
          }, error = function(e) paste0("<p>Error generating content: ", htmltools::htmlEscape(e$message), "</p>"))

          html_parts <- c(html_parts, content_html)

          if (nchar(item$comment) > 0) {
            html_parts <- c(html_parts,
              paste0("<div class='comment'>", htmltools::htmlEscape(item$comment), "</div>"))
          }
          html_parts <- c(html_parts, "</div>")
        }

        html_parts <- c(html_parts, "</body></html>")
        writeLines(paste(html_parts, collapse = "\n"), file)
      }
    )
  })
}
