# R/mod_models.R -- Models tab module (UI + server)

# ── UI ───────────────────────────────────────────────────────────────────────
mod_models_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        wellPanel(
          h4("Model Builder"),
          selectInput(ns("active_response"), "Response variable", choices = NULL),
          fluidRow(
            column(6, selectInput(ns("model_type"), "Model type",
                                  choices = MODEL_TYPES, selected = "lm")),
            column(6, selectInput(ns("weight_column"), "Weight column",
                                  choices = c("(none)" = ""), selected = ""))
          ),
          h5("Factor terms"),
          numericInput(ns("max_way"), "Max factor interaction order", value = 2, min = 1, max = 5),
          conditionalPanel(
            condition = "input.analysis_mode == 'regression'",
            numericInput(ns("poly_degree"), "Polynomial degree", value = 2, min = 1, max = 3)
          ),
          h5("Covariates & blocks"),
          uiOutput(ns("formula_cov_selector_ui")),
          conditionalPanel(
            condition = paste0("input['", ns("include_covariates"), "'] == true"),
            checkboxInput(ns("include_cov_fac"), "Include covariate \u00d7 factor interactions", value = FALSE)
          ),
          checkboxInput(ns("include_blocks"), "Include blocks", value = TRUE),
          checkboxInput(ns("include_block_fac"), "Include block \u00d7 factor interactions", value = FALSE),
          hr(),
          fluidRow(
            column(7,
              actionButton(ns("generate_formulas"), "Generate Formulas",
                           class = "btn-primary btn-sm w-100 mb-2",
                           icon = icon("cogs"))
            ),
            column(5,
              checkboxInput(ns("append_formulas"), "Append", value = FALSE)
            )
          ),
          div(
            style = "margin-bottom: 6px;",
            actionButton(ns("select_all_formulas"), "Select All",
                         class = "btn-sm btn-outline-secondary"),
            actionButton(ns("deselect_all_formulas"), "Deselect All",
                         class = "btn-sm btn-outline-secondary ms-1")
          ),
          div(style = "max-height: 300px; overflow-y: auto;",
            uiOutput(ns("formula_list_ui"))
          ),
          hr(),
          h5("Custom formula"),
          selectInput(ns("copy_from_formula"), "Start from existing formula",
                      choices = c("(blank)" = ""), width = "100%"),
          tags$div(style = "margin-bottom: 6px;",
            actionButton(ns("add_main_effects"), "+ Main Effects",
                         class = "btn-xs btn-outline-secondary"),
            actionButton(ns("add_2way"), "+ 2-way",
                         class = "btn-xs btn-outline-secondary"),
            actionButton(ns("add_3way"), "+ 3-way",
                         class = "btn-xs btn-outline-secondary")
          ),
          textAreaInput(ns("custom_formula"), NULL,
                        placeholder = "y ~ A + B + A:B",
                        rows = 3, resize = "vertical"),
          tags$style(sprintf("#%s { font-family: monospace; font-size: 12px; }", ns("custom_formula"))),
          uiOutput(ns("formula_custom_chooser")),
          actionButton(ns("add_custom"), "Add Custom", class = "btn-sm btn-outline-primary"),
          hr(),
          numericInput(ns("mc_alpha_sidebar"), "Significance level (\u03b1)",
                       value = 0.05, min = 0.001, max = 0.5, step = 0.01),
          actionButton(ns("run_models"), "Run Selected Models",
                       class = "btn-success w-100"),
          hr(),
          h5("Backward Elimination"),
          numericInput(ns("prune_alpha"), "Alpha threshold", value = 0.05,
                       min = 0.001, max = 0.5, step = 0.01),
          actionButton(ns("prune_btn"), "Prune Selected Models",
                       class = "btn-warning w-100",
                       icon = icon("scissors"))
        )
      ),
      column(8,
        wellPanel(
          h4("Multiple Comparisons"),
          checkboxInput(ns("mc_on"), "Enable multiple comparisons", value = FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("mc_on"), "'] == true"),
            uiOutput(ns("mc_no_terms_msg")),
            h5("Terms to test"),
            checkboxGroupInput(ns("mc_terms"), NULL, choices = NULL),
            h5("Methods"),
            checkboxGroupInput(ns("mc_method"), NULL,
                               choices = c("Student (unadjusted)" = "student",
                                           "Dunnett (vs control)" = "dunnett",
                                           "MaxT (custom pairs)" = "custom",
                                           "Tukey (all pairs)" = "tukey"),
                               selected = "tukey"),
            conditionalPanel(
              condition = paste0("input['", ns("mc_method"), "'] && ",
                                 "input['", ns("mc_method"), "'].indexOf('dunnett') >= 0"),
              uiOutput(ns("dunnett_controls_ui"))
            ),
            conditionalPanel(
              condition = paste0("input['", ns("mc_method"), "'] && ",
                                 "input['", ns("mc_method"), "'].indexOf('custom') >= 0"),
              h6("Select pairwise comparisons"),
              uiOutput(ns("mc_custom_pairs_ui"))
            ),
            actionButton(ns("run_mc_btn"), "Run Comparisons",
                         class = "btn-primary w-100 mt-2",
                         icon  = icon("calculator"))
          )
        ),
        verbatimTextOutput(ns("model_run_status"))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
mod_models_server <- function(id, rv, role_selectors, shared_reactives,
                              analysis_mode,
                              colour_theme, available_terms) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    syncing_ui <- shiny::reactiveVal(FALSE)
    # Unpack role selectors for convenience
    responses      <- role_selectors$responses
    factors_       <- role_selectors$factors_
    covariates     <- role_selectors$covariates
    blocks         <- role_selectors$blocks
    run_orders     <- role_selectors$run_orders
    all_covariates <- role_selectors$all_covariates
    weights_col    <- role_selectors$weights_col

    # Unpack shared reactives
    treatment       <- shared_reactives$treatment
    treatment_label <- shared_reactives$treatment_label

    # Unpack colour theme
    default_col       <- colour_theme$default_col
    cat_palette       <- colour_theme$cat_palette
    cat_scale_colour  <- colour_theme$cat_scale_colour
    cat_scale_fill    <- colour_theme$cat_scale_fill

    # ── Local helpers ────────────────────────────────────────────────────

    # Build term chooser buttons (namespaced for this module)
    build_term_buttons_ns <- function(terms, target, btn_class) {
      btns <- list()
      groups <- list(
        "Factors" = terms$factors,
        "2FI" = terms$fac_2fi,
        "3FI" = terms$fac_3fi,
        "Quadratic" = terms$quadratic,
        "Cubic" = terms$cubic,
        "Blocks" = terms$blocks,
        "Covariates" = terms$covariates,
        "Blk\u00d7Fac" = terms$blk_fac,
        "Cov\u00d7Fac" = terms$cov_fac
      )
      # Use namespaced input ID for Shiny.setInputValue
      ns_target <- ns(paste0(target, "_term_click"))
      for (grp_name in names(groups)) {
        grp_terms <- groups[[grp_name]]
        if (is.null(grp_terms) || length(grp_terms) == 0) next
        for (t in grp_terms) {
          js_call <- sprintf(
            "Shiny.setInputValue('%s', {term: '%s', nonce: Math.random()});",
            ns_target, gsub("'", "\\\\'", t)
          )
          btns <- c(btns, list(
            tags$button(
              type = "button",
              class = paste("btn", btn_class, "btn-sm"),
              style = "margin: 1px; padding: 1px 5px; font-size: 11px;",
              onclick = js_call,
              t
            )
          ))
        }
        btns <- c(btns, list(tags$span(" ", style = "margin-right: 4px;")))
      }
      div(style = "max-height: 80px; overflow-y: auto;", do.call(tagList, btns))
    }

    # ── Read-only guard ─────────────────────────────────────────────────
    observe({
      locked <- isTRUE(rv$read_only)
      toggle <- if (locked) shinyjs::disable else shinyjs::enable
      toggle(ns("generate_formulas"))
      toggle(ns("add_custom"))
      toggle(ns("custom_formula"))
      toggle(ns("run_models"))
      toggle(ns("prune_btn"))
      toggle(ns("run_mc_btn"))
      toggle(ns("mc_on"))
      toggle(ns("max_way"))
      toggle(ns("poly_degree"))
    })

    # ── Sync MC inputs to rv ─────────────────────────────────────────────
    # Write config then invalidate stale MC results via action function.
    observeEvent(input$mc_on, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      rv$mc_on <- input$mc_on
      apply_mc_config_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$mc_alpha_sidebar, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      rv$mc_alpha <- input$mc_alpha_sidebar
      apply_mc_config_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$mc_terms, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      rv$mc_terms <- input$mc_terms
      apply_mc_config_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$mc_method, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      rv$mc_methods <- input$mc_method
      apply_mc_config_change(rv)
    }, ignoreInit = TRUE)

    # ── Sync builder inputs to canonical rv state ──────────────────────────
    # All writes go through setter helpers (app_state.R) for validation/clamping.
    observeEvent(input$active_response, {
      if (isTRUE(syncing_ui())) return()
      new_val <- input$active_response
      if (identical(new_val, rv$model_active_response)) return()
      set_model_active_response(rv, new_val)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$custom_formula, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      set_model_custom_formula(rv, input$custom_formula)
    }, ignoreInit = TRUE)
    observeEvent(input$max_way, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      new_val <- as.integer(input$max_way %||% MAX_WAY_DEFAULT)
      if (identical(new_val, as.integer(rv$model_max_way %||% MAX_WAY_DEFAULT))) return()
      set_model_max_way(rv, new_val)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$poly_degree, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      new_val <- as.integer(input$poly_degree %||% POLY_DEGREE_DEFAULT)
      if (identical(new_val, as.integer(rv$model_poly_degree %||% POLY_DEGREE_DEFAULT))) return()
      set_model_poly_degree(rv, new_val)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$include_covariates, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      new_val <- isTRUE(input$include_covariates)
      if (identical(new_val, isTRUE(rv$model_include_covariates))) return()
      set_model_include_covariates(rv, new_val)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$formula_covariates, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      new_val <- input$formula_covariates %||% character(0)
      if (identical(sort(new_val), sort(rv$model_formula_covariates %||% character(0)))) return()
      set_model_formula_covariates(rv, new_val)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$max_covariates_per_formula, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      new_val <- as.integer(input$max_covariates_per_formula %||% 1L)
      if (identical(new_val, as.integer(rv$model_max_covariates_per_formula %||% 1L))) return()
      set_model_max_covariates_per_formula(rv, new_val)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$include_cov_fac, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      if (identical(isTRUE(input$include_cov_fac), isTRUE(rv$model_include_cov_fac))) return()
      set_model_include_cov_fac(rv, input$include_cov_fac)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$include_blocks, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      if (identical(isTRUE(input$include_blocks), isTRUE(rv$model_include_blocks))) return()
      set_model_include_blocks(rv, input$include_blocks)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$include_block_fac, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      if (identical(isTRUE(input$include_block_fac), isTRUE(rv$model_include_block_fac))) return()
      set_model_include_block_fac(rv, input$include_block_fac)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)
    observeEvent(input$append_formulas, {
      if (isTRUE(syncing_ui())) return()
      if (isTRUE(rv$read_only)) return()
      if (identical(isTRUE(input$append_formulas), isTRUE(rv$model_append_formulas))) return()
      set_model_append_formulas(rv, input$append_formulas)
      apply_model_spec_change(rv)
    }, ignoreInit = TRUE)

    # ── Active response selector ─────────────────────────────────────────
    # When the list of response columns changes, update choices and preserve
    # the canonical selection (rv$model_active_response) if still valid.
    # If invalid or NULL, fall back to first available and write back to rv
    # so canonical state and UI always agree.
    observe({
      resps <- responses()
      sel   <- rv$model_active_response
      if (is.null(sel) || !(sel %in% resps)) {
        sel <- if (length(resps) > 0) resps[1] else NULL
        rv$model_active_response <- sel
      }
      updateSelectInput(session, "active_response", choices = resps,
                        selected = sel %||% "")
    })

    # ── Weight column selector ─────────────────────────────────────────
    observe({
      wc <- weights_col()
      updateSelectInput(session, "weight_column",
                        choices = c("(none)" = "", wc),
                        selected = input$weight_column %||% "")
    })

    # ── Dynamic covariate selector for formula generator ─────────────────
    output$formula_cov_selector_ui <- renderUI({
      covs <- all_covariates()
      if (length(covs) == 0) {
        return(checkboxInput(ns("include_covariates"), "Include covariates", value = FALSE))
      }
      # Use canonical rv state for defaults; fall back to sensible startup values
      inc_cov <- isTRUE(rv$model_include_covariates)
      sel_covs <- rv$model_formula_covariates
      if (length(sel_covs) == 0) sel_covs <- covs
      sel_covs <- intersect(sel_covs, covs)  # only valid columns
      if (length(sel_covs) == 0) sel_covs <- covs
      max_cov <- rv$model_max_covariates_per_formula %||% 1L
      tagList(
        checkboxInput(ns("include_covariates"), "Include covariates", value = inc_cov),
        conditionalPanel(
          condition = paste0("input['", ns("include_covariates"), "'] == true"),
          checkboxGroupInput(ns("formula_covariates"), "Covariates to include",
                             choices = covs, selected = sel_covs, inline = TRUE),
          numericInput(ns("max_covariates_per_formula"), "Max covariates per formula",
                       value = max_cov, min = 1, max = max(1, length(covs)), step = 1)
        )
      )
    })

    # ── Editable formula list ────────────────────────────────────────────
    # Uses rv$formula_gen counter in IDs to avoid stale inputs after regeneration
    output$formula_list_ui <- renderUI({
      if (length(rv$formulas) == 0) return(p("No formulas generated yet.", class = "text-muted"))
      gen <- rv$formula_gen
      aliases <- rv$formula_aliases
      labels  <- rv$alias_labels
      inest   <- rv$inestimable_terms
      tagList(lapply(seq_along(rv$formulas), function(i) {
        f <- rv$formulas[[i]]
        alias_warning <- NULL
        if (f %in% names(aliases) && nrow(aliases[[f]]) > 0) {
          af <- aliases[[f]]
          pairs <- paste0(af$Term_1, " \u2194 ", af$Term_2,
                          " (r=", round(af$Correlation, 2), ")")
          alias_warning <- div(class = "alias-warning",
            icon("exclamation-triangle"),
            " Aliased: ", paste(pairs, collapse = "; ")
          )
        }
        # Show aliased terms: one line per aliased group with warning icon
        alias_info <- NULL
        if (length(labels) > 0) {
          parsed <- tryCatch(as.formula(f), error = function(e) NULL)
          if (!is.null(parsed)) {
            term_labels <- attr(terms(parsed), "term.labels")
            aliased_terms <- intersect(term_labels, names(labels))
            if (length(aliased_terms) > 0) {
              lines <- lapply(aliased_terms, function(t) {
                # Show the confounded alias label (e.g. "A:B" represents "A:B + C:D")
                alias_lbl <- labels[[t]]
                tags$div(style = "font-size: 12px; color: #856404;",
                  icon("exclamation-triangle", style = "color: #e6a817; font-size: 11px;"),
                  tags$span(style = "color: #856404;",
                    paste0(" Term '", t, "' represents ")),
                  tags$code(style = "font-size: 11px; font-weight: bold;", alias_lbl),
                  tags$span(style = "color: #856404;", " (confounded)")
                )
              })
              alias_info <- div(style = "margin: -6px 0 4px 48px;",
                do.call(tagList, lines)
              )
            }
          }
        }
        # Show inestimable terms: stop icon per term
        inest_info <- NULL
        if (length(inest) > 0) {
          parsed <- tryCatch(as.formula(f), error = function(e) NULL)
          if (!is.null(parsed)) {
            term_labels <- attr(terms(parsed), "term.labels")
            inest_in_f <- intersect(term_labels, inest)
            if (length(inest_in_f) > 0) {
              lines <- lapply(inest_in_f, function(t) {
                tags$div(style = "font-size: 12px; color: #dc3545;",
                  icon("ban", style = "color: #dc3545; font-size: 11px;"),
                  tags$span(style = "color: #dc3545;", " not estimable: "),
                  tags$code(style = "font-size: 11px;", t)
                )
              })
              inest_info <- div(style = "margin: -6px 0 4px 48px;",
                do.call(tagList, lines)
              )
            }
          }
        }
        div(
          fluidRow(
            column(1,
              checkboxInput(ns(paste0("fsel_", gen, "_", i)), NULL, value = TRUE)
            ),
            column(11,
              textInput(ns(paste0("fedit_", gen, "_", i)), NULL, value = f)
            )
          ),
          alias_info,
          inest_info,
          alias_warning
        )
      }))
    })

    # ── Apply model defaults from design metadata ──────────────────────
    observeEvent(rv$design_metadata, {
      md <- rv$design_metadata$model_defaults
      if (!is.null(md)) {
        if (!is.null(md$max_way)) updateNumericInput(session, "max_way", value = md$max_way)
        if (!is.null(md$poly_degree)) updateNumericInput(session, "poly_degree", value = md$poly_degree)
      }
    }, ignoreInit = TRUE)

    # ── Design-Aware Formula Limits ──────────────────────────────────────
    # Constrain max_way and poly_degree based on data and design rank
    observe({
      req(rv$data, length(factors_()) > 0)
      facs <- factors_()
      n_facs <- length(facs)

      # Max interaction order: capped by number of factors
      max_way_limit <- min(n_facs, 5L)
      cur_way <- isolate(input$max_way) %||% 2L
      if (cur_way > max_way_limit)
        updateNumericInput(session, "max_way", value = max_way_limit, max = max_way_limit)
      else
        updateNumericInput(session, "max_way", max = max_way_limit)

      # Polynomial degree: max is min(unique_levels - 1) across numeric factors
      mode <- analysis_mode() %||% "comparative"
      if (mode == "regression") {
        max_polys <- vapply(facs, function(cn) {
          length(unique(rv$data[[cn]])) - 1L
        }, integer(1))
        max_poly_limit <- max(1L, min(max_polys, na.rm = TRUE))
        max_poly_limit <- min(max_poly_limit, 5L)
        cur_poly <- isolate(input$poly_degree) %||% 2L
        if (cur_poly > max_poly_limit)
          updateNumericInput(session, "poly_degree", value = max_poly_limit, max = max_poly_limit)
        else
          updateNumericInput(session, "poly_degree", max = max_poly_limit)
      }
    })

    # ── Formula generation logic (shared by auto-observer & button) ─────
    generate_formulas <- function() {
      response <- rv$model_active_response %||% input$active_response
      req(response, rv$data)

      mode <- analysis_mode() %||% "comparative"
      max_way <- rv$model_max_way %||% MAX_WAY_DEFAULT

      # Filter covariates to user-selected subset
      sel_covs <- if (isTRUE(rv$model_include_covariates)) {
        fc <- rv$model_formula_covariates
        if (length(fc) == 0) all_covariates() else fc
      } else {
        character(0)
      }
      max_cov <- rv$model_max_covariates_per_formula %||% 1L

      formulas <- if (mode == "regression") {
        req(length(factors_()) > 0 || length(all_covariates()) > 0)
        build_regression_formulas(
          response          = response,
          factors           = factors_(),
          covariates        = sel_covs,
          blocks            = blocks(),
          max_way           = max_way,
          poly_degree       = rv$model_poly_degree %||% POLY_DEGREE_DEFAULT,
          include_covariates = length(sel_covs) > 0,
          include_cov_fac    = isTRUE(rv$model_include_cov_fac),
          include_blocks     = isTRUE(rv$model_include_blocks),
          include_block_fac  = isTRUE(rv$model_include_block_fac),
          max_covariates     = max_cov
        )
      } else {
        req(length(factors_()) > 0)
        build_formulas(
          response          = response,
          factors           = factors_(),
          covariates        = sel_covs,
          blocks            = blocks(),
          max_way           = max_way,
          include_covariates = length(sel_covs) > 0,
          include_blocks     = isTRUE(rv$model_include_blocks),
          include_block_fac  = isTRUE(rv$model_include_block_fac),
          max_covariates     = max_cov
        )
      }
      # Append mode: merge with existing formulas (skip duplicates)
      if (isTRUE(rv$model_append_formulas) && length(rv$formulas) > 0) {
        existing <- rv$formulas
        new_only <- setdiff(formulas, existing)
        if (length(new_only) == 0) {
          showNotification("No new formulas to add.", type = "message", duration = 3)
          return()
        }
        formulas <- c(existing, new_only)
      }

      rv$pending_alias_resolution <- NULL

      # Compute aliases for each formula
      aliases <- list()
      for (f in formulas) {
        af <- tryCatch(detect_formula_aliases(rv$data, f), error = function(e) data.frame())
        if (nrow(af) > 0) aliases[[f]] <- af
      }

      # Collapse aliased terms: remove redundant terms, build labels
      computed_labels <- list()
      if (length(aliases) > 0) {
        collapsed <- collapse_aliased_formulas(formulas, aliases)
        formulas <- collapsed$formulas
        computed_labels <- collapsed$alias_labels
        # Re-detect aliases on collapsed formulas (should be clean now)
        aliases <- list()
        for (f in formulas) {
          af <- tryCatch(detect_formula_aliases(rv$data, f), error = function(e) data.frame())
          if (nrow(af) > 0) aliases[[f]] <- af
        }
      }

      # Detect inestimable terms across all formulas
      all_inest <- character()
      for (f in formulas) {
        inest <- detect_inestimable_terms(f, rv$data)
        all_inest <- union(all_inest, inest)
      }

      # Atomic write: formulas + aliases + labels + inestimable + gen counter
      apply_generated_formulas(rv, formulas,
                               aliases = aliases,
                               alias_labels = computed_labels,
                               inestimable = all_inest)
    }

    # ── Reactive formula generation ──────────────────────────────────────
    # Auto-updates when any input changes.
    # IMPORTANT: all formula-builder inputs are read BEFORE the skip check
    # so they register as reactive dependencies even on the first (skipped) run.
    observe({
      req(rv$model_active_response, rv$data)

      # Touch canonical model-spec state to register dependencies
      analysis_mode()
      rv$model_active_response
      rv$model_max_way
      rv$model_include_covariates
      rv$model_formula_covariates
      rv$model_max_covariates_per_formula
      rv$model_poly_degree
      rv$model_include_cov_fac
      rv$model_include_blocks
      rv$model_include_block_fac
      rv$model_append_formulas
      factors_()
      blocks()
      all_covariates()

      # Skip if example loader already set formulas (avoids conflict/overwrite)
      if (isTRUE(isolate(rv$skip_auto_formula))) {
        rv$skip_auto_formula <- FALSE
        return()
      }

      generate_formulas()
    })

    # ── Manual formula generation (button) ───────────────────────────────
    observeEvent(input$generate_formulas, {
      if (is_locked(rv, "Formula generation")) return()
      showNotification("Generating formulas...", type = "message", duration = 2)
      rv$skip_auto_formula <- FALSE
      generate_formulas()
      showNotification(paste(length(rv$formulas), "formulas generated"), type = "message")
    }, ignoreInit = TRUE)

    # ── Collect selected & edited formulas ───────────────────────────────
    get_selected_formulas <- reactive({
      if (length(rv$formulas) == 0) return(character(0))
      gen <- rv$formula_gen
      sel <- character(0)
      for (i in seq_along(rv$formulas)) {
        checked <- input[[paste0("fsel_", gen, "_", i)]]
        edited  <- input[[paste0("fedit_", gen, "_", i)]]
        if (isTRUE(checked) && !is.null(edited) && nchar(trimws(edited)) > 0) {
          f <- trimws(edited)
          sel[f] <- f
        }
      }
      sel
    })

    # ── Select / deselect all formulas ───────────────────────────────────
    observeEvent(input$select_all_formulas, {
      gen <- rv$formula_gen
      for (i in seq_along(rv$formulas))
        updateCheckboxInput(session, paste0("fsel_", gen, "_", i), value = TRUE)
    })
    observeEvent(input$deselect_all_formulas, {
      gen <- rv$formula_gen
      for (i in seq_along(rv$formulas))
        updateCheckboxInput(session, paste0("fsel_", gen, "_", i), value = FALSE)
    })

    # ── Add custom formula ───────────────────────────────────────────────
    observeEvent(input$add_custom, {
      if (is_locked(rv, "Custom formula")) return()
      req(input$custom_formula, nchar(trimws(input$custom_formula)) > 0)
      f <- trimws(input$custom_formula)
      rv$formula_gen <- isolate(rv$formula_gen) + 1L
      rv$formulas <- c(rv$formulas, setNames(f, f))
      updateTextAreaInput(session, "custom_formula", value = "")

      # Check aliases for the new formula
      af <- tryCatch(detect_formula_aliases(rv$data, f), error = function(e) data.frame())
      if (nrow(af) > 0) rv$formula_aliases[[f]] <- af
    })

    # ── Custom formula term chooser buttons ──────────────────────────────
    output$formula_custom_chooser <- renderUI({
      terms <- available_terms()
      if (length(unlist(terms)) == 0) return(NULL)
      build_term_buttons_ns(terms, "custom", "btn-outline-secondary")
    })

    # Observer: when any custom term button is clicked, append to custom formula
    observeEvent(input$custom_term_click, {
      if (isTRUE(rv$read_only)) return()
      term <- input$custom_term_click$term
      if (is.null(term)) return()
      current <- input$custom_formula %||% ""
      stripped <- trimws(current)
      if (nchar(stripped) == 0) {
        resp <- input$active_response %||% "Y"
        updateTextAreaInput(session, "custom_formula", value = paste0(resp, " ~ ", term))
      } else {
        if (grepl("~", stripped)) {
          rhs <- trimws(sub("^[^~]+~", "", stripped))
          existing <- trimws(strsplit(rhs, "\\+")[[1]])
          if (!term %in% existing) {
            updateTextAreaInput(session, "custom_formula",
                            value = paste0(stripped, " + ", term))
          }
        } else {
          updateTextAreaInput(session, "custom_formula",
                          value = paste0(stripped, " + ", term))
        }
      }
    })

    # Copy from existing formula dropdown
    observeEvent(input$copy_from_formula, {
      if (isTRUE(rv$read_only)) return()
      f <- input$copy_from_formula
      if (is.null(f) || nchar(trimws(f)) == 0) return()
      updateTextAreaInput(session, "custom_formula", value = f)
    })

    # Populate "copy from" dropdown when formulas change
    observe({
      choices <- c("(blank)" = "")
      if (!is.null(rv$formulas) && length(rv$formulas) > 0) {
        fc <- rv$formulas
        names(fc) <- paste0("Formula ", seq_along(fc))
        choices <- c(choices, fc)
      }
      updateSelectInput(session, "copy_from_formula", choices = choices)
    })

    # Staged term addition: + Main Effects
    observeEvent(input$add_main_effects, {
      if (isTRUE(rv$read_only)) return()
      terms <- available_terms()
      main <- c(terms$factors, terms$covariates, terms$blocks)
      if (length(main) == 0) return()
      current <- trimws(input$custom_formula %||% "")
      resp <- input$active_response %||% "Y"
      if (nchar(current) == 0) {
        updateTextAreaInput(session, "custom_formula",
                            value = paste0(resp, " ~ ", paste(main, collapse = " + ")))
      } else {
        existing <- if (grepl("~", current)) {
          trimws(strsplit(trimws(sub("^[^~]+~", "", current)), "\\+")[[1]])
        } else character()
        new_terms <- setdiff(main, existing)
        if (length(new_terms) > 0) {
          sep <- if (grepl("~\\s*$", current)) "" else " + "
          updateTextAreaInput(session, "custom_formula",
                              value = paste0(current, sep, paste(new_terms, collapse = " + ")))
        }
      }
    })

    # Staged term addition: + 2-way interactions
    observeEvent(input$add_2way, {
      if (isTRUE(rv$read_only)) return()
      terms <- available_terms()
      main <- c(terms$factors, terms$covariates, terms$blocks)
      if (length(main) < 2) return()
      twoway <- combn(main, 2, paste, collapse = ":")
      current <- trimws(input$custom_formula %||% "")
      resp <- input$active_response %||% "Y"
      if (nchar(current) == 0) {
        updateTextAreaInput(session, "custom_formula",
                            value = paste0(resp, " ~ ", paste(twoway, collapse = " + ")))
      } else {
        existing <- if (grepl("~", current)) {
          trimws(strsplit(trimws(sub("^[^~]+~", "", current)), "\\+")[[1]])
        } else character()
        new_terms <- setdiff(twoway, existing)
        if (length(new_terms) > 0) {
          sep <- if (grepl("~\\s*$", current)) "" else " + "
          updateTextAreaInput(session, "custom_formula",
                              value = paste0(current, sep, paste(new_terms, collapse = " + ")))
        }
      }
    })

    # Staged term addition: + 3-way interactions
    observeEvent(input$add_3way, {
      if (isTRUE(rv$read_only)) return()
      terms <- available_terms()
      main <- c(terms$factors, terms$covariates, terms$blocks)
      if (length(main) < 3) return()
      threeway <- combn(main, 3, paste, collapse = ":")
      current <- trimws(input$custom_formula %||% "")
      resp <- input$active_response %||% "Y"
      if (nchar(current) == 0) {
        updateTextAreaInput(session, "custom_formula",
                            value = paste0(resp, " ~ ", paste(threeway, collapse = " + ")))
      } else {
        existing <- if (grepl("~", current)) {
          trimws(strsplit(trimws(sub("^[^~]+~", "", current)), "\\+")[[1]])
        } else character()
        new_terms <- setdiff(threeway, existing)
        if (length(new_terms) > 0) {
          sep <- if (grepl("~\\s*$", current)) "" else " + "
          updateTextAreaInput(session, "custom_formula",
                              value = paste0(current, sep, paste(new_terms, collapse = " + ")))
        }
      }
    })

    # ── Model fitting logic ──────────────────────────────────────────────
    # Extracted function: called directly or after alias resolution
    # Also called by other modules (e.g. data upload) via returned reference
    do_fit_models <- function(formulas_to_run) {
      withProgress(message = "Fitting models...", value = 0, {
        wt_col <- input$weight_column
        if (is.null(wt_col) || wt_col == "") wt_col <- NULL
        result <- fit_models(formulas_to_run, rv$data,
                             col_types = rv$col_types,
                             transforms = rv$transforms,
                             coding_values = rv$coding_values,
                             weight_column = wt_col,
                             level_labels = rv$level_labels)
        fitted_notes <- check_model_notes(result$models)
        incProgress(0.4)

        # Compute VIF before writing to rv
        computed_vif <- vif_summary(result$models)
        if (nrow(computed_vif) > 0) {
          model_cols <- setdiff(names(computed_vif), "Term")
          vif_vals <- computed_vif[, model_cols, drop = FALSE]
          high_mask <- apply(vif_vals, c(1,2), function(x) !is.na(x) && x > sqrt(5))
          if (any(high_mask)) {
            terms_flagged <- computed_vif$Term[apply(high_mask, 1, any)]
            showNotification(
              paste0("Collinearity detected (VIF): ",
                     paste(terms_flagged, collapse = ", "),
                     ". Check the Collinearity tab in Results."),
              type = "warning", duration = 10)
          }
        }

        incProgress(0.3)

        # Atomic write: models + errors + notes + VIF; clears stale MC results
        apply_fitted_models(rv,
                            models = result$models,
                            errors = result$errors,
                            notes  = fitted_notes,
                            vif_df = computed_vif)

        # Update MC term choices (local module input)
        all_terms_raw <- unique(unlist(lapply(rv$models, function(m) {
          a <- tryCatch(model_anova(m, type=3), error=function(e) NULL)
          if (!is.null(a)) rownames(a)[rownames(a) != "(Intercept)"] else character(0)
        })))
        factor_cols  <- c(factors_(), blocks())
        blk_cols     <- blocks()
        cat_factor_terms <- all_terms_raw[sapply(all_terms_raw, function(t) {
          parts <- strsplit(t, ":")[[1]]
          all(parts %in% factor_cols) &&
            all(sapply(parts, function(cn) {
              # Blocks always qualify for MC (they are categorical by nature)
              if (cn %in% blk_cols) return(TRUE)
              (rv$col_types[[cn]] %||% "Factor") == "Factor"
            }))
        })]
        # Sort: highest-order interactions first (treatment A:B before A, B)
        n_parts <- sapply(cat_factor_terms, function(t) length(strsplit(t, ":")[[1]]))
        cat_factor_terms <- cat_factor_terms[order(-n_parts, cat_factor_terms)]
        updateCheckboxGroupInput(session, "mc_terms",
                                 choices = cat_factor_terms, selected = cat_factor_terms)

        incProgress(0.3)
      })
    }

    # ── Run models button ────────────────────────────────────────────────
    observeEvent(input$run_models, {
      if (is_locked(rv, "Model fitting")) return()
      req(rv$data)
      formulas_to_run <- get_selected_formulas()
      req(length(formulas_to_run) > 0)

      # Re-detect aliases on final (possibly edited) formulas
      aliases_found <- list()
      for (f in formulas_to_run) {
        af <- tryCatch(detect_formula_aliases(rv$data, f), error = function(e) data.frame())
        if (nrow(af) > 0) aliases_found[[f]] <- af
      }

      if (length(aliases_found) > 0) {
        rv$pending_alias_resolution <- list(
          formulas = formulas_to_run,
          aliases  = aliases_found
        )
        show_alias_modal(aliases_found)
        return()
      }

      do_fit_models(formulas_to_run)
    })

    # ── Alias Resolution Modal ───────────────────────────────────────────
    show_alias_modal <- function(aliases_found) {
      # Build UI for each formula with aliases
      pair_idx <- 0L
      modal_items <- lapply(names(aliases_found), function(f) {
        af <- aliases_found[[f]]
        pair_rows <- lapply(seq_len(nrow(af)), function(j) {
          pair_idx <<- pair_idx + 1L
          t1 <- af$Term_1[j]; t2 <- af$Term_2[j]; corr <- af$Correlation[j]
          sign_sym <- if (corr >= 0) "+" else "-"
          relabel_text <- paste0(t1, " ", sign_sym, " ", t2)
          relabel_label <- paste0("Retain & relabel as '", relabel_text, "'")
          choice_vec <- c("remove", "relabel")
          names(choice_vec) <- c("Remove both terms", relabel_label)
          div(class = "alias-pair-row",
            tags$strong(paste0(t1, " \u2194 ", t2, " (r=", round(corr, 2), ")")),
            radioButtons(
              inputId  = ns(paste0("alias_choice_", pair_idx)),
              label    = NULL,
              choices  = choice_vec,
              selected = "relabel",
              inline   = TRUE
            )
          )
        })
        tagList(
          tags$hr(),
          tags$p(tags$code(f), style = "font-size: 0.85em;"),
          pair_rows
        )
      })

      showModal(modalDialog(
        title = "Resolve Aliased Terms",
        size  = "l",
        tagList(
          p("The following formulas contain aliased (perfectly confounded) term pairs.",
            "Choose how to handle each pair before fitting models."),
          modal_items
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("alias_resolve_confirm"), "Apply & Fit Models",
                       class = "btn-primary")
        )
      ))
    }

    observeEvent(input$alias_resolve_confirm, {
      if (is_locked(rv, "Alias resolution")) return()
      req(rv$pending_alias_resolution)
      pending  <- rv$pending_alias_resolution
      formulas <- pending$formulas
      aliases  <- pending$aliases
      new_labels <- rv$alias_labels

      # Build a mapping from old formula -> new formula
      rewrite_map <- list()  # old_f -> new_f

      pair_idx <- 0L
      for (f in names(aliases)) {
        af <- aliases[[f]]
        terms_to_remove <- character()
        for (j in seq_len(nrow(af))) {
          pair_idx <- pair_idx + 1L
          choice <- input[[paste0("alias_choice_", pair_idx)]] %||% "relabel"
          t1 <- af$Term_1[j]; t2 <- af$Term_2[j]; corr <- af$Correlation[j]

          if (choice == "remove") {
            terms_to_remove <- c(terms_to_remove, t1, t2)
          } else {
            # Relabel: remove t2, keep t1, relabel t1
            terms_to_remove <- c(terms_to_remove, t2)
            sign_sym <- if (corr >= 0) "+" else "-"
            new_labels[[t1]] <- paste0(t1, " ", sign_sym, " ", t2)
          }
        }

        # Rewrite formula: remove specified terms
        if (length(terms_to_remove) > 0) {
          parsed <- tryCatch(as.formula(f), error = function(e) NULL)
          if (!is.null(parsed)) {
            tt <- terms(parsed)
            term_labels <- attr(tt, "term.labels")
            keep <- setdiff(term_labels, unique(terms_to_remove))
            resp <- all.vars(parsed)[1]
            if (length(keep) > 0) {
              new_f <- paste0(resp, " ~ ", paste(keep, collapse = " + "))
            } else {
              new_f <- paste0(resp, " ~ 1")
            }
            rewrite_map[[f]] <- new_f
          }
        }
      }

      # Build resolved formula vector (avoids in-place mutation issues)
      resolved <- character()
      for (i in seq_along(formulas)) {
        old_f <- formulas[[i]]
        if (old_f %in% names(rewrite_map)) {
          new_f <- rewrite_map[[old_f]]
          resolved[new_f] <- new_f
        } else {
          resolved[old_f] <- old_f
        }
      }

      rv$alias_labels <- new_labels
      rv$pending_alias_resolution <- NULL
      removeModal()
      do_fit_models(resolved)
    })

    # ── Backward Elimination ─────────────────────────────────────────────
    observeEvent(input$prune_btn, {
      if (is_locked(rv, "Model pruning")) return()
      req(length(rv$models) > 0)
      alpha <- input$prune_alpha %||% 0.05

      # Prune only checked/selected models, not all fitted models
      selected <- get_selected_formulas()
      models_to_prune <- names(rv$models)[names(rv$models) %in% selected]
      if (length(models_to_prune) == 0) {
        showNotification("No models selected. Check the formulas you want to prune.", type = "warning")
        return()
      }

      pruned <- list()
      dup_notes <- character()
      for (mname in models_to_prune) {
        m <- rv$models[[mname]]
        pruned_m <- tryCatch(
          backward_eliminate(m, alpha = alpha),
          error = function(e) { message("Prune error: ", e$message); NULL }
        )
        if (!is.null(pruned_m)) {
          new_label <- deparse(formula(pruned_m))
          if (new_label %in% names(rv$models)) {
            # Pruned model already exists -- add annotation
            note <- paste0("Pruned from ", mname, " at \u03b1 = ", alpha)
            existing <- rv$prune_notes[[new_label]]
            rv$prune_notes[[new_label]] <- if (is.null(existing)) note
                                           else paste(existing, note, sep = "; ")
            dup_notes <- c(dup_notes, paste0(short_label(new_label, 50),
                                             " (already in list, pruned from ",
                                             short_label(mname, 30), ")"))
          } else if (!new_label %in% names(pruned)) {
            pruned[[new_label]] <- pruned_m
            rv$prune_notes[[new_label]] <- paste0("Pruned from ", mname,
                                                   " at \u03b1 = ", alpha)
          }
        }
      }

      if (length(pruned) > 0) {
        rv$models <- c(rv$models, pruned)
        for (f in names(pruned)) {
          rv$formulas <- c(rv$formulas, setNames(f, f))
        }
        rv$vif_df <- vif_summary(rv$models)
      }

      msgs <- character()
      if (length(pruned) > 0)
        msgs <- c(msgs, paste0("Added ", length(pruned), " pruned model(s)."))
      if (length(dup_notes) > 0)
        msgs <- c(msgs, paste0("Already in list: ", paste(dup_notes, collapse = "; ")))
      if (length(msgs) == 0)
        msgs <- "No terms removed \u2014 all p-values already \u2264 alpha."

      showNotification(paste(msgs, collapse = " "), type = "message", duration = 8)
    })

    # ── Multiple Comparisons ─────────────────────────────────────────────

    # Dynamic UI: custom pair selection based on selected terms
    # Uses emmeans pairwise contrast labels directly to ensure exact match with run_mc()
    output$mc_custom_pairs_ui <- renderUI({
      req(rv$data, length(input$mc_terms) > 0, length(rv$models) > 0)
      pair_choices <- character()

      # Use first model for computing contrasts and t-statistics
      m <- rv$models[[1]]

      for (spec in input$mc_terms) {
        spec_parts <- strsplit(spec, ":")[[1]]
        all_in_data <- all(spec_parts %in% names(rv$data))
        if (!all_in_data) next

        # Get actual emmeans pairwise contrast labels and t-statistics
        pw_info <- tryCatch({
          em <- suppressMessages(emmeans::emmeans(m, specs = spec_parts))
          pw <- as.data.frame(summary(emmeans::contrast(em, method = "pairwise", adjust = "none")))
          contr_col <- if ("contrast" %in% names(pw)) "contrast" else names(pw)[1]
          t_col <- if ("t.ratio" %in% names(pw)) "t.ratio" else NULL
          list(labels = as.character(pw[[contr_col]]),
               t_vals = if (!is.null(t_col)) pw[[t_col]] else NULL)
        }, error = function(e) NULL)

        if (is.null(pw_info) || length(pw_info$labels) == 0) next

        for (i in seq_along(pw_info$labels)) {
          label <- pw_info$labels[i]
          display <- paste0(spec, ": ", label)
          pair_choices <- c(pair_choices, setNames(label, display))
        }
      }
      if (length(pair_choices) == 0)
        return(p(class = "text-muted small", "Select factor terms above first."))
      checkboxGroupInput(ns("mc_custom_pairs"), NULL,
                         choices = pair_choices, selected = pair_choices)
    })

    # MC results computation helper
    run_mc_results <- function() {
      req(length(rv$models) > 0, length(input$mc_terms) > 0, length(input$mc_method) > 0)
      results <- list()
      custom_pairs <- input$mc_custom_pairs
      alpha <- input$mc_alpha_sidebar %||% 0.05
      for (mname in names(rv$models)) {
        model <- rv$models[[mname]]
        for (spec in input$mc_terms) {
          # Get per-term Dunnett control
          ctrl_id <- paste0("dunnett_ctrl_", gsub(":", "_", spec))
          ctrl_val <- input[[ctrl_id]]
          for (method in input$mc_method) {
            key <- paste(mname, spec, method, sep = "__")
            results[[key]] <- run_mc(model, spec, method,
                                     control        = ctrl_val,
                                     selected_pairs = custom_pairs,
                                     alpha          = alpha)
            # Add model column
            if (nrow(results[[key]]) > 0) results[[key]]$model <- mname
          }
        }
      }
      rv$mc_results <- results
    }

    # MC: show message and disable button when no categorical factor terms
    output$mc_no_terms_msg <- renderUI({
      mc_choices <- input$mc_terms
      # Check if there are any categorical factor terms available
      cat_factors <- names(Filter(function(r) r == "Factor", rv$roles))
      cat_factors <- cat_factors[sapply(cat_factors, function(cn) {
        (rv$col_types[[cn]] %||% "Factor") == "Factor"
      })]
      if (length(cat_factors) == 0) {
        shinyjs::disable(ns("run_mc_btn"))
        div(class = "alert alert-info p-2 mb-2",
            icon("info-circle"),
            "No categorical factor terms available for multiple comparisons. ",
            "Multiple comparisons require factors with Type = Factor (not Numeric). ",
            "Switch to Comparative mode or change factor types in Assign Roles.")
      } else {
        shinyjs::enable(ns("run_mc_btn"))
        NULL
      }
    })

    observeEvent(input$run_mc_btn, {
      if (is_locked(rv, "Multiple comparisons")) return()
      req(isTRUE(input$mc_on))
      withProgress(message = "Running comparisons...", value = 0.5, {
        run_mc_results()
      })
      showNotification("Multiple comparisons complete.", type = "message")
    })

    # Auto-recompute MC when models change (e.g. after outlier removal/refit)
    observeEvent(rv$models, {
      if (!isTRUE(input$mc_on)) return()
      if (length(rv$mc_results) == 0) return()  # Only rerun if MC was previously computed
      if (length(input$mc_terms) == 0 || length(input$mc_method) == 0) return()
      tryCatch({
        run_mc_results()
        showNotification("MC results updated (models changed).", type = "message", duration = 3)
      }, error = function(e) NULL)
    }, ignoreInit = TRUE)

    # Dynamic Dunnett control: one dropdown per selected MC term
    output$dunnett_controls_ui <- renderUI({
      terms <- input$mc_terms
      req(rv$data, length(terms) > 0)
      inputs <- lapply(terms, function(spec) {
        spec_parts <- strsplit(spec, ":")[[1]]
        all_in_data <- all(spec_parts %in% names(rv$data))
        if (!all_in_data) return(NULL)
        if (length(spec_parts) == 1) {
          lvls <- sort(unique(as.character(rv$data[[spec]])))
        } else {
          combo_df <- unique(rv$data[, spec_parts, drop = FALSE])
          lvls <- sort(unname(apply(combo_df, 1, function(r) paste(trimws(r), collapse = " "))))
        }
        selectInput(ns(paste0("dunnett_ctrl_", gsub(":", "_", spec))),
                    paste0("Control for ", spec), choices = lvls)
      })
      tagList(inputs)
    })

    # ── Model run status ─────────────────────────────────────────────────
    output$model_run_status <- renderText({
      n <- length(rv$models)
      if (n == 0) return("No models fitted yet.")
      lines <- sapply(names(rv$models), function(mname) {
        note <- rv$prune_notes[[mname]]
        if (!is.null(note)) paste0(mname, "  [", note, "]") else mname
      })
      paste(n, "model(s) fitted:", paste(lines, collapse = "\n"))
    })

    # ── Reset module UI to startup defaults ────────────────────────────
    reset_ui <- function() {
      syncing_ui(TRUE)
      on.exit(syncing_ui(FALSE), add = TRUE)
      # Reset model spec to defaults, then sync UI
      clear_model_spec(rv)
      sync_ui_from_rv()
      # Also reset ephemeral controls not in spec
      updateNumericInput(session, "prune_alpha", value = ALPHA_DEFAULT)
      updateSelectInput(session, "model_type", selected = "lm")
      updateSelectInput(session, "weight_column", selected = "")
    }

    # ── Sync module UI to current rv state (e.g. after session load) ────
    sync_ui_from_rv <- function() {
      syncing_ui(TRUE)
      on.exit(syncing_ui(FALSE), add = TRUE)
      # Builder controls — response selector
      # Contract: NULL means "not yet set, use first available response".
      # Resolve NULL/invalid to a concrete value in rv before syncing UI,
      # so canonical state and visible control always agree.
      resps <- responses()
      sel_resp <- rv$model_active_response
      if (is.null(sel_resp) || !(sel_resp %in% resps)) {
        sel_resp <- if (length(resps) > 0) resps[1] else NULL
        rv$model_active_response <- sel_resp
      }
      updateSelectInput(session, "active_response", choices = resps,
                        selected = sel_resp %||% "")
      updateTextAreaInput(session, "custom_formula", value = rv$model_custom_formula %||% "")
      updateNumericInput(session, "max_way", value = rv$model_max_way %||% MAX_WAY_DEFAULT)
      updateNumericInput(session, "poly_degree", value = rv$model_poly_degree %||% POLY_DEGREE_DEFAULT)
      updateCheckboxInput(session, "include_covariates", value = isTRUE(rv$model_include_covariates))
      # Covariate selection + max: only update if covariates exist
      covs <- all_covariates()
      if (length(covs) > 0) {
        sel_covs <- rv$model_formula_covariates %||% character(0)
        sel_covs <- intersect(sel_covs, covs)
        if (length(sel_covs) == 0) sel_covs <- covs
        updateCheckboxGroupInput(session, "formula_covariates",
                                 choices = covs, selected = sel_covs)
        updateNumericInput(session, "max_covariates_per_formula",
                           value = rv$model_max_covariates_per_formula %||% 1L)
      }
      updateCheckboxInput(session, "include_cov_fac", value = isTRUE(rv$model_include_cov_fac))
      updateCheckboxInput(session, "include_blocks", value = isTRUE(rv$model_include_blocks))
      updateCheckboxInput(session, "include_block_fac", value = isTRUE(rv$model_include_block_fac))
      updateCheckboxInput(session, "append_formulas", value = isTRUE(rv$model_append_formulas))
      # MC controls
      updateCheckboxInput(session, "mc_on", value = isTRUE(rv$mc_on))
      updateNumericInput(session, "mc_alpha_sidebar", value = rv$mc_alpha %||% ALPHA_DEFAULT)
      # mc_terms choices depend on fitted models; rebuild choices then select saved
      if (length(rv$models) > 0) {
        all_terms_raw <- unique(unlist(lapply(rv$models, function(m) {
          a <- tryCatch(model_anova(m, type = 3), error = function(e) NULL)
          if (!is.null(a)) rownames(a)[rownames(a) != "(Intercept)"] else character(0)
        })))
        factor_cols <- c(factors_(), blocks())
        blk_cols    <- blocks()
        cat_factor_terms <- all_terms_raw[sapply(all_terms_raw, function(t) {
          parts <- strsplit(t, ":")[[1]]
          all(parts %in% factor_cols) &&
            all(sapply(parts, function(cn) {
              if (cn %in% blk_cols) return(TRUE)
              (rv$col_types[[cn]] %||% "Factor") == "Factor"
            }))
        })]
        n_parts <- sapply(cat_factor_terms, function(t) length(strsplit(t, ":")[[1]]))
        cat_factor_terms <- cat_factor_terms[order(-n_parts, cat_factor_terms)]
        saved_terms <- rv$mc_terms %||% character(0)
        sel_terms <- intersect(saved_terms, cat_factor_terms)
        if (length(sel_terms) == 0) sel_terms <- cat_factor_terms
        updateCheckboxGroupInput(session, "mc_terms",
                                 choices = cat_factor_terms, selected = sel_terms)
      }
      saved_methods <- rv$mc_methods %||% character(0)
      valid_methods <- c("student", "dunnett", "custom", "tukey")
      sel_methods <- intersect(saved_methods, valid_methods)
      if (length(sel_methods) == 0) sel_methods <- "tukey"
      updateCheckboxGroupInput(session, "mc_method",
                               choices = c("Student (unadjusted)" = "student",
                                           "Dunnett (vs control)" = "dunnett",
                                           "MaxT (custom pairs)" = "custom",
                                           "Tukey (all pairs)" = "tukey"),
                               selected = sel_methods)
    }

    # ── Return exports for use by other modules ──────────────────────────
    list(
      do_fit_models = do_fit_models,
      set_custom_formula = function(text) {
        set_model_custom_formula(rv, text)
        updateTextAreaInput(session, "custom_formula", value = rv$model_custom_formula)
      },
      get_custom_formula = reactive({ rv$model_custom_formula }),
      get_active_response = reactive({ rv$model_active_response }),
      reset_ui = reset_ui,
      sync_ui_from_rv = sync_ui_from_rv
    )
  })
}
