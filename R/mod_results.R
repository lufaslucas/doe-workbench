# ── Module: Results ─────────────────────────────────────────────────────────
# UI and server for the Results tab (Model Statistics, Effects, Contour,
# Profiler, Multiple Comparisons, Robustness, Residuals).

# ── UI ─────────────────────────────────────────────────────────────────────
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        wellPanel(
          div(style = "display: flex; align-items: center; gap: 12px;",
            h5("Models to display", style = "margin: 0;"),
            actionLink(ns("show_models_all"), "Select all", style = "font-size: 0.85em;"),
            actionLink(ns("show_models_none"), "Deselect all", style = "font-size: 0.85em;")
          ),
          div(style = "max-height: 300px; overflow-y: auto;",
            checkboxGroupInput(ns("show_models"), NULL, choices = NULL)
          )
        )
      )
    ),
    tabsetPanel(id = ns("results_tabs"), selected = "Model Statistics",

      # ── Model Errors (conditional) ───────────────────────────────────────
      tabPanel("Model Errors",
        br(),
        uiOutput(ns("model_errors_ui"))
      ),

      # ── Model Statistics ──────────────────────────────────────────────────
      tabPanel("Model Statistics",
        br(),
        tabsetPanel(
          tabPanel("Summary",
            br(),
            uiOutput(ns("model_notes_ui")),
            fluidRow(
              column(3,
                radioButtons(ns("summary_plot_label"), "Model labels:",
                             choices = c("Model number" = "number", "Formula" = "formula"),
                             selected = "number", inline = TRUE)
              ),
              column(3,
                radioButtons(ns("metrics_view_mode"), "View:",
                             choices = c("Individual panels" = "panels", "Parallel plot" = "parallel"),
                             selected = "panels", inline = TRUE)
              ),
              column(3,
                radioButtons(ns("metrics_scale_mode"), "Y scale:",
                             choices = c("Natural" = "natural", "Best at top" = "best"),
                             selected = "natural", inline = TRUE)
              )
            ),
            h5("Actual vs. Predicted"),
            plotly::plotlyOutput(ns("avp_facet_plot"), width = "100%", height = "400px"),
            hr(),
            plotly::plotlyOutput(ns("summary_metrics_plot"), height = "auto"),
            hr(),
            DT::dataTableOutput(ns("rmse_table"))
          ),
          tabPanel("ANOVA",
            br(),
            fluidRow(
              column(8,
                checkboxGroupInput(ns("anova_show"), "Show columns:",
                                   choices = c("p-values" = "p", "VIF" = "vif"),
                                   selected = c("p", "vif"), inline = TRUE),
                conditionalPanel(
                  condition = paste0("input['", ns("anova_show"), "'] && input['",
                                     ns("anova_show"), "'].indexOf('vif') > -1"),
                  p(class = "text-muted small", style = "margin-top: -8px;",
                    tags$b("VIF"), " (Variance Inflation Factor) measures collinearity. ",
                    "VIF = 1: no collinearity. VIF > 5: moderate. VIF > 10: severe. ",
                    "For multi-df terms (factors with k levels), GVIF^(1/Df) is shown (comparable to standard VIF).")
                ),
                checkboxGroupInput(ns("anova_show_extra"), "ANOVA detail:",
                                   choices = c("df" = "df", "SS" = "SS",
                                               "MS" = "MS", "F values" = "F"),
                                   selected = character(0), inline = TRUE)
              ),
              column(4,
                selectInput(ns("anova_sort_order"), "Term ordering",
                            choices = c("By category (structured)" = "category",
                                        "By significance (p-value)" = "significance",
                                        "Formula order" = "formula"),
                            selected = "category")
              )
            ),
            tabsetPanel(id = ns("anova_type_tabs"), type = "pills",
              tabPanel("Type III",
                br(),
                DT::dataTableOutput(ns("anova_table"))
              ),
              tabPanel("Type I",
                br(),
                tags$small(class = "text-muted",
                  icon("info-circle"),
                  " Type I uses sequential sums of squares. The order terms enter the model ",
                  "affects their SS and p-values. Terms entered first absorb more variance."
                ),
                br(),
                DT::dataTableOutput(ns("anova_type1_table"))
              )
            )
          ),
          tabPanel("Lack of Fit",
            br(),
            p(class = "text-muted",
              "Tests whether the model adequately fits the data by comparing residual variance ",
              "at replicated points (pure error) vs. non-replicated points (lack of fit). ",
              "A significant p-value suggests the model form is inadequate."),
            fluidRow(
              column(8,
                checkboxGroupInput(ns("lof_show"), "Show columns:",
                                   choices = c("p-values" = "p",
                                               "df" = "df", "SS" = "SS",
                                               "MS" = "MS", "F values" = "F"),
                                   selected = c("p", "df", "SS", "MS", "F"), inline = TRUE)
              )
            ),
            DT::dataTableOutput(ns("lof_table"))
          ),
          tabPanel("Coefficients",
            br(),
            uiOutput(ns("coef_interpretation_ui")),
            uiOutput(ns("coef_term_filter_ui")),
            plotly::plotlyOutput(ns("coef_plot"), height = "auto"),
            hr(),
            DT::dataTableOutput(ns("coef_table"))
          )
        )
      ),

      # ── Effects (LS Means + Covariate Effects + Leverage) ─────────────
      tabPanel("Effects",
        br(),
        fluidRow(
          column(4, selectInput(ns("effects_term"), "Term to plot", choices = NULL)),
          column(3, radioButtons(ns("effects_view"), "View mode",
                                 choices = c("Overlay", "Faceted"),
                                 selected = "Overlay", inline = TRUE)),
          column(3, checkboxInput(ns("effects_show_data"), "Show adjusted data", value = TRUE))
        ),
        tabsetPanel(id = ns("effects_sub"), type = "pills",
          tabPanel("Partial Effects",
            br(),
            p(class = "text-muted",
              "Factor terms show LS means with 95% CI. Covariates show partial effect with 95% CI. ",
              "Adjusted data points have other model terms removed."),
            div(style = "width: 100%;",
              plotlyOutput(ns("effects_plot"), width = "100%", height = "500px")),
            br(),
            DT::dataTableOutput(ns("effects_table"))
          ),
          tabPanel("Leverage",
            br(),
            fluidRow(
              column(8,
                p(class = "text-muted",
                  "Y residuals vs term residuals after removing all other terms. ",
                  "The slope equals the coefficient; the spread shows leverage. ",
                  "Follows the term and models selected above.")
              ),
              column(4,
                selectInput(ns("leverage_panel_by"), "Panel by",
                            choices = c("None" = "none"), selected = "none")
              )
            ),
            div(style = "width: 100%;",
              plotlyOutput(ns("leverage_plot_combined"), width = "100%", height = "450px"))
          ),
          tabPanel("Side by Side",
            br(),
            p(class = "text-muted",
              "Partial effects (left) and leverage plot (right) for the selected term."),
            fluidRow(
              column(6, plotlyOutput(ns("effects_plot_sbs"), height = "450px")),
              column(6, plotlyOutput(ns("leverage_plot_sbs"), height = "450px"))
            ),
            br(),
            DT::dataTableOutput(ns("effects_table_sbs"))
          )
        )
      ),

      # ── Contour / Surface Plots ────────────────────────────────────────────
      tabPanel("Contour / Surface",
        br(),
        fluidRow(
          column(3, selectInput(ns("contour_x"), "X-axis factor", choices = NULL)),
          column(3, selectInput(ns("contour_y"), "Y-axis factor", choices = NULL)),
          column(3, radioButtons(ns("contour_type"), "Plot type",
                                 choices = c("Contour" = "contour", "Surface" = "surface",
                                             "Side by side" = "both"),
                                 selected = "contour", inline = TRUE)),
          column(3, sliderInput(ns("contour_grid_n"), "Grid resolution", min = 15, max = 60,
                                value = 30, step = 5))
        ),
        p(class = "text-muted small",
          "Response surface as a function of two factors. ",
          "Other factors held at their midpoints. One panel per active model."),
        uiOutput(ns("contour_hold_ui")),
        uiOutput(ns("contour_plots_ui"))
      ),

      # ── Profiler ───────────────────────────────────────────────────────────
      tabPanel("Profiler",
        br(),
        p(class = "text-muted",
          "Prediction Profiler: shows how the predicted response changes when each ",
          "factor is varied while holding all other factors at their current values. ",
          "Active models are shown as stacked rows with aligned variable panels."),
        fluidRow(
          column(2, checkboxInput(ns("profiler_ci"), "Show 95% CI", value = TRUE)),
          column(2, checkboxInput(ns("profiler_pi"), "Show 95% PI", value = FALSE)),
          column(2, radioButtons(ns("profiler_mode"), "Layout",
                                 choices = c("Aligned" = "aligned", "Collapsed" = "collapsed"),
                                 selected = "aligned", inline = TRUE)),
          column(2, sliderInput(ns("profiler_term_width"), "Panel width (px)",
                                min = 150, max = 400, value = 250, step = 25)),
          column(4, radioButtons(ns("profiler_y_scale"), "Y-axis scale",
                                 choices = c("Per panel" = "free", "Shared" = "shared",
                                             "Full response range" = "full"),
                                 selected = "free", inline = TRUE))
        ),
        uiOutput(ns("profiler_controls_ui")),
        div(style = "overflow-x: auto; width: 100%;", uiOutput(ns("profiler_plots_ui")))
      ),

      # ── Multiple Comparisons ──────────────────────────────────────────────
      tabPanel("Multiple Comparisons",
        br(),
        fluidRow(
          column(4,
            checkboxGroupInput(ns("mc_show_method"), "Methods", choices = NULL)
          ),
          column(4,
            checkboxGroupInput(ns("mc_show_term"), "Terms", choices = NULL)
          )
        ),
        uiOutput(ns("mc_plot_container")),
        br(),
        uiOutput(ns("mc_critical_values")),
        DT::dataTableOutput(ns("mc_table"))
      ),

      # ── Robustness (hidden -- TODO: review design before re-enabling) ──
      if (FALSE) tabPanel("Robustness",
        br(),
        fluidRow(
          column(3, selectInput(ns("robust_term"), "Factor term",
                                choices = NULL)),
          column(3, selectInput(ns("robust_final_model"), "Final model (for review)",
                                choices = NULL)),
          column(6, p(class = "text-muted", style = "margin-top:28px;",
            "Tests whether conclusions hold across all models. ",
            "Uses the significance level (\u03b1) set in the Models sidebar."))
        ),
        tabsetPanel(id = ns("robust_tabs"),

          # ── Model Review ──
          tabPanel("Model Review",
            br(),
            p(class = "text-muted",
              "Audits the selected final model against all other fitted models. ",
              "Flags conclusions that depend on model choice rather than data."),
            actionButton(ns("robust_review_run"), "Run Model Review",
                         class = "btn-primary"),
            br(), br(),
            uiOutput(ns("robust_review_verdict_ui")),
            br(),
            h6("LS Mean Comparison"),
            p(class = "text-muted small",
              "How do the final model's LS means compare with the consensus?"),
            DT::dataTableOutput(ns("robust_review_lsmean")),
            br(),
            h6("Pairwise Significance Audit"),
            p(class = "text-muted small",
              "Are the final model's pairwise significance conclusions shared by other models?"),
            DT::dataTableOutput(ns("robust_review_pairwise")),
            br(),
            h6("Coefficient Audit"),
            p(class = "text-muted small",
              "How do the final model's coefficient signs and significance compare?"),
            DT::dataTableOutput(ns("robust_review_coefs")),
            br(),
            h6("Model Complexity Comparison"),
            DT::dataTableOutput(ns("robust_review_complexity"))
          ),

          # ── LS Mean vs Limit ──
          tabPanel("LS Mean vs Limit",
            br(),
            p(class = "text-muted",
              "For each factor level, test whether its LS mean is ",
              "consistently above/below a user-specified limit across ",
              "all models. Results split into directional consistency ",
              "and statistical significance."),
            fluidRow(
              column(3, numericInput(ns("robust_common_limit"),
                                     "Common limit", value = 0)),
              column(2, actionButton(ns("robust_apply_limit"),
                                     "Apply to all",
                                     class = "btn-sm btn-outline-secondary",
                                     style = "margin-top:28px;"))
            ),
            uiOutput(ns("robust_level_inputs_ui")),
            br(),
            actionButton(ns("robust_lsmean_run"), "Run LS Mean vs Limit",
                         class = "btn-primary"),
            br(), br(),
            h6("Summary"),
            DT::dataTableOutput(ns("robust_lsmean_summary")),
            br(),
            h6("Detail (per model)"),
            DT::dataTableOutput(ns("robust_lsmean_detail"))
          ),

          # ── Pairwise Ordering ──
          tabPanel("Pairwise Ordering",
            br(),
            p(class = "text-muted",
              "Checks whether the rank ordering of LS means (e.g., A > B) ",
              "is consistent across all fitted models."),
            actionButton(ns("robust_order_run"), "Run Pairwise Ordering",
                         class = "btn-primary"),
            br(), br(),
            h6("Summary"),
            DT::dataTableOutput(ns("robust_order_summary")),
            br(),
            h6("Detail (per model)"),
            DT::dataTableOutput(ns("robust_order_detail"))
          ),

          # ── Pairwise Significance ──
          tabPanel("Pairwise Significance",
            br(),
            p(class = "text-muted",
              "Checks whether pairwise significance conclusions (p < alpha) ",
              "are consistent across all fitted models."),
            fluidRow(
              column(4, selectInput(ns("robust_mc_method"),
                                     "Adjustment method",
                                     choices = c("tukey", "sidak",
                                                 "bonferroni", "none"),
                                     selected = "tukey"))
            ),
            actionButton(ns("robust_sig_run"), "Run Pairwise Significance",
                         class = "btn-primary"),
            br(), br(),
            h6("Summary"),
            DT::dataTableOutput(ns("robust_sig_summary")),
            br(),
            h6("Detail (per model)"),
            DT::dataTableOutput(ns("robust_sig_detail"))
          ),

          # ── Covariate Mediation ──
          tabPanel("Covariate Mediation",
            br(),
            p(class = "text-muted",
              "Tests whether any covariates are influenced by the treatment ",
              "factor(s). A covariate that varies significantly across treatment ",
              "levels may be a mediator (response), not a true covariate."),
            fluidRow(
              column(3, numericInput(ns("robust_r2_thresh"),
                                     "R\u00b2 threshold", value = 0.30,
                                     min = 0.05, max = 0.90, step = 0.05))
            ),
            actionButton(ns("robust_mediation_run"), "Run Mediation Check",
                         class = "btn-primary"),
            br(), br(),
            DT::dataTableOutput(ns("robust_mediation_table"))
          ),

          # ── Outlier Influence ──
          tabPanel("Outlier Influence",
            br(),
            p(class = "text-muted",
              "Identifies influential observations via Cook's D and DFBETAS, ",
              "then iteratively removes the most influential point and refits. ",
              "Compares LS mean conclusions before and after removal."),
            fluidRow(
              column(4, selectInput(ns("robust_outlier_model"),
                                     "Model", choices = NULL)),
              column(2, numericInput(ns("robust_outlier_maxiter"),
                                     "Max iterations", value = 5L,
                                     min = 1L, max = 20L, step = 1L))
            ),
            actionButton(ns("robust_outlier_run"), "Run Outlier Influence",
                         class = "btn-primary"),
            br(), br(),
            h6("Influence Diagnostics"),
            DT::dataTableOutput(ns("robust_influence_table")),
            br(),
            h6("Removal Log"),
            DT::dataTableOutput(ns("robust_removal_log")),
            br(),
            h6("LS Mean Comparison (Original vs Clean)"),
            DT::dataTableOutput(ns("robust_conclusion_cmp")),
            br(),
            h6("Pairwise Significance Comparison (Original vs Clean)"),
            DT::dataTableOutput(ns("robust_pairwise_cmp"))
          ),

          # ── Coefficient Robustness ──
          tabPanel("Coefficient Robustness",
            br(),
            p(class = "text-muted",
              "Compares coefficient sign, significance, and importance ",
              "rank across all fitted models. Identifies terms whose ",
              "conclusions are sensitive to model specification."),
            actionButton(ns("robust_coef_run"), "Run Coefficient Check",
                         class = "btn-primary"),
            br(), br(),
            h6("Sign Stability"),
            p(class = "text-muted small",
              "Does each coefficient keep the same sign (+/\u2212) across models?"),
            DT::dataTableOutput(ns("robust_coef_sign")),
            br(),
            h6("Significance Stability"),
            p(class = "text-muted small",
              "Is each term consistently significant or non-significant?"),
            DT::dataTableOutput(ns("robust_coef_sig")),
            br(),
            h6("Importance Rank Stability"),
            p(class = "text-muted small",
              "Does the ranking by |standardised coefficient| stay consistent?"),
            DT::dataTableOutput(ns("robust_coef_rank")),
            br(),
            h6("Full Detail (per model)"),
            DT::dataTableOutput(ns("robust_coef_detail"))
          )
        )
      ),

      # ── Residuals ────────────────────────────────────────────────────────
      tabPanel("Residuals",
        br(),
        fluidRow(
          column(4, selectInput(ns("resid_model"), "Model", choices = NULL)),
          column(4, selectInput(ns("resid_colour_by"), "Colour points by",
                                choices = c("None" = "none"), selected = "none"))
        ),
        tabsetPanel(id = ns("resid_tabs"),
          tabPanel("Standard Diagnostics",
            br(),
            p(class = "text-muted",
              "Q-Q uses internally standardised residuals (rstandard). ",
              "Scale-Location uses externally studentised residuals (rstudent) \u2014 ",
              "these re-fit the model with each point omitted so the deleted residual ",
              "is independent of the fitted value."),
            plotlyOutput(ns("resid_standard"), width = "100%", height = "700px")
          ),
          tabPanel("vs Model Terms",
            br(),
            p(class = "text-muted",
              "Residuals plotted against each predictor included in the model. ",
              "Patterns suggest non-linearity or missing interactions."),
            uiOutput(ns("resid_vs_terms_ui"))
          ),
          tabPanel("vs Omitted Terms",
            br(),
            p(class = "text-muted",
              "Residuals plotted against columns not in the model. ",
              "Trends here suggest potential omitted-variable bias."),
            uiOutput(ns("resid_vs_omitted_ui"))
          ),
          tabPanel("Outliers",
            br(),
            p(class = "text-muted",
              "Points flagged by studentised residuals (|rstudent| > 2) or Cook's distance (> 4/n). ",
              "In screening and optimisation DoEs, influential points are common and expected ",
              "\u2014 they often represent the design points that drive the model. ",
              "Focus on outliers (large residuals) rather than influence alone."),
            fluidRow(
              column(6, radioButtons(ns("outlier_resid_type"), "Studentised residual type",
                                     choices = c("External (rstudent)" = "ext",
                                                 "Internal (rstandard)" = "int"),
                                     selected = "ext", inline = TRUE)),
              column(6, tags$small(class = "text-muted",
                "External = leave-one-out (more conservative); Internal = standard"))
            ),
            plotlyOutput(ns("outlier_influence_plot"), height = "380px"),
            hr(),
            fluidRow(
              column(8, DT::dataTableOutput(ns("outlier_table"))),
              column(4,
                wellPanel(
                  h5("Refit without selected outliers"),
                  p(class = "text-muted small",
                    "Select rows in the table, then click to create a new model ",
                    "with those observations excluded."),
                  actionButton(ns("refit_no_outliers"), "Refit Model",
                               class = "btn-warning", icon = icon("filter")),
                  br(), br(),
                  uiOutput(ns("excluded_info_ui")),
                  hr(),
                  h5("Iterative outlier removal"),
                  p(class = "text-muted small",
                    "Automatically remove the worst outlier and refit, repeating until ",
                    "no points exceed the threshold. Creates intermediate models."),
                  fluidRow(
                    column(6, numericInput(ns("iter_outlier_thresh"), "Threshold",
                                           value = 3, min = 2, max = 5, step = 0.5)),
                    column(6, radioButtons(ns("iter_outlier_type"), NULL,
                                            choices = c("External" = "ext", "Internal" = "int"),
                                            selected = "ext", inline = TRUE))
                  ),
                  actionButton(ns("iter_remove_outliers"), "Run Iterative Removal",
                               class = "btn-outline-warning btn-sm", icon = icon("repeat"))
                )
              )
            )
          ),
          tabPanel("Diagnostics",
            br(),
            p(class = "text-muted",
              "Automated diagnostic checks for model assumptions. ",
              "Traffic lights indicate severity: ",
              tags$span(style = "color: #28a745;", "green"), " = OK, ",
              tags$span(style = "color: #ffc107;", "amber"), " = caution, ",
              tags$span(style = "color: #dc3545;", "red"), " = violation detected."),
            uiOutput(ns("diagnostics_ui")),
            hr(),
            h5("Residual Pattern Analysis"),
            p(class = "text-muted small",
              "Checks for systematic patterns in residuals that may indicate ",
              "missing model terms, unequal variance, or other model misspecification."),
            uiOutput(ns("resid_pattern_ui"))
          )
        )
      )
    )
  )
}

# ── Server ─────────────────────────────────────────────────────────────────
#
# Parameters:
#   rv             - shared reactiveValues (models, model_errors, model_notes,
#                    vif_df, formulas, mc_results, excluded_obs, data,
#                    col_types, transforms, coding_values, alias_labels,
#                    roles, prune_notes)
#   colour_theme   - list of reactive colour helpers (cat_palette, default_col,
#                    cat_scale_colour, cat_scale_fill, cont_scale_colour,
#                    cont_scale_fill, cont_plotly_cs)
#   role_selectors - list of reactives: responses, factors_, covariates,
#                    blocks, run_orders, all_covariates
#   shared_reactives - list: treatment, treatment_label
#   mc_state       - list of reactives from Models tab: mc_on, mc_alpha,
#                    mc_terms, mc_methods
#
# Returns: list(active_models = <reactive>)

mod_results_server <- function(id, rv, colour_theme, role_selectors,
                               shared_reactives, mc_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # ── Local aliases for colour helpers ──────────────────────────────────
    cat_palette       <- colour_theme$cat_palette
    default_col       <- colour_theme$default_col
    cat_scale_colour  <- colour_theme$cat_scale_colour
    cat_scale_fill    <- colour_theme$cat_scale_fill
    cont_scale_colour <- colour_theme$cont_scale_colour
    cont_scale_fill   <- colour_theme$cont_scale_fill

    # ── Local aliases for role selectors ──────────────────────────────────
    factors_       <- role_selectors$factors_
    covariates     <- role_selectors$covariates
    blocks         <- role_selectors$blocks
    run_orders     <- role_selectors$run_orders
    all_covariates <- role_selectors$all_covariates
    treatment_label <- shared_reactives$treatment_label
    analysis_mode   <- shared_reactives$analysis_mode

    # ── Read-only guard for refit button ──────────────────────────────────
    observe({
      locked <- isTRUE(rv$read_only)
      toggle <- if (locked) shinyjs::disable else shinyjs::enable
      toggle(ns("refit_no_outliers"))
    })

    # ── Helper: build labelled model choices for show_models checkbox ─────
    labelled_model_choices <- function(model_names) {
      notes <- rv$model_notes
      choice_names <- lapply(seq_along(model_names), function(i) {
        mn <- model_names[i]
        prefix <- paste0("M", i, ": ")
        has_warnings <- !is.null(notes[[mn]]) && length(notes[[mn]]$warnings) > 0
        is_saturated <- any(grepl("Saturated model", notes[[mn]]$warnings, fixed = TRUE))
        suffix <- ""
        if (is_saturated) {
          suffix <- " [saturated]"
        } else if (has_warnings) {
          suffix <- " [!]"
        }
        if (has_warnings) {
          tooltip <- paste(notes[[mn]]$warnings, collapse = "\n")
          tags$span(style = "color: #dc3545;", title = tooltip,
                    paste0(prefix, mn, suffix))
        } else {
          tags$span(paste0(prefix, mn))
        }
      })
      list(names = choice_names, values = model_names)
    }

    # ── Helper: update show_models checkbox with HTML-aware labels ────────
    update_model_checkbox <- function(model_names, selected = model_names) {
      ch <- labelled_model_choices(model_names)
      updateCheckboxGroupInput(session, "show_models",
                               choiceNames = ch$names, choiceValues = ch$values,
                               selected = selected)
    }

    # ── active_models reactive (RETURNED to caller) ──────────────────────
    active_models <- reactive({
      req(length(rv$models) > 0)
      sel <- input$show_models
      if (is.null(sel) || length(sel) == 0) return(rv$models)
      rv$models[intersect(sel, names(rv$models))]
    })

    # ── Helper: get actual M-numbers for active models ─────────────────
    # Maps formula names to their true index in rv$models (e.g. "M2", "M8")
    model_m_labels <- function(model_names) {
      all_names <- names(rv$models)
      sapply(model_names, function(mn) {
        idx <- match(mn, all_names)
        if (is.na(idx)) mn else paste0("M", idx)
      }, USE.NAMES = FALSE)
    }

    # ── Sync show_models when rv$models changes (e.g. after do_fit_models) ─
    observeEvent(rv$models, {
      model_names <- names(rv$models)
      if (length(model_names) > 0) {
        update_model_checkbox(model_names)
      }
    })

    # ── Select all / Deselect all ────────────────────────────────────────
    observeEvent(input$show_models_all, {
      model_names <- names(rv$models)
      update_model_checkbox(model_names)
    })
    observeEvent(input$show_models_none, {
      model_names <- names(rv$models)
      update_model_checkbox(model_names, selected = character(0))
    })

    # ── Keep model selectors synced with active (checked) models ─────────
    observe({
      mods <- names(active_models())
      cur_resid <- isolate(input$resid_model)
      cur_out   <- isolate(input$robust_outlier_model)
      cur_final <- isolate(input$robust_final_model)
      safe_sel <- function(cur, default) {
        if (!is.null(cur) && length(cur) == 1 && cur %in% mods) cur else default
      }
      updateSelectInput(session, "resid_model",
                        choices = mods, selected = safe_sel(cur_resid, mods[1]))
      updateSelectInput(session, "robust_outlier_model",
                        choices = mods, selected = safe_sel(cur_out, mods[1]))
      updateSelectInput(session, "robust_final_model",
                        choices = mods, selected = safe_sel(cur_final, mods[length(mods)]))
    })

    # ── Helper: build term_roles for ANOVA ordering ──────────────────────
    term_roles <- reactive({
      all_terms_raw <- unique(unlist(lapply(active_models(), function(m) {
        a <- tryCatch(model_anova(m, type=3), error=function(e) NULL)
        if (!is.null(a)) rownames(a) else character(0)
      })))
      classify_anova_terms(all_terms_raw, factors_(), blocks(), all_covariates())
    })

    # ── Show/hide "Model Errors" tab ─────────────────────────────────────
    observe({
      errs <- rv$model_errors
      tab_sel <- paste0("#", ns("results_tabs"), " li a[data-value='Model Errors']")
      if (length(errs) > 0) {
        shinyjs::show(selector = tab_sel)
      } else {
        shinyjs::hide(selector = tab_sel)
      }
    })

    # ── Show/hide Multiple Comparisons tab based on mc_on ────────────────
    observe({
      if (isTRUE(mc_state$mc_on())) {
        showTab("results_tabs", target = "Multiple Comparisons")
      } else {
        hideTab("results_tabs", target = "Multiple Comparisons")
      }
    })

    # ── Show/hide Contour/Surface tab: needs >= 2 numeric predictors ────
    observe({
      mode <- analysis_mode()
      mods <- active_models()
      if (length(mods) == 0 || mode == "comparative") {
        hideTab("results_tabs", target = "Contour / Surface")
        return()
      }
      # Check if any active model has >= 2 numeric predictors
      has_enough <- any(sapply(mods, function(m) {
        preds <- all.vars(formula(m))[-1]
        numeric_preds <- preds[sapply(preds, function(p) {
          d <- model.frame(m)
          p %in% names(d) && is.numeric(d[[p]])
        })]
        length(numeric_preds) >= 2
      }))
      if (has_enough) showTab("results_tabs", target = "Contour / Surface")
      else hideTab("results_tabs", target = "Contour / Surface")
    })

    # ── Model errors panel ───────────────────────────────────────────────
    output$model_errors_ui <- renderUI({
      errs <- rv$model_errors
      if (length(errs) == 0) {
        return(div(class = "alert alert-success p-3",
                   icon("check-circle"), " All models fitted successfully."))
      }
      cards <- lapply(names(errs), function(label) {
        div(class = "alert alert-danger p-2 mb-2", style = "font-size: 13px;",
          tags$strong(tags$code(label)),
          tags$p(style = "margin: 4px 0 0 0;", errs[[label]])
        )
      })
      tagList(
        h5("The following formulas could not be fitted:"),
        do.call(tagList, cards),
        tags$p(class = "text-muted", style = "font-size: 12px; margin-top: 8px;",
          "These models are excluded from all results tabs. ",
          "Consider removing terms that the design cannot support (e.g. quadratic terms in a 2-level design).")
      )
    })

    # ── Model fitting notes/warnings ─────────────────────────────────────
    output$model_notes_ui <- renderUI({
      notes <- rv$model_notes
      if (length(notes) == 0) return(NULL)
      alerts <- lapply(names(notes), function(mname) {
        n <- notes[[mname]]
        if (length(n$warnings) == 0) return(NULL)
        div(class = "alert alert-warning p-2 mb-1", style = "font-size: 12px;",
          tags$strong(mname, ": "),
          tags$ul(style = "margin-bottom: 0; padding-left: 20px;",
            lapply(n$warnings, function(w) tags$li(w))
          )
        )
      })
      alerts <- Filter(Negate(is.null), alerts)
      if (length(alerts) == 0) return(NULL)
      do.call(tagList, alerts)
    })

    # ── Auto-expand ANOVA columns when single model selected ─────────────
    rv_anova_saved <- reactiveValues(show = NULL, show_extra = NULL, was_multi = TRUE)
    observe({
      n_active <- length(active_models())
      if (n_active == 1 && isTRUE(rv_anova_saved$was_multi)) {
        rv_anova_saved$show       <- input$anova_show
        rv_anova_saved$show_extra <- input$anova_show_extra
        rv_anova_saved$was_multi  <- FALSE
        updateCheckboxGroupInput(session, "anova_show",
          selected = c("p", "vif"))
        updateCheckboxGroupInput(session, "anova_show_extra",
          selected = c("df", "SS", "MS", "F"))
      } else if (n_active > 1 && !isTRUE(rv_anova_saved$was_multi)) {
        rv_anova_saved$was_multi <- TRUE
        if (!is.null(rv_anova_saved$show))
          updateCheckboxGroupInput(session, "anova_show",
            selected = rv_anova_saved$show)
        if (!is.null(rv_anova_saved$show_extra))
          updateCheckboxGroupInput(session, "anova_show_extra",
            selected = rv_anova_saved$show_extra)
      }
    })

    # ── ANOVA: Type III cross-model wide table ───────────────────────────
    output$anova_table <- DT::renderDataTable({
      req(length(active_models()) > 0)
      sort_mode <- input$anova_sort_order %||% "category"
      mods <- active_models()
      saturated <- names(mods)[sapply(mods, function(m) df.residual(m) == 0)]
      testable  <- mods[setdiff(names(mods), saturated)]
      if (length(testable) == 0) {
        msg <- paste0("No ANOVA results. All selected models are saturated (0 residual df): ",
                      paste(saturated, collapse = "; "),
                      ". Remove terms to free residual degrees of freedom.")
        return(DT::datatable(data.frame(Message = msg)))
      }
      wide <- type3_wide(testable, term_roles(), sort_mode = sort_mode)
      if (nrow(wide) == 0) return(DT::datatable(data.frame(Message = "No ANOVA results.")))
      if (length(rv$alias_labels) > 0) wide$term <- relabel_alias_terms(wide$term, rv$alias_labels)

      show <- c(input$anova_show %||% "p", input$anova_show_extra %||% character(0))

      # Merge VIF columns if requested
      vif_cols <- character(0)
      if ("vif" %in% show && nrow(rv$vif_df) > 0) {
        vif_wide <- rv$vif_df
        vif_model_cols <- setdiff(names(vif_wide), "Term")
        for (mc in vif_model_cols) {
          vif_col_name <- paste0(mc, "_VIF")
          wide[[vif_col_name]] <- vif_wide[[mc]][match(wide$term, vif_wide$Term)]
          vif_cols <- c(vif_cols, vif_col_name)
        }
      }

      # Interleave columns per model (use testable, not active_models, to match wide)
      model_names <- names(testable)
      ordered_cols <- "term"
      for (mn in model_names) {
        if ("df" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_df"))
        if ("SS" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_SS"))
        if ("MS" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_MS"))
        if ("F" %in% show)   ordered_cols <- c(ordered_cols, paste0(mn, "_F"))
        if ("p" %in% show)   ordered_cols <- c(ordered_cols, paste0(mn, "_p"))
        if ("vif" %in% show) ordered_cols <- c(ordered_cols, paste0(mn, "_VIF"))
      }
      ordered_cols <- intersect(ordered_cols, names(wide))
      wide <- wide[, ordered_cols, drop = FALSE]

      # Strip prefix for single model
      if (length(model_names) == 1) {
        prefix <- paste0(model_names[1], "_")
        names(wide) <- sub(prefix, "", names(wide), fixed = TRUE)
      }

      p_cols  <- grep("_p$|^p$", names(wide), value = TRUE)
      f_cols  <- grep("_F$|^F$", names(wide), value = TRUE)
      ss_cols <- grep("_SS$|^SS$", names(wide), value = TRUE)
      ms_cols <- grep("_MS$|^MS$", names(wide), value = TRUE)
      df_cols <- grep("_df$|^df$", names(wide), value = TRUE)

      # Hierarchical header for multi-model view
      # Built AFTER format_pvalue_dt so we know the final column count
      container_arg <- NULL

      # Per-term max VIF for collinearity highlighting
      max_vif <- rep(NA_real_, nrow(wide))
      if (nrow(rv$vif_df) > 0) {
        vif_wide_all <- rv$vif_df
        vif_model_cols_all <- setdiff(names(vif_wide_all), "Term")
        for (i in seq_len(nrow(wide))) {
          orig_term <- wide$term[i]
          idx <- match(orig_term, vif_wide_all$Term)
          if (!is.na(idx)) {
            vals <- suppressWarnings(as.numeric(unlist(vif_wide_all[idx, vif_model_cols_all])))
            vals <- vals[!is.na(vals)]
            if (length(vals) > 0) max_vif[i] <- max(vals)
          }
        }
      }
      wide$`.vif_max` <- max_vif

      # Build term summary comments
      # Denominator = models containing the term (non-NA p-values), not total models
      alpha <- mc_state$mc_alpha() %||% 0.05
      summaries <- sapply(seq_len(nrow(wide)), function(i) {
        parts <- character()
        # Count models containing this term and how many show significance
        # Use explicit per-column access to avoid unlist() type coercion issues
        n_present <- 0L
        n_sig     <- 0L
        for (pc in p_cols) {
          val <- wide[[pc]][i]
          if (is.numeric(val) && !is.na(val)) {
            n_present <- n_present + 1L
            if (val < alpha) n_sig <- n_sig + 1L
          }
        }
        if (n_present > 0) {
          if (n_sig == n_present) {
            parts <- c(parts, "Significant")
          } else if (n_sig > 0) {
            parts <- c(parts, paste0("Significant in ", n_sig, "/", n_present))
          } else {
            parts <- c(parts, "Not significant")
          }
        }
        vif_val <- max_vif[i]
        if (!is.na(vif_val) && vif_val > 5) {
          parts <- c(parts, paste0("Collinear (VIF=", round(vif_val, 1), ")"))
        }
        paste(parts, collapse = "; ")
      })
      wide$Summary <- summaries

      col_order <- c("term", "Summary", setdiff(names(wide), c("term", "Summary", ".vif_max")), ".vif_max")
      wide_display <- wide[, col_order, drop = FALSE]

      p_cols  <- grep("_p$|^p$", names(wide_display), value = TRUE)
      f_cols  <- grep("_F$|^F$", names(wide_display), value = TRUE)
      ss_cols <- grep("_SS$|^SS$", names(wide_display), value = TRUE)
      ms_cols <- grep("_MS$|^MS$", names(wide_display), value = TRUE)

      # Pre-format p-value columns (creates ._p_ shadow columns for styling)
      wide_display <- format_pvalue_dt(wide_display, p_cols, alpha = alpha)
      shadow_p_cols <- paste0("._p_", p_cols)

      vif_max_idx <- which(names(wide_display) == ".vif_max") - 1
      shadow_p_idx <- which(names(wide_display) %in% shadow_p_cols) - 1
      hide_cols <- list(list(visible = FALSE, targets = c(vif_max_idx, shadow_p_idx)))

      # Build hierarchical header for multi-model (after shadow columns are added)
      if (length(model_names) > 1) {
        show <- c(input$anova_show %||% "p", input$anova_show_extra %||% character(0))
        stat_labels <- character(0)
        if ("df" %in% show)  stat_labels <- c(stat_labels, "df")
        if ("SS" %in% show)  stat_labels <- c(stat_labels, "SS")
        if ("MS" %in% show)  stat_labels <- c(stat_labels, "MS")
        if ("F" %in% show)   stat_labels <- c(stat_labels, "F")
        if ("p" %in% show)   stat_labels <- c(stat_labels, "p")
        if ("vif" %in% show) stat_labels <- c(stat_labels, "VIF")
        n_stats <- length(stat_labels)

        # Count hidden trailing columns (.vif_max + shadow p-values)
        n_hidden <- length(vif_max_idx) + length(shadow_p_idx)

        if (n_stats > 0) {
          sub_cells <- unlist(lapply(model_names, function(mn) {
            lapply(stat_labels, function(s) tags$th(s))
          }), recursive = FALSE)
          # Add blank sub-header cells for hidden columns
          hidden_cells <- lapply(seq_len(n_hidden), function(i) tags$th(style = "display:none;", ""))

          container_arg <- htmltools::withTags(table(
            class = "display",
            thead(
              tr(
                th(rowspan = 2, "Term"),
                th(rowspan = 2, "Summary"),
                lapply(model_names, function(mn) th(colspan = n_stats, style = "text-align:center;", mn)),
                lapply(seq_len(n_hidden), function(i) th(rowspan = 2, style = "display:none;", ""))
              ),
              do.call(tr, sub_cells)
            )
          ))
        }
      }

      dt <- dt_table(wide_display, rownames = FALSE,
                      options = list(pageLength = 20, columnDefs = hide_cols),
                      container = container_arg, filter = "none")
      for (pc in p_cols) {
        shadow <- paste0("._p_", pc)
        dt <- DT::formatStyle(dt, pc,
                 backgroundColor = DT::styleInterval(alpha, c(PVALUE_GREEN, "white")),
                 fontWeight      = DT::styleInterval(alpha, c("bold", "normal")),
                 valueColumns    = shadow)
      }
      if (length(f_cols) > 0)  dt <- DT::formatRound(dt, f_cols, digits = 3)
      if (length(ss_cols) > 0) dt <- DT::formatRound(dt, ss_cols, digits = 3)
      if (length(ms_cols) > 0) dt <- DT::formatRound(dt, ms_cols, digits = 3)
      dt <- DT::formatStyle(dt, "term",
               color = DT::styleInterval(5, c("inherit", "#dc3545")),
               fontWeight = DT::styleInterval(5, c("normal", "bold")),
               valueColumns = ".vif_max")
      for (vc in vif_cols) {
        col_name <- if (length(model_names) == 1) "VIF" else vc
        if (col_name %in% names(wide_display)) {
          dt <- DT::formatRound(dt, col_name, digits = 2)
          dt <- DT::formatStyle(dt, col_name,
                   backgroundColor = DT::styleInterval(
                     c(5, 10),
                     c("white", "#fff3cd", "#f8d7da")),
                   fontWeight = DT::styleInterval(c(5), c("normal", "bold")))
        }
      }
      dt
    })

    # ── ANOVA Type I (sequential) cross-model table ──────────────────────
    output$anova_type1_table <- DT::renderDataTable({
      req(length(active_models()) > 0)
      mods <- active_models()
      saturated <- names(mods)[sapply(mods, function(m) df.residual(m) == 0)]
      testable  <- mods[setdiff(names(mods), saturated)]
      if (length(testable) == 0) {
        return(DT::datatable(data.frame(
          Message = "No Type I ANOVA results. All selected models are saturated (0 residual df).")))
      }
      model_names <- names(testable)
      show <- c(input$anova_show %||% "p", input$anova_show_extra %||% character(0))

      all_dfs <- lapply(model_names, function(mname) {
        m <- testable[[mname]]
        tryCatch({
          a <- anova(m)
          df <- as.data.frame(a)
          df$term <- rownames(df)
          names(df)[names(df) == "Sum Sq"]  <- "SS"
          names(df)[names(df) == "Df"]      <- "df_val"
          names(df)[names(df) == "F value"] <- "F_value"
          names(df)[names(df) == "Pr(>F)"]  <- "p_value"
          df$MS <- ifelse(df$df_val > 0, df$SS / df$df_val, NA_real_)
          out <- df[, c("term", "F_value", "p_value", "SS", "MS", "df_val"), drop = FALSE]
          names(out)[2:6] <- c(paste0(mname, "_F"), paste0(mname, "_p"),
                                paste0(mname, "_SS"), paste0(mname, "_MS"),
                                paste0(mname, "_df"))
          out
        }, error = function(e) NULL)
      })
      all_dfs <- Filter(Negate(is.null), all_dfs)
      if (length(all_dfs) == 0) return(DT::datatable(data.frame(Message = "No ANOVA results.")))

      # Merge all model ANOVA tables
      wide <- all_dfs[[1]]
      if (length(all_dfs) > 1) {
        for (i in 2:length(all_dfs)) wide <- merge(wide, all_dfs[[i]], by = "term", all = TRUE)
      }

      # Apply sort order (shared with Type III)
      sort_mode <- input$anova_sort_order %||% "category"
      if (sort_mode == "formula") {
        # Formula order: preserve sequential order from the longest model
        term_order <- unique(unlist(lapply(
          all_dfs[order(-sapply(all_dfs, nrow))],
          function(d) d$term
        )))
        term_order <- c(setdiff(term_order, "Residuals"), "Residuals")
        wide <- wide[match(term_order, wide$term, nomatch = 0L), , drop = FALSE]
      } else {
        wide <- order_anova_terms(wide, term_roles(), sort_mode = sort_mode)
      }

      ordered_cols <- "term"
      for (mn in model_names) {
        if ("df" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_df"))
        if ("SS" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_SS"))
        if ("MS" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_MS"))
        if ("F" %in% show)   ordered_cols <- c(ordered_cols, paste0(mn, "_F"))
        if ("p" %in% show)   ordered_cols <- c(ordered_cols, paste0(mn, "_p"))
      }
      ordered_cols <- intersect(ordered_cols, names(wide))
      wide <- wide[, ordered_cols, drop = FALSE]

      if (length(model_names) == 1) {
        prefix <- paste0(model_names[1], "_")
        names(wide) <- sub(prefix, "", names(wide), fixed = TRUE)
      }

      container_arg <- NULL
      if (length(model_names) > 1) {
        stat_labels <- character(0)
        if ("df" %in% show) stat_labels <- c(stat_labels, "df")
        if ("SS" %in% show) stat_labels <- c(stat_labels, "SS")
        if ("MS" %in% show) stat_labels <- c(stat_labels, "MS")
        if ("F" %in% show)  stat_labels <- c(stat_labels, "F")
        if ("p" %in% show)  stat_labels <- c(stat_labels, "p")
        n_stats <- length(stat_labels)
        if (n_stats > 0) {
          sub_cells <- unlist(lapply(model_names, function(mn) {
            lapply(stat_labels, function(s) tags$th(s))
          }), recursive = FALSE)
          container_arg <- htmltools::withTags(table(
            class = "display",
            thead(
              tr(
                th(rowspan = 2, "Term"),
                lapply(model_names, function(mn) th(colspan = n_stats, style = "text-align:center;", mn))
              ),
              do.call(tr, sub_cells)
            )
          ))
        }
      }

      p_cols  <- grep("_p$|^p$", names(wide), value = TRUE)
      f_cols  <- grep("_F$|^F$", names(wide), value = TRUE)
      ss_cols <- grep("_SS$|^SS$", names(wide), value = TRUE)
      ms_cols <- grep("_MS$|^MS$", names(wide), value = TRUE)

      # Pre-format p-value columns
      wide <- format_pvalue_dt(wide, p_cols)
      shadow_p_cols <- paste0("._p_", p_cols)
      shadow_p_idx <- which(names(wide) %in% shadow_p_cols) - 1
      hide_defs <- if (length(shadow_p_idx) > 0) list(list(visible = FALSE, targets = shadow_p_idx)) else list()

      dt <- dt_table(wide, rownames = FALSE,
                      options = list(pageLength = 20, columnDefs = hide_defs),
                      container = container_arg, filter = "none")
      for (pc in p_cols) {
        shadow <- paste0("._p_", pc)
        dt <- DT::formatStyle(dt, pc,
                 backgroundColor = DT::styleInterval(ALPHA_DEFAULT, c(PVALUE_GREEN, "white")),
                 fontWeight      = DT::styleInterval(ALPHA_DEFAULT, c("bold", "normal")),
                 valueColumns    = shadow)
      }
      if (length(f_cols) > 0)  dt <- DT::formatRound(dt, f_cols, digits = 3)
      if (length(ss_cols) > 0) dt <- DT::formatRound(dt, ss_cols, digits = 3)
      if (length(ms_cols) > 0) dt <- DT::formatRound(dt, ms_cols, digits = 3)
      dt
    })

    # ── Summary metrics plot ─────────────────────────────────────────────
    output$summary_metrics_plot <- renderPlotly({
      req(length(active_models()) > 0)
      smry <- rmse_summary(active_models())
      if (nrow(smry) == 0) return(plotly_empty())

      use_formula <- identical(input$summary_plot_label, "formula")
      view_mode <- input$metrics_view_mode %||% "panels"
      scale_mode <- input$metrics_scale_mode %||% "natural"
      n_mod <- nrow(smry)
      smry$Model_Num <- model_m_labels(smry$Model)
      model_labels <- if (use_formula) smry$Model else smry$Model_Num
      pal <- cat_palette()(max(6, n_mod))

      if (view_mode == "parallel") {
        # Parallel plot: one x-axis level per metric, one line per model
        metric_names <- c("R\u00b2", "Adj R\u00b2", "PRESS R\u00b2", "RMSE", "PRESS RMSE", "AIC", "BIC")
        # higher_better: TRUE = higher is better (R2), FALSE = lower is better (RMSE, AIC, BIC)
        higher_better <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)

        p <- plot_ly()
        for (i in seq_len(n_mod)) {
          raw_vals <- c(smry$R2[i], smry$Adj_R2[i], smry$PRESS_R2[i],
                        smry$RMSE[i], smry$PRESS_RMSE[i], smry$AIC[i], smry$BIC[i])

          if (scale_mode == "best") {
            # Normalize to [0,1] where 1 = best across models for that metric
            norm_vals <- numeric(length(raw_vals))
            all_vals_list <- list(smry$R2, smry$Adj_R2, smry$PRESS_R2,
                                  smry$RMSE, smry$PRESS_RMSE, smry$AIC, smry$BIC)
            for (j in seq_along(raw_vals)) {
              vals <- all_vals_list[[j]]
              rng <- range(vals, na.rm = TRUE)
              if (diff(rng) < 1e-10) { norm_vals[j] <- 1
              } else if (higher_better[j]) {
                norm_vals[j] <- (raw_vals[j] - rng[1]) / diff(rng)
              } else {
                norm_vals[j] <- 1 - (raw_vals[j] - rng[1]) / diff(rng)
              }
            }
            y_vals <- norm_vals
            hover_text <- paste0(metric_names, ": ", round(raw_vals, 4),
                                  " (score: ", round(norm_vals, 3), ")")
          } else {
            y_vals <- raw_vals
            hover_text <- paste0(metric_names, ": ", round(raw_vals, 4))
          }

          p <- add_trace(p, x = metric_names, y = y_vals, type = "scatter",
                         mode = "lines+markers", name = model_labels[i],
                         marker = list(size = 8), line = list(width = 2),
                         text = hover_text, hoverinfo = "text",
                         color = I(pal[i]))
        }

        y_title <- if (scale_mode == "best") "Score (1 = best)" else "Value"
        y_range <- if (scale_mode == "best") list(range = c(-0.05, 1.05)) else list()
        plot_h <- if (use_formula) max(400, 300 + n_mod * 12) else 350
        p %>% layout(
          yaxis = c(list(title = y_title), y_range),
          xaxis = list(title = "", categoryorder = "array", categoryarray = metric_names),
          legend = list(orientation = "h", y = 1.15, x = 0.5, xanchor = "center"),
          margin = list(t = 50), height = plot_h
        )
      } else {
        # Individual panels mode (original)
        x_labels <- model_labels
        pal6 <- cat_palette()(6)
        best_top <- (scale_mode == "best")

        build_panel <- function(metrics, values_list, title, y_range = NULL,
                                reverse = FALSE, show_legend = FALSE) {
          p <- plot_ly()
          for (i in seq_along(metrics)) {
            p <- add_trace(p, x = x_labels, y = values_list[[i]], type = "scatter",
                           mode = "lines+markers", name = metrics[i],
                           marker = list(size = 8), line = list(width = 2),
                           legendgroup = metrics[i], showlegend = show_legend,
                           hovertemplate = paste0("%{x}<br>", metrics[i],
                                                 ": %{y:.4f}<extra></extra>"),
                           color = I(pal6[i]))
          }
          yaxis <- list(title = title)
          if (!is.null(y_range)) yaxis$range <- y_range
          if (reverse && best_top) yaxis$autorange <- "reversed"
          p %>% layout(yaxis = yaxis, xaxis = list(title = ""))
        }

        rmse_max <- max(c(smry$RMSE, smry$PRESS_RMSE), na.rm = TRUE) * 1.1
        p1 <- build_panel(c("RMSE", "PRESS RMSE"),
                           list(smry$RMSE, smry$PRESS_RMSE),
                           "RMSE", y_range = c(0, rmse_max), reverse = TRUE, show_legend = TRUE)
        p2 <- build_panel(c("R\u00b2", "Adj R\u00b2", "PRESS R\u00b2"),
                           list(smry$R2, smry$Adj_R2, smry$PRESS_R2),
                           "R\u00b2", y_range = c(0, 1), show_legend = TRUE)
        p2 <- p2 %>% add_trace(y = rep(1, length(x_labels)), x = x_labels,
                                 type = "scatter", mode = "lines",
                                 line = list(dash = "dash", color = "grey70", width = 1),
                                 showlegend = FALSE, hoverinfo = "skip")
        p3 <- build_panel(c("AIC"), list(smry$AIC), "AIC", reverse = TRUE, show_legend = TRUE)
        p4 <- build_panel(c("BIC"), list(smry$BIC), "BIC", reverse = TRUE, show_legend = TRUE)

        plot_h <- if (use_formula) max(350, 250 + n_mod * 12) else 300
        x_order <- list(categoryorder = "array", categoryarray = x_labels,
                         tickangle = if (use_formula) -45 else 0)
        subplot(p1, p2, p3, p4, nrows = 1, shareX = TRUE, titleY = TRUE) %>%
          layout(legend = list(orientation = "h", y = 1.15, x = 0.5, xanchor = "center", yanchor = "bottom"),
                 margin = list(t = 50),
                 height = plot_h,
                 xaxis  = x_order, xaxis2 = x_order, xaxis3 = x_order, xaxis4 = x_order)
      }
    })

    # ── Actual vs. Predicted (faceted) ───────────────────────────────────
    output$avp_facet_plot <- renderPlotly({
      mods <- active_models()
      req(length(mods) > 0)
      use_formula <- identical(input$summary_plot_label, "formula")
      model_names <- names(mods)
      labels <- if (use_formula) model_names else model_m_labels(model_names)
      label_map <- setNames(labels, model_names)

      rows <- lapply(model_names, function(mn) {
        m <- mods[[mn]]
        mf <- model.frame(m)
        actual <- mf[[1]]
        fitted_vals <- fitted(m)
        resids <- residuals(m)
        resp_name <- names(mf)[1]
        n <- length(actual)
        row_ids <- if (ROW_ID_COL %in% names(rv$data))
                     rv$data[[ROW_ID_COL]][seq_len(n)] else seq_len(n)
        data.frame(
          Model = label_map[[mn]], Actual = actual, Predicted = fitted_vals,
          row_id = row_ids,
          hover = paste0(resp_name, ": ", round(actual, 3),
                         "\nPredicted: ", round(fitted_vals, 3),
                         "\nResidual: ", round(resids, 4),
                         "\nObs #: ", row_ids),
          stringsAsFactors = FALSE)
      })
      df_avp <- do.call(rbind, rows)
      df_avp$Model <- factor(df_avp$Model, levels = labels)
      lims <- range(c(df_avp$Actual, df_avp$Predicted), na.rm = TRUE)
      avp_col <- default_col()
      p <- ggplot(df_avp, aes(x = Predicted, y = Actual, text = hover,
                               key = row_id)) +
        geom_point(alpha = 0.6, colour = avp_col) +
        geom_abline(slope = 1, intercept = 0, colour = "red", linetype = "dashed") +
        facet_wrap(~ Model, nrow = 1) +
        coord_cartesian(xlim = lims, ylim = lims) +
        labs(x = "Predicted", y = "Actual") +
        theme_app()
      ggplotly(p, tooltip = "text", source = SEL_SOURCE) %>%
        apply_selection_style(df_avp$row_id, rv$selected_obs)
    })

    # ── RMSE / R^2 summary table ─────────────────────────────────────────
    output$rmse_table <- DT::renderDataTable({
      req(length(active_models()) > 0)
      smry <- rmse_summary(active_models())
      smry <- cbind(data.frame(`#` = model_m_labels(smry$Model), check.names = FALSE), smry)
      dt_table(smry, rownames=FALSE,
               options=list(pageLength=20), dom="Bt") |>
        DT::formatRound(c("RMSE","R2","Adj_R2","PRESS_R2"), digits=4) |>
        DT::formatRound(c("AIC","BIC"), digits=2) |>
        DT::formatRound("PRESS_RMSE", digits=4)
    })

    # ── Lack of Fit test (cross-model) ───────────────────────────────────
    output$lof_table <- DT::renderDataTable({
      req(length(active_models()) > 0)
      mods <- active_models()
      saturated <- names(mods)[sapply(mods, function(m) df.residual(m) == 0)]
      testable  <- mods[setdiff(names(mods), saturated)]
      if (length(testable) == 0) {
        return(DT::datatable(data.frame(
          Message = "No Lack of Fit results. All selected models are saturated (0 residual df).")))
      }
      model_names <- names(testable)
      show <- input$lof_show %||% "p"

      lof_rows <- lapply(model_names, function(mn) {
        m <- testable[[mn]]
        tryCatch({
          mf <- model.frame(m)
          resp <- mf[[1]]
          resids <- residuals(m)

          pred_cols <- mf[, -1, drop = FALSE]
          group_key <- if (ncol(pred_cols) > 0) {
            apply(pred_cols, 1, function(row) paste(row, collapse = "|"))
          } else {
            rep("all", nrow(mf))
          }
          groups <- split(seq_along(resp), group_key)

          rep_groups <- groups[sapply(groups, length) > 1]
          if (length(rep_groups) == 0) {
            return(data.frame(Source = c("Lack of Fit", "Pure Error"),
                              Model = mn, Note = "No replicates",
                              SS = NA_real_, df_val = NA_integer_,
                              MS = NA_real_, F_value = NA_real_,
                              p_value = NA_real_,
                              stringsAsFactors = FALSE))
          }
          ss_pe <- sum(sapply(rep_groups, function(idx) {
            sum((resp[idx] - mean(resp[idx]))^2)
          }))
          df_pe <- sum(sapply(rep_groups, length) - 1)

          ss_res <- sum(resids^2)
          df_res <- df.residual(m)

          ss_lof <- ss_res - ss_pe
          df_lof <- df_res - df_pe

          if (df_lof <= 0) {
            return(data.frame(Source = c("Lack of Fit", "Pure Error"),
                              Model = mn, Note = "Saturated model",
                              SS = c(ss_lof, ss_pe),
                              df_val = c(df_lof, df_pe),
                              MS = NA_real_, F_value = NA_real_,
                              p_value = NA_real_,
                              stringsAsFactors = FALSE))
          }

          ms_lof <- ss_lof / df_lof
          ms_pe  <- if (df_pe > 0) ss_pe / df_pe else NA_real_
          f_lof  <- if (!is.na(ms_pe) && ms_pe > 0) ms_lof / ms_pe else NA_real_
          p_lof  <- if (!is.na(f_lof) && df_pe > 0)
                      pf(f_lof, df_lof, df_pe, lower.tail = FALSE) else NA_real_

          data.frame(
            Source = c("Lack of Fit", "Pure Error"),
            Model = mn, Note = "",
            SS = c(ss_lof, ss_pe),
            df_val = c(df_lof, df_pe),
            MS = c(ms_lof, ms_pe),
            F_value = c(f_lof, NA_real_),
            p_value = c(p_lof, NA_real_),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          data.frame(Source = c("Lack of Fit", "Pure Error"),
                     Model = mn, Note = paste("Error:", e$message),
                     SS = NA_real_, df_val = NA_integer_,
                     MS = NA_real_, F_value = NA_real_,
                     p_value = NA_real_,
                     stringsAsFactors = FALSE)
        })
      })

      long <- do.call(rbind, lof_rows)
      notes <- unique(long$Note[nchar(long$Note) > 0])

      sources <- c("Lack of Fit", "Pure Error")
      wide <- data.frame(Source = sources, stringsAsFactors = FALSE)
      ordered_cols <- "Source"
      for (mn in model_names) {
        sub <- long[long$Model == mn, , drop = FALSE]
        note <- unique(sub$Note[nchar(sub$Note) > 0])
        for (src in sources) {
          row_idx <- which(wide$Source == src)
          src_row <- sub[sub$Source == src, , drop = FALSE]
          if (nrow(src_row) == 0) next
          if ("df" %in% show) wide[row_idx, paste0(mn, "_df")]  <- src_row$df_val
          if ("SS" %in% show) wide[row_idx, paste0(mn, "_SS")]  <- src_row$SS
          if ("MS" %in% show) wide[row_idx, paste0(mn, "_MS")]  <- src_row$MS
          if ("F" %in% show)  wide[row_idx, paste0(mn, "_F")]   <- src_row$F_value
          if ("p" %in% show)  wide[row_idx, paste0(mn, "_p")]   <- src_row$p_value
        }
        wide[1, paste0(mn, "_Note")] <- if (length(note) > 0) note[1] else ""
        wide[2, paste0(mn, "_Note")] <- ""
        ordered_cols <- c(ordered_cols, paste0(mn, "_Note"))
        if ("df" %in% show) ordered_cols <- c(ordered_cols, paste0(mn, "_df"))
        if ("SS" %in% show) ordered_cols <- c(ordered_cols, paste0(mn, "_SS"))
        if ("MS" %in% show) ordered_cols <- c(ordered_cols, paste0(mn, "_MS"))
        if ("F" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_F"))
        if ("p" %in% show)  ordered_cols <- c(ordered_cols, paste0(mn, "_p"))
      }
      ordered_cols <- intersect(ordered_cols, names(wide))
      wide <- wide[, ordered_cols, drop = FALSE]

      if (length(model_names) == 1) {
        prefix <- paste0(model_names[1], "_")
        names(wide) <- sub(prefix, "", names(wide), fixed = TRUE)
      }

      container_arg <- NULL
      if (length(model_names) > 1) {
        stat_labels <- c("Note")
        if ("df" %in% show) stat_labels <- c(stat_labels, "df")
        if ("SS" %in% show) stat_labels <- c(stat_labels, "SS")
        if ("MS" %in% show) stat_labels <- c(stat_labels, "MS")
        if ("F" %in% show)  stat_labels <- c(stat_labels, "F")
        if ("p" %in% show)  stat_labels <- c(stat_labels, "p")
        n_stats <- length(stat_labels)
        if (n_stats > 0) {
          sub_cells <- unlist(lapply(model_names, function(mn) {
            lapply(stat_labels, function(s) tags$th(s))
          }), recursive = FALSE)
          container_arg <- htmltools::withTags(table(
            class = "display",
            thead(
              tr(
                th(rowspan = 2, "Source"),
                lapply(model_names, function(mn) th(colspan = n_stats, style = "text-align:center;", mn))
              ),
              do.call(tr, sub_cells)
            )
          ))
        }
      }

      p_cols  <- grep("_p$|^p$", names(wide), value = TRUE)
      f_cols  <- grep("_F$|^F$", names(wide), value = TRUE)
      ss_cols <- grep("_SS$|^SS$", names(wide), value = TRUE)
      ms_cols <- grep("_MS$|^MS$", names(wide), value = TRUE)

      # Pre-format p-value columns
      wide <- format_pvalue_dt(wide, p_cols)
      shadow_p_cols <- paste0("._p_", p_cols)
      shadow_p_idx <- which(names(wide) %in% shadow_p_cols) - 1
      hide_defs <- if (length(shadow_p_idx) > 0) list(list(visible = FALSE, targets = shadow_p_idx)) else list()

      dt <- dt_table(wide, rownames = FALSE,
                      options = list(pageLength = 10, dom = "t", columnDefs = hide_defs),
                      container = container_arg, filter = "none")
      for (pc in p_cols) {
        shadow <- paste0("._p_", pc)
        dt <- DT::formatStyle(dt, pc,
                 backgroundColor = DT::styleInterval(ALPHA_DEFAULT, c(PVALUE_GREEN, "white")),
                 fontWeight      = DT::styleInterval(ALPHA_DEFAULT, c("bold", "normal")),
                 valueColumns    = shadow)
      }
      if (length(f_cols) > 0)  dt <- DT::formatRound(dt, f_cols, digits = 3)
      if (length(ss_cols) > 0) dt <- DT::formatRound(dt, ss_cols, digits = 3)
      if (length(ms_cols) > 0) dt <- DT::formatRound(dt, ms_cols, digits = 3)
      dt
    })

    # ── Dynamic Coefficient Interpretation Guide ─────────────────────────
    output$coef_interpretation_ui <- renderUI({
      req(length(active_models()) > 0, rv$data)
      parts <- list()
      # Dynamic intercept explanation
      intercept_desc <- "grand mean (effects coding)"
      all_numeric <- c(all_covariates(), factors_()[vapply(factors_(), function(cn)
        (rv$col_types[[cn]] %||% "Factor") == "Numeric", logical(1))])
      at_parts <- character()
      for (cn in all_numeric) {
        tr <- rv$transforms[[cn]] %||% "none"
        if (tr == "centre" || tr == "centre_scale") {
          at_parts <- c(at_parts, paste0(cn, " at mean"))
        } else if (tr == "coding") {
          at_parts <- c(at_parts, paste0(cn, " at midpoint"))
        } else {
          at_parts <- c(at_parts, paste0(cn, " at 0"))
        }
      }
      if (length(at_parts) > 0)
        intercept_desc <- paste0(intercept_desc, "; with ", paste(at_parts, collapse = ", "))
      parts <- c(parts, list(tags$b("Intercept"), paste0(" = ", intercept_desc, ". ")))
      facs <- factors_()
      if (length(facs) > 0) {
        parts <- c(parts, list(
          tags$b("Factors:"), " sum-to-zero contrasts \u2014 each coefficient is the deviation ",
          "from the grand mean; last level is derived as negative sum of others. "
        ))
      }
      covs <- all_covariates()
      num_factors <- facs[vapply(facs, function(cn) {
        (rv$col_types[[cn]] %||% "Factor") == "Numeric"
      }, logical(1))]
      all_numeric <- c(covs, num_factors)
      if (length(all_numeric) > 0) {
        centred  <- character()
        centred_scaled <- character()
        coded    <- character()
        raw      <- character()
        for (cn in all_numeric) {
          tr <- rv$transforms[[cn]] %||% "none"
          if (tr == "centre") centred <- c(centred, cn)
          else if (tr == "centre_scale") centred_scaled <- c(centred_scaled, cn)
          else if (tr == "coding") coded <- c(coded, cn)
          else raw <- c(raw, cn)
        }
        if (length(coded) > 0) {
          parts <- c(parts, list(
            tags$b("Coded (\u22121/+1):"), " ",
            paste(coded, collapse = ", "),
            " \u2014 coefficient = half the change from low to high. "
          ))
        }
        if (length(centred) > 0) {
          parts <- c(parts, list(
            tags$b("Centred:"), " ",
            paste(centred, collapse = ", "),
            " \u2014 coefficient = change per unit; intercept at mean. "
          ))
        }
        if (length(centred_scaled) > 0) {
          parts <- c(parts, list(
            tags$b("Centred & Scaled (range/2):"), " ",
            paste(centred_scaled, collapse = ", "),
            " \u2014 coefficient = change per half-range unit; intercept at mean. "
          ))
        }
        if (length(raw) > 0) {
          parts <- c(parts, list(
            tags$b("Unscaled:"), " ",
            paste(raw, collapse = ", "),
            " \u2014 coefficient = change per 1-unit increase (intercept at 0 \u2014 consider centring). "
          ))
        }
      }
      if (length(rv$alias_labels) > 0) {
        n_aliased <- length(rv$alias_labels)
        parts <- c(parts, list(
          tags$br(),
          icon("triangle-exclamation", style = "color: #e65100;"),
          tags$span(style = "color: #e65100;",
            paste0(" ", n_aliased, " term(s) confounded \u2014 highlighted rows show aliased pairs."))
        ))
      }
      p(class = "text-muted", style = "line-height: 1.6;", do.call(tagList, parts))
    })

    # ── Coefficient Term Filter ──────────────────────────────────────────
    output$coef_term_filter_ui <- renderUI({
      req(length(active_models()) > 0)
      cf <- coef_table(active_models(), rv$transforms, rv$coding_values)
      if (is.null(cf) || nrow(cf) == 0) return(NULL)
      if (length(rv$alias_labels) > 0 && "Term" %in% names(cf))
        cf$Term <- relabel_alias_terms(cf$Term, rv$alias_labels)
      all_terms <- unique(cf$Term[cf$Term != "(Intercept)"])
      if (length(all_terms) == 0) return(NULL)
      checkboxGroupInput(ns("coef_term_filter"), "Terms to display:",
                         choices = all_terms, selected = all_terms, inline = TRUE)
    })

    # ── Coefficient Interval Plot ────────────────────────────────────────
    output$coef_plot <- renderPlotly({
      req(length(active_models()) > 0)
      cf <- coef_table(active_models(), rv$transforms, rv$coding_values)
      if (is.null(cf) || nrow(cf) == 0) return(plotly_empty())
      if (length(rv$alias_labels) > 0 && "Term" %in% names(cf))
        cf$Term <- relabel_alias_terms(cf$Term, rv$alias_labels)
      cf <- cf[cf$Term != "(Intercept)", , drop = FALSE]
      if (nrow(cf) == 0) return(plotly_empty())
      sel_terms <- input$coef_term_filter
      if (!is.null(sel_terms) && length(sel_terms) > 0)
        cf <- cf[cf$Term %in% sel_terms, , drop = FALSE]
      if (nrow(cf) == 0) return(plotly_empty())
      formula_order <- unique(cf$Term)
      cf$Term <- factor(cf$Term, levels = rev(formula_order))
      cf$ci_lower <- cf$Estimate - qt(0.975, sapply(
        active_models()[cf$Model], df.residual)) * cf$Std.Error
      cf$ci_upper <- cf$Estimate + qt(0.975, sapply(
        active_models()[cf$Model], df.residual)) * cf$Std.Error
      cf$Significant <- ifelse(cf$ci_lower > 0 | cf$ci_upper < 0, "Yes", "No")
      n_models <- length(unique(cf$Model))
      n_terms  <- length(unique(cf$Term))
      plot_h   <- max(300, n_terms * 22 * n_models + 60)
      p <- ggplot(cf, aes(x = Estimate, y = Term, colour = Model)) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = "#999999", linewidth = 0.5) +
        geom_point(size = 2.5, position = position_dodge(0.5)) +
        geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper),
                       height = 0.2, linewidth = 0.6,
                       position = position_dodge(0.5)) +
        cat_scale_colour()(name = "Model") +
        labs(x = "Estimate (95% CI)", y = NULL, colour = "Model") +
        theme_app()
      ggplotly(p, height = plot_h) %>%
        layout(margin = list(l = 120))
    })

    # ── Coefficients Table ───────────────────────────────────────────────
    output$coef_table <- DT::renderDataTable({
      req(length(active_models()) > 0)
      cf <- coef_table(active_models(), rv$transforms, rv$coding_values)
      if (is.null(cf) || nrow(cf) == 0)
        return(DT::datatable(data.frame(Message = "No coefficients available.")))
      orig_terms <- cf$Term
      aliased_orig <- names(rv$alias_labels)
      cf$.is_aliased <- orig_terms %in% aliased_orig
      cf$Confounding <- vapply(orig_terms, function(t) {
        if (t %in% aliased_orig) {
          lbl <- rv$alias_labels[[t]]
          m <- regmatches(lbl, regexpr("\\[.*\\]", lbl))
          if (length(m) > 0) return(m) else return("")
        }
        ""
      }, character(1), USE.NAMES = FALSE)
      if (length(rv$alias_labels) > 0 && "Term" %in% names(cf))
        cf$Term <- relabel_alias_terms(cf$Term, rv$alias_labels)
      if (nrow(rv$vif_df) > 0) {
        vif_long <- rv$vif_df
        vif_model_cols <- setdiff(names(vif_long), "Term")
        vif_lookup <- do.call(rbind, lapply(vif_model_cols, function(mc) {
          data.frame(Term = vif_long$Term, Model = mc, VIF = vif_long[[mc]],
                     stringsAsFactors = FALSE)
        }))
        cf <- merge(cf, vif_lookup, by = c("Term", "Model"), all.x = TRUE)
      } else {
        cf$VIF <- NA_real_
      }
      col_order <- setdiff(names(cf), c("Confounding", ".is_aliased"))
      cf <- cf[, c(col_order, "Confounding", ".is_aliased"), drop = FALSE]
      aliased_col_idx <- which(names(cf) == ".is_aliased") - 1
      # Pre-format p-value column
      cf <- format_pvalue_dt(cf, "p.value")
      shadow_p_idx <- which(names(cf) == "._p_p.value") - 1
      non_p_num_cols <- c("Estimate", "Std.Error", "t.value")
      dt <- dt_table(cf, rownames = FALSE,
                      options = list(
                        pageLength = 30,
                        columnDefs = list(
                          list(visible = FALSE, targets = c(aliased_col_idx, shadow_p_idx))
                        )
                      ))
      dt <- DT::formatRound(dt, non_p_num_cols, digits = 4)
      if ("VIF" %in% names(cf)) {
        dt <- DT::formatRound(dt, "VIF", digits = 2)
        dt <- DT::formatStyle(dt, "VIF",
                 backgroundColor = DT::styleInterval(
                   c(5, 10),
                   c("white", VIF_AMBER, VIF_RED)),
                 fontWeight = DT::styleInterval(c(5), c("normal", "bold")))
      }
      dt <- DT::formatStyle(dt, "p.value",
               backgroundColor = DT::styleInterval(ALPHA_DEFAULT, c(PVALUE_GREEN, "white")),
               valueColumns = "._p_p.value")
      dt <- DT::formatStyle(dt, ".is_aliased",
               target = "row",
               backgroundColor = DT::styleEqual(TRUE, ALIASED_BG))
      dt
    })

    # ── Effects Plot (LS Means + Covariate Effects) ──────────────────────
    output$effects_plot <- renderPlotly({
      req(length(active_models()) > 0, input$effects_term)
      term <- input$effects_term
      factor_cols <- factors_()
      term_parts  <- strsplit(term, ":")[[1]]
      is_factor_term <- all(term_parts %in% factor_cols) &&
        all(sapply(term_parts, function(cn) {
          (rv$col_types[[cn]] %||% "Factor") == "Factor"
        }))

      if (is_factor_term) {
        spec <- term
        spec_terms <- strsplit(spec, ":")[[1]]
        is_interaction <- length(spec_terms) > 1

        if (is_interaction) {
          x_fac  <- spec_terms[1]
          tr_fac <- spec_terms[2]
          int_dfs <- lapply(names(active_models()), function(mn) {
            m <- active_models()[[mn]]
            em <- tryCatch(
              suppressMessages(emmeans::emmeans(m, specs = spec_terms)),
              error = function(e) NULL)
            if (is.null(em)) return(NULL)
            df <- as.data.frame(summary(em))
            df$model <- mn
            df
          })
          int_df <- bind_rows(Filter(Negate(is.null), int_dfs))
          if (nrow(int_df) == 0) return(plotly_empty())
          int_df[[x_fac]]  <- factor(int_df[[x_fac]])
          int_df[[tr_fac]] <- factor(int_df[[tr_fac]])
          y_col  <- if ("emmean" %in% names(int_df)) "emmean" else names(int_df)[3]
          has_ci <- all(c("lower.CL", "upper.CL") %in% names(int_df))

          if (input$effects_view == "Overlay") {
            n_models <- length(unique(int_df$model))
            p <- ggplot(int_df, aes_string(x = x_fac, y = y_col,
                                            colour = tr_fac, group = tr_fac)) +
              geom_point(size = 3) +
              geom_line(linewidth = 0.8)
            if (has_ci)
              p <- p + geom_errorbar(aes_string(ymin = "lower.CL", ymax = "upper.CL"),
                                      width = 0.15, linewidth = 0.5)
            if (isTRUE(input$effects_show_data)) {
              pr_list <- lapply(names(active_models()), function(mn) {
                get_partial_residuals_factor(active_models()[[mn]], spec, mn)
              })
              pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
              if (nrow(pr_df) > 0) {
                pr_df[[x_fac]]  <- factor(pr_df[[x_fac]])
                pr_df[[tr_fac]] <- factor(pr_df[[tr_fac]])
                p <- p + geom_jitter(data = pr_df,
                          aes_string(x = x_fac, y = "adjusted_y", colour = tr_fac),
                          alpha = 0.25, size = 1.5, width = 0.12, inherit.aes = FALSE)
              }
            }
            if (n_models > 1) p <- p + facet_wrap(~model)
            resp_name <- tryCatch(names(model.frame(active_models()[[1]]))[1],
                                  error = function(e) "Response")
            p <- p + cat_scale_colour()(name = tr_fac) +
              labs(title = paste("Interaction:", x_fac, "\u00d7", tr_fac),
                   x = x_fac, y = paste("LS Mean of", resp_name), colour = tr_fac,
                   subtitle = "Non-parallel lines suggest an interaction effect") +
              theme_app()
          } else {
            p <- ggplot(int_df, aes_string(x = x_fac, y = y_col,
                                            colour = tr_fac, group = tr_fac)) +
              geom_point(size = 3) +
              geom_line(linewidth = 0.8)
            if (has_ci)
              p <- p + geom_errorbar(aes_string(ymin = "lower.CL", ymax = "upper.CL"),
                                      width = 0.15, linewidth = 0.5)
            if (isTRUE(input$effects_show_data)) {
              pr_list <- lapply(names(active_models()), function(mn) {
                get_partial_residuals_factor(active_models()[[mn]], spec, mn)
              })
              pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
              if (nrow(pr_df) > 0) {
                pr_df[[x_fac]]  <- factor(pr_df[[x_fac]])
                pr_df[[tr_fac]] <- factor(pr_df[[tr_fac]])
                p <- p + geom_jitter(data = pr_df,
                          aes_string(x = x_fac, y = "adjusted_y", colour = tr_fac),
                          alpha = 0.25, size = 1.5, width = 0.12, inherit.aes = FALSE)
              }
            }
            resp_name <- tryCatch(names(model.frame(active_models()[[1]]))[1],
                                  error = function(e) "Response")
            p <- p + facet_wrap(~model) + cat_scale_colour()(name = tr_fac) +
              labs(title = paste("Interaction:", x_fac, "\u00d7", tr_fac, "(faceted)"),
                   x = x_fac, y = paste("LS Mean of", resp_name), colour = tr_fac,
                   subtitle = "Non-parallel lines suggest an interaction effect") +
              theme_app()
          }
          ggplotly(p)

        } else {
          ls_list <- lapply(names(active_models()), function(mn) {
            get_lsmeans_df(active_models()[[mn]], spec, mn)
          })
          ls_df <- bind_rows(Filter(Negate(is.null), ls_list))
          if (nrow(ls_df) == 0) return(plotly_empty())
          x_col  <- spec_terms[1]
          x_label <- spec_terms[1]
          y_col  <- if ("emmean" %in% names(ls_df)) "emmean" else names(ls_df)[2]
          has_ci <- all(c("lower.CL","upper.CL") %in% names(ls_df))
          if (has_ci) {
            ls_df$ci_lower <- ls_df$lower.CL
            ls_df$ci_upper <- ls_df$upper.CL
          }
          if (input$effects_view == "Overlay") {
            p <- ggplot(ls_df, aes_string(x=x_col, y=y_col, colour="model", group="model")) +
              geom_point(size=3, position=position_dodge(0.3))
            if (has_ci)
              p <- p + geom_errorbar(aes_string(ymin="ci_lower", ymax="ci_upper"),
                width=0.2, position=position_dodge(0.3))
            if (isTRUE(input$effects_show_data)) {
              pr_list <- lapply(names(active_models()), function(mn) {
                get_partial_residuals_factor(active_models()[[mn]], spec, mn)
              })
              pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
              if (nrow(pr_df) > 0)
                p <- p + geom_point(data=pr_df, aes_string(x=spec_terms[1], y="adjusted_y", colour="model"),
                                    alpha=0.3, size=1.5,
                                    position=position_jitterdodge(jitter.width=0.15, dodge.width=0.3),
                                    inherit.aes=FALSE)
            }
            p <- p + cat_scale_colour()(name = "Model") +
              labs(title=paste("LS Means \u2014",spec), x=x_label, y="LS Mean", colour="Model",
                   subtitle=if(isTRUE(input$effects_show_data)) "Points adjusted for other model terms" else NULL) +
              theme_app()
          } else {
            p <- ggplot(ls_df, aes_string(x=x_col, y=y_col, colour="model")) +
              geom_point(size=3)
            if (has_ci)
              p <- p + geom_errorbar(aes_string(ymin="ci_lower", ymax="ci_upper"), width=0.2)
            if (isTRUE(input$effects_show_data)) {
              pr_list <- lapply(names(active_models()), function(mn) {
                get_partial_residuals_factor(active_models()[[mn]], spec, mn)
              })
              pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
              if (nrow(pr_df) > 0)
                p <- p + geom_jitter(data=pr_df, aes_string(x=spec_terms[1], y="adjusted_y"),
                                     alpha=0.3, size=1.5, width=0.15, colour="grey40",
                                     inherit.aes=FALSE)
            }
            p <- p + facet_wrap(~model) + cat_scale_colour()(name = "Model") +
              labs(title=paste("LS Means \u2014",spec,"(faceted)"), x=x_label, y="LS Mean",
                   subtitle=if(isTRUE(input$effects_show_data)) "Points adjusted for other model terms" else NULL) +
              theme_app() + theme(legend.position="none")
          }
          ggplotly(p)
        }

      } else {
        cov <- term
        relevant <- Filter(function(m) {
          mf <- tryCatch(model.frame(m), error=function(e) NULL)
          if (is.null(mf)) return(FALSE)
          cov %in% names(mf)
        }, active_models())
        if (length(relevant) == 0) {
          return(plotly_empty() %>% layout(title=plotly_title("No fitted models contain this covariate.", size=12)))
        }
        eff_list <- lapply(names(relevant), function(mn) {
          get_effects_df(relevant[[mn]], cov, mn)
        })
        eff_df <- bind_rows(Filter(Negate(is.null), eff_list))
        if (nrow(eff_df) == 0) return(plotly_empty())
        resp_name <- names(model.frame(relevant[[1]]))[1]
        p <- ggplot(eff_df, aes(x=x, y=fit, colour=model, fill=model)) +
          geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.15, colour="transparent", linewidth=0) +
          geom_line(linewidth=1)
        if (isTRUE(input$effects_show_data)) {
          pr_list <- lapply(names(relevant), function(mn) {
            get_partial_residuals_covariate(relevant[[mn]], cov, mn)
          })
          pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
          if (nrow(pr_df) > 0)
            p <- p + geom_point(data=pr_df, aes(x=x, y=adjusted_y, colour=model),
                                alpha=0.4, size=1.5, inherit.aes=FALSE)
        }
        n_models <- length(unique(eff_df$model))
        if (input$effects_view == "Faceted" && n_models > 1) {
          p <- p + facet_wrap(~model) +
            guides(colour = "none", fill = "none")
        }
        p <- p + cat_scale_colour()(name = "Model") +
          cat_scale_fill()(name = "Model") +
          labs(title=paste("Partial effect of", cov, "on", resp_name),
               x=cov, y=paste("Predicted", resp_name),
               subtitle=if(isTRUE(input$effects_show_data))
                 "Points adjusted for other model terms" else
                 "Factors at reference level; other covariates at their mean",
               colour="Model") +
          guides(fill="none") +
          theme_app()
        ggplotly(p)
      }
    })

    # ── Effects Table ────────────────────────────────────────────────────
    output$effects_table <- DT::renderDataTable({
      req(length(active_models()) > 0, input$effects_term)
      term <- input$effects_term
      factor_cols <- factors_()
      term_parts  <- strsplit(term, ":")[[1]]
      is_factor_term <- all(term_parts %in% factor_cols) &&
        all(sapply(term_parts, function(cn) {
          (rv$col_types[[cn]] %||% "Factor") == "Factor"
        }))
      if (is_factor_term) {
        spec <- term
        is_interaction <- length(term_parts) > 1
        if (is_interaction) {
          int_dfs <- lapply(names(active_models()), function(mn) {
            m <- active_models()[[mn]]
            em <- tryCatch(
              suppressMessages(emmeans::emmeans(m, specs = term_parts)),
              error = function(e) NULL)
            if (is.null(em)) return(NULL)
            df <- as.data.frame(summary(em))
            df$model <- mn
            df
          })
          ls_df <- bind_rows(Filter(Negate(is.null), int_dfs))
        } else {
          ls_list <- lapply(names(active_models()), function(mn) {
            get_lsmeans_df(active_models()[[mn]], spec, mn)
          })
          ls_df <- bind_rows(Filter(Negate(is.null), ls_list))
        }
        if (nrow(ls_df) == 0)
          return(DT::datatable(data.frame(Message = "No LS means available.")))
        num_cols <- sapply(ls_df, is.numeric)
        ls_df[num_cols] <- lapply(ls_df[num_cols], function(x) round(x, 4))
        dt_table(ls_df, rownames = FALSE, caption = paste("LS Means \u2014", spec),
                 options = list(pageLength = 20))
      } else {
        return(NULL)
      }
    })

    # ── Leverage Plots (Added-Variable Plots) ────────────────────────────
    output$leverage_plot_combined <- renderPlotly({
      am <- active_models()
      req(length(am) > 0, input$effects_term)
      term <- input$effects_term
      panel_by <- input$leverage_panel_by %||% "none"
      all_dd <- list()
      for (mn in names(am)) {
        m <- am[[mn]]
        tl <- attr(terms(m), "term.labels")
        if (!(term %in% tl)) next
        lev_data <- tryCatch(leverage_plot_data(m), error = function(e) list())
        dd <- lev_data[[term]]
        if (!is.null(dd) && nrow(dd) > 0) {
          dd$Model <- mn
          fit <- tryCatch(lm(y_residual ~ x_residual, data = dd), error = function(e) NULL)
          if (!is.null(fit)) dd$fitted_line <- fitted(fit) else dd$fitted_line <- NA_real_
          # Attach panel-by variable from model frame
          if (panel_by != "none" && panel_by %in% names(rv$data)) {
            mf <- model.frame(m)
            if (nrow(dd) == nrow(mf)) {
              # Use model frame rownames to index back into rv$data
              mf_rows <- as.integer(rownames(mf))
              if (all(!is.na(mf_rows)) && max(mf_rows) <= nrow(rv$data)) {
                dd$.panel <- as.factor(rv$data[[panel_by]][mf_rows])
              } else {
                # Fallback: direct assignment if model frame rows match
                dd$.panel <- as.factor(mf[[panel_by]])
              }
            } else if (panel_by %in% names(model.frame(m))) {
              dd$.panel <- as.factor(model.frame(m)[[panel_by]])
            }
          }
          all_dd[[mn]] <- dd
        }
      }
      if (length(all_dd) == 0)
        return(plotly_empty() %>%
          layout(title = plotly_title(paste("Leverage plot \u2014", term, "\nNot available"), size=12)))
      combined <- do.call(rbind, all_dd)
      rownames(combined) <- NULL
      n_models <- length(unique(combined$Model))
      cols <- cat_palette()(max(n_models, 2))
      p <- ggplot(combined, aes(x = x_residual, y = y_residual, colour = Model)) +
        geom_point(alpha = 0.6, size = 2) +
        geom_line(aes(y = fitted_line), linewidth = 0.9) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
        scale_colour_manual(values = setNames(cols[seq_len(n_models)], unique(combined$Model))) +
        labs(title = paste("Leverage plot \u2014", term),
             x = paste(term, "| others"), y = "Response | others") +
        theme_app()
      if (panel_by != "none" && ".panel" %in% names(combined))
        p <- p + facet_wrap(~ .panel)
      ggplotly(p)
    })

    # ── Side-by-Side Effects + Leverage ─────────────────────────────────

    # Effects plot for SBS tab — mirrors effects_plot
    output$effects_plot_sbs <- renderPlotly({
      # Trigger same reactive dependencies
      am <- active_models()
      req(length(am) > 0, input$effects_term)
      # Reuse: read the effects_plot output by duplicating the logic
      # This is a lightweight proxy — the actual computation runs in effects_plot
      term <- input$effects_term
      factor_cols <- factors_()
      term_parts  <- strsplit(term, ":")[[1]]
      is_factor_term <- all(term_parts %in% factor_cols) &&
        all(sapply(term_parts, function(cn) (rv$col_types[[cn]] %||% "Factor") == "Factor"))
      if (is_factor_term) {
        spec <- term; spec_terms <- strsplit(spec, ":")[[1]]
        is_interaction <- length(spec_terms) > 1
        if (is_interaction) {
          x_fac <- spec_terms[1]; tr_fac <- spec_terms[2]
          int_dfs <- lapply(names(am), function(mn) {
            em <- tryCatch(suppressMessages(emmeans::emmeans(am[[mn]], specs=spec_terms)), error=function(e)NULL)
            if (is.null(em)) return(NULL)
            df <- as.data.frame(summary(em)); df$model <- mn; df
          })
          int_df <- bind_rows(Filter(Negate(is.null), int_dfs))
          if (nrow(int_df) == 0) return(plotly_empty())
          int_df[[x_fac]] <- factor(int_df[[x_fac]]); int_df[[tr_fac]] <- factor(int_df[[tr_fac]])
          y_col <- if ("emmean" %in% names(int_df)) "emmean" else names(int_df)[3]
          resp_name <- tryCatch(names(model.frame(am[[1]]))[1], error=function(e) "Response")
          p <- ggplot(int_df, aes_string(x=x_fac, y=y_col, colour=tr_fac, group=tr_fac)) +
            geom_point(size=3) + geom_line(linewidth=0.8) +
            cat_scale_colour()(name=tr_fac) +
            labs(title=paste("Interaction:", x_fac, "\u00d7", tr_fac), x=x_fac,
                 y=paste("LS Mean of", resp_name)) + theme_app()
          if (length(unique(int_df$model)) > 1) p <- p + facet_wrap(~model)
          return(ggplotly(p))
        }
        ls_list <- lapply(names(am), function(mn) get_lsmeans_df(am[[mn]], spec, mn))
        ls_df <- bind_rows(Filter(Negate(is.null), ls_list))
        if (nrow(ls_df) == 0) return(plotly_empty())
        x_col <- spec_terms[1]; y_col <- if ("emmean" %in% names(ls_df)) "emmean" else names(ls_df)[2]
        p <- ggplot(ls_df, aes_string(x=x_col, y=y_col, colour="model", group="model")) +
          geom_point(size=3, position=position_dodge(0.3)) + cat_scale_colour()(name="Model") +
          labs(title=paste("LS Means \u2014", spec), x=x_col, y="LS Mean") + theme_app()
        return(ggplotly(p))
      }
      # Covariate
      cov <- term
      relevant <- Filter(function(m) cov %in% names(model.frame(m)), am)
      if (length(relevant) == 0) return(plotly_empty())
      eff_list <- lapply(names(relevant), function(mn) get_effects_df(relevant[[mn]], cov, mn))
      eff_df <- bind_rows(Filter(Negate(is.null), eff_list))
      if (nrow(eff_df) == 0) return(plotly_empty())
      resp_name <- names(model.frame(relevant[[1]]))[1]
      p <- ggplot(eff_df, aes(x=x, y=fit, colour=model, fill=model)) +
        geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.15, colour="transparent") +
        geom_line(linewidth=1) + cat_scale_colour()(name="Model") + cat_scale_fill()(name="Model") +
        labs(title=paste("Partial effect of", cov), x=cov, y=paste("Predicted", resp_name)) +
        guides(fill="none") + theme_app()
      ggplotly(p)
    })

    # Leverage plot for SBS tab — mirrors leverage_plot_combined
    output$leverage_plot_sbs <- renderPlotly({
      am <- active_models()
      req(length(am) > 0, input$effects_term)
      term <- input$effects_term
      all_dd <- list()
      for (mn in names(am)) {
        m <- am[[mn]]
        tl <- attr(terms(m), "term.labels")
        if (!(term %in% tl)) next
        lev_data <- tryCatch(leverage_plot_data(m), error=function(e)list())
        dd <- lev_data[[term]]
        if (!is.null(dd) && nrow(dd) > 0) {
          dd$Model <- mn
          fit <- tryCatch(lm(y_residual ~ x_residual, data=dd), error=function(e)NULL)
          dd$fitted_line <- if (!is.null(fit)) fitted(fit) else NA_real_
          all_dd[[mn]] <- dd
        }
      }
      if (length(all_dd) == 0)
        return(plotly_empty() %>% layout(title=plotly_title(paste("Leverage \u2014", term, "\nNot available"), size=12)))
      combined <- do.call(rbind, all_dd); rownames(combined) <- NULL
      n_models <- length(unique(combined$Model))
      cols <- cat_palette()(max(n_models, 2))
      p <- ggplot(combined, aes(x=x_residual, y=y_residual, colour=Model)) +
        geom_point(alpha=0.6, size=2) + geom_line(aes(y=fitted_line), linewidth=0.9) +
        geom_hline(yintercept=0, linetype="dashed", colour="grey60") +
        scale_colour_manual(values=setNames(cols[seq_len(n_models)], unique(combined$Model))) +
        labs(title=paste("Leverage \u2014", term), x=paste(term, "| others"), y="Response | others") +
        theme_app()
      ggplotly(p)
    })

    # Effects table for SBS tab — mirrors effects_table
    output$effects_table_sbs <- DT::renderDataTable({
      # Delegate to the same data as effects_table by triggering the same logic
      req(length(active_models()) > 0, input$effects_term)
      term <- input$effects_term
      factor_cols <- factors_()
      term_parts <- strsplit(term, ":")[[1]]
      is_factor_term <- all(term_parts %in% factor_cols)
      if (!is_factor_term) return(NULL)
      spec <- term
      ls_list <- lapply(names(active_models()), function(mn) {
        get_lsmeans_df(active_models()[[mn]], spec, mn)
      })
      ls_df <- bind_rows(Filter(Negate(is.null), ls_list))
      if (nrow(ls_df) == 0) return(NULL)
      dt_table(ls_df, rownames=FALSE, options=list(pageLength=PAGE_LEN_DEFAULT))
    })

    # ── Contour / Surface Plots ──────────────────────────────────────────

    # Populate factor dropdowns from numeric factors in active models
    observe({
      mods <- active_models()
      req(length(mods) > 0)
      numeric_preds <- character(0)
      for (m in mods) {
        terms_m <- attr(terms(m), "term.labels")
        base_vars <- unique(unlist(lapply(terms_m, function(t) {
          t <- gsub("I\\(([^\\^]+)\\^[0-9]+\\)", "\\1", t)
          strsplit(t, ":")[[1]]
        })))
        for (v in base_vars) {
          if (v %in% names(rv$data) && is.numeric(rv$data[[v]])) {
            numeric_preds <- union(numeric_preds, v)
          }
        }
      }
      if (length(numeric_preds) >= 2) {
        updateSelectInput(session, "contour_x", choices = numeric_preds,
                          selected = numeric_preds[1])
        updateSelectInput(session, "contour_y", choices = numeric_preds,
                          selected = numeric_preds[2])
      }
    })

    # Hold-constant controls
    output$contour_hold_ui <- renderUI({
      mods <- active_models()
      req(length(mods) > 0, input$contour_x, input$contour_y)
      x_var <- input$contour_x
      y_var <- input$contour_y
      all_preds <- character(0)
      for (m in mods) {
        terms_m <- attr(terms(m), "term.labels")
        base_vars <- unique(unlist(lapply(terms_m, function(t) {
          t <- gsub("I\\(([^\\^]+)\\^[0-9]+\\)", "\\1", t)
          strsplit(t, ":")[[1]]
        })))
        all_preds <- union(all_preds, base_vars)
      }
      hold_vars <- setdiff(all_preds, c(x_var, y_var))
      if (length(hold_vars) == 0) return(NULL)
      controls <- lapply(hold_vars, function(v) {
        if (!v %in% names(rv$data)) return(NULL)
        vals <- rv$data[[v]]
        if (is.numeric(vals)) {
          column(3, sliderInput(ns(paste0("contour_hold_", v)), v,
                                min = round(min(vals, na.rm = TRUE), 3),
                                max = round(max(vals, na.rm = TRUE), 3),
                                value = round(mean(vals, na.rm = TRUE), 3),
                                step = round((max(vals, na.rm=TRUE) - min(vals, na.rm=TRUE))/20, 3)))
        } else {
          lvls <- levels(factor(vals))
          mid_lvl <- lvls[ceiling(length(lvls)/2)]
          column(3, selectInput(ns(paste0("contour_hold_", v)), v,
                                choices = lvls, selected = mid_lvl))
        }
      })
      fluidRow(
        tags$label("Hold-constant values:", class = "text-muted"),
        do.call(fluidRow, Filter(Negate(is.null), controls))
      )
    })

    # Render contour/surface plots -- one per active model
    output$contour_plots_ui <- renderUI({
      mods <- active_models()
      req(length(mods) > 0, input$contour_x, input$contour_y)
      req(input$contour_x != input$contour_y)

      plot_ids <- paste0("contour_plot_", seq_along(mods))
      lapply(seq_along(mods), function(i) {
        local({
          idx <- i
          nm <- names(mods)[idx]
          m <- mods[[idx]]

          # Helper: build prediction grid for this model
          build_contour_grid <- function() {
            x_var <- input$contour_x
            y_var <- input$contour_y
            grid_n <- input$contour_grid_n %||% 30
            x_vals <- rv$data[[x_var]]
            y_vals <- rv$data[[y_var]]
            req(is.numeric(x_vals), is.numeric(y_vals))
            x_seq <- seq(min(x_vals, na.rm = TRUE), max(x_vals, na.rm = TRUE), length.out = grid_n)
            y_seq <- seq(min(y_vals, na.rm = TRUE), max(y_vals, na.rm = TRUE), length.out = grid_n)
            grid_df <- expand.grid(setNames(list(x_seq, y_seq), c(x_var, y_var)))
            term_labels <- attr(terms(m), "term.labels")
            base_vars <- unique(unlist(lapply(term_labels, function(t) {
              t <- gsub("I\\(([^\\^]+)\\^[0-9]+\\)", "\\1", t)
              strsplit(t, ":")[[1]]
            })))
            hold_vars <- setdiff(base_vars, c(x_var, y_var))
            for (v in hold_vars) {
              hold_input <- input[[paste0("contour_hold_", v)]]
              if (!is.null(hold_input)) {
                grid_df[[v]] <- hold_input
              } else if (v %in% names(rv$data)) {
                vals <- rv$data[[v]]
                grid_df[[v]] <- if (is.numeric(vals)) mean(vals, na.rm = TRUE) else levels(factor(vals))[1]
              }
            }
            for (v in names(grid_df)) {
              if (v %in% names(rv$col_types) && rv$col_types[[v]] == "Factor")
                grid_df[[v]] <- factor(grid_df[[v]], levels = levels(factor(rv$data[[v]])))
            }
            for (v in names(grid_df)) {
              if (is.factor(grid_df[[v]]))
                contrasts(grid_df[[v]]) <- contr.sum(nlevels(grid_df[[v]]))
            }
            pred <- tryCatch(predict(m, newdata = grid_df), error = function(e) NULL)
            if (is.null(pred)) return(NULL)
            list(x_seq = x_seq, y_seq = y_seq, z_mat = matrix(pred, nrow = grid_n, ncol = grid_n))
          }

          # Contour-only or surface-only
          output[[plot_ids[idx]]] <- renderPlotly({
            ctype <- input$contour_type
            req(ctype %in% c("contour", "surface"))
            g <- build_contour_grid()
            if (is.null(g)) return(plotly_empty() %>% layout(title = plotly_title(paste(nm, "- prediction error"), size=12)))
            x_var <- input$contour_x; y_var <- input$contour_y
            if (identical(ctype, "surface")) {
              plot_ly(x = g$x_seq, y = g$y_seq, z = g$z_mat, type = "surface",
                      colorscale = "Viridis", showscale = TRUE) %>%
                layout(title = plotly_title(nm, size = 13),
                       scene = list(xaxis = list(title = x_var), yaxis = list(title = y_var),
                                    zaxis = list(title = "Predicted")))
            } else {
              plot_ly(x = g$x_seq, y = g$y_seq, z = g$z_mat, type = "contour",
                      colorscale = "Viridis", contours = list(showlabels = TRUE),
                      showscale = TRUE) %>%
                layout(title = plotly_title(nm, size = 13),
                       xaxis = list(title = x_var), yaxis = list(title = y_var))
            }
          })

          # Side-by-side: contour + surface
          output[[paste0(plot_ids[idx], "_contour")]] <- renderPlotly({
            g <- build_contour_grid()
            if (is.null(g)) return(plotly_empty())
            x_var <- input$contour_x; y_var <- input$contour_y
            plot_ly(x = g$x_seq, y = g$y_seq, z = g$z_mat, type = "contour",
                    colorscale = "Viridis", contours = list(showlabels = TRUE),
                    showscale = TRUE) %>%
              layout(title = plotly_title(paste(nm, "- Contour"), size = 12),
                     xaxis = list(title = x_var), yaxis = list(title = y_var))
          })
          output[[paste0(plot_ids[idx], "_surface")]] <- renderPlotly({
            g <- build_contour_grid()
            if (is.null(g)) return(plotly_empty())
            x_var <- input$contour_x; y_var <- input$contour_y
            plot_ly(x = g$x_seq, y = g$y_seq, z = g$z_mat, type = "surface",
                    colorscale = "Viridis", showscale = TRUE) %>%
              layout(title = plotly_title(paste(nm, "- Surface"), size = 12),
                     scene = list(xaxis = list(title = x_var), yaxis = list(title = y_var),
                                  zaxis = list(title = "Predicted")))
          })
        })
      })

      ctype <- input$contour_type
      n <- length(mods)

      if (identical(ctype, "both")) {
        # Side-by-side: contour on left, surface on right, one row per model
        rows <- lapply(seq_along(mods), function(i) {
          fluidRow(
            h5(names(mods)[i], class = "mt-3"),
            column(6, plotlyOutput(ns(paste0(plot_ids[i], "_contour")), height = "450px")),
            column(6, plotlyOutput(ns(paste0(plot_ids[i], "_surface")), height = "500px"))
          )
        })
      } else {
        # Single type: 2-up layout
        height <- if (identical(ctype, "surface")) "500px" else "450px"
        rows <- lapply(seq(1, n, by = 2), function(i) {
          cols <- list(
            column(6,
              h5(names(mods)[i], class = "mt-3"),
              plotlyOutput(ns(plot_ids[i]), height = height)
            )
          )
          if (i + 1 <= n) {
            cols[[2]] <- column(6,
              h5(names(mods)[i + 1], class = "mt-3"),
              plotlyOutput(ns(plot_ids[i + 1]), height = height)
            )
          }
          do.call(fluidRow, cols)
        })
      }
      do.call(tagList, rows)
    })

    # ── Profiler ─────────────────────────────────────────────────────────

    # Dynamic controls: one slider/dropdown per base predictor
    output$profiler_controls_ui <- renderUI({
      req(length(active_models()) > 0)
      sel_models <- names(active_models())
      all_vars <- character(0)
      for (mn in sel_models) {
        m <- rv$models[[mn]]
        if (is.null(m)) next
        mf <- model.frame(m)
        bv <- all.vars(formula(m))[-1]
        bv <- intersect(bv, names(mf))
        all_vars <- union(all_vars, bv)
      }
      req(length(all_vars) > 0)
      controls <- lapply(all_vars, function(p) {
        for (mn in sel_models) {
          m <- rv$models[[mn]]
          if (is.null(m)) next
          mf <- model.frame(m)
          if (p %in% names(mf)) {
            vals <- mf[[p]]
            if (is.factor(vals)) {
              return(column(2, selectInput(ns(paste0("prof_", p)), p,
                                           choices = levels(vals), selected = levels(vals)[1])))
            } else {
              rng <- range(vals, na.rm = TRUE)
              mid <- mean(vals, na.rm = TRUE)
              stp <- if (diff(rng) > 0) signif(diff(rng) / 50, 2) else 0.1
              return(column(2, sliderInput(ns(paste0("prof_", p)), p,
                                            min = rng[1], max = rng[2], value = mid, step = stp)))
            }
          }
        }
      })
      fluidRow(controls)
    })

    # Dynamic profiler plots UI -- one row per selected model
    output$profiler_plots_ui <- renderUI({
      req(length(active_models()) > 0)
      sel_models <- names(active_models())
      n_models <- length(sel_models)
      row_h <- max(280, 500 / n_models)
      mode <- input$profiler_mode %||% "aligned"
      tw <- input$profiler_term_width %||% 250

      # Compute total width
      all_vars <- character(0)
      for (mn in sel_models) {
        m <- rv$models[[mn]]
        if (is.null(m)) next
        mf <- model.frame(m)
        bv <- all.vars(formula(m))[-1]
        bv <- intersect(bv, names(mf))
        all_vars <- union(all_vars, bv)
      }
      n_terms <- length(all_vars)
      total_w <- max(600, n_terms * tw + 80)  # +80 for y-axis labels

      tagList(lapply(seq_along(sel_models), function(i) {
        plotlyOutput(ns(paste0("profiler_row_", i)),
                     width = paste0(total_w, "px"), height = paste0(row_h, "px"))
      }))
    })

    # Helper to build one profiler row
    build_profiler_row <- function(m, model_label, all_vars = NULL, y_limits = NULL) {
      mf <- model.frame(m)
      resp_name <- names(mf)[1]
      base_vars <- all.vars(formula(m))[-1]
      base_vars <- intersect(base_vars, names(mf))
      if (length(base_vars) == 0) return(plotly_empty())
      display_vars <- if (!is.null(all_vars)) all_vars else base_vars

      current <- list()
      for (p in base_vars) {
        val <- input[[paste0("prof_", p)]]
        if (is.null(val)) {
          if (is.factor(mf[[p]])) val <- levels(mf[[p]])[1]
          else val <- mean(mf[[p]], na.rm = TRUE)
        }
        current[[p]] <- val
      }

      curr_newdata <- as.data.frame(lapply(base_vars, function(p) {
        v <- current[[p]]
        if (is.factor(mf[[p]])) factor(v, levels = levels(mf[[p]]))
        else as.numeric(v)
      }))
      names(curr_newdata) <- base_vars
      curr_pred <- predict(m, newdata = curr_newdata)

      plots <- list()
      for (p in display_vars) {
        if (!(p %in% base_vars)) {
          g <- ggplot() + theme_void() + labs(x = p, y = "")
          plots[[p]] <- ggplotly(g)
          next
        }
        vals <- mf[[p]]
        if (is.factor(vals)) {
          grid_vals <- levels(vals)
          n_grid <- length(grid_vals)
        } else {
          rng <- range(vals, na.rm = TRUE)
          n_grid <- 50
          grid_vals <- seq(rng[1], rng[2], length.out = n_grid)
        }
        newdata <- as.data.frame(lapply(base_vars, function(pred) {
          if (pred == p) {
            if (is.factor(mf[[pred]])) factor(grid_vals, levels = levels(mf[[pred]]))
            else grid_vals
          } else {
            v <- current[[pred]]
            if (is.factor(mf[[pred]])) factor(rep(v, n_grid), levels = levels(mf[[pred]]))
            else rep(as.numeric(v), n_grid)
          }
        }))
        names(newdata) <- base_vars

        pred_ci <- predict(m, newdata = newdata, interval = "confidence")
        pred_pi <- predict(m, newdata = newdata, interval = "prediction")

        if (is.factor(vals)) {
          df <- data.frame(x = grid_vals, fit = pred_ci[, "fit"],
                           ci_lwr = pred_ci[, "lwr"], ci_upr = pred_ci[, "upr"],
                           stringsAsFactors = FALSE)
          g <- ggplot(df, aes(x = x, y = fit)) +
            geom_point(size = 3, colour = default_col()) +
            geom_errorbar(aes(ymin = ci_lwr, ymax = ci_upr), width = 0.2, colour = default_col()) +
            geom_hline(yintercept = curr_pred, linetype = "dashed", colour = "red", alpha = 0.5) +
            labs(x = p, y = resp_name)
          cv <- current[[p]]
          if (!is.null(cv) && cv %in% grid_vals)
            g <- g + geom_point(data = df[df$x == cv, , drop = FALSE],
                                colour = "red", size = 4, shape = 18)
        } else {
          df <- data.frame(x = grid_vals, fit = pred_ci[, "fit"],
                           ci_lwr = pred_ci[, "lwr"], ci_upr = pred_ci[, "upr"],
                           pi_lwr = pred_pi[, "lwr"], pi_upr = pred_pi[, "upr"])
          g <- ggplot(df, aes(x = x, y = fit)) +
            geom_line(colour = default_col(), linewidth = 0.8)
          if (isTRUE(input$profiler_ci))
            g <- g + geom_ribbon(aes(ymin = ci_lwr, ymax = ci_upr), alpha = 0.2, fill = default_col())
          if (isTRUE(input$profiler_pi))
            g <- g + geom_ribbon(aes(ymin = pi_lwr, ymax = pi_upr), alpha = 0.1, fill = cat_palette()(3)[3])
          cv <- as.numeric(current[[p]])
          if (!is.null(cv)) {
            g <- g + geom_vline(xintercept = cv, linetype = "dashed", colour = "red", alpha = 0.5) +
              geom_hline(yintercept = curr_pred, linetype = "dashed", colour = "red", alpha = 0.5)
          }
          g <- g + labs(x = p, y = resp_name)
        }
        g <- g + theme_app()
        if (!is.null(y_limits)) g <- g + coord_cartesian(ylim = y_limits)
        plots[[p]] <- ggplotly(g, tooltip = c("x", "y"))
      }

      if (length(plots) == 0) return(plotly_empty())
      pfont <- list(family = "Arial, Helvetica, sans-serif")
      share_y <- !is.null(y_limits) || identical(input$profiler_y_scale, "shared")
      # Equal widths per subplot panel to align across model rows
      n_plots <- length(plots)
      equal_widths <- rep(1 / n_plots, n_plots)
      subplot(plots, nrows = 1, shareY = share_y, titleX = TRUE, widths = equal_widths) %>%
        layout(title = list(
                 text = paste(model_label, "\u2014", resp_name,
                              "  [Current:", round(curr_pred, 2), "]"),
                 font = list(family = "Arial, Helvetica, sans-serif", size = 13)
               ),
               font = pfont,
               showlegend = FALSE)
    }

    # Dynamically render profiler row outputs
    observe({
      req(length(active_models()) > 0)
      sel_models <- names(active_models())
      mode <- input$profiler_mode %||% "aligned"
      all_vars <- character(0)
      for (mn in sel_models) {
        m <- rv$models[[mn]]
        if (is.null(m)) next
        mf <- model.frame(m)
        bv <- all.vars(formula(m))[-1]
        bv <- intersect(bv, names(mf))
        all_vars <- union(all_vars, bv)
      }
      # Compute y-axis limits based on scale mode
      y_scale <- input$profiler_y_scale %||% "free"
      y_lim <- NULL
      if (y_scale == "full") {
        # Full response range: use raw data + prediction intervals to capture all values
        all_vals <- tryCatch({
          resp_name <- names(model.frame(rv$models[[sel_models[1]]]))[1]
          rv$data[[resp_name]]
        }, error = function(e) NULL)
        # Also include prediction intervals from all models
        for (mn in sel_models) {
          m_tmp <- rv$models[[mn]]
          if (is.null(m_tmp)) next
          tryCatch({
            pi <- predict(m_tmp, interval = "prediction")
            all_vals <- c(all_vals, pi[, "lwr"], pi[, "upr"])
          }, error = function(e) NULL)
        }
        if (!is.null(all_vals) && is.numeric(all_vals) && length(all_vals) > 0) {
          rng <- range(all_vals, na.rm = TRUE)
          pad <- diff(rng) * 0.05
          y_lim <- c(rng[1] - pad, rng[2] + pad)
        }
      } else if (y_scale == "shared") {
        # Shared: compute from all predictions across all models INCLUDING CI/PI
        all_vals <- numeric(0)
        for (mn in sel_models) {
          m_tmp <- rv$models[[mn]]
          if (is.null(m_tmp)) next
          all_vals <- c(all_vals, fitted(m_tmp))
          # Also include prediction intervals to ensure full range
          tryCatch({
            pi <- predict(m_tmp, interval = "prediction")
            all_vals <- c(all_vals, pi[, "lwr"], pi[, "upr"])
          }, error = function(e) NULL)
        }
        if (length(all_vals) > 0) {
          rng <- range(all_vals, na.rm = TRUE)
          pad <- diff(rng) * 0.1
          y_lim <- c(rng[1] - pad, rng[2] + pad)
        }
      }

      for (i in seq_along(sel_models)) {
        local({
          idx <- i
          mn <- sel_models[idx]
          av <- if (mode == "aligned") all_vars else NULL
          yl <- y_lim
          output[[paste0("profiler_row_", idx)]] <- renderPlotly({
            req(rv$models[[mn]])
            build_profiler_row(rv$models[[mn]], mn, all_vars = av, y_limits = yl)
          })
        })
      }
    })

    # ── Multiple Comparisons Results ─────────────────────────────────────

    mc_collect <- reactive({
      req(isTRUE(mc_state$mc_on()), length(rv$mc_results) > 0,
          !is.null(input$mc_show_method))
      methods <- input$mc_show_method
      terms   <- input$mc_show_term %||% mc_state$mc_terms()
      model_names <- names(active_models())
      rows <- list()
      for (mname in model_names) {
        for (method in methods) {
          for (spec in terms) {
            key <- paste(mname, spec, method, sep = "__")
            if (!is.null(rv$mc_results[[key]]) && nrow(rv$mc_results[[key]]) > 0)
              rows[[key]] <- rv$mc_results[[key]]
          }
        }
      }
      if (length(rows) == 0) return(NULL)
      mc_df <- bind_rows(rows)
      if ("method" %in% names(mc_df)) {
        mc_df$method <- gsub("^custom$", "MaxT", mc_df$method)
        mc_df$method <- gsub("^student$", "Student", mc_df$method)
        mc_df$method <- gsub("^tukey$", "Tukey", mc_df$method, ignore.case = TRUE)
        mc_df$method <- gsub("^dunnett$", "Dunnett", mc_df$method, ignore.case = TRUE)
      }

      # Normalize contrast direction: always alphabetical (A - B, not B - A)
      # so Dunnett's "TrtA - Control" aligns with Student/Tukey's "Control - TrtA"
      est_col <- if ("estimate" %in% names(mc_df)) "estimate" else NULL
      lo_col  <- if ("lower.CL" %in% names(mc_df)) "lower.CL" else NULL
      hi_col  <- if ("upper.CL" %in% names(mc_df)) "upper.CL" else NULL
      if (!is.null(est_col) && "contrast" %in% names(mc_df)) {
        parts <- strsplit(as.character(mc_df$contrast), "\\s*-\\s*")
        for (j in seq_len(nrow(mc_df))) {
          pp <- trimws(parts[[j]])
          if (length(pp) == 2 && pp[1] > pp[2]) {
            # Flip to alphabetical: negate estimate, swap CIs
            mc_df$contrast[j] <- paste(pp[2], "-", pp[1])
            mc_df[[est_col]][j] <- -mc_df[[est_col]][j]
            if (!is.null(lo_col) && !is.null(hi_col)) {
              old_lo <- mc_df[[lo_col]][j]
              mc_df[[lo_col]][j] <- -mc_df[[hi_col]][j]
              mc_df[[hi_col]][j] <- -old_lo
            }
          }
        }
        # Sort contrasts alphabetically for display
        mc_df$contrast <- factor(mc_df$contrast,
                                  levels = sort(unique(mc_df$contrast)))
      }
      mc_df
    })

    output$mc_plot_container <- renderUI({
      mc_df <- mc_collect()
      n_contrasts <- if (!is.null(mc_df)) length(unique(mc_df$contrast)) else 0
      n_models    <- if (!is.null(mc_df)) length(unique(mc_df$model)) else 1
      n_methods   <- if (!is.null(mc_df)) length(unique(mc_df$method)) else 1
      # Each facet panel (one per contrast) has n_models × n_methods rows
      rows_per_panel <- max(n_models * n_methods, 1)
      h <- max(350, n_contrasts * (rows_per_panel * 28 + 40) + 80)
      plotlyOutput(session$ns("mc_plot"), width = "100%", height = paste0(h, "px"))
    })

    output$mc_plot <- renderPlotly({
      mc_df <- mc_collect()
      if (is.null(mc_df) || nrow(mc_df) == 0) return(plotly_empty())
      if (!"contrast" %in% names(mc_df)) mc_df$contrast <- as.character(seq_len(nrow(mc_df)))
      est_col <- if ("estimate" %in% names(mc_df)) "estimate" else names(mc_df)[2]
      lo_col <- if ("lower.CL" %in% names(mc_df)) "lower.CL" else NULL
      hi_col <- if ("upper.CL" %in% names(mc_df)) "upper.CL" else NULL

      # Replace long formula names with short M-labels for y-axis
      orig_models <- unique(mc_df$model)
      m_labels    <- model_m_labels(orig_models)
      mc_df$model <- factor(model_m_labels(mc_df$model), levels = m_labels)

      # Order methods by CI width (widest first → drawn in background)
      if (!is.null(lo_col) && !is.null(hi_col)) {
        ci_width <- mc_df[[hi_col]] - mc_df[[lo_col]]
        method_widths <- tapply(ci_width, mc_df$method, mean, na.rm = TRUE)
        method_order <- names(sort(method_widths, decreasing = TRUE))
        mc_df$method <- factor(mc_df$method, levels = method_order)
      }

      pal <- cat_palette()
      n_methods <- length(unique(mc_df$method))
      method_cols <- pal(max(n_methods, 3))[seq_len(n_methods)]

      # Dodge width: offset methods within each model on the y-axis
      dw <- 0.6

      # Layout: facet rows = comparison, y-axis = model formula
      p <- ggplot(mc_df, aes(x = model, y = .data[[est_col]], colour = method)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
        scale_colour_manual(values = method_cols) +
        coord_flip() +
        labs(x = NULL, y = "Estimate", colour = "Method") +
        theme_app()

      # CI error bars with dodge (legend suppressed — points carry the legend)
      if (!is.null(lo_col) && !is.null(hi_col))
        p <- p + geom_errorbar(aes(ymin = .data[[lo_col]], ymax = .data[[hi_col]]),
                                width = 0.2, linewidth = 0.8,
                                position = position_dodge(width = dw),
                                show.legend = FALSE)
      # Points on top with dodge
      p <- p + geom_point(size = 2.5, position = position_dodge(width = dw))

      # Facet rows by comparison, columns by spec (term) if multiple
      multi_spec <- length(unique(mc_df$spec)) > 1
      if (multi_spec) {
        p <- p + facet_grid(contrast ~ spec, scales = "free_y", space = "free_y",
                            labeller = label_both)
      } else {
        p <- p + facet_wrap(~ contrast, ncol = 1, scales = "free_y",
                            strip.position = "right")
      }

      pp <- ggplotly(p)

      # Fix plotly legend: deduplicate across facets.
      # ggplotly creates separate traces per facet panel, so each method
      # appears multiple times.  Use legendgroup to link them and only
      # show the legend entry for the FIRST marker trace of each method.
      method_lvls <- levels(mc_df$method)
      seen_methods <- character(0)
      for (i in seq_along(pp$x$data)) {
        tr <- pp$x$data[[i]]
        nm <- tr$name
        if (is.null(nm) || !nm %in% method_lvls) next
        # Link all traces of the same method for toggle-as-group
        pp$x$data[[i]]$legendgroup <- nm
        if (identical(tr$mode, "lines")) {
          # Error-bar traces: always hide from legend
          pp$x$data[[i]]$showlegend <- FALSE
        } else if (identical(tr$mode, "markers")) {
          if (nm %in% seen_methods) {
            # Already shown this method in the legend — hide duplicate
            pp$x$data[[i]]$showlegend <- FALSE
          } else {
            pp$x$data[[i]]$showlegend <- TRUE
            seen_methods <- c(seen_methods, nm)
          }
        }
      }
      pp
    })

    # ── Critical values annotation above MC table ─────────────────────
    output$mc_critical_values <- renderUI({
      mc_df <- mc_collect()
      if (is.null(mc_df) || nrow(mc_df) == 0) return(NULL)

      alpha  <- mc_state$mc_alpha() %||% ALPHA_DEFAULT
      mods   <- active_models()
      if (length(mods) == 0) return(NULL)
      err_df <- df.residual(mods[[1]])

      methods_shown <- unique(mc_df$method)

      # Back-calculate critical values from CI: crit = (upper - lower) / (2 * SE)
      items <- list()
      for (m in methods_shown) {
        sub <- mc_df[mc_df$method == m, , drop = FALSE]
        cv <- tryCatch({
          # Use first row with valid SE and CIs
          row <- sub[!is.na(sub$SE) & !is.na(sub$lower.CL) & !is.na(sub$upper.CL), ][1, ]
          if (is.na(row$SE) || row$SE <= 0) stop("no valid row")
          crit <- (row$upper.CL - row$lower.CL) / (2 * row$SE)
          n_comp <- nrow(sub)
          if (m == "Student") {
            sprintf("Student t: t\u2090\u2082,df=%d = %.4f (%d unadjusted comparisons)",
                    err_df, crit, n_comp)
          } else if (m == "Tukey") {
            sprintf("Tukey HSD: q\u2090/\u221a2 = %.4f (df = %d, %d simultaneous comparisons)",
                    crit, err_df, n_comp)
          } else if (m == "Dunnett") {
            sprintf("Dunnett: d\u2090 = %.4f (df = %d, %d comparisons vs control)",
                    crit, err_df, n_comp)
          } else if (m == "MaxT") {
            sprintf("MaxT: mvt-adjusted critical = %.4f (df = %d, %d selected comparisons)",
                    crit, err_df, n_comp)
          } else {
            sprintf("%s: critical = %.4f (df = %d)", m, crit, err_df)
          }
        }, error = function(e) m)
        items <- c(items, list(tags$li(cv)))
      }
      tags$div(
        class = "text-muted small mb-2",
        tags$strong(paste0("Critical values (\u03b1 = ", alpha, "):")),
        tags$ul(style = "margin-bottom: 0;", items)
      )
    })

    output$mc_table <- DT::renderDataTable({
      mc_df <- mc_collect()
      if (is.null(mc_df) || nrow(mc_df) == 0)
        return(DT::datatable(data.frame(Message = "No results.")))
      mc_df <- mc_df[, !names(mc_df) %in% c("t.ratio"), drop = FALSE]
      # Replace formula names with short M-labels
      if ("model" %in% names(mc_df))
        mc_df$model <- model_m_labels(mc_df$model)
      front <- intersect(c("model", "spec", "method", "contrast"), names(mc_df))
      mc_df <- mc_df[, c(front, setdiff(names(mc_df), front)), drop = FALSE]
      num_cols <- intersect(c("estimate", "SE", "lower.CL", "upper.CL", "df"), names(mc_df))
      p_cols <- grep("p.value|p_value|p\\.adj", names(mc_df), value = TRUE)
      alpha <- mc_state$mc_alpha() %||% 0.05
      # Pre-format p-value columns
      mc_df <- format_pvalue_dt(mc_df, p_cols, alpha = alpha)
      shadow_p_cols <- paste0("._p_", p_cols)
      shadow_p_idx <- which(names(mc_df) %in% shadow_p_cols) - 1
      hide_defs <- if (length(shadow_p_idx) > 0) list(list(visible = FALSE, targets = shadow_p_idx)) else list()

      dt <- dt_table(mc_df, rownames = FALSE, options = list(pageLength = 20, columnDefs = hide_defs))
      for (nc in num_cols) dt <- DT::formatRound(dt, nc, digits = 3)
      for (pc in p_cols) {
        shadow <- paste0("._p_", pc)
        dt <- DT::formatStyle(dt, pc,
                 backgroundColor = DT::styleInterval(alpha, c(PVALUE_GREEN, "white")),
                 valueColumns    = shadow)
      }
      dt
    })

    # ── Robustness (delegated to mod_results_robustness.R) ────────────────
    results_robustness_server(input, output, session, rv,
                               active_models, mc_state,
                               colour_theme, role_selectors)

    # ── Residuals (delegated to mod_results_residuals.R) ─────────────────
    results_residuals_server(input, output, session, rv,
                              active_models, update_model_checkbox,
                              colour_theme, role_selectors,
                              shared_reactives)

    # ── Sync mc_show_method / mc_show_term when mc_results change ────────

    observeEvent(rv$mc_results, {
      if (length(rv$mc_results) == 0) return()
      keys <- names(rv$mc_results)
      if (length(keys) == 0) return()
      parts <- strsplit(keys, "__")
      methods_found <- unique(sapply(parts, function(p) if (length(p) >= 3) p[3] else NA_character_))
      methods_found <- methods_found[!is.na(methods_found)]
      terms_found <- unique(sapply(parts, function(p) if (length(p) >= 2) p[2] else NA_character_))
      terms_found <- terms_found[!is.na(terms_found)]

      mc_labels <- c(student = "Student", dunnett = "Dunnett",
                     custom = "MaxT", tukey = "Tukey")
      mc_choices <- methods_found
      names(mc_choices) <- ifelse(mc_choices %in% names(mc_labels),
                                   mc_labels[mc_choices], mc_choices)
      updateCheckboxGroupInput(session, "mc_show_method",
                               choices = mc_choices, selected = mc_choices)
      updateCheckboxGroupInput(session, "mc_show_term",
                               choices = terms_found, selected = terms_found)
    })

    # ── Update effects_term, resid_colour_by, robustness dropdowns ───────
    observeEvent(rv$models, {
      if (length(rv$models) == 0) return()

      facs <- factors_(); blks <- blocks()
      colour_choices <- build_colour_choices(
        factors = facs, blocks = blks,
        treatment_label = if (length(facs) > 1) treatment_label() else NULL)
      updateSelectInput(session, "resid_colour_by", choices = colour_choices)

      # Leverage panel-by: factors + blocks
      panel_ch <- c("None" = "none")
      for (f in facs) panel_ch <- c(panel_ch, setNames(f, f))
      for (b in blks) panel_ch <- c(panel_ch, setNames(b, paste0(b, " (block)")))
      updateSelectInput(session, "leverage_panel_by", choices = panel_ch)

      all_terms_raw <- unique(unlist(lapply(rv$models, function(m) {
        a <- tryCatch(model_anova(m, type=3), error=function(e) NULL)
        if (!is.null(a)) rownames(a)[rownames(a) != "(Intercept)"] else character(0)
      })))
      factor_cols  <- factors_()
      block_cols   <- blocks()
      factor_or_block <- c(factor_cols, block_cols)

      all_factor_terms <- all_terms_raw[sapply(all_terms_raw, function(t) {
        parts <- strsplit(t, ":")[[1]]
        if (!all(parts %in% factor_or_block)) return(FALSE)
        if (all(sapply(parts, function(cn) (rv$col_types[[cn]] %||% "Factor") == "Factor")))
          return(TRUE)
        length(parts) == 1
      })]

      cat_factor_terms <- all_terms_raw[sapply(all_terms_raw, function(t) {
        parts <- strsplit(t, ":")[[1]]
        all(parts %in% factor_cols) &&
          all(sapply(parts, function(cn) (rv$col_types[[cn]] %||% "Factor") == "Factor"))
      })]

      updateSelectInput(session, "robust_term",
                        choices = cat_factor_terms,
                        selected = cat_factor_terms[1])

      cov_cols <- all_covariates()
      # Include blocks that are model terms (may not pass the factor-type filter)
      block_in_model <- intersect(block_cols, all_terms_raw)
      block_add <- setdiff(block_in_model, all_factor_terms)
      effects_choices <- all_factor_terms
      if (length(block_add) > 0)
        effects_choices <- c(effects_choices, setNames(block_add, paste0(block_add, " (block)")))
      if (length(cov_cols) > 0)
        effects_choices <- c(effects_choices, setNames(cov_cols, cov_cols))
      if (length(rv$alias_labels) > 0 && length(effects_choices) > 0) {
        display_names <- relabel_alias_terms(effects_choices, rv$alias_labels)
        names(effects_choices) <- display_names
      }
      updateSelectInput(session, "effects_term", choices = effects_choices)
    })

    # ── Return active_models reactive for use by other modules ───────────
    list(active_models = active_models)
  })
}