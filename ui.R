# ui.R — navbarPage with 6 tabs

ui <- navbarPage(
  title = "Model Comparison App",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  id = "main_nav",

  # ── Tab 1: Upload ────────────────────────────────────────────────────────
  tabPanel(
    "Upload",
    fluidRow(
      column(4,
        wellPanel(
          h4("Data Upload"),
          fileInput("file", "Choose CSV or Excel file",
                    accept = c(".csv", ".xlsx", ".xls")),
          conditionalPanel(
            condition = "input.file != null && input.file.name.endsWith('.csv')",
            radioButtons("sep", "CSV Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ",", inline = TRUE)
          ),
          hr(),
          selectInput("example_choice", "Example dataset",
                      choices = c(
                        "RCBD (2 factors, blocks, covariate)"      = "rcbd",
                        "Mediator Trap (covariate is a response)"  = "mediator",
                        "Block\u2013Covariate Collinearity"        = "collinear",
                        "2\u2074\u207b\u00b9 Fractional Factorial" = "fracfact",
                        "CCD (Central Composite Design)"           = "ccd"
                      ),
                      selected = "rcbd"),
          actionButton("load_example", "Load Example Dataset",
                       class = "btn-outline-secondary w-100",
                       icon  = icon("flask")),
          hr(),
          radioButtons("analysis_mode", "Analysis mode",
                       choices = c("Comparative (ANOVA)" = "comparative",
                                   "Regression (MLR)"    = "regression"),
                       selected = "comparative", inline = TRUE),
          p(class = "text-muted small",
            tags$b("Comparative:"), " factors as categorical \u2014 LS means, pairwise comparisons.",
            tags$br(),
            tags$b("Regression:"), " factors as numeric \u2014 polynomial models, coefficients.")
        )
      ),
      column(8,
        h4("Data Preview (first 20 rows)"),
        DT::dataTableOutput("raw_preview")
      )
    )
  ),

  # ── Tab 2: Assign Roles ──────────────────────────────────────────────────
  tabPanel(
    "Assign Roles",
    fluidRow(
      column(12,
        h5("Column roles and types"),
        p(class = "text-muted",
          tags$b("Role:"),
          " Response = numeric outcome \u00b7 Factor = categorical predictor \u00b7 ",
          "Covariate = continuous predictor \u00b7 Block = blocking variable \u00b7 ",
          "Run Order = experiment run order (included as covariate in models) \u00b7 ",
          "Ignore = excluded.",
          tags$br(),
          tags$b("Type:"),
          " Controls how the column is coerced when fitting models. ",
          "Factor = as.factor() \u00b7 Numeric = as.numeric().",
          tags$br(),
          tags$b("Transform:"),
          " None = raw values \u00b7 Centre = subtract mean \u00b7 ",
          "Coding = rescale to \u22121/+1 (specify actual values for each level)."
        ),
        uiOutput("role_ui")
      )
    )
  ),

  # ── Tab 3: Explore ───────────────────────────────────────────────────────
  tabPanel(
    "Explore",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("explore_response", "Response variable", choices = NULL),
        selectInput("explore_colour_by", "Colour by",
                    choices = c("None" = "none"), selected = "none"),
        selectInput("explore_facet_by", "Facet by",
                    choices = c("None" = "none"), selected = "none"),
        radioButtons("explore_smooth", "Fitted line",
                     choices = c("Linear" = "lm", "Smoother" = "loess"),
                     selected = "lm", inline = TRUE)
      ),
      mainPanel(width = 9,
        tabsetPanel(id = "explore_tabs",
          tabPanel("Distribution",
            br(),
            plotlyOutput("dist_plot", height = "450px")
          ),
          tabPanel("By Factor",
            br(),
            selectInput("explore_factor", "Factor", choices = NULL),
            plotlyOutput("by_factor_plot", height = "450px")
          ),
          tabPanel("Covariate Plots",
            br(),
            selectInput("explore_covariate", "Covariate", choices = NULL),
            plotlyOutput("covariate_plot", height = "500px")
          ),
          tabPanel("By Block",
            br(),
            uiOutput("by_block_ui")
          )
        )
      )
    )
  ),

  # ── Tab 4: Design ──────────────────────────────────────────────────────
  tabPanel(
    "Design",
    tabsetPanel(id = "design_tabs",

      tabPanel("Design Matrix",
        br(),
        p(class = "text-muted",
          "Coded design matrix after applying transforms. Shows how factor values ",
          "are coded for model fitting."),
        DT::dataTableOutput("design_matrix_table")
      ),

      tabPanel("Scatterplot Matrix",
        br(),
        fluidRow(
          column(4, selectInput("splom_colour", "Colour by",
                                choices = c("None" = "none"))),
          column(4, sliderInput("splom_jitter", "Jitter amount",
                                min = 0, max = 0.4, value = 0.15, step = 0.05))
        ),
        plotlyOutput("design_splom", height = "600px")
      ),

      tabPanel("2D Design Map",
        br(),
        fluidRow(
          column(3, selectInput("design_x", "X axis", choices = NULL)),
          column(3, selectInput("design_y", "Y axis", choices = NULL)),
          column(3, selectInput("design_row_facet", "Row facet",
                                choices = c("None" = "none"))),
          column(3, selectInput("design_col_facet", "Column facet",
                                choices = c("None" = "none")))
        ),
        fluidRow(
          column(3, selectInput("design_2d_colour", "Colour by",
                                choices = c("None" = "none"))),
          column(3, sliderInput("design_2d_jitter", "Jitter amount",
                                min = 0, max = 0.4, value = 0.15, step = 0.05)),
          column(3, radioButtons("design_2d_mode", "Display",
                                 choices = c("Points" = "points", "Heatmap" = "heatmap"),
                                 selected = "points", inline = TRUE))
        ),
        plotlyOutput("design_2d", height = "500px")
      ),

      tabPanel("Needle Plot",
        br(),
        p(class = "text-muted",
          "Response vs treatment combinations. Needles show deviation from the ",
          "overall mean. Reorder factors or sort by response to identify patterns."),
        fluidRow(
          column(4, selectInput("needle_response", "Response", choices = NULL)),
          column(4, uiOutput("needle_factor_order_ui")),
          column(4, radioButtons("needle_sort", "Sort by",
                                 choices = c("Factor levels" = "factors",
                                             "Response value" = "response"),
                                 selected = "factors", inline = TRUE))
        ),
        fluidRow(
          column(4, selectInput("needle_colour", "Colour by",
                                choices = c("None" = "none"))),
          column(4, selectInput("needle_facet", "Facet by",
                                choices = c("None" = "none")))
        ),
        plotlyOutput("needle_plot", height = "500px")
      ),

      tabPanel("3D Design Space",
        br(),
        fluidRow(
          column(2, selectInput("design_3d_x", "X axis", choices = NULL)),
          column(2, selectInput("design_3d_y", "Y axis", choices = NULL)),
          column(2, selectInput("design_3d_z", "Z axis", choices = NULL)),
          column(3, selectInput("design_3d_colour", "Colour by",
                                choices = c("None" = "none"))),
          column(3, selectInput("design_3d_shape", "Shape by",
                                choices = c("None" = "none")))
        ),
        plotlyOutput("design_3d", height = "600px")
      ),

      tabPanel("Alias Structure",
        br(),
        p(class = "text-muted",
          "Compare the terms you intend to fit (full model) against a larger set ",
          "of potential terms (alias model) to find confounded effects. ",
          "|Correlation| = 1 means fully confounded terms."),
        fluidRow(
          column(6,
            textInput("alias_full_formula", "Full model (terms to fit)",
                      placeholder = "A + B + A:B"),
            p(class = "text-muted small",
              "Right-hand side of the model formula you plan to fit.")
          ),
          column(6,
            textInput("alias_check_formula", "Alias model (terms to check against)",
                      placeholder = "A + B + C + A:B + A:C + B:C + A:B:C"),
            p(class = "text-muted small",
              "Larger model including higher-order interactions, block \u00d7 factor terms, etc.")
          )
        ),
        fluidRow(
          column(4, numericInput("alias_threshold", "Correlation threshold",
                                 value = 0.99, min = 0.5, max = 1, step = 0.01)),
          column(4, actionButton("alias_auto_fill", "Auto-fill from roles",
                                 class = "btn-outline-secondary mt-4",
                                 icon = icon("magic")))
        ),
        DT::dataTableOutput("alias_table")
      ),

      tabPanel("Power Analysis",
        br(),
        fluidRow(
          column(3, numericInput("power_sigma", "Error SD (\u03c3)",
                                 value = 1, min = 0.001, step = 0.1)),
          column(3, numericInput("power_delta", "Min detectable effect (\u03b4)",
                                 value = 1, min = 0.001, step = 0.1)),
          column(3, numericInput("power_alpha", "Significance level (\u03b1)",
                                 value = 0.05, min = 0.001, max = 0.5, step = 0.01)),
          column(3, numericInput("power_max_order", "Max interaction order",
                                 value = 2, min = 1, max = 5))
        ),
        p(class = "text-muted",
          "Power to detect an effect of size \u03b4 given error SD \u03c3. ",
          "Based on the non-centrality parameter of the F distribution."),
        DT::dataTableOutput("power_table"),
        br(),
        plotlyOutput("power_curve", height = "400px")
      )
    )
  ),

  # ── Tab 5: Models ────────────────────────────────────────────────────────
  tabPanel(
    "Models",
    useShinyjs(),
    fluidRow(
      column(4,
        wellPanel(
          h4("Model Builder"),
          selectInput("active_response", "Response variable", choices = NULL),
          h5("Factor terms"),
          numericInput("max_way", "Max factor interaction order", value = 2, min = 1, max = 5),
          conditionalPanel(
            condition = "input.analysis_mode == 'regression'",
            numericInput("poly_degree", "Polynomial degree", value = 2, min = 1, max = 3)
          ),
          h5("Covariates & blocks"),
          checkboxInput("include_covariates", "Include covariates", value = TRUE),
          conditionalPanel(
            condition = "input.include_covariates == true",
            checkboxInput("include_cov_fac", "Include covariate \u00d7 factor interactions", value = FALSE)
          ),
          checkboxInput("include_blocks", "Include blocks", value = TRUE),
          checkboxInput("include_block_fac", "Include block \u00d7 factor interactions", value = FALSE),
          actionButton("generate_formulas_btn", "Generate Formulas",
                       class = "btn-primary w-100 mb-2"),
          hr(),
          div(
            style = "margin-bottom: 6px;",
            actionButton("select_all_formulas", "Select All",
                         class = "btn-sm btn-outline-secondary"),
            actionButton("deselect_all_formulas", "Deselect All",
                         class = "btn-sm btn-outline-secondary ms-1")
          ),
          div(style = "max-height: 300px; overflow-y: auto;",
            uiOutput("formula_list_ui")
          ),
          hr(),
          h5("Custom formula"),
          textInput("custom_formula", NULL, placeholder = "y ~ A + B + A:B"),
          actionButton("add_custom", "Add Custom", class = "btn-sm btn-outline-primary"),
          hr(),
          actionButton("run_models", "Run Selected Models",
                       class = "btn-success w-100"),
          hr(),
          h5("Backward Elimination"),
          numericInput("prune_alpha", "Alpha threshold", value = 0.05,
                       min = 0.001, max = 0.5, step = 0.01),
          actionButton("prune_btn", "Prune Selected Models",
                       class = "btn-warning w-100",
                       icon = icon("scissors"))
        )
      ),
      column(8,
        wellPanel(
          h4("Multiple Comparisons"),
          checkboxInput("mc_on", "Enable multiple comparisons", value = FALSE),
          conditionalPanel(
            condition = "input.mc_on == true",
            h5("Terms to test"),
            checkboxGroupInput("mc_terms", NULL, choices = NULL),
            h5("Methods"),
            checkboxGroupInput("mc_method", NULL,
                               choices = c("Tukey" = "tukey",
                                           "Student (unadjusted)" = "student",
                                           "Dunnett" = "dunnett",
                                           "maxT" = "maxT",
                                           "mvtnorm (custom contrasts)" = "mvtnorm"),
                               selected = "tukey"),
            conditionalPanel(
              condition = "input.mc_method && input.mc_method.indexOf('dunnett') >= 0",
              selectInput("dunnett_control", "Dunnett control level", choices = NULL)
            ),
            conditionalPanel(
              condition = "input.mc_method && input.mc_method.indexOf('mvtnorm') >= 0",
              textAreaInput("contrasts_input",
                            "Contrast matrix (rows = contrasts, space-separated coefficients)",
                            rows = 4, placeholder = "1 -1 0\n1 0 -1")
            ),
            actionButton("run_mc_btn", "Run Comparisons",
                         class = "btn-primary w-100 mt-2",
                         icon  = icon("calculator"))
          )
        ),
        verbatimTextOutput("model_run_status")
      )
    )
  ),

  # ── Tab 5: Results ───────────────────────────────────────────────────────
  tabPanel(
    "Results",
    fluidRow(
      column(12,
        wellPanel(
          h5("Models to display"),
          div(style = "max-height: 150px; overflow-y: auto;",
            checkboxGroupInput("show_models", NULL, choices = NULL)
          )
        )
      )
    ),
    tabsetPanel(id = "results_tabs",

      # ── ANOVA ─────────────────────────────────────────────────────────────
      tabPanel("ANOVA Table (Type III)",
        br(),
        tabsetPanel(
          tabPanel("Cross-model comparison",
            br(),
            fluidRow(
              column(8,
                checkboxGroupInput("anova_show", "Show columns:",
                                   choices = c("p-values" = "p",
                                               "F values" = "F",
                                               "VIF"      = "vif"),
                                   selected = "p", inline = TRUE)
              )
            ),
            DT::dataTableOutput("anova_table"),
            br(),
            h5("Model fit summary"),
            DT::dataTableOutput("rmse_table")
          ),
          tabPanel("Single model detail",
            br(),
            selectInput("anova_single_model", "Model", choices = NULL, width = "60%"),
            DT::dataTableOutput("anova_single_table")
          ),
          tabPanel("Collinearity (VIF)",
            br(),
            p(class = "text-muted",
              "VIF values across models (GVIF adjusted for df). ",
              "Amber (> 2.24) and red (> 3.16) cells indicate problematic collinearity."),
            DT::dataTableOutput("vif_table")
          ),
          tabPanel("Coefficients",
            br(),
            p(class = "text-muted",
              "Effects coding (sum-to-zero contrasts): intercept = grand mean, ",
              "factor effects sum to zero. Covariates are centred at their mean. ",
              "The last level of each factor is the reference (derived as negative sum of others)."),
            DT::dataTableOutput("coef_table")
          )
        )
      ),

      # ── Effects (merged LS Means + Covariate Effects + Leverage) ────────
      tabPanel("Effects",
        br(),
        tabsetPanel(id = "effects_tabs",
          tabPanel("Partial Effects",
            br(),
            fluidRow(
              column(4, selectInput("effects_term", "Term to plot", choices = NULL)),
              column(3, radioButtons("effects_view", "View mode",
                                     choices = c("Overlay", "Faceted"),
                                     selected = "Overlay", inline = TRUE)),
              column(3, checkboxInput("effects_show_data", "Show adjusted data", value = TRUE))
            ),
            p(class = "text-muted",
              "Factor terms show LS means with 95% CI. Covariates show partial effect with 95% CI. ",
              "Adjusted data points have other model terms removed."),
            plotlyOutput("effects_plot", height = "500px"),
            br(),
            DT::dataTableOutput("effects_table")
          ),
          tabPanel("Leverage Plots",
            br(),
            fluidRow(
              column(4, selectInput("leverage_model", "Model", choices = NULL))
            ),
            p(class = "text-muted",
              "Added-variable (leverage) plots: Y residuals vs term residuals, ",
              "both after removing all other terms. The slope equals the coefficient; ",
              "the spread shows leverage. A flat line means the term adds nothing."),
            uiOutput("leverage_plots_ui")
          )
        )
      ),

      # ── Multiple Comparisons ──────────────────────────────────────────────
      tabPanel("Multiple Comparisons",
        br(),
        fluidRow(
          column(4,
            checkboxGroupInput("mc_show_method", "Methods to display", choices = NULL)
          ),
          column(4,
            selectInput("mc_show_model", "Model", choices = NULL)
          )
        ),
        plotlyOutput("mc_plot", height = "400px"),
        br(),
        DT::dataTableOutput("mc_table")
      ),

      # ── Residuals ────────────────────────────────────────────────────────
      tabPanel("Residuals",
        br(),
        fluidRow(
          column(4, selectInput("resid_model", "Model", choices = NULL)),
          column(4, selectInput("resid_colour_by", "Colour points by",
                                choices = c("None" = "none"), selected = "none"))
        ),
        tabsetPanel(id = "resid_tabs",
          tabPanel("Standard Diagnostics",
            br(),
            p(class = "text-muted",
              "Q-Q uses internally standardised residuals (rstandard). ",
              "Scale-Location uses externally studentised residuals (rstudent) \u2014 ",
              "these re-fit the model with each point omitted so the deleted residual ",
              "is independent of the fitted value."),
            plotlyOutput("resid_standard", height = "600px")
          ),
          tabPanel("Actual vs. Predicted",
            br(),
            plotlyOutput("resid_actual_vs_pred", height = "450px")
          ),
          tabPanel("vs Run Order",
            br(),
            uiOutput("resid_vs_runorder_ui")
          ),
          tabPanel("vs Model Terms",
            br(),
            p(class = "text-muted",
              "Residuals plotted against each predictor included in the model. ",
              "Patterns suggest non-linearity or missing interactions."),
            uiOutput("resid_vs_terms_ui")
          ),
          tabPanel("vs Omitted Terms",
            br(),
            p(class = "text-muted",
              "Residuals plotted against columns not in the model. ",
              "Trends here suggest potential omitted-variable bias."),
            uiOutput("resid_vs_omitted_ui")
          )
        )
      )
    )
  ),

  # ── Tab 6: Report ────────────────────────────────────────────────────────
  tabPanel(
    "Report",
    fluidRow(
      column(6,
        wellPanel(
          h4("Generate Report"),
          uiOutput("report_summary"),
          hr(),
          radioButtons("report_format", "Output format",
                       choices = c("Word (.docx)" = "word", "PDF" = "pdf",
                                   "HTML" = "html"),
                       selected = "word"),
          downloadButton("download_report", "Download Report",
                         class = "btn-primary w-100 mt-2")
        )
      ),
      column(6,
        wellPanel(
          h5("Report will include:"),
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
    )
  )
)
