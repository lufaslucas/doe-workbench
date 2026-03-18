# R/mod_design.R — Design tab module (UI + server)

# ── UI ───────────────────────────────────────────────────────────────────────
mod_design_ui <- function(id) {
  ns <- NS(id)

  tagList(
    # Shared formula inputs for Assessment & Simulation
    wellPanel(
      h5("Design Model"),
      p(class = "text-muted small",
        "Define the model you plan to fit and a larger alias model to check for confounding. ",
        "Used by Assessment (alias structure & power) and Simulation."),
      fluidRow(
        column(5,
          textInput(ns("alias_full_formula"), "Model formula (terms to fit)",
                    placeholder = "Auto-populated from roles"),
          p(class = "text-muted small", "Terms to include in your planned model (e.g. main effects, interactions, blocks)."),
          div(style = "margin-bottom: 6px;",
            tags$small(class = "text-muted", "Click to add terms:"),
            uiOutput(ns("formula_model_chooser"), inline = TRUE)
          )
        ),
        column(5,
          textInput(ns("alias_check_formula"), "Alias model (terms to check against)",
                    placeholder = "Auto-populated from roles"),
          p(class = "text-muted small",
            "Larger model including higher-order interactions, block \u00d7 factor terms, etc."),
          div(style = "margin-bottom: 6px;",
            tags$small(class = "text-muted", "Click to add terms:"),
            uiOutput(ns("formula_alias_chooser"), inline = TRUE)
          )
        ),
        column(2,
          numericInput(ns("alias_threshold"), "Correlation threshold",
                       value = 0.99, min = 0.5, max = 1, step = 0.01),
          actionButton(ns("alias_auto_fill"), "Reset to defaults",
                       class = "btn-outline-secondary btn-sm w-100 mt-2",
                       icon = icon("undo"))
        )
      )
    ),
    tabsetPanel(id = ns("design_tabs"),

      # ── Visualisation ──
      tabPanel("Visualisation",
        tabsetPanel(id = ns("design_viz_tabs"),

          tabPanel("Design Matrix",
            br(),
            fluidRow(
              column(4,
                radioButtons(ns("design_matrix_mode"), "Display mode",
                             choices = c("Coded dataset" = "coded",
                                         "Model formula terms" = "model",
                                         "Model + alias terms" = "alias"),
                             selected = "coded", inline = TRUE)
              ),
              column(8,
                p(class = "text-muted small mt-2",
                  tags$b("Coded dataset:"), " all columns at their coded/transformed levels. ",
                  tags$b("Model formula:"), " expanded model matrix columns for the design model. ",
                  tags$b("Model + alias:"), " model matrix plus additional alias formula terms.")
              )
            ),
            DT::dataTableOutput(ns("design_matrix_table"))
          ),

          tabPanel("Scatterplot Matrix",
            br(),
            fluidRow(
              column(3, selectInput(ns("splom_colour"), "Colour by",
                                    choices = c("None" = "none"))),
              column(3, sliderInput(ns("splom_jitter"), "Jitter amount",
                                    min = 0, max = 0.4, value = 0.05, step = 0.01)),
              column(3, checkboxInput(ns("splom_fit_line"), "Show fit line", value = TRUE)),
              column(3, checkboxInput(ns("splom_show_corr"), "Show correlations (upper)", value = TRUE))
            ),
            plotOutput(ns("design_splom_gg"), height = "600px")
          ),

          tabPanel("Correlation Map",
            br(),
            fluidRow(
              column(4,
                radioButtons(ns("corr_map_terms"), "Terms to include",
                             choices = c("Model terms" = "model",
                                         "Model + alias terms" = "alias"),
                             selected = "model", inline = TRUE)
              ),
              column(4,
                selectInput(ns("corr_map_formula"), "Formula",
                            choices = NULL)
              ),
              column(4,
                p(class = "text-muted small mt-2",
                  "Absolute correlations between coded model matrix columns. ",
                  "Blue = orthogonal (0), Red = fully confounded (1).")
              )
            ),
            plotOutput(ns("design_corr_map"), height = "700px")
          ),

          tabPanel("2D Design Map",
            br(),
            fluidRow(
              column(2, selectInput(ns("design_x"), "X axis", choices = NULL)),
              column(2, selectInput(ns("design_y"), "Y axis", choices = NULL)),
              column(2, selectInput(ns("design_2d_colour"), "Colour by",
                                    choices = c("None" = "none"))),
              column(2, radioButtons(ns("design_facet_mode"), "Facet mode",
                                     choices = c("Row & Col" = "grid", "Wrap" = "wrap"),
                                     selected = "grid", inline = TRUE)),
              column(2,
                conditionalPanel(
                  condition = paste0("input['", ns("design_facet_mode"), "'] == 'grid'"),
                  selectInput(ns("design_row_facet"), "Facet row",
                              choices = c("None" = "none")),
                  selectInput(ns("design_col_facet"), "Facet col",
                              choices = c("None" = "none"))
                ),
                conditionalPanel(
                  condition = paste0("input['", ns("design_facet_mode"), "'] == 'wrap'"),
                  selectInput(ns("design_wrap_facet"), "Facet wrap",
                              choices = c("None" = "none"))
                )
              )
            ),
            fluidRow(
              column(3, sliderInput(ns("design_2d_jitter"), "Jitter amount",
                                    min = 0, max = 0.4, value = 0.05, step = 0.01)),
              column(3, radioButtons(ns("design_2d_mode"), "Display",
                                     choices = c("Points" = "points", "Heatmap" = "heatmap"),
                                     selected = "points", inline = TRUE)),
              column(3, checkboxInput(ns("design_2d_show_values"), "Show values on heatmap",
                                      value = TRUE))
            ),
            plotlyOutput(ns("design_2d"), height = "500px")
          ),

          tabPanel("3D Design Space",
            br(),
            fluidRow(
              column(2, selectInput(ns("design_3d_x"), "X axis", choices = NULL)),
              column(2, selectInput(ns("design_3d_y"), "Y axis", choices = NULL)),
              column(2, selectInput(ns("design_3d_z"), "Z axis", choices = NULL)),
              column(3, selectInput(ns("design_3d_colour"), "Colour by",
                                    choices = c("None" = "none"))),
              column(3, selectInput(ns("design_3d_shape"), "Shape by",
                                    choices = c("None" = "none")))
            ),
            plotlyOutput(ns("design_3d"), height = "600px")
          )
        )
      ),

      # ── Assessment ──
      tabPanel("Assessment",
        tabsetPanel(id = ns("design_assess_tabs"),

          tabPanel("Alias Structure",
            br(),
            p(class = "text-muted",
              "Compare the model formula against the alias model to find confounded effects. ",
              "|Correlation| = 1 means fully confounded terms."),
            DT::dataTableOutput(ns("alias_table")),
            uiOutput(ns("alias_push_formulas_ui"))
          ),

          tabPanel("Power Analysis",
            br(),
            fluidRow(
              column(3, numericInput(ns("power_sigma"), "Error SD (\u03c3)",
                                     value = 1, min = 0.001, step = 0.1)),
              column(3, numericInput(ns("power_delta"), "Min detectable effect (\u03b4)",
                                     value = 2, min = 0.001, step = 0.1)),
              column(3, numericInput(ns("power_alpha"), "Significance level (\u03b1)",
                                     value = 0.05, min = 0.001, max = 0.5, step = 0.01)),
              column(3, numericInput(ns("power_max_order"), "Max interaction order",
                                     value = 2, min = 1, max = 5))
            ),
            p(class = "text-muted",
              "Power to detect an effect of size \u03b4 given error SD \u03c3. ",
              "Based on the non-centrality parameter of the F distribution."),
            DT::dataTableOutput(ns("power_table")),
            br(),
            plotlyOutput(ns("power_curve"), height = "400px")
          )
        )
      ),

      # ── Simulation ──
      tabPanel("Simulation",
        br(),
        p(class = "text-muted",
          "Specify true coefficients for each model term. ",
          "The Apparent Effect column shows the expected estimate accounting for ",
          "confounding bias from aliased terms."),
        fluidRow(
          column(6,
            h5("Model Term Coefficients"),
            fluidRow(
              column(6, numericInput(ns("sim_set_all_model"), "Set all to:", value = 0, step = 0.5)),
              column(6, actionButton(ns("sim_apply_all_model"), "Apply",
                                     class = "btn-sm btn-outline-secondary mt-4"))
            ),
            uiOutput(ns("sim_model_effects_ui"))
          ),
          column(6,
            h5("Alias Term Coefficients"),
            fluidRow(
              column(6, numericInput(ns("sim_set_all_alias"), "Set all to:", value = 0, step = 0.5)),
              column(6, actionButton(ns("sim_apply_all_alias"), "Apply",
                                     class = "btn-sm btn-outline-secondary mt-4"))
            ),
            uiOutput(ns("sim_alias_effects_ui"))
          )
        ),
        hr(),
        h5("Apparent Effects"),
        p(class = "text-muted small",
          "True Coefficient = specified value. Confounding Bias = contamination from aliased terms. ",
          "Apparent Effect = True Coefficient + Confounding Bias (what you would actually estimate)."),
        DT::dataTableOutput(ns("sim_real_effects_table")),
        hr(),
        h5("Simulate Response Data"),
        p(class = "text-muted small",
          "Generate simulated response data using the coefficients above plus random noise."),
        fluidRow(
          column(3, numericInput(ns("sim_sigma"), "Error SD (\u03c3)",
                                 value = 1, min = 0, step = 0.1)),
          column(3, numericInput(ns("sim_grand_mean"), "Grand mean",
                                 value = 50, step = 1)),
          column(3, checkboxInput(ns("sim_include_alias"), "Include alias effects",
                                  value = TRUE)),
          column(3, actionButton(ns("sim_run"), "Simulate Data",
                                 class = "btn-primary mt-4",
                                 icon = icon("play")))
        ),
        DT::dataTableOutput(ns("sim_data_table"))
      ),

      # ── Balance Checks ──
      tabPanel("Balance",
        br(),
        p(class = "text-muted",
          "Checks for imported designs: replication balance, run-order balance, ",
          "and carryover (previous treatment) patterns. Use the buttons below to add ",
          "previous-treatment columns as covariates for carryover adjustment."),
        fluidRow(
          column(4,
            wellPanel(
              h5("Replication Summary"),
              p(class = "text-muted small",
                "Replication counts per factor and treatment combination. ",
                "Unequal replication reduces power for some comparisons."),
              uiOutput(ns("balance_replication_tables")),
              br(),
              uiOutput(ns("balance_replication_flag"))
            )
          ),
          column(4,
            wellPanel(
              h5("Run-Order Balance"),
              p(class = "text-muted small",
                "Cross-tabulation of factors/treatments vs run-order quartiles. ",
                "Ideally levels should be evenly spread across run orders."),
              uiOutput(ns("balance_runorder_tables"))
            )
          ),
          column(4,
            wellPanel(
              h5("Carryover Balance"),
              p(class = "text-muted small",
                "Which level preceded each level (by run order). Imbalanced carryover ",
                "can bias comparisons in sequential designs."),
              uiOutput(ns("balance_carryover_tables"))
            )
          )
        ),
        uiOutput(ns("balance_cov_panel_ui"))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
mod_design_server <- function(id, rv, colour_theme, role_selectors,
                              shared_reactives, analysis_mode,
                              models_exports, navigate_to, observe_main_nav,
                              available_terms) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Convenience aliases — role selectors
    responses       <- role_selectors$responses
    factors_        <- role_selectors$factors_
    covariates      <- role_selectors$covariates
    blocks          <- role_selectors$blocks
    run_orders      <- role_selectors$run_orders
    all_covariates  <- role_selectors$all_covariates

    # Convenience aliases — shared reactives
    treatment       <- shared_reactives$treatment
    treatment_label <- shared_reactives$treatment_label

    # Convenience aliases — colour theme
    cat_palette       <- colour_theme$cat_palette
    default_col       <- colour_theme$default_col
    cat_scale_colour  <- colour_theme$cat_scale_colour
    cat_scale_fill    <- colour_theme$cat_scale_fill
    cont_scale_colour <- colour_theme$cont_scale_colour
    cont_scale_fill   <- colour_theme$cont_scale_fill
    cont_plotly_cs    <- colour_theme$cont_plotly_cs

    # ── Read-only guard ─────────────────────────────────────────────────
    observe({
      locked <- isTRUE(rv$read_only)
      toggle <- if (locked) shinyjs::disable else shinyjs::enable
      toggle(ns("alias_auto_fill"))
      toggle(ns("alias_full_formula"))
      toggle(ns("alias_check_formula"))
      toggle(ns("alias_push_removed"))
      toggle(ns("alias_push_combined"))
      toggle(ns("sim_run"))
      toggle(ns("sim_apply_all_model"))
      toggle(ns("sim_apply_all_alias"))
      toggle(ns("add_balance_covariates"))
    })

    # ── Populate design selectors when data/roles change ───────────────────
    observe({
      facs <- factors_()
      resps <- responses()
      blks  <- blocks()

      # Colour-by choices: None + treatment + factors + blocks + responses
      colour_choices <- c("None" = "none")
      if (length(facs) > 1) {
        trt_lab <- treatment_label()
        colour_choices <- c(colour_choices, setNames(".treatment", trt_lab))
      }
      for (f in facs) colour_choices <- c(colour_choices, setNames(f, f))
      for (b in blks) colour_choices <- c(colour_choices, setNames(b, paste0(b, " (block)")))
      for (r in resps) colour_choices <- c(colour_choices, setNames(r, paste0(r, " (response)")))

      # Shape-by: treatment + factors + blocks only (categorical)
      shape_choices <- c("None" = "none")
      if (length(facs) > 1) {
        trt_lab <- treatment_label()
        shape_choices <- c(shape_choices, setNames(".treatment", trt_lab))
      }
      for (f in facs) shape_choices <- c(shape_choices, setNames(f, f))
      for (b in blks) shape_choices <- c(shape_choices, setNames(b, paste0(b, " (block)")))

      # Axis choices: factors + blocks, then covariates/run order if needed
      axis_choices <- c(facs, blks)
      if (length(axis_choices) < 3) {
        axis_choices <- c(axis_choices, covariates())
      }
      if (length(axis_choices) < 3) {
        axis_choices <- c(axis_choices, run_orders())
      }
      axis_choices <- unique(axis_choices)

      # Facet choices: treatment + factors + blocks
      facet_choices <- c("None" = "none")
      if (length(facs) > 1) {
        facet_choices <- c(facet_choices, setNames(".treatment", treatment_label()))
      }
      for (f in facs) facet_choices <- c(facet_choices, setNames(f, f))
      for (b in blks) facet_choices <- c(facet_choices, setNames(b, paste0(b, " (block)")))

      updateSelectInput(session, "splom_colour", choices = colour_choices)
      updateSelectInput(session, "design_2d_colour", choices = colour_choices)
      updateSelectInput(session, "design_row_facet", choices = facet_choices)
      updateSelectInput(session, "design_col_facet", choices = facet_choices)
      updateSelectInput(session, "design_wrap_facet", choices = facet_choices)
      updateSelectInput(session, "design_3d_colour", choices = colour_choices)
      updateSelectInput(session, "design_3d_shape", choices = shape_choices)

      # Show/hide 2D and 3D tabs based on available axis variables
      if (length(axis_choices) >= 2) {
        shinyjs::show(selector = "[data-value='2D Design Map']")

        # When blocks exist (e.g. Latin Square), use blocks as axes + treatment as colour
        if (length(blks) >= 2) {
          auto_x <- blks[1]   # Row
          auto_y <- blks[2]   # Col
        } else {
          auto_x <- axis_choices[1]
          auto_y <- axis_choices[2]
        }
        updateSelectInput(session, "design_x", choices = axis_choices,
                          selected = auto_x)
        updateSelectInput(session, "design_y", choices = axis_choices,
                          selected = auto_y)

        # Auto-assign remaining factors to colour/facets
        # Only facet by factors with limited levels (repeated in design)
        remaining <- setdiff(c(facs, blks), c(auto_x, auto_y))
        is_facetable <- function(col) {
          if (is.null(rv$data) || !(col %in% names(rv$data))) return(FALSE)
          vals <- rv$data[[col]]
          n_unique <- length(unique(vals))
          n_rows   <- nrow(rv$data)
          # Facetable: few levels AND levels are repeated (not space-filling)
          n_unique <= 12 && n_rows > n_unique
        }
        facetable  <- Filter(is_facetable, remaining)
        auto_colour <- "none"
        auto_row    <- "none"
        auto_col    <- "none"
        auto_wrap   <- "none"

        # When blocks are on axes, default colour to treatment
        if (length(blks) >= 2) {
          if (length(facs) > 1) {
            auto_colour <- ".treatment"
          } else if (length(facs) == 1) {
            auto_colour <- facs[1]
          }
        }

        # Assign facets: default to grid mode (row + col), no wrap
        if (length(facetable) >= 1) auto_col  <- facetable[1]
        if (length(facetable) >= 2) auto_row  <- facetable[2]
        # 3+ facetable vars: use colour for the third rather than wrap
        if (auto_colour == "none") {
          used <- c(auto_x, auto_y, auto_row, auto_col)
          leftover <- setdiff(remaining, used)
          if (length(leftover) >= 1) auto_colour <- leftover[1]
        }

        updateSelectInput(session, "design_2d_colour",  selected = auto_colour)
        updateRadioButtons(session, "design_facet_mode", selected = "grid")
        updateSelectInput(session, "design_row_facet",  selected = auto_row)
        updateSelectInput(session, "design_col_facet",  selected = auto_col)
        updateSelectInput(session, "design_wrap_facet", selected = auto_wrap)
      } else {
        shinyjs::hide(selector = "[data-value='2D Design Map']")
      }

      if (length(axis_choices) >= 3) {
        shinyjs::show(selector = "[data-value='3D Design Space']")
        updateSelectInput(session, "design_3d_x", choices = axis_choices)
        updateSelectInput(session, "design_3d_y", choices = axis_choices,
                          selected = if (length(axis_choices) > 1) axis_choices[2] else axis_choices[1])
        updateSelectInput(session, "design_3d_z", choices = axis_choices,
                          selected = if (length(axis_choices) > 2) axis_choices[3] else axis_choices[1])

        # Auto-assign colour if there's a remaining factor/block beyond the 3 axes
        remaining_3d <- setdiff(c(facs, blks), axis_choices[1:3])
        if (length(remaining_3d) >= 1) {
          updateSelectInput(session, "design_3d_colour", selected = remaining_3d[1])
        } else if (length(facs) > 1) {
          updateSelectInput(session, "design_3d_colour", selected = ".treatment")
        }
      } else {
        shinyjs::hide(selector = "[data-value='3D Design Space']")
      }
    })

    # ── Design Matrix table (three modes: coded dataset, model formula, model+alias) ──
    output$design_matrix_table <- DT::renderDataTable({
      req(rv$data)
      mode <- input$design_matrix_mode %||% "coded"

      if (mode == "coded") {
        # Coded dataset: all columns with roles, at their coded/transformed levels
        df <- rv$data
        coded <- data.frame(row.names = seq_len(nrow(df)))
        # Include all non-Ignore columns in role order
        role_order <- c("Run Order", "Block", "Factor", "Covariate", "Response")
        for (role in role_order) {
          role_cols <- names(Filter(function(r) r == role, rv$roles))
          for (cn in role_cols) {
            if (!cn %in% names(df)) next
            vals <- df[[cn]]
            # Apply transforms for numeric columns
            tr <- rv$transforms[[cn]]
            if (is.numeric(vals) && !is.null(tr) && tr != "none") {
              if (tr == "centre") {
                vals <- vals - mean(vals, na.rm = TRUE)
              } else if (tr == "coding") {
                cv <- rv$coding_values[[cn]]
                if (!is.null(cv) && !is.null(cv$low) && !is.null(cv$high) && cv$high != cv$low) {
                  vals <- (vals - (cv$high + cv$low) / 2) / ((cv$high - cv$low) / 2)
                }
              }
            }
            coded[[cn]] <- vals
          }
        }
        dt <- dt_table(coded, rownames = TRUE,
                       options = list(pageLength = 30))
        num_cols <- names(coded)[sapply(coded, is.numeric)]
        if (length(num_cols) > 0) dt <- DT::formatRound(dt, num_cols, digits = 3)
        dt

      } else {
        # Model or Alias mode: build expanded model matrix
        full_str  <- input$alias_full_formula %||% ""
        check_str <- input$alias_check_formula %||% ""

        if (nchar(trimws(full_str)) == 0)
          return(DT::datatable(data.frame(Message = "Enter a model formula in the Design Model panel above.")))

        model_terms <- trimws(strsplit(full_str, "\\+")[[1]])
        model_terms <- model_terms[nchar(model_terms) > 0]

        if (mode == "alias" && nchar(trimws(check_str)) > 0) {
          check_terms <- trimws(strsplit(check_str, "\\+")[[1]])
          check_terms <- check_terms[nchar(check_terms) > 0]
          # Alias = model formula terms + any additional terms from alias formula
          all_terms <- unique(c(model_terms, check_terms))
        } else {
          all_terms <- model_terms
        }

        # Build a numeric design matrix for each term
        df <- rv$data
        # Apply coding transforms
        coded_df <- df
        for (cn in names(rv$transforms)) {
          if (!cn %in% names(coded_df) || !is.numeric(coded_df[[cn]])) next
          tr <- rv$transforms[[cn]]
          if (is.null(tr) || tr == "none") next
          if (tr == "centre") {
            coded_df[[cn]] <- coded_df[[cn]] - mean(coded_df[[cn]], na.rm = TRUE)
          } else if (tr == "coding") {
            cv <- rv$coding_values[[cn]]
            if (!is.null(cv) && !is.null(cv$low) && !is.null(cv$high) && cv$high != cv$low) {
              coded_df[[cn]] <- (coded_df[[cn]] - (cv$high + cv$low) / 2) / ((cv$high - cv$low) / 2)
            }
          }
        }

        # For categorical columns, convert to numeric for centering
        for (cn in names(coded_df)) {
          if (!is.numeric(coded_df[[cn]])) {
            coded_df[[cn]] <- as.numeric(as.factor(coded_df[[cn]]))
            coded_df[[cn]] <- coded_df[[cn]] - mean(coded_df[[cn]], na.rm = TRUE)
          }
        }

        build_term_col <- function(term) {
          if (grepl("^I\\(", term)) {
            inner <- gsub("^I\\((.+)\\)$", "\\1", term)
            if (grepl("\\^", inner)) {
              parts <- strsplit(inner, "\\^")[[1]]
              var_name <- trimws(parts[1])
              power <- as.numeric(trimws(parts[2]))
              if (var_name %in% names(coded_df) && !is.na(power))
                return(coded_df[[var_name]]^power)
            }
            return(NULL)
          }
          parts <- strsplit(term, ":")[[1]]
          if (!all(parts %in% names(coded_df))) return(NULL)
          col_vals <- rep(1, nrow(coded_df))
          for (p in parts) col_vals <- col_vals * coded_df[[p]]
          col_vals
        }

        result <- data.frame(row.names = seq_len(nrow(df)))
        for (t in all_terms) {
          cv <- build_term_col(t)
          if (!is.null(cv)) {
            result[[t]] <- cv
          }
        }

        # Flag which terms are model vs alias-only
        if (mode == "alias" && nchar(trimws(check_str)) > 0) {
          alias_only <- setdiff(all_terms, model_terms)
          # Annotate column names to indicate alias-only
          new_names <- names(result)
          for (i in seq_along(new_names)) {
            if (new_names[i] %in% alias_only)
              new_names[i] <- paste0(new_names[i], " \u2020")
          }
          names(result) <- new_names
        }

        if (ncol(result) == 0)
          return(DT::datatable(data.frame(Message = "No valid terms could be computed. Check that term variables exist in the data.")))

        dt <- dt_table(result, rownames = TRUE,
                       options = list(pageLength = 30),
                       caption = if (mode == "alias") "\u2020 = alias-only term (not in model formula)" else NULL)
        num_cols <- names(result)[sapply(result, is.numeric)]
        if (length(num_cols) > 0) dt <- DT::formatRound(dt, num_cols, digits = 3)
        dt
      }
    })

    # ── Scatterplot Matrix ─────────────────────────────────────────────────
    output$design_splom_gg <- renderPlot({
      req(rv$data)
      # Include factors, blocks, and covariates
      facs <- c(factors_(), blocks(), covariates())
      req(length(facs) >= 2)
      df   <- rv$data
      # Add .treatment column if needed
      trt <- treatment()
      if (!is.null(trt)) df$.treatment <- trt
      jit  <- input$splom_jitter %||% 0.15
      show_fit  <- isTRUE(input$splom_fit_line)
      show_corr <- isTRUE(input$splom_show_corr)

      colour_var <- input$splom_colour
      has_colour <- !is.null(colour_var) && colour_var != "none" && colour_var %in% names(df)

      n_facs <- length(facs)
      plots <- list()

      for (i in seq_len(n_facs)) {
        for (j in seq_len(n_facs)) {
          fi <- facs[i]  # row
          fj <- facs[j]  # col
          x_vals <- if (is.numeric(df[[fj]])) df[[fj]] else as.numeric(as.factor(df[[fj]]))
          y_vals <- if (is.numeric(df[[fi]])) df[[fi]] else as.numeric(as.factor(df[[fi]]))

          if (i == j) {
            # Diagonal: histogram of level counts
            plot_df <- data.frame(x = df[[fi]])
            p <- ggplot(plot_df, aes(x = x)) +
              geom_bar(fill = default_col(), alpha = 0.7, colour = "white") +
              labs(x = fi, y = "Count") +
              ggtitle(fi) +
              theme_app() +
              theme(plot.title = element_text(hjust = 0.5, size = 10))

          } else if (i > j) {
            # Lower-left: scatterplot with jitter and optional fit line
            both_factor <- !is.numeric(df[[fi]]) && !is.numeric(df[[fj]])
            plot_df <- data.frame(x = x_vals + runif(length(x_vals), -jit, jit),
                                   y = y_vals + runif(length(y_vals), -jit, jit))
            if (has_colour) {
              plot_df$colour <- as.factor(df[[colour_var]])
              p <- ggplot(plot_df, aes(x = x, y = y, colour = colour)) +
                geom_point(alpha = 0.6, size = 1.5) +
                labs(x = fj, y = fi) +
                theme_app() + theme(legend.position = "none")
            } else {
              p <- ggplot(plot_df, aes(x = x, y = y)) +
                geom_point(alpha = 0.6, size = 1.5, colour = default_col()) +
                labs(x = fj, y = fi) +
                theme_app()
            }
            # Only show fit line for pairs with at least one numeric variable
            if (show_fit && !both_factor &&
                length(unique(x_vals)) >= 2 && length(unique(y_vals)) >= 2) {
              p <- p + geom_smooth(method = "lm", se = FALSE, colour = cat_palette()(2)[2],
                                    linewidth = 0.7, alpha = 0.5)
            }

          } else {
            # Upper-right: association metric
            # Use Cramer's V for factor*factor, Pearson r for numeric pairs
            both_factor <- !is.numeric(df[[fi]]) && !is.numeric(df[[fj]])
            if (show_corr) {
              if (both_factor) {
                # Cramer's V for categorical association
                tbl <- tryCatch(table(df[[fi]], df[[fj]]), error = function(e) NULL)
                r_val <- tryCatch({
                  if (is.null(tbl) || min(dim(tbl)) < 2) NA
                  else {
                    chi <- chisq.test(tbl, correct = FALSE)
                    n <- sum(tbl)
                    k <- min(nrow(tbl), ncol(tbl))
                    sqrt(chi$statistic / (n * (k - 1)))
                  }
                }, error = function(e) NA)
                r_label <- if (is.na(r_val)) "NA" else sprintf("V=%.3f", r_val)
                r_colour <- if (is.na(r_val)) "grey50" else {
                  if (r_val > 0.3) "#d62728" else if (r_val > 0.1) "#ff7f0e" else "#2ca02c"
                }
              } else {
                r_val <- tryCatch(cor(x_vals, y_vals, use = "complete.obs"),
                                   error = function(e) NA)
                r_label <- if (is.na(r_val)) "NA" else sprintf("%.3f", r_val)
                r_colour <- if (is.na(r_val)) "grey50" else {
                  if (r_val >= 0) "#2ca02c" else "#d62728"
                }
              }
              if (is.na(r_val)) {
                p <- ggplot() +
                  annotate("text", x = 0.5, y = 0.5, label = "NA", size = 5, colour = "grey50") +
                  theme_void() + xlim(0, 1) + ylim(0, 1)
              } else {
                bubble_size <- 3 + abs(r_val) * 15
                p <- ggplot() +
                  annotate("point", x = 0.5, y = 0.5, size = bubble_size,
                            colour = r_colour, alpha = 0.3) +
                  annotate("text", x = 0.5, y = 0.5,
                            label = r_label,
                            size = 4, fontface = "bold", colour = r_colour) +
                  labs(x = fj, y = fi) +
                  theme_void() +
                  xlim(0, 1) + ylim(0, 1)
              }
            } else {
              # If correlations disabled, show scatterplot in upper too
              plot_df <- data.frame(x = x_vals + runif(length(x_vals), -jit, jit),
                                     y = y_vals + runif(length(y_vals), -jit, jit))
              p <- ggplot(plot_df, aes(x = x, y = y)) +
                geom_point(alpha = 0.6, size = 1.5, colour = default_col()) +
                labs(x = fj, y = fi) +
                theme_app()
            }
          }

          plots[[(i - 1) * n_facs + j]] <- p
        }
      }

      # Arrange using patchwork or gridExtra
      if (requireNamespace("patchwork", quietly = TRUE)) {
        combined <- patchwork::wrap_plots(plots, ncol = n_facs)
        combined + patchwork::plot_annotation(title = "Scatterplot Matrix")
      } else {
        gridExtra::grid.arrange(grobs = plots, ncol = n_facs,
                                 top = "Scatterplot Matrix")
      }
    }, res = 96)

    # ── Design Correlation Map (JMP-style) ─────────────────────────────────
    # Populate formula selector for the correlation map
    observe({
      choices <- rv$formulas
      if (is.null(choices) || length(choices) == 0) {
        updateSelectInput(session, "corr_map_formula", choices = character(0))
        return()
      }
      names(choices) <- paste0("Formula ", seq_along(choices))
      updateSelectInput(session, "corr_map_formula", choices = choices)
    })

    output$design_corr_map <- renderPlot({
      req(rv$data, input$corr_map_formula)
      formula_str <- input$corr_map_formula
      mode <- input$corr_map_terms %||% "model"

      f <- tryCatch(as.formula(formula_str), error = function(e) NULL)
      req(f)

      resp <- all.vars(f)[1]
      req(resp %in% names(rv$data))

      d <- rv$data

      # Use orthonormal contrasts for proper design evaluation
      # contr.helmert normalized to unit columns gives orthonormal coding
      make_orthonormal <- function(k) {
        C <- contr.helmert(k)
        # Normalize each column to unit length
        apply(C, 2, function(col) col / sqrt(sum(col^2)))
      }
      fac_cols <- names(d)[sapply(d, function(x) is.factor(x) || is.character(x))]
      for (col in fac_cols) {
        d[[col]] <- as.factor(d[[col]])
        nlev <- nlevels(d[[col]])
        if (nlev >= 2) contrasts(d[[col]]) <- make_orthonormal(nlev)
      }

      # Build model matrix for model terms
      model_terms <- attr(terms(f), "term.labels")
      if (length(model_terms) == 0) {
        plot.new(); text(0.5, 0.5, "No model terms to display", cex = 1.2); return()
      }

      mm <- tryCatch({
        model.matrix(f, data = d)[, -1, drop = FALSE]  # remove intercept
      }, error = function(e) NULL)
      if (is.null(mm) || ncol(mm) == 0) {
        plot.new(); text(0.5, 0.5, "Cannot build model matrix", cex = 1.2); return()
      }

      # Also centre numeric columns
      for (j in seq_len(ncol(mm))) {
        if (length(unique(mm[, j])) > 2 || is.numeric(mm[, j])) {
          mm[, j] <- mm[, j] - mean(mm[, j], na.rm = TRUE)
        }
      }

      # Determine the full set of terms for the correlation map
      alias_term_labels <- character()
      use_formula <- f
      if (mode == "alias") {
        fac_names <- intersect(all.vars(f)[-1], names(d))
        alias_terms <- character()
        if (length(fac_names) >= 2) {
          combos2 <- combn(fac_names, 2, simplify = FALSE)
          twofis <- sapply(combos2, paste, collapse = ":")
          alias_terms <- setdiff(twofis, model_terms)
        }
        num_facs <- intersect(fac_names, names(d)[sapply(d, is.numeric)])
        quads <- paste0("I(", num_facs, "^2)")
        alias_terms <- c(alias_terms, setdiff(quads, model_terms))

        if (length(alias_terms) > 0) {
          alias_f <- tryCatch(
            as.formula(paste(resp, "~", paste(c(model_terms, alias_terms), collapse = " + "))),
            error = function(e) NULL
          )
          if (!is.null(alias_f)) {
            use_formula <- alias_f
            alias_term_labels <- alias_terms
          }
        }
      }

      # Build the model and extract the full model matrix + assignment
      m_full <- tryCatch(model_fit(use_formula, data = d), error = function(e) NULL)
      if (is.null(m_full)) {
        plot.new(); text(0.5, 0.5, "Cannot fit model", cex = 1.2); return()
      }
      mm <- model.matrix(m_full)[, -1, drop = FALSE]
      # Centre all columns
      for (j in seq_len(ncol(mm))) {
        mm[, j] <- mm[, j] - mean(mm[, j], na.rm = TRUE)
      }
      assign_vec <- m_full$assign[-1]
      all_terms <- attr(terms(use_formula), "term.labels")

      # Compute per-TERM correlation (collapse multi-column factors)
      # For each pair of terms, compute max absolute correlation between their columns
      term_labels <- all_terms[sort(unique(assign_vec))]
      n_terms <- length(term_labels)

      term_cor <- matrix(NA_real_, n_terms, n_terms,
                         dimnames = list(term_labels, term_labels))
      for (ti in seq_len(n_terms)) {
        cols_i <- which(assign_vec == sort(unique(assign_vec))[ti])
        for (tj in seq_len(n_terms)) {
          if (ti == tj) { term_cor[ti, tj] <- NA; next }
          cols_j <- which(assign_vec == sort(unique(assign_vec))[tj])
          # Max absolute correlation between any pair of columns
          cors <- abs(cor(mm[, cols_i, drop = FALSE],
                          mm[, cols_j, drop = FALSE],
                          use = "complete.obs"))
          term_cor[ti, tj] <- max(cors, na.rm = TRUE)
        }
      }

      # Convert to long form for ggplot
      n <- n_terms
      long <- expand.grid(x = seq_len(n), y = seq_len(n))
      long$value <- as.vector(term_cor)
      long$x_name <- term_labels[long$x]
      long$y_name <- term_labels[long$y]
      long$y <- n + 1 - long$y  # flip y

      # Count model-only terms for separator line
      n_model_terms <- sum(!term_labels %in% alias_term_labels)

      p <- ggplot(long, aes(x = x, y = y, fill = value)) +
        geom_tile(colour = "white", linewidth = 0.3) +
        cont_scale_fill()(limits = c(0, 1), na.value = "grey90", name = "|r|") +
        scale_x_continuous(breaks = seq_len(n), labels = term_labels,
                           expand = c(0, 0), position = "top") +
        scale_y_continuous(breaks = seq_len(n), labels = rev(term_labels),
                           expand = c(0, 0)) +
        labs(x = NULL, y = NULL, title = "Design Correlation Map") +
        theme_minimal(base_size = 11) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0.5, size = 9),
          axis.text.y = element_text(size = 9),
          panel.grid = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold")
        ) +
        coord_fixed()

      # Add correlation text for non-diagonal cells with |r| > 0.01
      label_data <- long[!is.na(long$value) & long$value > 0.01, ]
      if (nrow(label_data) > 0) {
        label_data$label <- sprintf("%.2f", label_data$value)
        label_data$text_colour <- ifelse(label_data$value > 0.6, "white", "black")
        p <- p + geom_text(data = label_data,
                           aes(label = label, colour = text_colour),
                           size = 3, show.legend = FALSE) +
          scale_colour_identity()
      }

      # Add separator line if alias terms are present
      if (mode == "alias" && n_model_terms > 0 && n_model_terms < n) {
        p <- p +
          geom_hline(yintercept = n - n_model_terms + 0.5, colour = "grey40",
                     linewidth = 0.8, linetype = "dashed") +
          geom_vline(xintercept = n_model_terms + 0.5, colour = "grey40",
                     linewidth = 0.8, linetype = "dashed")
      }

      p
    }, res = 96)

    # ── 2D Design Map ──────────────────────────────────────────────────────
    output$design_2d <- renderPlotly({
      req(rv$data, input$design_x, input$design_y)
      df    <- rv$data
      # Add .treatment column if needed
      trt <- treatment()
      if (!is.null(trt)) df$.treatment <- trt
      x_col <- input$design_x; y_col <- input$design_y
      req(x_col %in% names(df), y_col %in% names(df))

      mode <- input$design_2d_mode %||% "points"
      colour_var <- input$design_2d_colour
      has_colour <- !is.null(colour_var) && colour_var != "none" && colour_var %in% names(df)

      # Facet variables (toggle between grid and wrap modes)
      facet_mode <- input$design_facet_mode %||% "grid"
      row_f  <- if (facet_mode == "grid") input$design_row_facet else NULL
      col_f  <- if (facet_mode == "grid") input$design_col_facet else NULL
      wrap_f <- if (facet_mode == "wrap") input$design_wrap_facet else NULL
      # Validate columns exist in data
      if (!is.null(row_f) && !(row_f %in% names(df)))  row_f <- "none"
      if (!is.null(col_f) && !(col_f %in% names(df)))  col_f <- "none"
      if (!is.null(wrap_f) && !(wrap_f %in% names(df))) wrap_f <- "none"
      has_row  <- !is.null(row_f) && row_f != "none"
      has_col  <- !is.null(col_f) && col_f != "none"
      has_wrap <- !is.null(wrap_f) && wrap_f != "none"

      if (mode == "heatmap") {
        # Determine fill variable
        if (has_colour && is.numeric(df[[colour_var]])) {
          fill_var <- colour_var
          fill_label <- colour_var
        } else {
          fill_var <- NULL
          fill_label <- "Count"
        }

        # Convert x/y to factors with sorted levels
        x_levels <- sort(unique(df[[x_col]]))
        y_levels <- sort(unique(df[[y_col]]))
        df[[x_col]] <- factor(df[[x_col]], levels = x_levels)
        df[[y_col]] <- factor(df[[y_col]], levels = y_levels)

        # Build aggregation groups including facet variables
        group_cols <- list(x = df[[x_col]], y = df[[y_col]])
        if (has_wrap) group_cols[[wrap_f]] <- df[[wrap_f]]
        if (has_row)  group_cols[[row_f]]  <- df[[row_f]]
        if (has_col)  group_cols[[col_f]]  <- df[[col_f]]

        if (!is.null(fill_var)) {
          agg <- aggregate(df[[fill_var]], by = group_cols, FUN = mean, na.rm = TRUE)
          names(agg)[ncol(agg)] <- "value"
        } else {
          agg <- aggregate(rep(1, nrow(df)), by = group_cols, FUN = sum)
          names(agg)[ncol(agg)] <- "value"
        }
        names(agg)[1:2] <- c("x", "y")

        p <- ggplot(agg, aes(x = x, y = y, fill = value)) +
          geom_tile(colour = "white", linewidth = 0.5) +
          cont_scale_fill()(name = fill_label) +
          labs(title = paste("Design Heatmap (", fill_label, ")"),
               x = x_col, y = y_col)

        # Conditional value labels toggle
        if (isTRUE(input$design_2d_show_values)) {
          p <- p + geom_text(aes(label = round(value, 2)), size = 3)
        }

        # Faceting on aggregated data
        p <- apply_facets(p, row_var = row_f, col_var = col_f, wrap_var = wrap_f)

        p <- p + theme_app()
        ggplotly(p)

      } else {
        # Points mode
        jit <- input$design_2d_jitter %||% 0.15

        # Keep numeric factors on continuous scales; convert categorical to factor
        x_numeric <- is.numeric(df[[x_col]])
        y_numeric <- is.numeric(df[[y_col]])
        if (!x_numeric) df[[x_col]] <- as.factor(df[[x_col]])
        if (!y_numeric) df[[y_col]] <- as.factor(df[[y_col]])

        # Adjust jitter: only apply to axes that are factor/discrete
        jit_w <- if (!x_numeric) jit else 0
        jit_h <- if (!y_numeric) jit else 0

        if (has_colour) {
          if (!is.numeric(df[[colour_var]])) df[[colour_var]] <- as.factor(df[[colour_var]])
          p <- ggplot(df, aes_string(x = x_col, y = y_col, colour = colour_var)) +
            geom_jitter(width = jit_w, height = jit_h, alpha = 0.7, size = 2.5)
          if (is.numeric(df[[colour_var]]))
            p <- p + cont_scale_colour()()
          else
            p <- p + cat_scale_colour()()
        } else {
          pal <- cat_palette()
          p <- ggplot(df, aes_string(x = x_col, y = y_col)) +
            geom_jitter(width = jit_w, height = jit_h, alpha = 0.7, size = 2.5, colour = pal(1)[1])
        }
        # Rename .treatment legend to dynamic label
        colour_label <- if (!is.null(colour_var) && colour_var == ".treatment") treatment_label() else colour_var
        p <- p + labs(title = "2D Design Map", x = x_col, y = y_col, colour = colour_label)

        # Faceting on original data
        p <- apply_facets(p, row_var = row_f, col_var = col_f, wrap_var = wrap_f)

        p <- p + theme_app()
        ggplotly(p)
      }
    })

    # ── 3D Design Space ────────────────────────────────────────────────────
    output$design_3d <- renderPlotly({
      req(rv$data, input$design_3d_x, input$design_3d_y, input$design_3d_z)
      df <- rv$data
      # Add .treatment column if needed
      trt <- treatment()
      if (!is.null(trt)) df$.treatment <- trt
      xc <- input$design_3d_x; yc <- input$design_3d_y; zc <- input$design_3d_z
      req(xc %in% names(df), yc %in% names(df), zc %in% names(df))

      # Convert to numeric positions; track categorical axes for custom tick labels
      cat_axis <- list(x = FALSE, y = FALSE, z = FALSE)
      axis_levels <- list(x = NULL, y = NULL, z = NULL)

      to_numeric <- function(vals, axis_key) {
        if (is.numeric(vals)) return(vals)
        fv <- as.factor(vals)
        cat_axis[[axis_key]] <<- TRUE
        axis_levels[[axis_key]] <<- levels(fv)
        as.numeric(fv)
      }
      x_vals <- to_numeric(df[[xc]], "x")
      y_vals <- to_numeric(df[[yc]], "y")
      z_vals <- to_numeric(df[[zc]], "z")

      # Add jitter for discrete-looking columns
      jit <- 0.08
      if (length(unique(x_vals)) <= 10) x_vals <- x_vals + runif(length(x_vals), -jit, jit)
      if (length(unique(y_vals)) <= 10) y_vals <- y_vals + runif(length(y_vals), -jit, jit)
      if (length(unique(z_vals)) <= 10) z_vals <- z_vals + runif(length(z_vals), -jit, jit)

      hover_text <- paste0(xc, " = ", df[[xc]], "\n",
                           yc, " = ", df[[yc]], "\n",
                           zc, " = ", df[[zc]])

      colour_var <- input$design_3d_colour
      shape_var  <- input$design_3d_shape

      has_colour <- !is.null(colour_var) && colour_var != "none" && colour_var %in% names(df)
      has_shape  <- !is.null(shape_var) && shape_var != "none" && shape_var %in% names(df)

      # Pre-compute shape symbols if shape variable is active
      shape_symbols <- NULL
      if (has_shape) {
        sym_map <- c("circle", "square", "diamond", "cross", "x",
                     "triangle-up", "triangle-down", "star")
        sv <- as.factor(df[[shape_var]])
        shape_symbols <- sym_map[((as.integer(sv) - 1L) %% length(sym_map)) + 1L]
      }

      mk_base <- list(size = 5)
      if (!is.null(shape_symbols)) mk_base$symbol <- shape_symbols

      # Dynamic legend labels for .treatment
      colour_label <- if (has_colour && colour_var == ".treatment") treatment_label() else colour_var
      shape_label  <- if (has_shape && shape_var == ".treatment") treatment_label() else shape_var

      if (has_colour && is.numeric(df[[colour_var]])) {
        mk <- c(mk_base, list(color = df[[colour_var]], colorscale = "Viridis",
                               showscale = TRUE,
                               colorbar = list(title = colour_label)))
        p <- plot_ly(x = x_vals, y = y_vals, z = z_vals,
                     type = "scatter3d", mode = "markers",
                     marker = mk, text = hover_text, hoverinfo = "text")
      } else if (has_colour) {
        # For categorical colour with shape, build traces per group
        if (has_shape) {
          cv <- as.factor(df[[colour_var]])
          pal <- cat_palette()(length(levels(cv)))
          p <- plot_ly()
          for (gi in seq_along(levels(cv))) {
            g <- levels(cv)[gi]
            idx <- which(cv == g)
            p <- add_trace(p, x = x_vals[idx], y = y_vals[idx], z = z_vals[idx],
                           type = "scatter3d", mode = "markers",
                           marker = list(size = 5, color = pal[gi],
                                         symbol = shape_symbols[idx]),
                           text = hover_text[idx], hoverinfo = "text",
                           name = g)
          }
        } else {
          cv <- as.factor(df[[colour_var]])
          pal <- cat_palette()(length(levels(cv)))
          p <- plot_ly(x = x_vals, y = y_vals, z = z_vals,
                       color = cv, colors = pal,
                       type = "scatter3d", mode = "markers",
                       marker = mk_base,
                       text = hover_text, hoverinfo = "text")
        }
      } else {
        mk <- c(mk_base, list(color = default_col()))
        p <- plot_ly(x = x_vals, y = y_vals, z = z_vals,
                     type = "scatter3d", mode = "markers",
                     marker = mk, text = hover_text, hoverinfo = "text")
      }

      # Build axis specs with custom tick labels for categorical axes
      make_axis <- function(title, axis_key) {
        ax <- list(title = title)
        if (cat_axis[[axis_key]]) {
          lvls <- axis_levels[[axis_key]]
          ax$tickvals <- seq_along(lvls)
          ax$ticktext <- lvls
          ax$tickmode <- "array"
        }
        ax
      }
      p %>% layout(
        title = "3D Design Space",
        scene = list(xaxis = make_axis(xc, "x"),
                     yaxis = make_axis(yc, "y"),
                     zaxis = make_axis(zc, "z")))
    })

    # ── Formula Term Chooser Buttons ───────────────────────────────────────

    # Helper: build term chooser buttons for either model or alias formula
    build_term_buttons <- function(terms, target, btn_class) {
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
      for (grp_name in names(groups)) {
        grp_terms <- groups[[grp_name]]
        if (is.null(grp_terms) || length(grp_terms) == 0) next
        for (t in grp_terms) {
          # Use namespaced input ID for Shiny.setInputValue inside module
          js_call <- sprintf(
            "Shiny.setInputValue('%s', {term: '%s', nonce: Math.random()});",
            ns(paste0(target, "_term_click")), gsub("'", "\\\\'", t)
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

    # Render model formula chooser buttons
    output$formula_model_chooser <- renderUI({
      terms <- available_terms()
      if (length(unlist(terms)) == 0) return(NULL)
      build_term_buttons(terms, "model", "btn-outline-primary")
    })

    # Render alias formula chooser buttons
    output$formula_alias_chooser <- renderUI({
      terms <- available_terms()
      if (length(unlist(terms)) == 0) return(NULL)
      build_term_buttons(terms, "alias", "btn-outline-info")
    })

    # Observer: when any model term button is clicked, append to model formula
    observeEvent(input$model_term_click, {
      if (isTRUE(rv$read_only)) return()
      term <- input$model_term_click$term
      if (is.null(term)) return()
      current <- input$alias_full_formula %||% ""
      if (nchar(trimws(current)) == 0) {
        updateTextInput(session, "alias_full_formula", value = term)
      } else {
        existing <- trimws(strsplit(current, "\\+")[[1]])
        if (!term %in% existing) {
          updateTextInput(session, "alias_full_formula",
                          value = paste0(current, " + ", term))
        }
      }
    })

    # Observer: when any alias term button is clicked, append to alias formula
    observeEvent(input$alias_term_click, {
      if (isTRUE(rv$read_only)) return()
      term <- input$alias_term_click$term
      if (is.null(term)) return()
      current <- input$alias_check_formula %||% ""
      if (nchar(trimws(current)) == 0) {
        updateTextInput(session, "alias_check_formula", value = term)
      } else {
        existing <- trimws(strsplit(current, "\\+")[[1]])
        if (!term %in% existing) {
          updateTextInput(session, "alias_check_formula",
                          value = paste0(current, " + ", term))
        }
      }
    })

    # Render custom formula term chooser buttons (for Models tab — rendered here,
    # wired via models_exports callbacks for cross-module communication)
    output$formula_custom_chooser <- renderUI({
      terms <- available_terms()
      if (length(unlist(terms)) == 0) return(NULL)
      build_term_buttons(terms, "custom", "btn-outline-secondary")
    })

    # Observer: when any custom term button is clicked, append to custom formula
    observeEvent(input$custom_term_click, {
      term <- input$custom_term_click$term
      if (is.null(term)) return()
      current <- models_exports$get_custom_formula() %||% ""
      # If empty or just "y ~ " placeholder, start with response ~ term
      stripped <- trimws(current)
      if (nchar(stripped) == 0) {
        # Auto-prepend response variable
        resp <- models_exports$get_active_response() %||% "Y"
        models_exports$set_custom_formula(paste0(resp, " ~ ", term))
      } else {
        # Extract RHS (after ~)
        if (grepl("~", stripped)) {
          rhs <- trimws(sub("^[^~]+~", "", stripped))
          existing <- trimws(strsplit(rhs, "\\+")[[1]])
          if (!term %in% existing) {
            models_exports$set_custom_formula(paste0(stripped, " + ", term))
          }
        } else {
          models_exports$set_custom_formula(paste0(stripped, " + ", term))
        }
      }
    })

    # ── Alias Structure ────────────────────────────────────────────────────

    # Helper: build auto-fill formula strings from current roles
    auto_fill_alias <- function() {
      facs <- factors_()
      blks <- blocks()
      covs <- all_covariates()
      if (length(facs) == 0) return()

      # Full model: main effects + 2FI + blocks (typical design model)
      full_parts <- facs
      if (length(facs) >= 2) {
        combos2 <- combn(facs, 2, simplify = FALSE)
        full_parts <- c(full_parts, sapply(combos2, paste, collapse = ":"))
      }
      if (length(blks) > 0) full_parts <- c(full_parts, blks)
      if (length(covs) > 0) full_parts <- c(full_parts, covs)
      updateTextInput(session, "alias_full_formula",
                      value = paste(full_parts, collapse = " + "))

      # Alias model: all factor interactions + block x factor
      check_parts <- facs
      max_ord <- min(length(facs), 4)
      if (max_ord >= 2) {
        for (ord in 2:max_ord) {
          combos <- combn(facs, ord, simplify = FALSE)
          check_parts <- c(check_parts, sapply(combos, paste, collapse = ":"))
        }
      }
      if (length(blks) > 0)
        check_parts <- c(check_parts, blks,
                          as.vector(outer(blks, facs, function(b, f) paste0(b, ":", f))))
      if (length(covs) > 0)
        check_parts <- c(check_parts, covs,
                          as.vector(outer(covs, facs, function(cv, f) paste0(cv, ":", f))))
      updateTextInput(session, "alias_check_formula",
                      value = paste(check_parts, collapse = " + "))
    }

    # Reset to defaults on button click
    observeEvent(input$alias_auto_fill, auto_fill_alias())

    # Clear alias formulas when data changes, then auto-fill if roles exist
    observeEvent(rv$data, {
      updateTextInput(session, "alias_full_formula", value = "")
      updateTextInput(session, "alias_check_formula", value = "")
      if (length(factors_()) > 0) auto_fill_alias()
    }, ignoreInit = TRUE)

    # Auto-fill when user navigates to Design tab (only if formulas are empty)
    # main_nav is on the parent session (app-level tabsetPanel)
    observeEvent(observe_main_nav(), {
      if (observe_main_nav() == "Design") {
        isolate({
          if (nchar(trimws(input$alias_full_formula %||% "")) == 0 && length(factors_()) > 0)
            auto_fill_alias()
        })
      }
    })

    # Alias Structure table
    output$alias_table <- DT::renderDataTable({
      req(rv$data)
      full_str  <- input$alias_full_formula  %||% ""
      check_str <- input$alias_check_formula %||% ""
      threshold <- input$alias_threshold %||% 0.99

      if (nchar(trimws(full_str)) == 0)
        return(DT::datatable(data.frame(Message = "Enter a full model formula.")))

      # Parse terms from the RHS strings
      full_terms  <- trimws(strsplit(full_str, "\\+")[[1]])
      check_terms <- if (nchar(trimws(check_str)) > 0)
                       trimws(strsplit(check_str, "\\+")[[1]])
                     else NULL

      result <- compute_aliases(rv$data, full_terms, check_terms, threshold)
      dt <- dt_table(result, rownames = FALSE,
                      options = list(pageLength = 30))
      if ("Correlation" %in% names(result))
        dt <- DT::formatRound(dt, "Correlation", digits = 3)
      dt
    })

    # Show "Send to Models" buttons when fully aliased pairs exist
    output$alias_push_formulas_ui <- renderUI({
      req(rv$data)
      full_str  <- input$alias_full_formula  %||% ""
      check_str <- input$alias_check_formula %||% ""
      threshold <- input$alias_threshold %||% 0.99
      if (nchar(trimws(full_str)) == 0) return(NULL)

      full_terms  <- trimws(strsplit(full_str, "\\+")[[1]])
      full_terms  <- full_terms[nchar(full_terms) > 0]
      check_terms <- if (nchar(trimws(check_str)) > 0)
                       trimws(strsplit(check_str, "\\+")[[1]]) else NULL

      result <- compute_aliases(rv$data, full_terms, check_terms, threshold)
      if (!"Correlation" %in% names(result) || nrow(result) == 0) return(NULL)

      # Find fully aliased pairs (|r| >= threshold) where both terms are in the model
      fully <- result[abs(result$Correlation) >= threshold, , drop = FALSE]
      if ("In_model" %in% names(fully))
        fully <- fully[fully$In_model == "Both in model", , drop = FALSE]
      if (nrow(fully) == 0) return(NULL)

      # Build two suggested formulas
      resp <- models_exports$get_active_response() %||% "Y"
      terms_to_remove <- unique(c(fully$Term_1, fully$Term_2))
      terms_to_drop_second <- unique(fully$Term_2)

      # Formula 1: remove both terms from each aliased pair
      clean_terms <- setdiff(full_terms, terms_to_remove)
      f_removed <- if (length(clean_terms) > 0)
        paste0(resp, " ~ ", paste(clean_terms, collapse = " + "))
      else NULL

      # Formula 2: keep first term from each pair, relabel
      keep_terms <- setdiff(full_terms, terms_to_drop_second)
      relabels <- character()
      for (i in seq_len(nrow(fully))) {
        t1 <- fully$Term_1[i]; t2 <- fully$Term_2[i]; corr <- fully$Correlation[i]
        sign_sym <- if (corr >= 0) "+" else "-"
        relabels <- c(relabels, paste0(t1, " \u2192 ", t1, " ", sign_sym, " ", t2))
      }
      f_combined <- paste0(resp, " ~ ", paste(keep_terms, collapse = " + "))

      tagList(
        hr(),
        h6("Send resolved formula to Models tab"),
        p(class = "text-muted small",
          "Push a formula with aliases resolved to the custom formula builder."),
        fluidRow(
          if (!is.null(f_removed)) column(6,
            tags$div(class = "card p-2 mb-2",
              tags$small(tags$b("Remove aliased pairs:")),
              tags$br(),
              tags$code(f_removed, style = "font-size: 0.8em;"),
              tags$br(),
              actionButton(ns("alias_push_removed"), "Send to Models",
                           class = "btn-sm btn-outline-primary mt-1",
                           icon = icon("arrow-right"))
            )
          ),
          column(6,
            tags$div(class = "card p-2 mb-2",
              tags$small(tags$b("Combine aliased pairs:")),
              tags$br(),
              tags$code(f_combined, style = "font-size: 0.8em;"),
              tags$br(),
              tags$small(class = "text-muted", paste(relabels, collapse = "; ")),
              tags$br(),
              actionButton(ns("alias_push_combined"), "Send to Models",
                           class = "btn-sm btn-outline-primary mt-1",
                           icon = icon("arrow-right"))
            )
          )
        )
      )
    })

    # Push alias-resolved formula to custom formula input on Models tab
    observeEvent(input$alias_push_removed, {
      if (is_locked(rv, "Alias push")) return()
      req(rv$data)
      full_str  <- input$alias_full_formula %||% ""
      threshold <- input$alias_threshold %||% 0.99
      check_str <- input$alias_check_formula %||% ""
      full_terms  <- trimws(strsplit(full_str, "\\+")[[1]])
      full_terms  <- full_terms[nchar(full_terms) > 0]
      check_terms <- if (nchar(trimws(check_str)) > 0)
                       trimws(strsplit(check_str, "\\+")[[1]]) else NULL
      result <- compute_aliases(rv$data, full_terms, check_terms, threshold)
      fully <- result[abs(result$Correlation) >= threshold, , drop = FALSE]
      if ("In_model" %in% names(fully))
        fully <- fully[fully$In_model == "Both in model", , drop = FALSE]
      terms_to_remove <- unique(c(fully$Term_1, fully$Term_2))
      clean_terms <- setdiff(full_terms, terms_to_remove)
      resp <- models_exports$get_active_response() %||% "Y"
      if (length(clean_terms) > 0) {
        f <- paste0(resp, " ~ ", paste(clean_terms, collapse = " + "))
        models_exports$set_custom_formula(f)
        navigate_to("Models")
        showNotification("Formula sent to Models tab (aliased pairs removed).", type = "message")
      }
    })

    observeEvent(input$alias_push_combined, {
      if (is_locked(rv, "Alias push")) return()
      req(rv$data)
      full_str  <- input$alias_full_formula %||% ""
      threshold <- input$alias_threshold %||% 0.99
      check_str <- input$alias_check_formula %||% ""
      full_terms  <- trimws(strsplit(full_str, "\\+")[[1]])
      full_terms  <- full_terms[nchar(full_terms) > 0]
      check_terms <- if (nchar(trimws(check_str)) > 0)
                       trimws(strsplit(check_str, "\\+")[[1]]) else NULL
      result <- compute_aliases(rv$data, full_terms, check_terms, threshold)
      fully <- result[abs(result$Correlation) >= threshold, , drop = FALSE]
      if ("In_model" %in% names(fully))
        fully <- fully[fully$In_model == "Both in model", , drop = FALSE]
      terms_to_drop <- unique(fully$Term_2)
      keep_terms <- setdiff(full_terms, terms_to_drop)
      resp <- models_exports$get_active_response() %||% "Y"
      f <- paste0(resp, " ~ ", paste(keep_terms, collapse = " + "))
      models_exports$set_custom_formula(f)

      # Store alias labels for display
      new_labels <- rv$alias_labels
      for (i in seq_len(nrow(fully))) {
        t1 <- fully$Term_1[i]; t2 <- fully$Term_2[i]; corr <- fully$Correlation[i]
        sign_sym <- if (corr >= 0) "+" else "-"
        new_labels[[t1]] <- paste0(t1, " ", sign_sym, " ", t2)
      }
      rv$alias_labels <- new_labels

      navigate_to("Models")
      showNotification("Formula sent to Models tab (aliased pairs combined). Labels will apply after fitting.",
                       type = "message")
    })

    # ── Power Analysis ─────────────────────────────────────────────────────
    output$power_table <- DT::renderDataTable({
      req(rv$data, length(factors_()) > 0)
      sigma     <- input$power_sigma     %||% 1
      delta     <- input$power_delta     %||% 1
      alpha     <- input$power_alpha     %||% 0.05
      max_order <- input$power_max_order %||% 2
      result <- design_power(rv$data, factors_(), sigma, delta, alpha, max_order)
      if (nrow(result) == 0)
        return(DT::datatable(data.frame(Message = "No terms computed.")))
      dt <- dt_table(result, rownames = FALSE,
                      options = list(pageLength = 30), dom = "Bt")
      if ("Power" %in% names(result)) {
        dt <- DT::formatRound(dt, "Power", digits = 3)
        dt <- DT::formatRound(dt, "SE", digits = 4)
        dt <- DT::formatStyle(dt, "Power",
                 backgroundColor = DT::styleInterval(
                   c(0.5, 0.8), c("#f8d7da", "#fff3cd", "#c8f7c5")),
                 fontWeight = DT::styleInterval(c(0.8), c("normal", "bold")))
      }
      dt
    })

    # Power Curve: power vs effect size for each term
    output$power_curve <- renderPlotly({
      req(rv$data, length(factors_()) > 0)
      sigma     <- input$power_sigma     %||% 1
      alpha     <- input$power_alpha     %||% 0.05
      max_order <- input$power_max_order %||% 2
      req(sigma > 0)

      deltas <- seq(0.1, sigma * 4, length.out = 25)
      curves <- lapply(deltas, function(d) {
        pw <- design_power(rv$data, factors_(), sigma, d, alpha, max_order)
        if (nrow(pw) > 0 && "Power" %in% names(pw)) pw$delta <- d
        pw
      })
      curves_df <- bind_rows(curves)
      if (nrow(curves_df) == 0 || !"Power" %in% names(curves_df))
        return(plotly_empty())

      user_delta <- input$power_delta %||% 1

      p <- ggplot(curves_df, aes(x = delta, y = Power, colour = Term)) +
        geom_line(linewidth = 0.8) +
        geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey50") +
        geom_vline(xintercept = user_delta, linetype = "dashed", colour = "#DD1D21",
                   linewidth = 0.6) +
        annotate("text", x = max(deltas) * 0.9, y = 0.82, label = "80% power",
                 colour = "grey50", size = 3) +
        annotate("text", x = user_delta, y = 0.02,
                 label = paste0("\u03b4 = ", user_delta),
                 colour = "#DD1D21", size = 3, hjust = -0.1) +
        labs(title = "Power Curve", x = "Effect size (\u03b4)", y = "Power",
             colour = "Term") +
        ylim(0, 1) +
        theme_app()
      ggplotly(p)
    })

    # ── Simulation ─────────────────────────────────────────────────────────

    # Dynamic UI: model term effect inputs
    output$sim_model_effects_ui <- renderUI({
      full_str <- input$alias_full_formula %||% ""
      if (nchar(trimws(full_str)) == 0)
        return(p(class = "text-muted", "Enter a model formula above."))

      model_terms <- trimws(strsplit(full_str, "\\+")[[1]])
      model_terms <- model_terms[nchar(model_terms) > 0]
      if (length(model_terms) == 0)
        return(p(class = "text-muted", "No valid terms in model formula."))

      rows <- split(seq_along(model_terms), ceiling(seq_along(model_terms) / 3))
      tagList(lapply(rows, function(indices) {
        fluidRow(lapply(indices, function(i) {
          column(4, numericInput(ns(paste0("sim_eff_", i)), model_terms[i],
                                 value = 0, step = 0.5))
        }))
      }))
    })

    # Dynamic UI: alias term effect inputs
    output$sim_alias_effects_ui <- renderUI({
      full_str <- input$alias_full_formula %||% ""
      check_str <- input$alias_check_formula %||% ""
      if (nchar(trimws(full_str)) == 0 || nchar(trimws(check_str)) == 0)
        return(p(class = "text-muted", "Enter both formulas above."))

      model_terms <- trimws(strsplit(full_str, "\\+")[[1]])
      check_terms <- trimws(strsplit(check_str, "\\+")[[1]])
      alias_only <- setdiff(check_terms, model_terms)
      alias_only <- alias_only[nchar(alias_only) > 0]
      if (length(alias_only) == 0)
        return(p(class = "text-muted", "No additional alias terms."))

      rows <- split(seq_along(alias_only), ceiling(seq_along(alias_only) / 3))
      tagList(lapply(rows, function(indices) {
        fluidRow(lapply(indices, function(i) {
          column(4, numericInput(ns(paste0("sim_ali_", i)), alias_only[i],
                                 value = 0, step = 0.5))
        }))
      }))
    })

    # Real effects table (updates reactively as inputs change)
    output$sim_real_effects_table <- DT::renderDataTable({
      req(rv$data)
      full_str <- input$alias_full_formula %||% ""
      check_str <- input$alias_check_formula %||% ""
      if (nchar(trimws(full_str)) == 0)
        return(DT::datatable(data.frame(Message = "Enter a model formula above.")))

      model_terms <- trimws(strsplit(full_str, "\\+")[[1]])
      model_terms <- model_terms[nchar(model_terms) > 0]
      check_terms <- if (nchar(trimws(check_str)) > 0)
                       trimws(strsplit(check_str, "\\+")[[1]]) else character(0)

      # Gather model effects
      model_effects <- list()
      for (i in seq_along(model_terms)) {
        val <- input[[paste0("sim_eff_", i)]]
        model_effects[[model_terms[i]]] <- if (!is.null(val)) val else 0
      }

      # Always gather alias effects (regardless of sim_include_alias checkbox)
      alias_only <- setdiff(check_terms, model_terms)
      alias_effects <- NULL
      if (length(alias_only) > 0) {
        alias_effects <- list()
        for (i in seq_along(alias_only)) {
          val <- input[[paste0("sim_ali_", i)]]
          alias_effects[[alias_only[i]]] <- if (!is.null(val)) val else 0
        }
      }

      alias_info <- compute_alias_matrix(rv$data, model_terms,
                      if (length(check_terms) > 0) check_terms else model_terms)
      if (is.null(alias_info))
        return(DT::datatable(data.frame(Message = "Cannot compute alias matrix (singular design?).")))

      result <- compute_real_effects(alias_info, model_effects, alias_effects)

      # Add Confounded_With column showing alias structure regardless of effect sizes
      A <- alias_info$A
      if (length(alias_info$alias_terms) > 0 && ncol(A) > 0) {
        result$Confounded_With <- sapply(seq_len(nrow(A)), function(i) {
          nonzero <- which(abs(A[i, ]) > 0.1)
          if (length(nonzero) == 0) return("")
          paste(paste0(alias_info$alias_terms[nonzero], "(", round(A[i, nonzero], 2), ")"),
                collapse = ", ")
        })
      } else {
        result$Confounded_With <- ""
      }

      # Add simulated OLS estimate if simulation data exists
      # Uses same centred design matrix (X_model) that generated the data
      if (!is.null(rv$sim_data)) {
        Xm <- alias_info$X_model
        y  <- rv$sim_data
        # OLS: beta_hat = (X'X)^-1 X'y
        beta_hat <- tryCatch({
          as.numeric(solve(crossprod(Xm), crossprod(Xm, y - mean(y))))
        }, error = function(e) NULL)
        if (!is.null(beta_hat) && length(beta_hat) == length(alias_info$model_terms)) {
          names(beta_hat) <- alias_info$model_terms
          result$Simulated_Estimate <- round(
            sapply(result$Term, function(t) beta_hat[t] %||% NA_real_), 4)
        }
      }

      dt <- dt_table(result, rownames = FALSE,
                      options = list(pageLength = 50), dom = "Bt")
      num_cols <- intersect(c("True_Coefficient", "Confounding_Bias", "Apparent_Effect",
                              "Simulated_Estimate"), names(result))
      if (length(num_cols) > 0) dt <- DT::formatRound(dt, num_cols, digits = 4)
      dt
    })

    # Set all model/alias effects to a single value
    observeEvent(input$sim_apply_all_model, {
      if (isTRUE(rv$read_only)) return()
      val <- input$sim_set_all_model %||% 0
      full_str <- input$alias_full_formula %||% ""
      model_terms <- trimws(strsplit(full_str, "\\+")[[1]])
      model_terms <- model_terms[nchar(model_terms) > 0]
      for (i in seq_along(model_terms))
        updateNumericInput(session, paste0("sim_eff_", i), value = val)
    })

    observeEvent(input$sim_apply_all_alias, {
      if (isTRUE(rv$read_only)) return()
      val <- input$sim_set_all_alias %||% 0
      full_str <- input$alias_full_formula %||% ""
      check_str <- input$alias_check_formula %||% ""
      model_terms <- trimws(strsplit(full_str, "\\+")[[1]])
      check_terms <- trimws(strsplit(check_str, "\\+")[[1]])
      alias_only <- setdiff(check_terms, model_terms)
      alias_only <- alias_only[nchar(alias_only) > 0]
      for (i in seq_along(alias_only))
        updateNumericInput(session, paste0("sim_ali_", i), value = val)
    })

    # Simulate data on button click
    observeEvent(input$sim_run, {
      if (is_locked(rv, "Simulation")) return()
      req(rv$data)
      full_str <- input$alias_full_formula %||% ""
      check_str <- input$alias_check_formula %||% ""
      if (nchar(trimws(full_str)) == 0) {
        showNotification("Enter a model formula first.", type = "warning")
        return()
      }

      model_terms <- trimws(strsplit(full_str, "\\+")[[1]])
      model_terms <- model_terms[nchar(model_terms) > 0]
      check_terms <- if (nchar(trimws(check_str)) > 0)
                       trimws(strsplit(check_str, "\\+")[[1]]) else character(0)

      model_effects <- list()
      for (i in seq_along(model_terms)) {
        val <- input[[paste0("sim_eff_", i)]]
        model_effects[[model_terms[i]]] <- if (!is.null(val)) val else 0
      }

      alias_only <- setdiff(check_terms, model_terms)
      alias_effects <- NULL
      if (isTRUE(input$sim_include_alias) && length(alias_only) > 0) {
        alias_effects <- list()
        for (i in seq_along(alias_only)) {
          val <- input[[paste0("sim_ali_", i)]]
          alias_effects[[alias_only[i]]] <- if (!is.null(val)) val else 0
        }
      }

      alias_info <- compute_alias_matrix(rv$data, model_terms,
                      if (length(check_terms) > 0) check_terms else model_terms)
      if (is.null(alias_info)) {
        showNotification("Cannot compute alias matrix.", type = "error")
        return()
      }

      sigma <- input$sim_sigma %||% 1
      grand_mean <- input$sim_grand_mean %||% 50

      rv$sim_data <- simulate_design_response(
        rv$data, alias_info, model_effects, alias_effects, sigma, grand_mean
      )
      showNotification("Simulated data generated.", type = "message")
    })

    # Display simulated data
    output$sim_data_table <- DT::renderDataTable({
      req(rv$data, rv$sim_data)
      sim_df <- rv$data
      sim_df$Simulated_Y <- round(rv$sim_data, 4)
      dt_table(sim_df, rownames = FALSE,
               options = list(pageLength = 20)) %>%
        DT::formatRound("Simulated_Y", digits = 4)
    })

    # ── Balance Checks ─────────────────────────────────────────────────────

    # Treatment combination column for balance checks
    balance_treatment <- reactive({
      req(rv$data)
      facs <- factors_()
      req(length(facs) > 0)
      facs_in <- intersect(facs, names(rv$data))
      req(length(facs_in) > 0)
      apply(rv$data[, facs_in, drop = FALSE], 1, function(r) paste(r, collapse = ":"))
    })

    # Replication summary — per-factor + treatment combination tables
    output$balance_replication_tables <- renderUI({
      req(rv$data)
      facs <- factors_()
      blks <- blocks()
      req(length(facs) > 0)
      n <- nrow(rv$data)

      # Check if factors have repeated levels (balance tables are only useful then)
      has_repeats <- any(sapply(facs, function(f) {
        if (!f %in% names(rv$data)) return(FALSE)
        length(unique(rv$data[[f]])) < n
      }))
      if (!has_repeats) {
        return(div(class = "alert alert-info p-2",
          icon("info-circle"),
          " All factors have unique values (e.g. space-filling design). ",
          "Use the Scatterplot Matrix in the Explore tab to review design balance and coverage."))
      }

      mode <- analysis_mode()

      # Build per-factor tables
      make_factor_table <- function(col_name, label) {
        if (!col_name %in% names(rv$data)) return(NULL)
        tab <- as.data.frame(table(Level = rv$data[[col_name]]), stringsAsFactors = FALSE)
        names(tab) <- c(label, "Count")
        tab
      }

      # Treatment combination table
      trt <- balance_treatment()
      trt_lab <- treatment_label()
      trt_tab <- as.data.frame(table(Treatment = trt), stringsAsFactors = FALSE)
      names(trt_tab) <- c(trt_lab, "Count")

      # Build ordered list: comparative = treatment first, regression = factors first
      tables <- list()
      if (mode == "comparative") {
        tables[[trt_lab]] <- trt_tab
        for (f in facs) {
          tab <- make_factor_table(f, f)
          if (!is.null(tab)) tables[[f]] <- tab
        }
      } else {
        for (f in facs) {
          tab <- make_factor_table(f, f)
          if (!is.null(tab)) tables[[f]] <- tab
        }
        tables[[trt_lab]] <- trt_tab
      }
      # Add blocks
      for (b in blks) {
        tab <- make_factor_table(b, b)
        if (!is.null(tab)) tables[[b]] <- tab
      }

      tagList(lapply(names(tables), function(nm) {
        tab <- tables[[nm]]
        balanced <- length(unique(tab[[2]])) == 1
        tick_icon <- if (balanced) {
          tags$span(
            title = paste0("All levels have equal counts (", tab[[2]][1], " each)"),
            style = "cursor: help;",
            icon("check-circle", style = "color: green;")
          )
        }
        tagList(
          tags$h6(nm, tick_icon),
          tags$table(class = "table table-sm table-bordered",
            style = "font-size: 11px; margin-bottom: 8px;",
            tags$thead(tags$tr(lapply(names(tab), tags$th))),
            tags$tbody(
              lapply(seq_len(nrow(tab)), function(i) {
                tags$tr(lapply(tab[i, ], function(v) tags$td(as.character(v))))
              })
            )
          )
        )
      }))
    })

    output$balance_replication_flag <- renderUI({
      trt <- balance_treatment()
      counts <- table(trt)
      if (length(unique(counts)) == 1) {
        div(class = "alert alert-success p-2",
          icon("check-circle"),
          paste0("Balanced: each treatment appears ", counts[1], " time(s)."))
      } else {
        unreplicated <- sum(counts == 1)
        div(class = "alert alert-warning p-2",
          icon("exclamation-triangle"),
          paste0("Unequal counts (range: ", min(counts), "\u2013", max(counts), "). "),
          if (unreplicated > 0) paste0(unreplicated, " treatment(s) appear only once."))
      }
    })

    # Run-order balance — per-factor + treatment
    output$balance_runorder_tables <- renderUI({
      req(rv$data)
      facs <- factors_()
      ro <- run_orders()
      req(length(facs) > 0)
      if (length(ro) == 0 || !(ro[1] %in% names(rv$data))) {
        return(div(class = "alert alert-info p-2",
          icon("info-circle"),
          "No Run Order column assigned. Set a column's role to 'Run Order' to check."))
      }
      ro_col <- rv$data[[ro[1]]]
      n_bins <- min(4, length(unique(ro_col)))
      ro_bin <- cut(as.numeric(ro_col), breaks = n_bins, labels = paste0("Q", seq_len(n_bins)))

      mode <- analysis_mode()
      trt <- balance_treatment()

      # Build tables for each factor + treatment
      make_ro_table <- function(grouping, label) {
        tab <- as.data.frame.matrix(table(grouping, ro_bin))
        tab <- cbind(Level = rownames(tab), tab)
        names(tab)[1] <- label
        rownames(tab) <- NULL
        # Chi-squared test
        chi <- tryCatch(chisq.test(table(grouping, ro_bin), simulate.p.value = TRUE),
                        error = function(e) NULL)
        balanced <- is.null(chi) || chi$p.value >= 0.05
        list(tab = tab, balanced = balanced,
             p = if (!is.null(chi)) signif(chi$p.value, 3) else NA)
      }

      trt_lab <- treatment_label()
      tables <- list()
      if (mode == "comparative") {
        tables[[trt_lab]] <- make_ro_table(trt, trt_lab)
        for (f in facs) {
          if (f %in% names(rv$data)) tables[[f]] <- make_ro_table(rv$data[[f]], f)
        }
      } else {
        for (f in facs) {
          if (f %in% names(rv$data)) tables[[f]] <- make_ro_table(rv$data[[f]], f)
        }
        tables[[trt_lab]] <- make_ro_table(trt, trt_lab)
      }

      tagList(lapply(names(tables), function(nm) {
        info <- tables[[nm]]
        tick_icon <- if (info$balanced) {
          tags$span(
            title = paste0("Chi-squared test (p = ", info$p,
                           "): levels are evenly distributed across run-order quartiles"),
            style = "cursor: help;",
            icon("check-circle", style = "color: green;")
          )
        }
        tagList(
          tags$h6(nm, tick_icon),
          tags$table(class = "table table-sm table-bordered",
            style = "font-size: 11px; margin-bottom: 8px;",
            tags$thead(tags$tr(lapply(names(info$tab), tags$th))),
            tags$tbody(
              lapply(seq_len(nrow(info$tab)), function(i) {
                tags$tr(lapply(info$tab[i, ], function(v) tags$td(as.character(v))))
              })
            )
          )
        )
      }))
    })

    # Carryover balance — per-factor + treatment
    output$balance_carryover_tables <- renderUI({
      req(rv$data)
      facs <- factors_()
      ro <- run_orders()
      req(length(facs) > 0)
      if (length(ro) == 0 || !(ro[1] %in% names(rv$data))) {
        return(div(class = "alert alert-info p-2",
          icon("info-circle"),
          "No Run Order column assigned. Carryover check requires run order."))
      }
      ord <- order(as.numeric(rv$data[[ro[1]]]))
      mode <- analysis_mode()
      trt <- balance_treatment()

      rotated_header_js <- DT::JS(
        "function(settings, json) {",
        "  $(this.api().table().header()).find('th').css({",
        "    'writing-mode': 'vertical-rl',",
        "    'text-orientation': 'mixed',",
        "    'max-width': '30px',",
        "    'height': '100px',",
        "    'vertical-align': 'bottom',",
        "    'font-size': '11px'",
        "  });",
        "}"
      )

      make_carryover <- function(vals, label) {
        sorted <- vals[ord]
        prev <- c(NA, sorted[-length(sorted)])
        valid <- !is.na(prev)
        tab_raw <- table(sorted[valid], prev[valid])
        chi <- tryCatch(chisq.test(tab_raw, simulate.p.value = TRUE),
                        error = function(e) NULL)
        balanced <- is.null(chi) || chi$p.value >= 0.05
        p_val <- if (!is.null(chi)) signif(chi$p.value, 3) else NA

        tab_df <- as.data.frame.matrix(tab_raw)
        tab_df <- cbind(Current = rownames(tab_df), tab_df)
        names(tab_df)[1] <- paste0("Current ", label)
        rownames(tab_df) <- NULL
        list(tab = tab_df, balanced = balanced, p = p_val)
      }

      trt_lab <- treatment_label()
      tables <- list()
      if (mode == "comparative") {
        tables[[trt_lab]] <- make_carryover(trt, trt_lab)
        for (f in facs) {
          if (f %in% names(rv$data)) tables[[f]] <- make_carryover(rv$data[[f]], f)
        }
      } else {
        for (f in facs) {
          if (f %in% names(rv$data)) tables[[f]] <- make_carryover(rv$data[[f]], f)
        }
      }

      tagList(lapply(names(tables), function(nm) {
        info <- tables[[nm]]
        # Green tick with tooltip when chi-squared test shows no significant imbalance;
        # amber warning when imbalanced (carryover can bias factor effects)
        tick_icon <- if (info$balanced) {
          tags$span(
            title = paste0("Chi-squared test of independence (p = ",
                           if (!is.na(info$p)) info$p else "N/A",
                           "): no significant carryover imbalance detected"),
            style = "cursor: help;",
            icon("check-circle", style = "color: green;")
          )
        } else {
          tags$span(
            title = paste0("Chi-squared test of independence (p = ",
                           if (!is.na(info$p)) info$p else "N/A",
                           "): imbalanced carryover can bias factor effects"),
            style = "cursor: help;",
            icon("exclamation-triangle", style = "color: #e67e22;")
          )
        }
        tagList(
          tags$h6(nm, tick_icon),
          tags$div(style = "max-height: 200px; overflow-y: auto; margin-bottom: 8px;",
            tags$table(class = "table table-sm table-bordered",
              style = "font-size: 11px;",
              tags$thead(tags$tr(lapply(names(info$tab), function(h) {
                if (h == names(info$tab)[1]) {
                  tags$th(h)
                } else {
                  tags$th(style = "writing-mode: vertical-rl; text-orientation: mixed; max-width: 30px; height: 80px; vertical-align: bottom;", h)
                }
              }))),
              tags$tbody(
                lapply(seq_len(nrow(info$tab)), function(i) {
                  tags$tr(lapply(info$tab[i, ], function(v) tags$td(as.character(v))))
                })
              )
            )
          ),
          tags$br()
        )
      }))
    })

    # Dynamic covariate choices: treatment + each factor + response
    output$balance_cov_panel_ui <- renderUI({
      ro <- run_orders()
      if (length(ro) == 0 || !(ro[1] %in% names(rv$data))) return(NULL)

      facs <- factors_()
      cov_choices <- c("Previous treatment" = "prev_treatment",
                       "Previous response"  = "prev_response")
      for (f in facs) {
        cov_choices <- c(cov_choices, setNames(paste0("prev_factor_", f), paste0("Previous ", f)))
      }
      fluidRow(
        column(12,
          wellPanel(
            h5("Add Covariate Columns"),
            p(class = "text-muted small",
              "Add lagged columns to the dataset for carryover adjustment in the model."),
            fluidRow(
              column(8,
                checkboxGroupInput(ns("balance_covariates"), "Columns to add",
                  choices = cov_choices, selected = NULL)
              ),
              column(4,
                actionButton(ns("add_balance_covariates"), "Add to Dataset",
                             class = "btn-outline-primary mt-3",
                             icon = icon("plus"))
              )
            )
          )
        )
      )
    })

    # Add covariate columns for carryover adjustment
    observeEvent(input$add_balance_covariates, {
      if (is_locked(rv, "Add covariates")) return()
      req(rv$data, length(input$balance_covariates) > 0)
      ro <- run_orders()
      req(length(ro) > 0, ro[1] %in% names(rv$data))
      ord <- order(as.numeric(rv$data[[ro[1]]]))
      added <- character()

      if ("prev_treatment" %in% input$balance_covariates) {
        trt <- balance_treatment()
        trt_sorted <- trt[ord]
        prev <- rep(NA_character_, nrow(rv$data))
        prev[ord] <- c(NA, trt_sorted[-length(trt_sorted)])
        rv$data$PrevTreatment <- factor(prev)
        rv$roles[["PrevTreatment"]] <- "Covariate"
        rv$col_types[["PrevTreatment"]] <- "Factor"
        added <- c(added, "PrevTreatment")
      }

      if ("prev_response" %in% input$balance_covariates) {
        resps <- responses()
        if (length(resps) > 0) {
          resp_col <- resps[1]
          resp_sorted <- rv$data[[resp_col]][ord]
          prev_resp <- rep(NA_real_, nrow(rv$data))
          prev_resp[ord] <- c(NA, resp_sorted[-length(resp_sorted)])
          rv$data$PrevResponse <- prev_resp
          rv$roles[["PrevResponse"]] <- "Covariate"
          rv$col_types[["PrevResponse"]] <- "Numeric"
          added <- c(added, "PrevResponse")
        }
      }

      # Per-factor previous columns
      facs <- factors_()
      for (f in facs) {
        key <- paste0("prev_factor_", f)
        if (key %in% input$balance_covariates && f %in% names(rv$data)) {
          col_sorted <- rv$data[[f]][ord]
          prev_col <- rep(NA, nrow(rv$data))
          prev_col[ord] <- c(NA, col_sorted[-length(col_sorted)])
          new_name <- paste0("Prev", f)
          rv$data[[new_name]] <- if (is.numeric(rv$data[[f]])) {
            as.numeric(prev_col)
          } else {
            factor(prev_col)
          }
          rv$roles[[new_name]] <- "Covariate"
          rv$col_types[[new_name]] <- if (is.numeric(rv$data[[f]])) "Numeric" else "Factor"
          added <- c(added, new_name)
        }
      }

      if (length(added) > 0) {
        showNotification(paste0("Added: ", paste(added, collapse = ", ")),
                         type = "message", duration = 5)
      }
    })

    # ── Return exports for use by other modules ──────────────────────────
    list(
      set_alias_full_formula = function(text) {
        updateTextInput(session, "alias_full_formula", value = text)
      },
      set_alias_check_formula = function(text) {
        updateTextInput(session, "alias_check_formula", value = text)
      }
    )
  })
}
