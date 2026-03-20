# R/mod_data_generate.R — Data > Generate Design sub-tab module (UI + server)

# ── UI ───────────────────────────────────────────────────────────────────────
mod_data_generate_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Generate Design",
    br(),
    fluidRow(
      column(4,
        wellPanel(
          h4("Design Generator"),
          p(class = "text-muted small",
            "Generate experimental designs using R packages. ",
            "Select a design category, type, and function, then choose an example ",
            "or specify custom arguments."),
          hr(),
          selectInput(ns("doe_category"), "Design category",
                      choices = c("Select..." = "", doe_category_choices())),
          uiOutput(ns("doe_design_ui")),
          uiOutput(ns("doe_entry_ui")),
          hr(),
          uiOutput(ns("doe_example_ui")),
          hr(),
          uiOutput(ns("doe_args_ui")),
          tags$details(
            style = "border: 1px solid #dee2e6; border-radius: 4px; padding: 8px; margin-bottom: 12px;",
            tags$summary(style = "cursor: pointer; font-weight: 600; font-size: 14px;",
              "Additional arguments"
            ),
            tags$div(style = "margin-top: 8px;",
              uiOutput(ns("doe_advanced_args_ui"))
            )
          ),
          actionButton(ns("doe_generate"), "Generate Design",
                       class = "btn-primary w-100 mb-2",
                       icon = icon("cogs")),
          hr(),
          checkboxInput(ns("doe_add_response"),
                        "Add simulated null response (N(0,1))",
                        value = TRUE),
          actionButton(ns("doe_use_design"), "Use This Design",
                       class = "btn-success w-100",
                       icon = icon("arrow-right"))
        )
      ),
      column(8,
        uiOutput(ns("doe_design_description")),
        wellPanel(
          h5("Function Reference"),
          uiOutput(ns("doe_func_info")),
          hr(),
          h5("R Function Call"),
          verbatimTextOutput(ns("doe_call_preview"))
        ),
        br(),
        h5("Generated Design Preview"),
        p(class = "text-muted small",
          "Click 'Generate Design' to create the design. ",
          "Rename columns below, then click 'Use This Design' to load it into the app."),
        plotly::plotlyOutput(ns("doe_gen_plot"), height = "400px"),
        uiOutput(ns("doe_gen_plot_note")),
        hr(),
        uiOutput(ns("doe_rename_ui")),
        DT::dataTableOutput(ns("doe_preview_table"))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
mod_data_generate_server <- function(id, rv, analysis_mode, navigate_to,
                                      set_analysis_mode, reset_downstream,
                                      colour_theme, design_exports) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Unpack colour theme reactives used by the design plot
    cat_palette    <- colour_theme$cat_palette
    cont_plotly_cs <- colour_theme$cont_plotly_cs

    # ── Read-only guard ─────────────────────────────────────────────────
    observe({
      locked <- isTRUE(rv$read_only)
      toggle <- if (locked) shinyjs::disable else shinyjs::enable
      toggle(ns("doe_generate"))
      toggle(ns("doe_use_design"))
    })

    # Local reactiveValues for generated design state
    rv_doe <- reactiveValues(
      generated = NULL,   # data.frame of generated design
      current_entry_id = NULL
    )

    # Cascading UI: Design type selector (depends on category)
    output$doe_design_ui <- renderUI({
      req(input$doe_category, input$doe_category != "")
      choices <- doe_design_choices(input$doe_category)
      if (is.null(choices)) return(NULL)
      selectInput(ns("doe_design"), "Design type", choices = choices)
    })

    # Cascading UI: Entry (function) selector (depends on design)
    output$doe_entry_ui <- renderUI({
      req(input$doe_category, input$doe_design)
      choices <- doe_entry_choices(input$doe_category, input$doe_design)
      if (is.null(choices)) return(NULL)
      if (length(choices) == 1) {
        # Single function -- auto-select but still display
        tagList(
          selectInput(ns("doe_entry"), "R function", choices = choices, selected = choices[1])
        )
      } else {
        selectInput(ns("doe_entry"), "R function", choices = choices)
      }
    })

    # Example selector (depends on entry)
    output$doe_example_ui <- renderUI({
      req(input$doe_entry)
      choices <- doe_example_choices(input$doe_entry)
      if (is.null(choices) || length(choices) == 0) return(NULL)
      # Default to first example so arguments are pre-filled
      selectInput(ns("doe_example"), "Start from example",
                  choices = c(choices, "Custom" = ""),
                  selected = choices[1])
    })

    # Function info display
    # Design type descriptions -- structured guidance for each design family
    doe_design_descriptions <- list(
      "crd" = list(
        title = "Completely Randomised Design (CRD)",
        icon  = "random",
        desc  = "Simplest experimental design: treatments are allocated to experimental units purely at random with specified replications.",
        when  = "Homogeneous experimental units (no known nuisance factors). Few factors, typically 1 treatment factor with multiple levels.",
        watch = "No blocking \u2014 all variation between units inflates the error. If units are not homogeneous, RCBD is better.",
        vs    = "Simplest to analyse but least efficient when nuisance factors are present. Use RCBD/Latin Square to control variability."
      ),
      "fractional_factorial" = list(
        title = "Fractional Factorial Design",
        icon  = "th-large",
        desc  = "Uses a fraction (1/2, 1/4, ...) of a full factorial. Aliases higher-order interactions with lower-order effects.",
        when  = "Many factors (4\u201315), limited budget. Screening or Resolution IV+ factor studies.",
        watch = "Design resolution determines what is confounded. Res III: main effects aliased with 2FI. Res IV: 2FI aliased with 2FI. Res V+: main effects and 2FI are clear.",
        vs    = "Fewer runs than full factorial; check the alias structure to understand confounding patterns."
      ),
      "plackett_burman" = list(
        title = "Plackett-Burman Design",
        icon  = "filter",
        desc  = "Resolution III screening design. Efficient for identifying the vital few factors from many candidates.",
        when  = "Screening 6\u201347 factors to find which ones matter, before a follow-up optimisation experiment.",
        watch = "All 2-factor interactions are confounded with main effects. Cannot estimate interactions \u2014 use only for factor screening.",
        vs    = "Fewer runs than Res III fractional factorials for some n. Supplement with fold-over or augmentation for interaction estimation."
      ),
      "rcbd" = list(
        title = "Randomised Complete Block Design (RCBD)",
        icon  = "grip-horizontal",
        desc  = "Each block contains all treatments exactly once. Removes block-to-block variability from the treatment comparison.",
        when  = "Known nuisance variable (day, batch, operator) that can be blocked. Each block can accommodate all treatments.",
        watch = "Assumes no block \u00d7 treatment interaction. If blocks are too large, consider BIB or incomplete block designs.",
        vs    = "Simpler than Latin Square (one blocking factor). For two blocking factors use a Latin Square."
      ),
      "latin" = list(
        title = "Latin Square Design",
        icon  = "border-all",
        desc  = "Simultaneously controls two blocking factors (rows and columns). Each treatment appears exactly once in each row and each column.",
        when  = "Two nuisance variables, each with the same number of levels as treatments (e.g., 4 treatments \u00d7 4 rows \u00d7 4 columns).",
        watch = "Assumes no interaction between rows, columns, and treatments. Number of treatments must equal number of row and column levels.",
        vs    = "More efficient than RCBD when two sources of variation exist. Cannot fit interactions between blocking factors and treatment."
      ),
      "bib" = list(
        title = "Balanced Incomplete Block Design (BIB)",
        icon  = "grip-vertical",
        desc  = "Each block contains a subset of treatments; every pair of treatments appears together equally often across blocks.",
        when  = "Block size is too small for all treatments. Pairwise balance is important.",
        watch = "Not every (t, k) combination yields a valid BIB. Requires \u03BB = r(k\u22121)/(t\u22121) to be an integer.",
        vs    = "More complex analysis than RCBD but handles smaller blocks. All pairwise comparisons have equal precision."
      ),
      "ccd" = list(
        title = "Central Composite Design (CCD)",
        icon  = "bullseye",
        desc  = "Augments a factorial with centre and axial (star) points. Fits full second-order (quadratic) response surface models.",
        when  = "Optimisation of 2\u20136 continuous factors. Need to estimate curvature and locate optima.",
        watch = "Axial point distance (\u03B1) determines design properties: face-centred (\u03B1=1), rotatable (\u03B1=k^{1/4}), or spherical. Centre replicates estimate pure error.",
        vs    = "More efficient than a 3-level full factorial. Preferred over Box-Behnken when extreme factor combinations are feasible."
      ),
      "bbd" = list(
        title = "Box-Behnken Design (BBD)",
        icon  = "circle",
        desc  = "Three-level response surface design. Points lie on the midpoints of edges of a cube \u2014 avoids corner (extreme) combinations.",
        when  = "Optimisation of 3\u20137 factors where extreme corners are unsafe, costly, or physically impossible.",
        watch = "Cannot estimate effects at factorial vertices. Fewer runs than CCD for 3\u20135 factors but not rotatable.",
        vs    = "Choose over CCD when you must avoid extreme factor combinations. Slightly fewer runs for 3\u20134 factors."
      ),
      "lhs" = list(
        title = "Latin Hypercube Sampling (LHS)",
        icon  = "braille",
        desc  = "Space-filling design. Each factor's range is divided into n equal intervals; each interval is sampled exactly once.",
        when  = "Computer experiments, emulators, or exploratory studies over continuous factors where you want uniform coverage.",
        watch = "Does not support factorial structure or interaction estimation directly. Typically analysed with Gaussian Process or regression models.",
        vs    = "Better space-filling than random sampling. Use Maximin LHS for designs that maximise minimum inter-point distance."
      ),
      "d_optimal" = list(
        title = "Optimal Design (D/I/A-optimal)",
        icon  = "crosshairs",
        desc  = "Algorithmic designs that optimise a statistical criterion (D-optimal: minimise variance of coefficients; I-optimal: minimise prediction variance).",
        when  = "Irregular design spaces, mixture constraints, non-standard factor levels, or augmenting an existing experiment.",
        watch = "Results depend on the assumed model. A misspecified model can produce a poor design. Check alias structure.",
        vs    = "Most flexible \u2014 handles constraints that classical designs cannot. But less intuitive and not always orthogonal."
      )
    )

    output$doe_design_description <- renderUI({
      req(input$doe_design)
      # Map design selection to description key
      design_key <- input$doe_design
      info <- doe_design_descriptions[[design_key]]
      if (is.null(info)) return(NULL)

      wellPanel(
        style = "background-color: #f8f9fa; border-left: 4px solid #003B73; margin-bottom: 12px;",
        fluidRow(
          column(12,
            h5(icon(info$icon), " ", info$title),
            tags$p(info$desc),
            fluidRow(
              column(4,
                tags$p(class = "small",
                  tags$strong(icon("calendar-check"), " When to use:"),
                  tags$br(), info$when)
              ),
              column(4,
                tags$p(class = "small",
                  tags$strong(icon("exclamation-circle"), " Watch out:"),
                  tags$br(), info$watch)
              ),
              column(4,
                tags$p(class = "small",
                  tags$strong(icon("balance-scale"), " Compared to alternatives:"),
                  tags$br(), info$vs)
              )
            )
          )
        )
      )
    })

    output$doe_func_info <- renderUI({
      req(input$doe_entry)
      lookup <- doe_entry_lookup()
      if (!input$doe_entry %in% names(lookup)) return(p("Select a function."))
      info <- lookup[[input$doe_entry]]
      entry <- info$entry
      # Build function-specific rdocumentation URL automatically
      func_doc_url <- paste0("https://www.rdocumentation.org/packages/",
                              entry$package, "/topics/", entry$func)
      pkg_url <- entry$pkg_url %||%
        paste0("https://cran.r-project.org/web/packages/", entry$package, "/index.html")
      tagList(
        tags$p(
          tags$strong(paste0(entry$package, "::", entry$func)),
          " \u2014 ", entry$description
        ),
        tags$p(
          tags$a(href = func_doc_url, target = "_blank",
                 class = "btn btn-sm btn-outline-info me-2",
                 icon("book"),
                 paste0("  ", entry$func, "() documentation")),
          tags$a(href = pkg_url, target = "_blank",
                 class = "btn btn-sm btn-outline-secondary",
                 icon("external-link-alt"),
                 paste0("  ", entry$package, " on CRAN"))
        )
      )
    })

    # Dynamic argument inputs
    output$doe_args_ui <- renderUI({
      req(input$doe_entry)
      lookup <- doe_entry_lookup()
      if (!input$doe_entry %in% names(lookup)) return(NULL)
      entry <- lookup[[input$doe_entry]]$entry

      # If an example is selected, use its values as defaults
      example_vals <- NULL
      if (!is.null(input$doe_example) && input$doe_example != "") {
        if (input$doe_example %in% names(entry$examples)) {
          example_vals <- entry$examples[[input$doe_example]]
        }
      }

      arg_inputs <- lapply(names(entry$args), function(arg_name) {
        arg_def <- entry$args[[arg_name]]
        # Use example value if available, otherwise default
        val <- if (!is.null(example_vals) && arg_name %in% names(example_vals)) {
          example_vals[[arg_name]]
        } else {
          arg_def$default
        }
        input_id <- ns(paste0("doe_arg_", arg_name))

        if (arg_def$type == "numeric") {
          numericInput(input_id, arg_def$label, value = val)
        } else if (arg_def$type == "logical") {
          checkboxInput(input_id, arg_def$label, value = isTRUE(val))
        } else {
          textInput(input_id, arg_def$label, value = as.character(val))
        }
      })
      do.call(tagList, arg_inputs)
    })

    # Advanced arguments UI (collapsible section)
    output$doe_advanced_args_ui <- renderUI({
      req(input$doe_entry)
      lookup <- doe_entry_lookup()
      if (!input$doe_entry %in% names(lookup)) return(NULL)
      entry <- lookup[[input$doe_entry]]$entry
      adv <- entry$advanced_args
      if (is.null(adv) || length(adv) == 0)
        return(tags$p(class = "text-muted small", "No additional arguments for this function."))

      adv_inputs <- lapply(names(adv), function(arg_name) {
        arg_def <- adv[[arg_name]]
        val <- arg_def$default
        input_id <- ns(paste0("doe_adv_", arg_name))

        # Label: R arg name in monospace + friendly description
        lbl <- tags$span(
          tags$code(arg_name, style = "font-size: 12px; background: #f0f0f0; padding: 1px 4px; border-radius: 3px;"),
          tags$span(paste0(" \u2014 ", arg_def$label), style = "font-weight: normal; font-size: 13px;")
        )

        if (!is.null(arg_def$choices)) {
          selectInput(input_id, lbl, choices = arg_def$choices, selected = val)
        } else if (arg_def$type == "numeric") {
          if (is.na(val)) val <- NULL
          numericInput(input_id, lbl, value = val)
        } else if (arg_def$type == "logical") {
          checkboxInput(input_id, lbl, value = isTRUE(val))
        } else {
          textInput(input_id, lbl, value = as.character(val %||% ""))
        }
      })
      do.call(tagList, adv_inputs)
    })

    # Helper: collect all args (primary + advanced) from UI inputs
    # For advanced args, NULL/NA/empty values fall back to the registered default
    collect_doe_args <- function(entry, notify = FALSE) {
      args <- list()
      # Primary args
      for (arg_name in names(entry$args)) {
        input_id <- paste0("doe_arg_", arg_name)
        val <- input[[input_id]]
        if (!is.null(val)) {
          arg_def <- entry$args[[arg_name]]
          if (arg_def$type == "numeric") {
            args[[arg_name]] <- as.numeric(val)
          } else if (arg_def$type == "logical") {
            args[[arg_name]] <- isTRUE(val)
          } else {
            args[[arg_name]] <- as.character(val)
          }
        } else {
          args[[arg_name]] <- entry$args[[arg_name]]$default
        }
      }
      # Advanced args -- fall back to default if blank/NA
      reset_names <- character()
      if (!is.null(entry$advanced_args)) {
        for (arg_name in names(entry$advanced_args)) {
          input_id <- paste0("doe_adv_", arg_name)
          val <- input[[input_id]]
          arg_def <- entry$advanced_args[[arg_name]]
          used_default <- FALSE

          if (is.null(val)) {
            # Input not yet rendered -- use default
            args[[arg_name]] <- arg_def$default
            next
          }

          if (arg_def$type == "numeric") {
            num_val <- suppressWarnings(as.numeric(val))
            if (is.na(num_val) && !identical(arg_def$default, NA)) {
              # User cleared the field -- reset to default
              args[[arg_name]] <- arg_def$default
              used_default <- TRUE
            } else {
              args[[arg_name]] <- num_val
            }
          } else if (arg_def$type == "logical") {
            args[[arg_name]] <- isTRUE(val)
          } else {
            # text / select -- empty string falls back to default
            str_val <- as.character(val)
            if (nzchar(str_val)) {
              args[[arg_name]] <- str_val
            } else {
              args[[arg_name]] <- arg_def$default
              if (!identical(arg_def$default, "") && !identical(arg_def$default, NA))
                used_default <- TRUE
            }
          }

          if (used_default) reset_names <- c(reset_names, arg_name)
        }
      }
      if (notify && length(reset_names) > 0) {
        showNotification(
          paste0("Reset to default: ", paste(reset_names, collapse = ", ")),
          type = "message", duration = 4)
      }
      args
    }

    # R call preview
    output$doe_call_preview <- renderText({
      req(input$doe_entry)
      lookup <- doe_entry_lookup()
      if (!input$doe_entry %in% names(lookup)) return("")
      entry <- lookup[[input$doe_entry]]$entry
      args <- collect_doe_args(entry)
      tryCatch(entry$build_call(args), error = function(e) paste("Error:", e$message))
    })

    # Generate design
    observeEvent(input$doe_generate, {
      if (is_locked(rv, "Design generation")) return()
      req(input$doe_entry)
      lookup <- doe_entry_lookup()
      if (!input$doe_entry %in% names(lookup)) {
        showNotification("No design function selected.", type = "error")
        return()
      }
      entry <- lookup[[input$doe_entry]]$entry

      # Collect all argument values (primary + advanced); notify on defaults reset
      args <- collect_doe_args(entry, notify = TRUE)

      # Check if the required package is installed
      pkg <- entry$package
      if (!requireNamespace(pkg, quietly = TRUE)) {
        showNotification(
          paste0("Package '", pkg, "' is not installed. ",
                 "Install it with: install.packages('", pkg, "')"),
          type = "error", duration = 10)
        return()
      }

      # Run the design generation
      result <- tryCatch({
        entry$run(args)
      }, error = function(e) {
        showNotification(paste("Error generating design:", e$message),
                         type = "error", duration = 10)
        NULL
      })

      if (!is.null(result)) {
        rv_doe$generated <- result
        rv_doe$current_entry_id <- input$doe_entry
        showNotification(
          paste0("Design generated: ", nrow(result), " runs \u00d7 ",
                 ncol(result), " columns."),
          type = "message")
      }
    })

    # Column rename inputs above the preview table
    output$doe_rename_ui <- renderUI({
      req(rv_doe$generated)
      cols <- names(rv_doe$generated)
      inputs <- lapply(seq_along(cols), function(i) {
        tags$div(
          style = "flex: 1 1 auto; min-width: 90px; max-width: 180px;",
          textInput(
            inputId = ns(paste0("doe_colname_", i)),
            label = tags$small(class = "text-muted", cols[i]),
            value = cols[i],
            width = "100%"
          )
        )
      })
      tagList(
        tags$label("Rename columns", class = "fw-bold"),
        tags$div(
          style = "display: flex; gap: 6px; flex-wrap: wrap; align-items: flex-end; margin-bottom: 8px;",
          inputs
        )
      )
    })

    # Reactive: current column names (original + any renames)
    doe_col_names <- reactive({
      req(rv_doe$generated)
      orig <- names(rv_doe$generated)
      new_names <- sapply(seq_along(orig), function(i) {
        val <- input[[paste0("doe_colname_", i)]]
        if (is.null(val) || trimws(val) == "") orig[i] else trimws(val)
      })
      if (anyDuplicated(new_names)) new_names <- make.unique(new_names)
      new_names
    })

    # Preview table -- shows renamed columns
    output$doe_preview_table <- DT::renderDataTable({
      req(rv_doe$generated)
      df <- rv_doe$generated
      cn <- doe_col_names()
      if (length(cn) == ncol(df)) names(df) <- cn
      dt_table(df, options = list(pageLength = 20), rownames = FALSE)
    })

    # ── Design Generation Visualisation ────────────────────────────────────
    output$doe_gen_plot <- renderPlotly({
      req(rv_doe$generated)
      df <- rv_doe$generated
      cn <- doe_col_names()
      if (length(cn) == ncol(df)) names(df) <- cn

      # Identify plottable columns (exclude run order, block-like if many factors)
      all_cols <- names(df)
      plot_cols <- all_cols[!grepl("^runorder$|^run.?order$|^rep$|^replicate$|^plot$|^plots$",
                                   tolower(all_cols))]
      if (length(plot_cols) == 0) plot_cols <- all_cols
      n <- length(plot_cols)

      pal <- tryCatch(cat_palette()(max(n, 8)), error = function(e) RColorBrewer::brewer.pal(min(n, 8), "Set2"))

      # Latin Square detection: has Row, Col, and Treatment columns
      lc_names <- tolower(names(df))
      is_latin <- all(c("row", "col", "treatment") %in% lc_names)

      if (is_latin) {
        # Latin Square: 2D heatmap — Col vs Row, coloured by Treatment
        row_col <- names(df)[lc_names == "row"]
        col_col <- names(df)[lc_names == "col"]
        trt_col <- names(df)[lc_names == "treatment"]
        df[[row_col]] <- as.factor(df[[row_col]])
        df[[col_col]] <- as.factor(df[[col_col]])
        df[[trt_col]] <- as.factor(df[[trt_col]])
        n_trt <- nlevels(df[[trt_col]])
        trt_pal <- tryCatch(cat_palette()(n_trt), error = function(e) RColorBrewer::brewer.pal(min(n_trt, 8), "Set2"))
        trt_map <- setNames(trt_pal[seq_len(n_trt)], levels(df[[trt_col]]))
        p <- plot_ly(df, x = ~df[[col_col]], y = ~df[[row_col]],
                     type = "scatter", mode = "markers+text",
                     color = ~df[[trt_col]], colors = trt_map,
                     text = ~df[[trt_col]], textposition = "middle right",
                     marker = list(size = 16)) %>%
          layout(xaxis = list(title = col_col, type = "category"),
                 yaxis = list(title = row_col, type = "category",
                              autorange = "reversed"),
                 title = "Latin Square Design")
      } else if (n == 1) {
        # 1 factor: plot vs run order
        p <- plot_ly(df, x = seq_len(nrow(df)), y = ~df[[plot_cols[1]]],
                     type = "scatter", mode = "markers",
                     marker = list(color = pal[1], size = 8)) %>%
          layout(xaxis = list(title = "Run"), yaxis = list(title = plot_cols[1]))
      } else if (n == 2) {
        # 2 factors: scatter
        p <- plot_ly(df, x = ~df[[plot_cols[1]]], y = ~df[[plot_cols[2]]],
                     type = "scatter", mode = "markers",
                     marker = list(color = pal[1], size = 8)) %>%
          layout(xaxis = list(title = plot_cols[1]), yaxis = list(title = plot_cols[2]))
      } else {
        # 3+ factors: use shared 3D scatter with jitter + categorical tick labels
        colour_col <- if (n >= 4) plot_cols[4] else NULL
        p <- plot_3d_scatter(df, plot_cols[1], plot_cols[2], plot_cols[3],
                             colour_col = colour_col,
                             cat_pal = pal,
                             dcol = pal[1],
                             cont_cs = tryCatch(cont_plotly_cs(), error = function(e) NULL),
                             title = "Design Preview")
      }
      p %>% layout(margin = list(t = 30))
    })

    output$doe_gen_plot_note <- renderUI({
      req(rv_doe$generated)
      df <- rv_doe$generated
      cn <- doe_col_names()
      if (length(cn) == ncol(df)) names(df) <- cn

      all_cols <- names(df)
      plot_cols <- all_cols[!grepl("^runorder$|^run.?order$|^rep$|^replicate$|^plot$|^plots$",
                                   tolower(all_cols))]
      if (length(plot_cols) == 0) plot_cols <- all_cols
      n <- length(plot_cols)

      if (n >= 5) {
        hidden <- plot_cols[5:n]
        p(class = "text-muted small", icon("info-circle"),
          paste0(" Plot shows first 3 factors as axes + ", plot_cols[4],
                 " as colour. Not shown: ", paste(hidden, collapse = ", "), "."))
      } else if (n == 4) {
        p(class = "text-muted small", icon("info-circle"),
          paste0(" Colour: ", plot_cols[4]))
      } else NULL
    })

    # Use This Design -- load into app
    observeEvent(input$doe_use_design, {
      if (is_locked(rv, "Use design")) return()
      req(rv_doe$generated)
      df <- rv_doe$generated

      # Apply column renames from the inline rename inputs
      cn <- doe_col_names()
      if (length(cn) == ncol(df)) names(df) <- cn

      # Add a simulated null response if requested
      if (isTRUE(input$doe_add_response)) {
        set.seed(Sys.time())
        df$Response <- round(rnorm(nrow(df), mean = 0, sd = 1), 4)
      }

      reset_downstream()
      rv$data <- stamp_row_ids(df)
      rv$transforms    <- list()
      rv$coding_values <- list()
      rv$level_labels  <- list()

      # Auto-assign roles based on column names
      lookup <- doe_entry_lookup()
      entry_info <- if (!is.null(rv_doe$current_entry_id) &&
                        rv_doe$current_entry_id %in% names(lookup)) {
        lookup[[rv_doe$current_entry_id]]
      } else NULL

      category <- if (!is.null(entry_info)) entry_info$category else ""

      auto_roles <- sapply(names(df), function(col) {
        cl <- tolower(col)
        if (grepl("^runorder$|^run.?order$", cl, ignore.case = TRUE)) return("Run Order")
        if (grepl("^block$|^blk$", cl, ignore.case = TRUE)) return("Block")
        if (grepl("^row$|^col$", cl, ignore.case = TRUE)) return("Block")
        if (grepl("^rep$|^replicate$", cl, ignore.case = TRUE)) return("Ignore")
        if (grepl("^plot$|^plots$", cl, ignore.case = TRUE)) return("Ignore")
        # For generated designs, non-response numeric columns are factors
        if (is.numeric(df[[col]])) {
          if (grepl("^response$|^yield$|^y$", cl, ignore.case = TRUE)) return("Response")
          return("Factor")
        }
        "Factor"
      }, USE.NAMES = TRUE)
      rv$roles <- as.list(auto_roles)

      # Auto-assign types
      is_regression <- category %in% c("Screening", "Optimisation", "Optimal", "Space Filling")
      auto_types <- sapply(names(df), function(col) {
        if (is.numeric(df[[col]])) "Numeric" else "Factor"
      }, USE.NAMES = TRUE)
      rv$col_types <- as.list(auto_types)

      # Auto-set coding transforms for regression-type designs
      # Skip for RSM designs (CCD/BBD) where output is already in coded form
      design_type_attr <- attr(rv_doe$generated, "design_type")
      already_coded <- identical(design_type_attr, "rsm")
      if (is_regression) {
        set_analysis_mode("regression")
        if (!already_coded) {
          for (col in names(df)) {
            if (is.numeric(df[[col]]) && auto_roles[[col]] == "Factor") {
              rv$transforms[[col]] <- "coding"
              rng <- range(df[[col]], na.rm = TRUE)
              rv$coding_values[[col]] <- list(low = rng[1], high = rng[2])
            }
          }
        }
      } else {
        set_analysis_mode("comparative")
      }

      # Auto-populate design model formulas using design metadata
      # Read attributes from original generated df (rv_doe$generated) because
      # modifying df (e.g., adding Response column) strips custom attributes in R
      orig_df <- rv_doe$generated
      fac_names <- names(auto_roles[auto_roles == "Factor"])
      blk_names <- names(auto_roles[auto_roles == "Block"])
      if (length(fac_names) > 0) {
        design_res  <- attr(orig_df, "design_resolution")
        design_type <- attr(orig_df, "design_type")
        aliased     <- attr(orig_df, "design_aliased")

        if (identical(design_type, "rsm")) {
          # RSM (CCD/BBD): default = full quadratic (main + 2FI + quadratic)
          full_parts <- fac_names
          if (length(fac_names) >= 2) {
            combos2 <- combn(fac_names, 2, simplify = FALSE)
            full_parts <- c(full_parts, sapply(combos2, paste, collapse = ":"))
          }
          full_parts <- c(full_parts, paste0("I(", fac_names, "^2)"))
          if (length(blk_names) > 0) full_parts <- c(full_parts, blk_names)

        } else if (!is.null(design_res) && !is.na(design_res)) {
          # Fractional factorial / PB: use resolution to pick model
          if (design_res <= 3) {
            # Res III: main effects aliased with 2FIs -- model = main effects only
            full_parts <- fac_names
          } else if (design_res == 4) {
            # Res IV: main effects clear, 2FIs aliased with each other -- main only
            full_parts <- fac_names
          } else {
            # Res V+: main + 2FIs are clear
            full_parts <- fac_names
            if (length(fac_names) >= 2) {
              # Include only 2FIs not aliased with other 2FIs
              combos2 <- combn(fac_names, 2, simplify = FALSE)
              fi2_terms <- sapply(combos2, paste, collapse = ":")
              # Check alias structure: if aliased$fi2 exists, filter out aliased pairs
              if (!is.null(aliased) && !is.null(aliased$fi2) && length(aliased$fi2) > 0) {
                # aliased$fi2 entries like "AB=CDE" -- terms before "=" are aliased with what follows
                aliased_2fi <- character(0)
                for (a in aliased$fi2) {
                  parts <- strsplit(a, "=")[[1]]
                  # Only exclude 2FIs aliased with other 2FIs (not higher-order)
                  alias_partners <- parts[-1]
                  has_2fi_alias <- any(nchar(alias_partners) == 2)  # 2-char = 2FI in coded names
                  if (has_2fi_alias) aliased_2fi <- c(aliased_2fi, parts[1])
                }
                # Convert coded aliases (e.g. "AB") to named (e.g. "A:B") for filtering
                # For Res V+ this should be empty, so include all 2FIs
              }
              full_parts <- c(full_parts, fi2_terms)
            }
          }
          if (length(blk_names) > 0) full_parts <- c(full_parts, blk_names)

        } else {
          # Fallback: main effects + 2FI if <=6 factors
          full_parts <- fac_names
          if (length(fac_names) >= 2 && length(fac_names) <= 6) {
            combos2 <- combn(fac_names, 2, simplify = FALSE)
            full_parts <- c(full_parts, sapply(combos2, paste, collapse = ":"))
          }
          if (length(blk_names) > 0) full_parts <- c(full_parts, blk_names)
        }

        design_exports$set_alias_full_formula(
                        paste(full_parts, collapse = " + "))

        # Alias formula: always include all interactions for checking
        check_parts <- fac_names
        max_ord <- min(length(fac_names), if (length(fac_names) <= 5) 4 else 3)
        if (max_ord >= 2) {
          for (ord in 2:max_ord) {
            combos <- combn(fac_names, ord, simplify = FALSE)
            check_parts <- c(check_parts, sapply(combos, paste, collapse = ":"))
          }
        }
        # For RSM: add quadratic terms to alias model too
        if (identical(design_type, "rsm")) {
          check_parts <- c(check_parts, paste0("I(", fac_names, "^2)"))
          if (length(fac_names) >= 2) {
            check_parts <- c(check_parts, paste0("I(", fac_names, "^3)"))
          }
        }
        if (length(blk_names) > 0) {
          check_parts <- c(check_parts, blk_names,
                            as.vector(outer(blk_names, fac_names,
                                            function(b, f) paste0(b, ":", f))))
        }
        design_exports$set_alias_check_formula(
                        paste(check_parts, collapse = " + "))

        # Auto-populate Models tab with default formulas based on design type
        resp_names <- names(auto_roles[auto_roles == "Response"])
        if (length(resp_names) > 0) {
          r <- resp_names[1]
          default_formulas <- character(0)

          if (is_regression) {
            # Regression designs: progressive (Linear -> +2FI -> Full Quadratic for RSM)
            # Main effects
            main_f <- paste0(r, " ~ ", paste(c(fac_names, blk_names), collapse = " + "))
            default_formulas[main_f] <- main_f

            # + 2FI
            if (length(fac_names) >= 2) {
              fi2 <- sapply(combn(fac_names, 2, simplify = FALSE), paste, collapse = ":")
              fi2_f <- paste0(r, " ~ ", paste(c(fac_names, fi2, blk_names), collapse = " + "))
              default_formulas[fi2_f] <- fi2_f
            }

            # + Quadratic (for RSM)
            if (identical(design_type, "rsm")) {
              quad_terms <- paste0("I(", fac_names, "^2)")
              fi2 <- if (length(fac_names) >= 2) {
                sapply(combn(fac_names, 2, simplify = FALSE), paste, collapse = ":")
              } else character(0)
              quad_f <- paste0(r, " ~ ", paste(c(fac_names, fi2, quad_terms, blk_names),
                                               collapse = " + "))
              default_formulas[quad_f] <- quad_f
            }
          } else {
            # Comparative designs: single default formula
            # Use full_parts which already includes the right terms for the design type
            default_f <- paste0(r, " ~ ", paste(full_parts, collapse = " + "))
            default_formulas[default_f] <- default_f
          }

          if (length(default_formulas) > 0) {
            rv$formula_gen <- rv$formula_gen + 1L
            rv$formulas <- default_formulas
            rv$formula_aliases <- list()
            rv$alias_labels <- list()
            rv$inestimable_terms <- character()
          }
        }
      }

      # Store design metadata for download/reimport
      rv$design_metadata <- list(
        design_type   = attr(orig_df, "design_type"),
        resolution    = attr(orig_df, "design_resolution"),
        analysis_mode = if (is_regression) "regression" else "comparative",
        generator     = input$doe_type
      )

      showNotification(
        paste0("Design loaded: ", nrow(df), " runs \u00d7 ", ncol(df),
               " columns. Switch to 'Assign Roles' to review."),
        type = "message")
      navigate_to("Assign Roles")
    })
  })
}
