# server.R — All reactive logic

server <- function(input, output, session) {

  rv <- reactiveValues(
    data         = NULL,
    roles        = list(),
    col_types    = list(),   # colname -> "Factor" | "Numeric"
    transforms    = list(),   # colname -> "none" | "centre" | "coding"
    coding_values = list(),   # colname -> list(low = num, high = num)
    formulas     = character(0),
    models       = list(),
    mc_results   = list(),
    vif_df       = data.frame(),
    selected_ids = NULL
  )

  # ── Tab 1: Upload ──────────────────────────────────────────────────────

  # Helper: clear downstream state when data changes
  reset_downstream <- function() {
    rv$formulas   <- character(0)
    rv$models     <- list()
    rv$mc_results <- list()
    rv$vif_df     <- data.frame()
  }

  observeEvent(input$file, {
    req(input$file)
    path <- input$file$datapath
    ext  <- tools::file_ext(input$file$name)

    df <- tryCatch({
      if (tolower(ext) %in% c("xlsx", "xls")) {
        readxl::read_excel(path)
      } else {
        read.csv(path, sep = input$sep, stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })

    if (is.null(df)) return()
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    rv$data <- df
    rv$transforms    <- list()
    rv$coding_values <- list()
    reset_downstream()

    # Auto-suggest roles (with run-order auto-detection)
    run_order_pattern <- "^run.?order$|^run$|^std.?order$|^exp.?id$|^obs$|^observation$"
    auto_roles <- sapply(names(df), function(col) {
      if (grepl(run_order_pattern, col, ignore.case = TRUE)) return("Run Order")
      if (is.numeric(df[[col]])) "Response" else "Factor"
    }, USE.NAMES = TRUE)
    rv$roles <- as.list(auto_roles)

    # Auto-suggest types (regression mode -> factors default to Numeric)
    mode <- input$analysis_mode %||% "comparative"
    auto_types <- sapply(names(df), function(col) {
      if (is.numeric(df[[col]])) {
        "Numeric"
      } else if (mode == "regression") {
        "Numeric"  # In regression mode, factors are treated as numeric by default
      } else {
        "Factor"
      }
    }, USE.NAMES = TRUE)
    rv$col_types <- as.list(auto_types)

    showNotification(paste("Loaded", nrow(df), "rows \u00d7", ncol(df), "columns."),
                     type = "message")
  })

  output$raw_preview <- DT::renderDataTable({
    req(rv$data)
    DT::datatable(head(rv$data, 20),
                  options = list(scrollX = TRUE, pageLength = 10),
                  rownames = FALSE)
  })

  observeEvent(input$load_example, {
    choice <- input$example_choice %||% "rcbd"
    reset_downstream()

    # Auto-switch analysis mode for regression examples
    if (choice %in% c("fracfact", "ccd")) {
      updateRadioButtons(session, "analysis_mode", selected = "regression")
    } else {
      updateRadioButtons(session, "analysis_mode", selected = "comparative")
    }

    if (choice == "rcbd") {
      # ── RCBD example (original) ───────────────────────────────────────
      set.seed(42)
      blocks_list <- lapply(1:4, function(b) {
        ord <- sample(4)
        data.frame(
          Block = paste0("B", b),
          A     = c("Low","High","Low","High")[ord],
          B     = c("Control","Control","Treatment","Treatment")[ord],
          stringsAsFactors = FALSE
        )
      })
      df <- do.call(rbind, blocks_list)
      df$RunOrder <- seq_len(nrow(df))

      block_effect <- c(B1=0, B2=1.8, B3=-1.2, B4=0.5)
      A_eff  <- ifelse(df$A == "High", 8, 0)
      B_eff  <- ifelse(df$B == "Treatment", 5, 0)
      AB_eff <- ifelse(df$A == "High" & df$B == "Treatment", 3, 0)
      blk    <- block_effect[df$Block]

      df$Temp_C <- round(22 + 0.15 * df$RunOrder + rnorm(16, 0, 0.8), 1)
      df$Yield  <- round(50 + A_eff + B_eff + AB_eff + blk + 0.4*(df$Temp_C-22) + rnorm(16,0,1.5), 2)
      df$Purity <- round(80 + 0.4*A_eff + 0.6*B_eff + 0.2*AB_eff + 0.3*blk - 0.2*(df$Temp_C-22) + rnorm(16,0,1.2), 2)

      rv$data <- df
      rv$roles <- list(
        Block    = "Block",
        A        = "Factor",
        B        = "Factor",
        RunOrder = "Run Order",
        Temp_C   = "Covariate",
        Yield    = "Response",
        Purity   = "Response"
      )
      rv$col_types <- list(
        Block    = "Factor",
        A        = "Factor",
        B        = "Factor",
        RunOrder = "Numeric",
        Temp_C   = "Numeric",
        Yield    = "Numeric",
        Purity   = "Numeric"
      )
      showNotification(
        "Example RCBD loaded: 16 runs \u00b7 2 factors \u00b7 4 blocks \u00b7 Temp_C covariate \u00b7 2 responses.",
        type = "message")

    } else if (choice == "mediator") {
      # ── Mediator Trap: "covariate" is actually a response ─────────────
      set.seed(99)
      n_per <- 8
      df <- data.frame(
        Catalyst = rep(c("A","B","C"), each = n_per),
        stringsAsFactors = FALSE
      )
      cat_eff <- c(A = 10, B = 15, C = 20)
      df$Viscosity <- round(cat_eff[df$Catalyst] + rnorm(nrow(df), 0, 1.5), 2)
      df$Strength  <- round(30 + 2 * df$Viscosity + rnorm(nrow(df), 0, 2), 2)

      rv$data <- df
      rv$roles <- list(
        Catalyst  = "Factor",
        Viscosity = "Covariate",
        Strength  = "Response"
      )
      rv$col_types <- list(
        Catalyst  = "Factor",
        Viscosity = "Numeric",
        Strength  = "Numeric"
      )
      showNotification(paste0(
        "Mediator Trap loaded: 24 runs \u00b7 1 factor (Catalyst) \u00b7 ",
        "Viscosity pre-assigned as covariate. ",
        "Try comparing Strength ~ Catalyst vs Strength ~ Catalyst + Viscosity."),
        type = "message")

    } else if (choice == "collinear") {
      # ── Block-Covariate Collinearity ──────────────────────────────────
      set.seed(77)
      plots      <- paste0("P", 1:4)
      fert_lvls  <- c("Low","Med","High")
      n_per_cell <- 3
      df <- expand.grid(
        Plot       = plots,
        Fertilizer = fert_lvls,
        Rep        = seq_len(n_per_cell),
        stringsAsFactors = FALSE
      )
      df$Rep <- NULL
      df$RunOrder <- sample(nrow(df))

      elevation  <- c(P1=100, P2=200, P3=300, P4=400)
      df$SoilMoisture <- round(
        50 - 0.08 * elevation[df$Plot] + rnorm(nrow(df), 0, 3), 1)

      fert_eff  <- c(Low=0, Med=6, High=12)
      block_eff <- c(P1=0, P2=2, P3=4, P4=5)
      df$Biomass <- round(
        40 + fert_eff[df$Fertilizer] + block_eff[df$Plot] +
        0.5 * df$SoilMoisture + rnorm(nrow(df), 0, 2), 2)

      rv$data <- df
      rv$roles <- list(
        Plot         = "Block",
        Fertilizer   = "Factor",
        RunOrder     = "Run Order",
        SoilMoisture = "Covariate",
        Biomass      = "Response"
      )
      rv$col_types <- list(
        Plot         = "Factor",
        Fertilizer   = "Factor",
        RunOrder     = "Numeric",
        SoilMoisture = "Numeric",
        Biomass      = "Numeric"
      )
      showNotification(paste0(
        "Collinearity example loaded: 36 runs \u00b7 1 factor (Fertilizer) \u00b7 ",
        "4 blocks (Plot) \u00b7 SoilMoisture covariate correlated with Plot. ",
        "Compare models with Block only, SoilMoisture only, and both."),
        type = "message")

    } else if (choice == "fracfact") {
      # ── 2^(4-1) Fractional Factorial (Resolution IV) ──────────────────
      # 4 factors in 8 runs; D = A*B*C (generator)
      set.seed(123)
      base <- expand.grid(A = c(-1, 1), B = c(-1, 1), C = c(-1, 1))
      base$D <- base$A * base$B * base$C   # generator D = ABC
      base$RunOrder <- sample(nrow(base))

      # True model: Y = 40 + 5*A + 3*B - 2*C + 1.5*D + 2*A*B + noise
      base$Yield <- round(40 + 5*base$A + 3*base$B - 2*base$C + 1.5*base$D +
                          2*base$A*base$B + rnorm(8, 0, 0.8), 2)

      rv$data <- base
      rv$roles <- list(
        A = "Factor", B = "Factor", C = "Factor", D = "Factor",
        RunOrder = "Run Order", Yield = "Response"
      )
      rv$col_types <- list(
        A = "Numeric", B = "Numeric", C = "Numeric", D = "Numeric",
        RunOrder = "Numeric", Yield = "Numeric"
      )
      rv$transforms <- list(
        A = "coding", B = "coding", C = "coding", D = "coding",
        RunOrder = "none", Yield = "none"
      )
      rv$coding_values <- list(
        A = list(low = -1, high = 1), B = list(low = -1, high = 1),
        C = list(low = -1, high = 1), D = list(low = -1, high = 1)
      )
      showNotification(
        paste0("2\u2074\u207b\u00b9 fractional factorial loaded: 8 runs, 4 factors (A-D), ",
               "D = ABC generator (Resolution IV). ",
               "Switch to Regression mode to fit polynomial models."),
        type = "message")

    } else if (choice == "ccd") {
      # ── CCD: 2 factors, face-centred (alpha=1) ────────────────────────
      set.seed(456)
      # Factorial portion (2^2)
      fact <- expand.grid(x1 = c(-1, 1), x2 = c(-1, 1))
      # Axial (star) points
      axial <- data.frame(
        x1 = c(-1, 1, 0, 0),
        x2 = c(0, 0, -1, 1)
      )
      # Centre points (3 replicates)
      centre <- data.frame(x1 = rep(0, 3), x2 = rep(0, 3))
      df <- rbind(fact, axial, centre)
      df$RunOrder <- sample(nrow(df))

      # True model: Y = 80 + 4*x1 - 3*x2 + 2*x1*x2 - 5*x1^2 - 4*x2^2 + noise
      df$Response <- round(80 + 4*df$x1 - 3*df$x2 + 2*df$x1*df$x2 -
                           5*df$x1^2 - 4*df$x2^2 + rnorm(nrow(df), 0, 0.6), 2)

      rv$data <- df
      rv$roles <- list(
        x1 = "Factor", x2 = "Factor",
        RunOrder = "Run Order", Response = "Response"
      )
      rv$col_types <- list(
        x1 = "Numeric", x2 = "Numeric",
        RunOrder = "Numeric", Response = "Numeric"
      )
      rv$transforms <- list(
        x1 = "coding", x2 = "coding",
        RunOrder = "none", Response = "none"
      )
      rv$coding_values <- list(
        x1 = list(low = -1, high = 1),
        x2 = list(low = -1, high = 1)
      )
      showNotification(
        paste0("CCD loaded: 11 runs, 2 factors (x1, x2), face-centred design. ",
               "True model has quadratic curvature. ",
               "Use Regression mode with full quadratic model."),
        type = "message")
    }
  })

  # ── Tab 2: Assign Roles ─────────────────────────────────────────────────

  output$role_ui <- renderUI({
    req(rv$data)
    cols       <- names(rv$data)
    role_ch    <- c("Response", "Factor", "Covariate", "Block", "Run Order", "Ignore")
    type_ch    <- c("Numeric", "Factor")
    n_per_row  <- 3

    rows <- lapply(seq(1, length(cols), by = n_per_row), function(start) {
      col_idx <- start:min(start + n_per_row - 1, length(cols))
      fluidRow(
        lapply(col_idx, function(i) {
          cn           <- cols[i]
          cur_role     <- rv$roles[[cn]]    %||% "Ignore"
          cur_type     <- rv$col_types[[cn]] %||%
                          if (is.numeric(rv$data[[cn]])) "Numeric" else "Factor"
          cur_transform <- rv$transforms[[cn]] %||% "none"

          # Column min/max for coding defaults
          col_vals <- suppressWarnings(as.numeric(as.character(rv$data[[cn]])))
          col_min  <- if (any(!is.na(col_vals))) min(col_vals, na.rm = TRUE) else 0
          col_max  <- if (any(!is.na(col_vals))) max(col_vals, na.rm = TRUE) else 1
          cur_code_low  <- rv$coding_values[[cn]]$low  %||% col_min
          cur_code_high <- rv$coding_values[[cn]]$high %||% col_max

          column(4,
            tags$div(
              style = "border:1px solid #dee2e6; border-radius:4px; padding:8px; margin-bottom:8px;",
              tags$strong(cn),
              selectInput(paste0("role_", cn), "Role",
                          choices = role_ch, selected = cur_role),
              selectInput(paste0("type_", cn), "Type",
                          choices = type_ch, selected = cur_type),
              conditionalPanel(
                condition = paste0("input.type_", cn, " == 'Numeric' && ",
                                   "input.role_", cn, " != 'Response' && ",
                                   "input.role_", cn, " != 'Ignore'"),
                selectInput(paste0("transform_", cn), "Transform",
                            choices = c("None" = "none", "Centre" = "centre",
                                        "Coding" = "coding"),
                            selected = cur_transform),
                conditionalPanel(
                  condition = paste0("input.transform_", cn, " == 'coding'"),
                  fluidRow(
                    column(6,
                      numericInput(paste0("code_low_", cn), "\u22121 level",
                                   value = cur_code_low)
                    ),
                    column(6,
                      numericInput(paste0("code_high_", cn), "+1 level",
                                   value = cur_code_high)
                    )
                  )
                )
              )
            )
          )
        })
      )
    })

    tagList(rows)
  })

  # Sync role and type selects -> rv$roles / rv$col_types
  observe({
    req(rv$data)
    cols <- names(rv$data)
    for (col in cols) {
      local({
        cn <- col
        observeEvent(input[[paste0("role_", cn)]], {
          rv$roles[[cn]] <- input[[paste0("role_", cn)]]
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("type_", cn)]], {
          rv$col_types[[cn]] <- input[[paste0("type_", cn)]]
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("transform_", cn)]], {
          rv$transforms[[cn]] <- input[[paste0("transform_", cn)]]
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("code_low_", cn)]], {
          if (is.null(rv$coding_values[[cn]])) rv$coding_values[[cn]] <- list()
          rv$coding_values[[cn]]$low <- input[[paste0("code_low_", cn)]]
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("code_high_", cn)]], {
          if (is.null(rv$coding_values[[cn]])) rv$coding_values[[cn]] <- list()
          rv$coding_values[[cn]]$high <- input[[paste0("code_high_", cn)]]
        }, ignoreInit = TRUE)
      })
    }
  })

  # When analysis mode changes, update factor column types and transforms
  observeEvent(input$analysis_mode, {
    req(rv$data, length(rv$roles) > 0)
    mode <- input$analysis_mode
    new_type <- if (mode == "regression") "Numeric" else "Factor"
    for (cn in names(rv$roles)) {
      if (rv$roles[[cn]] == "Factor") {
        rv$col_types[[cn]] <- new_type
        updateSelectInput(session, paste0("type_", cn), selected = new_type)
        # Default transform: coding for regression, none for comparative
        new_tr <- if (mode == "regression") "coding" else "none"
        rv$transforms[[cn]] <- new_tr
        updateSelectInput(session, paste0("transform_", cn), selected = new_tr)
      }
    }
    reset_downstream()
  }, ignoreInit = TRUE)

  # Role-based reactive helpers
  responses  <- reactive({ names(Filter(function(r) r == "Response",  rv$roles)) })
  factors_   <- reactive({ names(Filter(function(r) r == "Factor",    rv$roles)) })
  covariates <- reactive({ names(Filter(function(r) r == "Covariate", rv$roles)) })
  blocks     <- reactive({ names(Filter(function(r) r == "Block",     rv$roles)) })
  run_orders <- reactive({ names(Filter(function(r) r == "Run Order", rv$roles)) })

  # All covariates for model building (Covariate + Run Order roles)
  all_covariates <- reactive({ unique(c(covariates(), run_orders())) })

  # ── Tab 3: Explore ──────────────────────────────────────────────────────

  # Treatment column: combined factor levels (for shape/group in plots)
  treatment <- reactive({
    facs <- factors_()
    if (length(facs) == 0 || is.null(rv$data)) return(NULL)
    facs_in <- intersect(facs, names(rv$data))
    if (length(facs_in) == 0) return(NULL)
    as.factor(apply(rv$data[, facs_in, drop = FALSE], 1,
                    function(r) paste(r, collapse = ":")))
  })

  observe({
    updateSelectInput(session, "explore_response",  choices = responses())
    updateSelectInput(session, "explore_factor",    choices = factors_())
    updateSelectInput(session, "explore_covariate", choices = all_covariates())

    # Colour-by: any column in the dataset
    cols <- names(rv$data %||% data.frame())
    colour_ch <- c("None" = "none", "Treatment" = ".treatment")
    for (cn in cols) colour_ch <- c(colour_ch, setNames(cn, cn))
    updateSelectInput(session, "explore_colour_by", choices = colour_ch)

    # Facet-by: blocks + factors
    facet_ch <- c("None" = "none")
    for (b in blocks()) facet_ch <- c(facet_ch, setNames(b, b))
    for (f in factors_()) facet_ch <- c(facet_ch, setNames(f, f))
    updateSelectInput(session, "explore_facet_by", choices = facet_ch)
  })

  # Helper: get colour vector for explore plots
  explore_colour <- reactive({
    cb <- input$explore_colour_by
    if (is.null(cb) || cb == "none") return(NULL)
    if (cb == ".treatment") return(treatment())
    if (cb %in% names(rv$data)) return(as.factor(rv$data[[cb]]))
    NULL
  })

  # Helper: apply faceting to a ggplot
  explore_facet <- function(p) {
    fb <- input$explore_facet_by
    if (!is.null(fb) && fb != "none" && fb %in% names(rv$data))
      p <- p + facet_wrap(reformulate(fb))
    p
  }

  output$dist_plot <- renderPlotly({
    req(rv$data, input$explore_response)
    y_col <- input$explore_response
    req(y_col %in% names(rv$data))
    df <- rv$data
    cv <- explore_colour()
    if (!is.null(cv)) {
      df$.colour <- cv
      p <- ggplot(df, aes_string(x = y_col, fill = ".colour")) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       alpha = 0.5, colour = "white", position = "identity") +
        geom_density(aes_string(colour = ".colour"), linewidth = 1) +
        labs(fill = input$explore_colour_by, colour = input$explore_colour_by)
    } else {
      p <- ggplot(df, aes_string(x = y_col)) +
        geom_histogram(aes(y = after_stat(density)), bins = 30,
                       fill = "#1f77b4", alpha = 0.6, colour = "white") +
        geom_density(colour = "#d62728", linewidth = 1)
    }
    p <- p + labs(title = paste("Distribution of", y_col), x = y_col, y = "Density") +
      theme_minimal()
    p <- explore_facet(p)
    ggplotly(p)
  })

  output$by_factor_plot <- renderPlotly({
    req(rv$data, input$explore_response, input$explore_factor)
    resp <- input$explore_response; fac <- input$explore_factor
    req(resp %in% names(rv$data), fac %in% names(rv$data))
    df <- rv$data; df[[fac]] <- as.factor(df[[fac]])
    trt <- treatment()
    cv  <- explore_colour()

    if (!is.null(trt)) {
      df$.treatment <- trt
      p <- ggplot(df, aes_string(x = fac, y = resp)) +
        geom_boxplot(aes_string(fill = fac), alpha = 0.5, outlier.shape = NA)
      if (!is.null(cv)) {
        df$.colour <- cv
        p <- ggplot(df, aes_string(x = fac, y = resp)) +
          geom_boxplot(aes_string(fill = fac), alpha = 0.5, outlier.shape = NA) +
          geom_jitter(aes(shape = .treatment, colour = .colour),
                      width = 0.2, alpha = 0.7, size = 2) +
          labs(colour = input$explore_colour_by, shape = "Treatment")
      } else {
        p <- p + geom_jitter(aes(shape = .treatment), width = 0.2, alpha = 0.7, size = 2) +
          labs(shape = "Treatment")
      }
    } else {
      p <- ggplot(df, aes_string(x = fac, y = resp, fill = fac)) +
        geom_boxplot(alpha = 0.5, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.7, size = 2)
    }
    p <- p + labs(title = paste(resp, "by", fac), x = fac, y = resp) +
      theme_minimal()
    p <- explore_facet(p)
    ggplotly(p)
  })

  output$by_block_ui <- renderUI({
    if (length(blocks()) == 0) {
      p("No block variable assigned.", class = "text-muted p-3")
    } else {
      tagList(
        selectInput("explore_block", "Block variable", choices = blocks()),
        plotlyOutput("by_block_plot", height = "500px")
      )
    }
  })

  output$by_block_plot <- renderPlotly({
    req(rv$data, input$explore_response, input$explore_block)
    resp <- input$explore_response; blk <- input$explore_block
    req(resp %in% names(rv$data), blk %in% names(rv$data))
    df <- rv$data; df[[blk]] <- as.factor(df[[blk]])
    trt <- treatment()
    cv  <- explore_colour()

    if (!is.null(trt)) {
      df$.treatment <- trt
      p <- ggplot(df, aes_string(x = blk, y = resp))
      if (!is.null(cv)) {
        df$.colour <- cv
        p <- p + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
          geom_jitter(aes(shape = .treatment, colour = .colour),
                      width = 0.2, alpha = 0.7, size = 2) +
          labs(colour = input$explore_colour_by, shape = "Treatment")
      } else {
        p <- p + geom_boxplot(alpha = 0.3, outlier.shape = NA) +
          geom_jitter(aes(shape = .treatment, colour = .treatment),
                      width = 0.2, alpha = 0.7, size = 2) +
          labs(colour = "Treatment", shape = "Treatment")
      }
    } else {
      p <- ggplot(df, aes_string(x = blk, y = resp, fill = blk)) +
        geom_boxplot(alpha = 0.5, outlier.shape = NA) +
        geom_jitter(width = 0.2, alpha = 0.7, size = 2)
    }
    p <- p + labs(title = paste(resp, "by block:", blk), x = blk, y = resp) +
      theme_minimal()
    p <- explore_facet(p)
    ggplotly(p)
  })

  output$covariate_plot <- renderPlotly({
    req(rv$data, input$explore_response, input$explore_covariate)
    resp <- input$explore_response; cov_name <- input$explore_covariate
    req(resp %in% names(rv$data), cov_name %in% names(rv$data))
    df <- rv$data
    trt <- treatment()
    cv  <- explore_colour()
    sm  <- input$explore_smooth %||% "lm"

    if (!is.null(trt)) {
      df$.treatment <- trt
      if (!is.null(cv)) {
        df$.colour <- cv
        p <- ggplot(df, aes_string(x = cov_name, y = resp)) +
          geom_point(aes(shape = .treatment, colour = .colour), alpha = 0.7, size = 2) +
          geom_smooth(aes(group = .treatment, linetype = .treatment),
                      method = sm, se = TRUE, colour = "grey30", linewidth = 0.8) +
          labs(colour = input$explore_colour_by, shape = "Treatment",
               linetype = "Treatment")
      } else {
        p <- ggplot(df, aes_string(x = cov_name, y = resp,
                                    colour = ".treatment", shape = ".treatment")) +
          geom_point(alpha = 0.7, size = 2) +
          geom_smooth(method = sm, se = TRUE, linewidth = 0.8) +
          labs(colour = "Treatment", shape = "Treatment")
      }
    } else {
      p <- ggplot(df, aes_string(x = cov_name, y = resp)) +
        geom_point(alpha = 0.6, colour = "#1f77b4", size = 2) +
        geom_smooth(method = sm, se = TRUE, colour = "#d62728", linewidth = 0.8)
    }
    p <- p + labs(title = paste(resp, "vs", cov_name), x = cov_name, y = resp) +
      theme_minimal()
    p <- explore_facet(p)
    ggplotly(p)
  })

  # ── Tab 4: Design ──────────────────────────────────────────────────────

  # Populate design selectors when data/roles change
  observe({
    facs <- factors_()
    resps <- responses()
    blks  <- blocks()

    # Colour-by choices: None + factors + blocks + responses
    colour_choices <- c("None" = "none")
    for (f in facs) colour_choices <- c(colour_choices, setNames(f, f))
    for (b in blks) colour_choices <- c(colour_choices, setNames(b, paste0(b, " (block)")))
    for (r in resps) colour_choices <- c(colour_choices, setNames(r, paste0(r, " (response)")))

    # Shape-by: factors + blocks only (categorical)
    shape_choices <- c("None" = "none")
    for (f in facs) shape_choices <- c(shape_choices, setNames(f, f))
    for (b in blks) shape_choices <- c(shape_choices, setNames(b, paste0(b, " (block)")))

    # Axis choices: factors only
    axis_choices <- facs

    # Facet choices: factors + blocks
    facet_choices <- c("None" = "none")
    for (f in facs) facet_choices <- c(facet_choices, setNames(f, f))
    for (b in blks) facet_choices <- c(facet_choices, setNames(b, paste0(b, " (block)")))

    updateSelectInput(session, "splom_colour", choices = colour_choices)
    updateSelectInput(session, "design_x", choices = axis_choices)
    updateSelectInput(session, "design_y", choices = axis_choices,
                      selected = if (length(axis_choices) > 1) axis_choices[2] else axis_choices[1])
    updateSelectInput(session, "design_row_facet", choices = facet_choices)
    updateSelectInput(session, "design_col_facet", choices = facet_choices)
    updateSelectInput(session, "design_2d_colour", choices = colour_choices)
    updateSelectInput(session, "design_3d_x", choices = axis_choices)
    updateSelectInput(session, "design_3d_y", choices = axis_choices,
                      selected = if (length(axis_choices) > 1) axis_choices[2] else axis_choices[1])
    updateSelectInput(session, "design_3d_z", choices = axis_choices,
                      selected = if (length(axis_choices) > 2) axis_choices[3] else axis_choices[1])
    updateSelectInput(session, "design_3d_colour", choices = colour_choices)
    updateSelectInput(session, "design_3d_shape", choices = shape_choices)
  })

  # Design Matrix table (coded values)
  output$design_matrix_table <- DT::renderDataTable({
    req(rv$data, length(factors_()) > 0)
    facs  <- factors_()
    coded <- coded_design_matrix(rv$data, facs, rv$transforms, rv$coding_values)
    # Add response columns for reference
    resps <- responses()
    for (r in resps) if (r %in% names(rv$data)) coded[[r]] <- rv$data[[r]]
    DT::datatable(coded, rownames = TRUE,
                  options = list(scrollX = TRUE, pageLength = 30, dom = "frtip")) %>%
      DT::formatRound(names(coded)[sapply(coded, is.numeric)], digits = 3)
  })

  # Scatterplot Matrix
  output$design_splom <- renderPlotly({
    req(rv$data, length(factors_()) >= 2)
    facs <- factors_()
    df   <- rv$data
    jit  <- input$splom_jitter %||% 0.15

    dims <- lapply(facs, function(f) {
      vals <- if (is.numeric(df[[f]])) df[[f]] else as.numeric(as.factor(df[[f]]))
      vals <- vals + runif(length(vals), -jit, jit)
      list(label = f, values = vals)
    })

    colour_var <- input$splom_colour
    if (!is.null(colour_var) && colour_var != "none" && colour_var %in% names(df)) {
      cv <- df[[colour_var]]
      if (is.numeric(cv)) {
        marker <- list(color = cv, colorscale = "Viridis",
                       showscale = TRUE, colorbar = list(title = colour_var))
      } else {
        marker <- list(color = as.numeric(as.factor(cv)), colorscale = "Viridis",
                       showscale = TRUE, colorbar = list(title = colour_var))
      }
    } else {
      marker <- list(color = "#1f77b4")
    }

    plot_ly(type = "splom", dimensions = dims, marker = marker,
            text = apply(df[, facs, drop = FALSE], 1,
                         function(r) paste(facs, "=", r, collapse = "\n")),
            hoverinfo = "text") %>%
      layout(title = "Scatterplot Matrix of Design Factors",
             dragmode = "select")
  })

  # 2D Design Map
  output$design_2d <- renderPlotly({
    req(rv$data, input$design_x, input$design_y)
    df    <- rv$data
    x_col <- input$design_x; y_col <- input$design_y
    req(x_col %in% names(df), y_col %in% names(df))

    mode <- input$design_2d_mode %||% "points"
    colour_var <- input$design_2d_colour
    has_colour <- !is.null(colour_var) && colour_var != "none" && colour_var %in% names(df)

    if (mode == "heatmap" && has_colour && is.numeric(df[[colour_var]])) {
      # Heatmap mode: tile plot coloured by numeric variable (e.g. response)
      agg <- aggregate(df[[colour_var]],
                       by = list(x = df[[x_col]], y = df[[y_col]]),
                       FUN = mean, na.rm = TRUE)
      names(agg) <- c("x", "y", "value")
      agg$x <- as.factor(agg$x); agg$y <- as.factor(agg$y)
      p <- ggplot(agg, aes(x = x, y = y, fill = value)) +
        geom_tile(colour = "white", linewidth = 0.5) +
        geom_text(aes(label = round(value, 2)), size = 3) +
        scale_fill_viridis_c(name = colour_var) +
        labs(title = paste("Design Heatmap (", colour_var, ")"),
             x = x_col, y = y_col)
    } else {
      # Points mode
      jit <- input$design_2d_jitter %||% 0.15
      df[[x_col]] <- as.factor(df[[x_col]])
      df[[y_col]] <- as.factor(df[[y_col]])

      if (has_colour) {
        if (!is.numeric(df[[colour_var]])) df[[colour_var]] <- as.factor(df[[colour_var]])
        p <- ggplot(df, aes_string(x = x_col, y = y_col, colour = colour_var)) +
          geom_jitter(width = jit, height = jit, alpha = 0.7, size = 2.5)
        if (is.numeric(df[[colour_var]]))
          p <- p + scale_colour_viridis_c()
      } else {
        p <- ggplot(df, aes_string(x = x_col, y = y_col)) +
          geom_jitter(width = jit, height = jit, alpha = 0.7, size = 2.5, colour = "#1f77b4")
      }
      p <- p + labs(title = "2D Design Map", x = x_col, y = y_col)
    }

    # Faceting
    row_f <- input$design_row_facet; col_f <- input$design_col_facet
    has_row <- !is.null(row_f) && row_f != "none" && row_f %in% names(df)
    has_col <- !is.null(col_f) && col_f != "none" && col_f %in% names(df)
    if (has_row && has_col) {
      p <- p + facet_grid(reformulate(col_f, row_f))
    } else if (has_row) {
      p <- p + facet_wrap(reformulate(row_f), ncol = 1)
    } else if (has_col) {
      p <- p + facet_wrap(reformulate(col_f), nrow = 1)
    }

    p <- p + theme_minimal()
    ggplotly(p)
  })

  # 3D Design Space
  output$design_3d <- renderPlotly({
    req(rv$data, input$design_3d_x, input$design_3d_y, input$design_3d_z)
    df <- rv$data
    xc <- input$design_3d_x; yc <- input$design_3d_y; zc <- input$design_3d_z
    req(xc %in% names(df), yc %in% names(df), zc %in% names(df))

    x_vals <- if (is.numeric(df[[xc]])) df[[xc]] else as.numeric(as.factor(df[[xc]]))
    y_vals <- if (is.numeric(df[[yc]])) df[[yc]] else as.numeric(as.factor(df[[yc]]))
    z_vals <- if (is.numeric(df[[zc]])) df[[zc]] else as.numeric(as.factor(df[[zc]]))

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

    if (has_colour && is.numeric(df[[colour_var]])) {
      p <- plot_ly(x = x_vals, y = y_vals, z = z_vals,
                   type = "scatter3d", mode = "markers",
                   marker = list(color = df[[colour_var]], colorscale = "Viridis",
                                 showscale = TRUE, size = 5,
                                 colorbar = list(title = colour_var)),
                   text = hover_text, hoverinfo = "text")
    } else if (has_colour) {
      p <- plot_ly(x = x_vals, y = y_vals, z = z_vals,
                   color = as.factor(df[[colour_var]]),
                   type = "scatter3d", mode = "markers",
                   marker = list(size = 5),
                   text = hover_text, hoverinfo = "text")
    } else {
      p <- plot_ly(x = x_vals, y = y_vals, z = z_vals,
                   type = "scatter3d", mode = "markers",
                   marker = list(color = "#1f77b4", size = 5),
                   text = hover_text, hoverinfo = "text")
    }

    if (has_shape) {
      sym_map <- c("circle", "square", "diamond", "cross", "x",
                   "triangle-up", "triangle-down", "star")
      sv <- as.factor(df[[shape_var]])
      symbols <- sym_map[as.integer(sv)]
      p <- p %>% style(marker = list(symbol = symbols))
    }

    p %>% layout(
      title = "3D Design Space",
      scene = list(xaxis = list(title = xc),
                   yaxis = list(title = yc),
                   zaxis = list(title = zc)))
  })

  # Alias Structure
  # Auto-fill alias formulas from roles
  observeEvent(input$alias_auto_fill, {
    facs <- factors_()
    blks <- blocks()
    covs <- all_covariates()

    # Full model: main effects + 2FI (typical design model)
    full_parts <- facs
    if (length(facs) >= 2) {
      combos2 <- combn(facs, 2, simplify = FALSE)
      full_parts <- c(full_parts, sapply(combos2, paste, collapse = ":"))
    }
    updateTextInput(session, "alias_full_formula",
                    value = paste(full_parts, collapse = " + "))

    # Alias model: all factor interactions + block×factor
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
    updateTextInput(session, "alias_check_formula",
                    value = paste(check_parts, collapse = " + "))
  })

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
    dt <- DT::datatable(result, rownames = FALSE,
                        options = list(scrollX = TRUE, pageLength = 30, dom = "frtip"))
    if ("Correlation" %in% names(result))
      dt <- DT::formatRound(dt, "Correlation", digits = 3)
    dt
  })

  # Power Analysis table
  output$power_table <- DT::renderDataTable({
    req(rv$data, length(factors_()) > 0)
    sigma     <- input$power_sigma     %||% 1
    delta     <- input$power_delta     %||% 1
    alpha     <- input$power_alpha     %||% 0.05
    max_order <- input$power_max_order %||% 2
    result <- design_power(rv$data, factors_(), sigma, delta, alpha, max_order)
    if (nrow(result) == 0)
      return(DT::datatable(data.frame(Message = "No terms computed.")))
    dt <- DT::datatable(result, rownames = FALSE,
                        options = list(scrollX = TRUE, pageLength = 30, dom = "t"))
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

    p <- ggplot(curves_df, aes(x = delta, y = Power, colour = Term)) +
      geom_line(linewidth = 0.8) +
      geom_hline(yintercept = 0.8, linetype = "dashed", colour = "grey50") +
      annotate("text", x = max(deltas) * 0.9, y = 0.82, label = "80% power",
               colour = "grey50", size = 3) +
      labs(title = "Power Curve", x = "Effect size (\u03b4)", y = "Power",
           colour = "Term") +
      ylim(0, 1) +
      theme_minimal()
    ggplotly(p)
  })

  # Needle Plot: response vs nested factor combinations
  observe({
    resps <- responses()
    facs  <- factors_()
    blks  <- blocks()
    updateSelectInput(session, "needle_response", choices = resps)

    # Colour-by: same as design colour choices
    colour_ch <- c("None" = "none")
    for (f in facs) colour_ch <- c(colour_ch, setNames(f, f))
    for (b in blks) colour_ch <- c(colour_ch, setNames(b, paste0(b, " (block)")))
    for (r in resps) colour_ch <- c(colour_ch, setNames(r, paste0(r, " (response)")))
    updateSelectInput(session, "needle_colour", choices = colour_ch)

    # Facet-by: blocks
    facet_ch <- c("None" = "none")
    for (b in blks) facet_ch <- c(facet_ch, setNames(b, b))
    updateSelectInput(session, "needle_facet", choices = facet_ch)
  })

  # Dynamic factor order UI
  output$needle_factor_order_ui <- renderUI({
    facs <- factors_()
    if (length(facs) == 0) return(NULL)
    selectInput("needle_factor_order", "Factor nesting order",
                choices = facs, selected = facs, multiple = TRUE)
  })

  output$needle_plot <- renderPlotly({
    req(rv$data, input$needle_response, length(factors_()) > 0)
    resp_col <- input$needle_response
    req(resp_col %in% names(rv$data))

    df <- rv$data
    fac_order <- input$needle_factor_order %||% factors_()
    fac_order <- intersect(fac_order, names(df))
    if (length(fac_order) == 0) fac_order <- factors_()
    sort_mode <- input$needle_sort %||% "factors"

    # Create treatment ID from nested factors
    df$.trt_id <- apply(df[, fac_order, drop = FALSE], 1,
                         function(r) paste(r, collapse = ":"))

    grand_mean <- mean(df[[resp_col]], na.rm = TRUE)

    if (sort_mode == "response") {
      # Sort by mean response per treatment
      trt_means <- aggregate(df[[resp_col]],
                              by = list(.trt_id = df$.trt_id), FUN = mean, na.rm = TRUE)
      names(trt_means)[2] <- "mean_y"
      trt_order <- trt_means$.trt_id[order(trt_means$mean_y)]
      df$.trt_id <- factor(df$.trt_id, levels = trt_order)
    } else {
      df$.trt_id <- factor(df$.trt_id)
    }

    # Colour
    colour_var <- input$needle_colour
    has_colour <- !is.null(colour_var) && colour_var != "none" && colour_var %in% names(df)

    df$.grand_mean <- grand_mean

    if (has_colour) {
      if (!is.numeric(df[[colour_var]])) df[[colour_var]] <- as.factor(df[[colour_var]])
      p <- ggplot(df, aes_string(x = ".trt_id", y = resp_col, colour = colour_var)) +
        geom_hline(yintercept = grand_mean, colour = "grey40", linewidth = 0.8) +
        geom_segment(aes_string(x = ".trt_id", xend = ".trt_id",
                                 y = ".grand_mean", yend = resp_col),
                     alpha = 0.5, linewidth = 0.6) +
        geom_point(size = 2.5, alpha = 0.8)
    } else {
      p <- ggplot(df, aes_string(x = ".trt_id", y = resp_col)) +
        geom_hline(yintercept = grand_mean, colour = "grey40", linewidth = 0.8) +
        geom_segment(aes_string(x = ".trt_id", xend = ".trt_id",
                                 y = ".grand_mean", yend = resp_col),
                     colour = "#1f77b4", alpha = 0.5, linewidth = 0.6) +
        geom_point(colour = "#1f77b4", size = 2.5, alpha = 0.8)
    }

    # Faceting
    facet_var <- input$needle_facet
    has_facet <- !is.null(facet_var) && facet_var != "none" && facet_var %in% names(df)
    if (has_facet) p <- p + facet_wrap(reformulate(facet_var), scales = "free_x")

    p <- p +
      annotate("text", x = 1, y = grand_mean, label = paste("mean =", round(grand_mean, 2)),
               vjust = -0.5, hjust = 0, colour = "grey40", size = 3) +
      labs(title = paste("Needle plot \u2014", resp_col),
           x = paste(fac_order, collapse = " : "),
           y = resp_col) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p)
  })

  # ── Tab 5: Models ──────────────────────────────────────────────────────

  observe({
    updateSelectInput(session, "active_response", choices = responses())
  })

  # Editable formula list (textInput per formula)
  output$formula_list_ui <- renderUI({
    if (length(rv$formulas) == 0) return(p("No formulas generated yet.", class = "text-muted"))
    tagList(lapply(seq_along(rv$formulas), function(i) {
      f <- rv$formulas[[i]]
      fluidRow(
        column(1,
          checkboxInput(paste0("fsel_", i), NULL, value = TRUE)
        ),
        column(11,
          textInput(paste0("fedit_", i), NULL, value = f)
        )
      )
    }))
  })

  observeEvent(input$generate_formulas_btn, {
    req(input$active_response)
    mode <- input$analysis_mode %||% "comparative"

    if (mode == "regression") {
      req(length(factors_()) > 0 || length(all_covariates()) > 0)
      formulas <- build_regression_formulas(
        response          = input$active_response,
        factors           = factors_(),
        covariates        = all_covariates(),
        blocks            = blocks(),
        max_way           = input$max_way,
        poly_degree       = input$poly_degree %||% 2,
        include_covariates = isTRUE(input$include_covariates),
        include_cov_fac    = isTRUE(input$include_cov_fac),
        include_blocks     = isTRUE(input$include_blocks),
        include_block_fac  = isTRUE(input$include_block_fac)
      )
    } else {
      req(length(factors_()) > 0)
      formulas <- build_formulas(
        response          = input$active_response,
        factors           = factors_(),
        covariates        = all_covariates(),
        blocks            = blocks(),
        max_way           = input$max_way,
        include_covariates = isTRUE(input$include_covariates),
        include_blocks     = isTRUE(input$include_blocks),
        include_block_fac  = isTRUE(input$include_block_fac)
      )
    }
    rv$formulas <- formulas
  })

  # Collect selected & edited formulas for model fitting
  get_selected_formulas <- reactive({
    if (length(rv$formulas) == 0) return(character(0))
    sel <- character(0)
    for (i in seq_along(rv$formulas)) {
      checked <- input[[paste0("fsel_", i)]]
      edited  <- input[[paste0("fedit_", i)]]
      if (isTRUE(checked) && !is.null(edited) && nchar(trimws(edited)) > 0) {
        f <- trimws(edited)
        sel[f] <- f
      }
    }
    sel
  })

  observeEvent(input$select_all_formulas, {
    for (i in seq_along(rv$formulas))
      updateCheckboxInput(session, paste0("fsel_", i), value = TRUE)
  })
  observeEvent(input$deselect_all_formulas, {
    for (i in seq_along(rv$formulas))
      updateCheckboxInput(session, paste0("fsel_", i), value = FALSE)
  })

  observeEvent(input$add_custom, {
    req(input$custom_formula, nchar(trimws(input$custom_formula)) > 0)
    f <- trimws(input$custom_formula)
    rv$formulas <- c(rv$formulas, setNames(f, f))
    updateTextInput(session, "custom_formula", value = "")
  })

  observeEvent(input$run_models, {
    req(rv$data)
    formulas_to_run <- get_selected_formulas()
    req(length(formulas_to_run) > 0)

    withProgress(message = "Fitting models...", value = 0, {
      rv$models <- fit_models(formulas_to_run, rv$data,
                              col_types = rv$col_types,
                              transforms = rv$transforms,
                              coding_values = rv$coding_values)
      incProgress(0.4)

      model_names <- names(rv$models)
      updateCheckboxGroupInput(session, "show_models",
                               choices = model_names, selected = model_names)
      updateSelectInput(session, "resid_model",       choices = model_names)

      # Update colour-by options for residual plots
      colour_choices <- c("None" = "none")
      facs <- factors_(); blks <- blocks()
      if (length(facs) > 1)
        colour_choices <- c(colour_choices, "Treatment (all factors)" = ".treatment")
      for (f in facs) colour_choices <- c(colour_choices, setNames(f, f))
      for (b in blks) colour_choices <- c(colour_choices, setNames(b, paste0(b, " (block)")))
      updateSelectInput(session, "resid_colour_by", choices = colour_choices)
      updateSelectInput(session, "mc_show_model",      choices = model_names)
      updateSelectInput(session, "anova_single_model", choices = model_names,
                        selected = model_names[1])

      # Factor-only terms for effects / MC
      all_terms_raw <- unique(unlist(lapply(rv$models, function(m) {
        a <- tryCatch(car::Anova(m, type=3), error=function(e) NULL)
        if (!is.null(a)) rownames(a)[rownames(a) != "(Intercept)"] else character(0)
      })))
      factor_cols  <- factors_()

      # Categorical factor terms (type = Factor) — valid for MC and LS means
      cat_factor_terms <- all_terms_raw[sapply(all_terms_raw, function(t) {
        parts <- strsplit(t, ":")[[1]]
        all(parts %in% factor_cols) &&
          all(sapply(parts, function(cn) (rv$col_types[[cn]] %||% "Factor") == "Factor"))
      })]

      # All factor-role terms (including numeric-coded) — valid for effects plots
      # For numeric factors: only main effects (not interactions, which need 2D surfaces)
      all_factor_terms <- all_terms_raw[sapply(all_terms_raw, function(t) {
        parts <- strsplit(t, ":")[[1]]
        if (!all(parts %in% factor_cols)) return(FALSE)
        # For categorical factors: include interactions too
        if (all(sapply(parts, function(cn) (rv$col_types[[cn]] %||% "Factor") == "Factor")))
          return(TRUE)
        # For numeric factors: only main effects (single term, no interaction)
        length(parts) == 1
      })]

      updateCheckboxGroupInput(session, "mc_terms",
                               choices = cat_factor_terms, selected = cat_factor_terms)

      # Effects: factor terms (filtered above) + covariates
      cov_cols <- all_covariates()
      effects_choices <- c(all_factor_terms, setNames(cov_cols, cov_cols))
      updateSelectInput(session, "effects_term", choices = effects_choices)

      incProgress(0.3)

      # Compute VIF
      rv$vif_df <- vif_summary(rv$models)
      if (nrow(rv$vif_df) > 0) {
        model_cols <- setdiff(names(rv$vif_df), "Term")
        vif_vals <- rv$vif_df[, model_cols, drop = FALSE]
        high_mask <- apply(vif_vals, c(1,2), function(x) !is.na(x) && x > sqrt(5))
        if (any(high_mask)) {
          terms_flagged <- rv$vif_df$Term[apply(high_mask, 1, any)]
          showNotification(
            paste0("Collinearity detected (VIF): ",
                   paste(terms_flagged, collapse = ", "),
                   ". Check the Collinearity tab in Results."),
            type = "warning", duration = 10)
        }
      }

      incProgress(0.3)
    })
  })

  # ── Backward Elimination ───────────────────────────────────────────────
  observeEvent(input$prune_btn, {
    req(length(rv$models) > 0)
    alpha <- input$prune_alpha %||% 0.05

    # Prune each currently fitted model
    pruned <- list()
    for (mname in names(rv$models)) {
      m <- rv$models[[mname]]
      pruned_m <- tryCatch(
        backward_eliminate(m, alpha = alpha),
        error = function(e) { message("Prune error: ", e$message); NULL }
      )
      if (!is.null(pruned_m)) {
        new_label <- deparse(formula(pruned_m))
        if (!new_label %in% names(rv$models) && !new_label %in% names(pruned)) {
          pruned[[new_label]] <- pruned_m
        }
      }
    }

    if (length(pruned) > 0) {
      rv$models <- c(rv$models, pruned)
      # Add pruned formulas to the formula list
      for (f in names(pruned)) {
        rv$formulas <- c(rv$formulas, setNames(f, f))
      }
      model_names <- names(rv$models)
      updateCheckboxGroupInput(session, "show_models",
                               choices = model_names, selected = model_names)
      updateSelectInput(session, "resid_model", choices = model_names)
      updateSelectInput(session, "mc_show_model", choices = model_names)
      updateSelectInput(session, "anova_single_model", choices = model_names)

      # Recompute VIF for all models including pruned
      rv$vif_df <- vif_summary(rv$models)

      showNotification(
        paste0("Added ", length(pruned), " pruned model(s) (alpha = ", alpha, ")."),
        type = "message")
    } else {
      showNotification("No terms removed \u2014 all p-values already \u2264 alpha.",
                       type = "message")
    }
  })

  # ── Multiple Comparisons ──────────────────────────────────────────────
  run_mc_results <- function() {
    req(length(rv$models) > 0, length(input$mc_terms) > 0, length(input$mc_method) > 0)
    results <- list()
    for (mname in names(rv$models)) {
      model <- rv$models[[mname]]
      for (spec in input$mc_terms) {
        for (method in input$mc_method) {
          key <- paste(mname, spec, method, sep = "__")
          results[[key]] <- run_mc(model, spec, method,
                                   control      = input$dunnett_control,
                                   contrast_mat = input$contrasts_input)
        }
      }
    }
    rv$mc_results <- results
    updateCheckboxGroupInput(session, "mc_show_method",
                             choices = input$mc_method, selected = input$mc_method)
  }

  observeEvent(input$run_mc_btn, {
    req(isTRUE(input$mc_on))
    withProgress(message = "Running comparisons...", value = 0.5, {
      run_mc_results()
    })
    showNotification("Multiple comparisons complete.", type = "message")
  })

  # Hide/show the Multiple Comparisons results tab
  observe({
    if (isTRUE(input$mc_on)) {
      showTab("results_tabs", target = "Multiple Comparisons")
    } else {
      hideTab("results_tabs", target = "Multiple Comparisons")
    }
  })

  observe({
    req(length(factors_()) > 0, rv$data)
    lvls <- unique(as.character(rv$data[[factors_()[1]]]))
    updateSelectInput(session, "dunnett_control", choices = lvls)
  })

  output$model_run_status <- renderText({
    n <- length(rv$models)
    if (n == 0) "No models fitted yet."
    else paste(n, "model(s) fitted:", paste(names(rv$models), collapse = "\n"))
  })

  # ── Tab 5: Results ──────────────────────────────────────────────────────

  active_models <- reactive({
    req(length(rv$models) > 0)
    sel <- input$show_models
    if (is.null(sel) || length(sel) == 0) return(rv$models)
    rv$models[intersect(sel, names(rv$models))]
  })

  # Helper: build term_roles for ANOVA ordering
  term_roles <- reactive({
    all_terms_raw <- unique(unlist(lapply(active_models(), function(m) {
      a <- tryCatch(car::Anova(m, type=3), error=function(e) NULL)
      if (!is.null(a)) rownames(a) else character(0)
    })))
    classify_anova_terms(all_terms_raw, factors_(), blocks(), all_covariates())
  })

  # ── ANOVA: cross-model wide table (F/p toggle) ──────────────────────────
  output$anova_table <- DT::renderDataTable({
    req(length(active_models()) > 0)
    wide <- type3_wide(active_models(), term_roles())
    if (nrow(wide) == 0) return(DT::datatable(data.frame(Message = "No ANOVA results.")))

    show <- input$anova_show %||% "p"  # now a character vector from checkboxes

    # Build column selection based on checkboxes
    keep <- "term"
    if ("p" %in% show) keep <- c(keep, grep("_p$", names(wide), value = TRUE))
    if ("F" %in% show) keep <- c(keep, grep("_F$", names(wide), value = TRUE))
    wide <- wide[, intersect(keep, names(wide)), drop = FALSE]

    # Merge VIF columns if requested
    vif_cols <- character(0)
    if ("vif" %in% show && nrow(rv$vif_df) > 0) {
      vif_wide <- rv$vif_df
      # Rename VIF model columns to ModelName_VIF
      vif_model_cols <- setdiff(names(vif_wide), "Term")
      for (mc in vif_model_cols) {
        vif_col_name <- paste0(mc, "_VIF")
        wide[[vif_col_name]] <- vif_wide[[mc]][match(wide$term, vif_wide$Term)]
        vif_cols <- c(vif_cols, vif_col_name)
      }
    }

    # Interleave columns: for each model, show p, F, VIF together
    model_names <- names(active_models())
    ordered_cols <- "term"
    for (mn in model_names) {
      if ("p" %in% show)   ordered_cols <- c(ordered_cols, paste0(mn, "_p"))
      if ("F" %in% show)   ordered_cols <- c(ordered_cols, paste0(mn, "_F"))
      if ("vif" %in% show) ordered_cols <- c(ordered_cols, paste0(mn, "_VIF"))
    }
    ordered_cols <- intersect(ordered_cols, names(wide))
    wide <- wide[, ordered_cols, drop = FALSE]

    p_cols <- grep("_p$", names(wide), value = TRUE)
    f_cols <- grep("_F$", names(wide), value = TRUE)

    dt <- DT::datatable(wide, rownames = FALSE,
                        options = list(scrollX = TRUE, pageLength = 20, dom = "frtip"),
                        filter = "top")
    for (pc in p_cols) {
      dt <- DT::formatStyle(dt, pc,
               backgroundColor = DT::styleInterval(0.05, c("#c8f7c5", "white")),
               fontWeight      = DT::styleInterval(0.05, c("bold", "normal")))
      dt <- DT::formatRound(dt, pc, digits = 4)
    }
    if (length(f_cols) > 0) dt <- DT::formatRound(dt, f_cols, digits = 3)
    for (vc in vif_cols) {
      if (vc %in% names(wide)) {
        dt <- DT::formatRound(dt, vc, digits = 2)
        dt <- DT::formatStyle(dt, vc,
                 backgroundColor = DT::styleInterval(
                   c(sqrt(5), sqrt(10)),
                   c("white", "#fff3cd", "#f8d7da")),
                 fontWeight = DT::styleInterval(c(sqrt(5)), c("normal", "bold")))
      }
    }
    dt
  })

  # ── ANOVA: RMSE / R^2 summary ─────────────────────────────────────────────
  output$rmse_table <- DT::renderDataTable({
    req(length(active_models()) > 0)
    smry <- rmse_summary(active_models())
    DT::datatable(smry, rownames=FALSE,
                  options=list(scrollX=TRUE, dom="t", pageLength=20)) |>
      DT::formatRound(c("RMSE","R2","Adj_R2"), digits=4)
  })

  # ── ANOVA: single model full table ───────────────────────────────────────
  output$anova_single_table <- DT::renderDataTable({
    req(input$anova_single_model, length(rv$models) > 0)
    mname <- input$anova_single_model
    req(mname %in% names(rv$models))
    m  <- rv$models[[mname]]
    a  <- tryCatch(car::Anova(m, type=3), error=function(e) NULL)
    if (is.null(a)) return(DT::datatable(data.frame(Message="ANOVA failed.")))
    df <- as.data.frame(a)
    df$term <- rownames(df)
    df <- df[df$term != "(Intercept)", ]
    rownames(df) <- NULL

    # Order terms
    tr <- classify_anova_terms(df$term, factors_(), blocks(), all_covariates())
    role_order <- c("Factor" = 1, "Block" = 2, "Covariate" = 3, "Residuals" = 4)
    df$`.sort` <- sapply(df$term, function(t) role_order[tr[t]] %||% 3.5)
    df <- df[order(df$`.sort`), ]
    df$`.sort` <- NULL

    num_cols <- sapply(df, is.numeric)
    df[num_cols] <- lapply(df[num_cols], round, 4)
    p_col <- grep("Pr\\(", names(df), value=TRUE)
    dt <- DT::datatable(df[, c("term", setdiff(names(df),"term"))],
                        rownames=FALSE,
                        options=list(scrollX=TRUE, dom="t", pageLength=30),
                        caption=paste("Type III ANOVA \u2014", mname))
    if (length(p_col) > 0)
      dt <- DT::formatStyle(dt, p_col,
               backgroundColor = DT::styleInterval(0.05, c("#c8f7c5","white")))
    dt
  })

  # ── VIF Collinearity Table ─────────────────────────────────────────────
  output$vif_table <- DT::renderDataTable({
    req(nrow(rv$vif_df) > 0)
    wide <- rv$vif_df
    model_cols <- setdiff(names(wide), "Term")
    dt <- DT::datatable(wide, rownames = FALSE,
                        options = list(scrollX = TRUE, pageLength = 30, dom = "t"))
    # Colour each model column: red > 3.16 (Very High), amber > 2.24 (High)
    for (mc in model_cols) {
      dt <- DT::formatRound(dt, mc, digits = 2)
      dt <- DT::formatStyle(dt, mc,
               backgroundColor = DT::styleInterval(
                 c(sqrt(5), sqrt(10)),
                 c("white", "#fff3cd", "#f8d7da")),
               fontWeight = DT::styleInterval(
                 c(sqrt(5)),
                 c("normal", "bold")))
    }
    dt
  })

  # ── Coefficients Table ──────────────────────────────────────────────────
  output$coef_table <- DT::renderDataTable({
    req(length(active_models()) > 0)
    cf <- coef_table(active_models())
    if (is.null(cf) || nrow(cf) == 0)
      return(DT::datatable(data.frame(Message = "No coefficients available.")))
    num_cols <- c("Estimate", "Std.Error", "t.value", "p.value")
    dt <- DT::datatable(cf, rownames = FALSE,
                        options = list(scrollX = TRUE, pageLength = 30, dom = "frtip"))
    dt <- DT::formatRound(dt, num_cols, digits = 4)
    dt <- DT::formatStyle(dt, "p.value",
             backgroundColor = DT::styleInterval(0.05, c("#c8f7c5", "white")))
    dt
  })

  # ── Unified Effects Tab (LS Means + Covariate Effects) ──────────────────
  output$effects_plot <- renderPlotly({
    req(length(active_models()) > 0, input$effects_term)
    term <- input$effects_term

    factor_cols <- factors_()
    term_parts  <- strsplit(term, ":")[[1]]
    # A term is a "categorical factor" only if the role is Factor AND the type
    # is Factor (i.e. not numeric-coded as in regression mode)
    is_factor_term <- all(term_parts %in% factor_cols) &&
      all(sapply(term_parts, function(cn) {
        (rv$col_types[[cn]] %||% "Factor") == "Factor"
      }))

    if (is_factor_term) {
      # ── Factor term: LS Means with adjusted data overlay ──────────────
      spec <- term
      ls_list <- lapply(names(active_models()), function(mn) {
        get_lsmeans_df(active_models()[[mn]], spec, mn)
      })
      ls_df <- bind_rows(Filter(Negate(is.null), ls_list))
      if (nrow(ls_df) == 0) return(plotly_empty())

      spec_terms <- strsplit(spec, ":")[[1]]
      y_col      <- if ("emmean" %in% names(ls_df)) "emmean" else names(ls_df)[2]
      has_ci     <- all(c("lower.CL","upper.CL") %in% names(ls_df))
      if (has_ci) {
        ls_df$ci_lower <- ls_df$lower.CL
        ls_df$ci_upper <- ls_df$upper.CL
      }
      colours    <- model_colours(length(unique(ls_df$model)))

      # For interactions, create a combined x-axis label
      if (length(spec_terms) == 1) {
        x_col <- spec_terms[1]
        x_label <- spec_terms[1]
      } else {
        # Concatenate factor levels for x-axis (e.g., "Low:Control")
        ls_df$x_combined <- apply(ls_df[, spec_terms, drop=FALSE], 1,
                                   function(r) paste(r, collapse=":"))
        x_col <- "x_combined"
        x_label <- paste(spec_terms, collapse = " : ")
      }

      if (input$effects_view == "Overlay") {
        p <- ggplot(ls_df, aes_string(x=x_col, y=y_col, colour="model", group="model")) +
          geom_point(size=3, position=position_dodge(0.3))
        if (has_ci)
          p <- p + geom_errorbar(
            aes_string(ymin="ci_lower", ymax="ci_upper"),
            width=0.2, position=position_dodge(0.3))

        # Adjusted data overlay
        if (isTRUE(input$effects_show_data)) {
          pr_list <- lapply(names(active_models()), function(mn) {
            get_partial_residuals_factor(active_models()[[mn]], spec, mn)
          })
          pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
          if (nrow(pr_df) > 0) {
            if (length(spec_terms) > 1)
              pr_df$x_combined <- apply(pr_df[, spec_terms, drop=FALSE], 1,
                                            function(r) paste(r, collapse=":"))
            pr_x <- if (length(spec_terms) > 1) "x_combined" else spec_terms[1]
            p <- p + geom_point(data=pr_df, aes_string(x=pr_x, y="adjusted_y", colour="model"),
                                alpha=0.3, size=1.5,
                                position=position_jitterdodge(jitter.width=0.15, dodge.width=0.3),
                                inherit.aes=FALSE)
          }
        }

        p <- p + scale_colour_manual(values=colours) +
          labs(title=paste("LS Means \u2014",spec), x=x_label, y="LS Mean", colour="Model",
               subtitle=if(isTRUE(input$effects_show_data)) "Points adjusted for other model terms" else NULL) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle=if(length(spec_terms)>1) 45 else 0, hjust=1))
      } else {
        p <- ggplot(ls_df, aes_string(x=x_col, y=y_col, colour="model")) +
          geom_point(size=3)
        if (has_ci)
          p <- p + geom_errorbar(
            aes_string(ymin="ci_lower", ymax="ci_upper"),
            width=0.2)

        if (isTRUE(input$effects_show_data)) {
          pr_list <- lapply(names(active_models()), function(mn) {
            get_partial_residuals_factor(active_models()[[mn]], spec, mn)
          })
          pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
          if (nrow(pr_df) > 0) {
            if (length(spec_terms) > 1)
              pr_df$x_combined <- apply(pr_df[, spec_terms, drop=FALSE], 1,
                                            function(r) paste(r, collapse=":"))
            pr_x <- if (length(spec_terms) > 1) "x_combined" else spec_terms[1]
            p <- p + geom_jitter(data=pr_df, aes_string(x=pr_x, y="adjusted_y"),
                                 alpha=0.3, size=1.5, width=0.15, colour="grey40",
                                 inherit.aes=FALSE)
          }
        }

        p <- p + facet_wrap(~model) + scale_colour_manual(values=colours) +
          labs(title=paste("LS Means \u2014",spec,"(faceted)"), x=x_label, y="LS Mean",
               subtitle=if(isTRUE(input$effects_show_data)) "Points adjusted for other model terms" else NULL) +
          theme_minimal() + theme(legend.position="none",
            axis.text.x = element_text(angle=if(length(spec_terms)>1) 45 else 0, hjust=1))
      }
      ggplotly(p)

    } else {
      # ── Covariate term: partial effect with adjusted data overlay ──────
      cov <- term

      # Filter models that include this covariate
      relevant <- Filter(function(m) {
        mf <- tryCatch(model.frame(m), error=function(e) NULL)
        if (is.null(mf)) return(FALSE)
        cov %in% names(mf)
      }, active_models())

      if (length(relevant) == 0) {
        return(plotly_empty() %>%
          layout(title="No fitted models contain this covariate."))
      }

      eff_list <- lapply(names(relevant), function(mn) {
        get_effects_df(relevant[[mn]], cov, mn)
      })
      eff_df <- bind_rows(Filter(Negate(is.null), eff_list))
      if (nrow(eff_df) == 0) return(plotly_empty())

      # Get response name from first relevant model
      resp_name <- names(model.frame(relevant[[1]]))[1]

      colours <- model_colours(length(unique(eff_df$model)))
      p <- ggplot(eff_df, aes(x=x, y=fit, colour=model, fill=model)) +
        geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.15, colour=NA) +
        geom_line(linewidth=1)

      # Adjusted data overlay (partial residuals)
      if (isTRUE(input$effects_show_data)) {
        pr_list <- lapply(names(relevant), function(mn) {
          get_partial_residuals_covariate(relevant[[mn]], cov, mn)
        })
        pr_df <- bind_rows(Filter(Negate(is.null), pr_list))
        if (nrow(pr_df) > 0)
          p <- p + geom_point(data=pr_df, aes(x=x, y=adjusted_y, colour=model),
                              alpha=0.4, size=1.5, inherit.aes=FALSE)
      }

      p <- p + scale_colour_manual(values=colours) +
        scale_fill_manual(values=colours) +
        labs(title=paste("Partial effect of", cov, "on", resp_name),
             x=cov, y=paste("Predicted", resp_name),
             subtitle=if(isTRUE(input$effects_show_data))
               "Points adjusted for other model terms" else
               "Factors at reference level; other covariates at their mean",
             colour="Model") +
        guides(fill="none") +
        theme_minimal()
      ggplotly(p)
    }
  })

  # ── Effects Table (LS Means or covariate summary) ──────────────────────
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
      ls_list <- lapply(names(active_models()), function(mn) {
        get_lsmeans_df(active_models()[[mn]], spec, mn)
      })
      ls_df <- bind_rows(Filter(Negate(is.null), ls_list))
      if (nrow(ls_df) == 0)
        return(DT::datatable(data.frame(Message = "No LS means available.")))
      # Round numeric columns
      num_cols <- sapply(ls_df, is.numeric)
      ls_df[num_cols] <- lapply(ls_df[num_cols], function(x) round(x, 4))
      DT::datatable(ls_df, rownames = FALSE, caption = paste("LS Means \u2014", spec),
                    options = list(scrollX = TRUE, pageLength = 20))
    } else {
      return(NULL)
    }
  })

  # ── Leverage Plots (Added-Variable Plots) ─────────────────────────────

  # Update leverage model selector when models change
  observe({
    model_names <- names(rv$models)
    updateSelectInput(session, "leverage_model", choices = model_names)
  })

  output$leverage_plots_ui <- renderUI({
    req(input$leverage_model, rv$models[[input$leverage_model]])
    m <- rv$models[[input$leverage_model]]
    term_labels <- attr(terms(m), "term.labels")
    if (length(term_labels) == 0) return(p("No terms in this model.", class = "text-muted"))
    tagList(lapply(seq_along(term_labels), function(i) {
      plotlyOutput(paste0("leverage_plot_", i), height = "350px")
    }))
  })

  observe({
    req(input$leverage_model, rv$models[[input$leverage_model]])
    m <- rv$models[[input$leverage_model]]
    lev_data <- leverage_plot_data(m)
    term_labels <- attr(terms(m), "term.labels")

    for (i in seq_along(term_labels)) {
      local({
        idx <- i; tl <- term_labels[idx]
        output[[paste0("leverage_plot_", idx)]] <- renderPlotly({
          dd <- lev_data[[tl]]
          if (is.null(dd) || nrow(dd) == 0) return(plotly_empty())

          fit <- lm(y_residual ~ x_residual, data = dd)
          dd$fitted_line <- fitted(fit)
          ci <- predict(fit, interval = "confidence")
          dd$ci_lower <- ci[, "lwr"]
          dd$ci_upper <- ci[, "upr"]

          p <- ggplot(dd, aes(x = x_residual, y = y_residual)) +
            geom_point(alpha = 0.6, colour = "#1f77b4") +
            geom_line(aes(y = fitted_line), colour = "#d62728", linewidth = 1) +
            geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                        alpha = 0.15, fill = "#d62728") +
            geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
            labs(title = paste("Leverage plot \u2014", tl),
                 x = paste(tl, "| others"),
                 y = paste("Response | others")) +
            theme_minimal()
          ggplotly(p)
        })
      })
    }
  })

  # ── Multiple Comparisons Results ────────────────────────────────────────
  output$mc_plot <- renderPlotly({
    req(isTRUE(input$mc_on), length(rv$mc_results) > 0,
        !is.null(input$mc_show_method), !is.null(input$mc_show_model))
    mname   <- input$mc_show_model
    methods <- input$mc_show_method
    rows <- list()
    for (method in methods) {
      for (spec in input$mc_terms) {
        key <- paste(mname, spec, method, sep="__")
        if (!is.null(rv$mc_results[[key]]) && nrow(rv$mc_results[[key]]) > 0)
          rows[[key]] <- rv$mc_results[[key]]
      }
    }
    if (length(rows) == 0) return(plotly_empty())
    mc_df <- bind_rows(rows)
    if (!"contrast" %in% names(mc_df)) mc_df$contrast <- as.character(seq_len(nrow(mc_df)))
    est_col <- if ("estimate" %in% names(mc_df)) "estimate" else names(mc_df)[2]
    se_col  <- if ("SE" %in% names(mc_df)) "SE" else
               if ("std.error" %in% names(mc_df)) "std.error" else NULL
    p <- ggplot(mc_df, aes_string(x="contrast", y=est_col, colour="method")) +
      geom_point(size=2.5) +
      geom_hline(yintercept=0, linetype="dashed", colour="grey50") +
      coord_flip() +
      labs(title=paste("Comparison estimates \u2014", mname),
           x="Contrast", y="Estimate", colour="Method") +
      theme_minimal()
    if (!is.null(se_col))
      p <- p + geom_errorbar(
        aes_string(ymin=paste0(est_col," - 1.96*",se_col),
                   ymax=paste0(est_col," + 1.96*",se_col)),
        width=0.2)
    ggplotly(p)
  })

  output$mc_table <- DT::renderDataTable({
    req(isTRUE(input$mc_on), length(rv$mc_results) > 0,
        !is.null(input$mc_show_method), !is.null(input$mc_show_model))
    mname <- input$mc_show_model; methods <- input$mc_show_method
    rows <- list()
    for (method in methods) {
      for (spec in input$mc_terms) {
        key <- paste(mname, spec, method, sep="__")
        if (!is.null(rv$mc_results[[key]]) && nrow(rv$mc_results[[key]]) > 0)
          rows[[key]] <- rv$mc_results[[key]]
      }
    }
    if (length(rows) == 0) return(DT::datatable(data.frame(Message="No results.")))
    mc_df <- bind_rows(rows)
    p_cols <- grep("p.value|p_value|p\\.adj", names(mc_df), value=TRUE)
    dt <- DT::datatable(mc_df, rownames=FALSE,
                        options=list(scrollX=TRUE, pageLength=20))
    for (pc in p_cols) {
      dt <- DT::formatStyle(dt, pc,
               backgroundColor=DT::styleInterval(0.05, c("#c8f7c5","white")))
      dt <- DT::formatRound(dt, pc, digits=4)
    }
    dt
  })

  # ── Residuals ─────────────────────────────────────────────────────────────

  # Helper: build diagnostic data frame for a model
  resid_df <- reactive({
    req(input$resid_model, length(rv$models) > 0)
    mname <- input$resid_model
    req(mname %in% names(rv$models))
    m  <- rv$models[[mname]]
    mf <- model.frame(m)
    n <- length(fitted(m))
    df <- data.frame(
      index         = seq_along(fitted(m)),
      fitted_vals   = fitted(m),
      actual        = model.response(mf),
      resid_raw     = residuals(m),
      resid_std     = rstandard(m),
      resid_ext     = rstudent(m),
      cooks         = cooks.distance(m)
    )
    # Add run order if available
    ro <- run_orders()
    if (length(ro) > 0 && ro[1] %in% names(rv$data)) {
      df$run_order <- rv$data[[ro[1]]][seq_len(n)]
    }
    # Build hover text: response name, actual, fitted, residual, run order
    resp_name <- names(mf)[1]
    hover_parts <- paste0(
      resp_name, ": ", round(df$actual, 3),
      "\nFitted: ", round(df$fitted_vals, 3),
      "\nResidual: ", round(df$resid_raw, 4),
      "\nObs #: ", df$index
    )
    if (!is.null(df$run_order))
      hover_parts <- paste0(hover_parts, "\nRun Order: ", df$run_order)
    df$hover <- hover_parts
    df
  })

  # Helper: get colour vector for residual points
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

  # Standard 2x2 diagnostics
  output$resid_standard <- renderPlotly({
    df_d <- resid_df()
    df_d$sqrt_abs_ext <- sqrt(abs(df_d$resid_ext))
    df_d$theo_q       <- qqnorm(df_d$resid_std, plot.it=FALSE)$x
    cv <- resid_colour_vec()
    has_colour <- !is.null(cv)
    if (has_colour) df_d$colour_by <- cv

    if (has_colour) {
      p1 <- ggplot(df_d, aes(x=fitted_vals, y=resid_raw, colour=colour_by, text=hover)) +
        geom_point(alpha=0.6)
      p2 <- ggplot(df_d, aes(x=theo_q, y=resid_std, colour=colour_by, text=hover)) +
        geom_point(alpha=0.6)
      p3 <- ggplot(df_d, aes(x=fitted_vals, y=sqrt_abs_ext, colour=colour_by, text=hover)) +
        geom_point(alpha=0.6)
    } else {
      p1 <- ggplot(df_d, aes(x=fitted_vals, y=resid_raw, text=hover)) +
        geom_point(alpha=0.6)
      p2 <- ggplot(df_d, aes(x=theo_q, y=resid_std, text=hover)) +
        geom_point(alpha=0.6)
      p3 <- ggplot(df_d, aes(x=fitted_vals, y=sqrt_abs_ext, text=hover)) +
        geom_point(alpha=0.6)
    }

    p1 <- p1 +
      geom_hline(yintercept=0, linetype="dashed", colour="red") +
      geom_smooth(se=FALSE, colour="blue", linewidth=0.8) +
      labs(title="Residuals vs Fitted", x="Fitted values", y="Residuals") +
      theme_minimal() + theme(legend.position="none")

    p2 <- p2 +
      geom_abline(slope=1, intercept=0, colour="red", linetype="dashed") +
      labs(title="Normal Q-Q (internally standardised)",
           x="Theoretical quantiles", y="Standardised residuals") +
      theme_minimal() + theme(legend.position="none")

    p3 <- p3 +
      geom_smooth(se=FALSE, colour="blue", linewidth=0.8) +
      labs(title="Scale-Location (externally studentised)",
           x="Fitted values", y="\u221a|Externally studentised residuals|") +
      theme_minimal() + theme(legend.position="none")

    p4 <- ggplot(df_d, aes(x=index, y=cooks, text=hover)) +
      geom_col(fill="#1f77b4", alpha=0.7) +
      geom_hline(yintercept=4/nrow(df_d), linetype="dashed", colour="red") +
      labs(title="Cook's Distance", x="Observation index", y="Cook's D") +
      theme_minimal()

    subplot(ggplotly(p1, tooltip="text"), ggplotly(p2, tooltip="text"),
            ggplotly(p3, tooltip="text"), ggplotly(p4, tooltip="text"),
            nrows=2, titleX=TRUE, titleY=TRUE, margin=0.06) %>%
      layout(title=list(text=paste("Residual Diagnostics \u2014", input$resid_model)))
  })

  # Actual vs Predicted
  output$resid_actual_vs_pred <- renderPlotly({
    df_d <- resid_df()
    cv <- resid_colour_vec()
    has_colour <- !is.null(cv)
    if (has_colour) df_d$colour_by <- cv
    lims <- range(c(df_d$actual, df_d$fitted_vals), na.rm=TRUE)
    if (has_colour) {
      p <- ggplot(df_d, aes(x=fitted_vals, y=actual, colour=colour_by, text=hover)) +
        geom_point(alpha=0.6)
    } else {
      p <- ggplot(df_d, aes(x=fitted_vals, y=actual, text=hover)) +
        geom_point(alpha=0.6, colour="#1f77b4")
    }
    p <- p +
      geom_abline(slope=1, intercept=0, colour="red", linetype="dashed") +
      coord_fixed(xlim=lims, ylim=lims) +
      labs(title=paste("Actual vs. Predicted \u2014", input$resid_model),
           x="Predicted (fitted)", y="Actual", colour=NULL) +
      theme_minimal()
    ggplotly(p, tooltip="text")
  })

  # vs Run Order (uses Run Order role)
  output$resid_vs_runorder_ui <- renderUI({
    ro <- run_orders()
    if (length(ro) == 0 || !any(ro %in% names(rv$data %||% data.frame()))) {
      p("No Run Order column assigned. Set a column's role to 'Run Order' in the Assign Roles tab.",
        class="text-muted")
    } else {
      plotlyOutput("resid_vs_runorder_plot", height="400px")
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
    if (has_colour) {
      p <- ggplot(df_d, aes(x=run_order, y=resid_raw, colour=colour_by, text=hover)) +
        geom_point(alpha=0.6)
    } else {
      p <- ggplot(df_d, aes(x=run_order, y=resid_raw, text=hover)) +
        geom_point(alpha=0.6, colour="#1f77b4")
    }
    p <- p +
      geom_hline(yintercept=0, linetype="dashed", colour="red") +
      geom_smooth(method="loess", se=FALSE, colour="blue", linewidth=0.8) +
      labs(title=paste("Residuals vs Run Order \u2014", input$resid_model),
           x=ro_col, y="Residuals", colour=NULL) +
      theme_minimal()
    ggplotly(p, tooltip="text")
  })

  # vs Model Terms - dynamic plots
  output$resid_vs_terms_ui <- renderUI({
    req(input$resid_model, rv$models[[input$resid_model]])
    m         <- rv$models[[input$resid_model]]
    pred_vars <- names(model.frame(m))[-1]
    tagList(lapply(seq_along(pred_vars), function(i) {
      plotlyOutput(paste0("resid_term_", i), height="320px")
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
          # Don't colour if colour-by matches this x-axis variable
          use_colour <- !is.null(cv) && !identical(cb, vn)
          df_t <- data.frame(x=mf[[vn]], resid=resids, hover=resid_df()$hover)
          if (use_colour) df_t$colour_by <- cv
          if (use_colour) {
            p <- ggplot(df_t, aes(x=x, y=resid, colour=colour_by, text=hover)) +
              geom_point(alpha=0.6)
          } else {
            p <- ggplot(df_t, aes(x=x, y=resid, text=hover)) +
              geom_point(alpha=0.6)
          }
          p <- p +
            geom_hline(yintercept=0, linetype="dashed", colour="red") +
            labs(title=paste("Residuals vs", vn), x=vn, y="Residuals", colour=NULL) +
            theme_minimal()
          if (is.numeric(mf[[vn]]))
            p <- p + geom_smooth(method="loess", se=FALSE, colour="blue", linewidth=0.8)
          ggplotly(p, tooltip="text")
        })
      })
    }
  })

  # vs Omitted Terms
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
    if (length(omitted) == 0) {
      return(p("All data columns are included in this model.", class="text-muted"))
    }
    tagList(lapply(seq_along(omitted), function(i) {
      plotlyOutput(paste0("resid_omit_", i), height="320px")
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
          # Use model's row indices to match residuals to data
          m2 <- rv$models[[input$resid_model]]
          row_idx <- as.integer(names(fitted(m2)))
          if (is.null(row_idx) || any(is.na(row_idx))) row_idx <- seq_len(n)
          x_vals <- rv$data[[cn]][row_idx]
          df_o   <- data.frame(x=x_vals, resid=df_r$resid_raw, hover=df_r$hover)
          if (use_colour && length(cv) == n) df_o$colour_by <- cv
          use_colour <- use_colour && "colour_by" %in% names(df_o)
          if (use_colour) {
            p <- ggplot(df_o, aes(x=x, y=resid, colour=colour_by, text=hover)) +
              geom_point(alpha=0.6)
          } else {
            p <- ggplot(df_o, aes(x=x, y=resid, text=hover)) +
              geom_point(alpha=0.6)
          }
          p <- p +
            geom_hline(yintercept=0, linetype="dashed", colour="red") +
            labs(title=paste("Residuals vs", cn, "(not in model)"),
                 x=cn, y="Residuals", colour=NULL) +
            theme_minimal()
          if (is.numeric(x_vals))
            p <- p + geom_smooth(method="loess", se=FALSE, colour="orange", linewidth=0.8)
          ggplotly(p, tooltip="text")
        })
      })
    }
  })

  # ── Tab 6: Report ──────────────────────────────────────────────────────

  output$report_summary <- renderUI({
    tagList(
      p(strong(length(rv$models)), "model(s) fitted,",
        strong(length(input$show_models)), "selected for report.")
    )
  })

  output$download_report <- downloadHandler(
    filename = function() {
      ext <- switch(input$report_format, word = "docx", pdf = "pdf", html = "html")
      paste0("ModelComparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".", ext)
    },
    content = function(file) {
      req(rv$data, length(active_models()) > 0)

      all_terms_raw <- unique(unlist(lapply(active_models(), function(m) {
        a <- tryCatch(car::Anova(m, type=3), error=function(e) NULL)
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
        mc_on           = isTRUE(input$mc_on),
        mc_terms        = input$mc_terms %||% character(0),
        mc_methods      = input$mc_show_method %||% character(0)
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
            input         = "report_template.Rmd",
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
}
