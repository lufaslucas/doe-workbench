# R/mod_data_upload.R — Data > Upload sub-tab module (UI + server)

# ── UI ───────────────────────────────────────────────────────────────────────
mod_data_upload_ui <- function(id) {
  ns <- NS(id)
  tabPanel("Upload Dataset",
    br(),
    fluidRow(
      column(4,
        wellPanel(
          h4("Data Upload"),
          fileInput(ns("file"), "Choose CSV or Excel file",
                    accept = c(".csv", ".xlsx", ".xls")),
          conditionalPanel(
            condition = paste0("input['", ns("file"), "'] != null && ",
                               "input['", ns("file"), "'].name.endsWith('.csv')"),
            radioButtons(ns("sep"), "CSV Separator",
                         choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                         selected = ",", inline = TRUE)
          ),
          hr(),
          selectInput(ns("example_choice"), "Example dataset",
                      choices = c(
                        "RCBD (2 factors, blocks, covariate)"       = "rcbd",
                        "Latin Square (4\u00d74, 2 factors)"        = "latin",
                        "Mediator Trap (covariate is a response)"  = "mediator",
                        "Block\u2013Covariate Collinearity"        = "collinear",
                        "2\u2074\u207b\u00b9 Fractional Factorial" = "fracfact",
                        "CCD (Central Composite Design)"           = "ccd",
                        "Historical MLR (confounded)"              = "historical",
                        "Unbalanced 1-way (unequal n)"            = "unbalanced"
                      ),
                      selected = "rcbd"),
          actionButton(ns("load_example"), "Load Example Dataset",
                       class = "btn-outline-secondary w-100",
                       icon  = icon("flask"))
        )
      ),
      column(8,
        h4("Data Preview"),
        DT::dataTableOutput(ns("raw_preview"))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
mod_data_upload_server <- function(id, rv, analysis_mode, navigate_to,
                                   set_analysis_mode, reset_downstream,
                                   fit_models, models_exports, design_exports) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Read-only guard ─────────────────────────────────────────────────
    observe({
      locked <- isTRUE(rv$read_only)
      toggle <- if (locked) shinyjs::disable else shinyjs::enable
      toggle(ns("file"))
      toggle(ns("load_example"))
    })

    # ── File upload handler ──────────────────────────────────────────────
    observeEvent(input$file, {
      req(input$file)
      if (is_locked(rv, "Data upload")) return()
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
      rv$data <- stamp_row_ids(df)
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
      mode <- analysis_mode()
      auto_types <- sapply(names(df), function(col) {
        if (is.numeric(df[[col]])) {
          "Numeric"
        } else if (mode == "regression") {
          "Numeric"
        } else {
          "Factor"
        }
      }, USE.NAMES = TRUE)
      rv$col_types <- as.list(auto_types)

      showNotification(paste("Loaded", nrow(df), "rows \u00d7", ncol(df), "columns."),
                       type = "message")
    })

    # ── Data preview table ───────────────────────────────────────────────
    output$raw_preview <- DT::renderDataTable({
      req(rv$data)
      display_data <- rv$data[, setdiff(names(rv$data), ROW_ID_COL), drop = FALSE]
      # Prepend row-id column so user can identify observations
      display_data <- cbind(Obs = rv$data[[ROW_ID_COL]], display_data)

      dt <- dt_table(display_data,
               options = list(pageLength = PAGE_LEN_DEFAULT),
               rownames = FALSE)

      # Highlight selected observations
      sel_ids <- rv$selected_obs
      if (!is.null(sel_ids) && length(sel_ids) > 0) {
        matched <- sel_ids[sel_ids %in% display_data$Obs]
        if (length(matched) > 0) {
          dt <- dt %>% DT::formatStyle(
            "Obs",
            target = "row",
            backgroundColor = DT::styleEqual(
              matched, rep("#e3f2fd", length(matched))
            ),
            fontWeight = DT::styleEqual(
              matched, rep("bold", length(matched))
            )
          )
        }
      }
      dt
    })

    # ── Example dataset loading ──────────────────────────────────────────
    observeEvent(input$load_example, {
      if (is_locked(rv, "Example loading")) return()
      choice <- input$example_choice %||% "rcbd"
      reset_downstream()

      # Auto-switch analysis mode for regression examples
      if (choice %in% c("fracfact", "ccd", "historical")) {
        set_analysis_mode("regression")
      } else {
        set_analysis_mode("comparative")
      }

      if (choice == "rcbd") {
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

        rv$data <- stamp_row_ids(df)
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

      } else if (choice == "latin") {
        set.seed(77)
        treatments <- c("A1B1","A1B2","A2B1","A2B2")
        latin <- matrix(c(
          1,2,3,4,
          2,1,4,3,
          3,4,1,2,
          4,3,2,1
        ), nrow = 4, byrow = TRUE)

        rows <- list()
        for (r in 1:4) {
          for (cc in 1:4) {
            trt_idx <- latin[r, cc]
            rows <- c(rows, list(data.frame(
              Row = paste0("R", r),
              Col = paste0("C", cc),
              A   = c("Low","Low","High","High")[trt_idx],
              B   = c("Low","High","Low","High")[trt_idx],
              stringsAsFactors = FALSE
            )))
          }
        }
        df <- do.call(rbind, rows)
        df$RunOrder <- sample(nrow(df))

        A_eff   <- ifelse(df$A == "High", 1, -1) * 5
        B_eff   <- ifelse(df$B == "High", 1, -1) * 3
        AB_eff  <- ifelse(df$A == "High", 1, -1) * ifelse(df$B == "High", 1, -1) * 2
        row_eff <- c(R1 = -2, R2 = 1, R3 = 3, R4 = -2)[df$Row]
        col_eff <- c(C1 = 1, C2 = -1, C3 = 2, C4 = -2)[df$Col]

        df$Yield <- round(60 + A_eff + B_eff + AB_eff + row_eff + col_eff + rnorm(16, 0, 1.5), 2)

        rv$data <- stamp_row_ids(df)
        rv$roles <- list(
          Row      = "Block",
          Col      = "Block",
          A        = "Factor",
          B        = "Factor",
          RunOrder = "Run Order",
          Yield    = "Response"
        )
        rv$col_types <- list(
          Row      = "Factor",
          Col      = "Factor",
          A        = "Factor",
          B        = "Factor",
          RunOrder = "Numeric",
          Yield    = "Numeric"
        )
        showNotification(
          "Latin Square loaded: 16 runs \u00b7 2 factors (A, B) \u00b7 Row & Col blocks.",
          type = "message")

      } else if (choice == "mediator") {
        set.seed(99)
        n_per <- 8
        df <- data.frame(
          Catalyst = rep(c("A","B","C"), each = n_per),
          stringsAsFactors = FALSE
        )
        cat_eff <- c(A = 10, B = 15, C = 20)
        df$Viscosity <- round(cat_eff[df$Catalyst] + rnorm(nrow(df), 0, 1.5), 2)
        df$Strength  <- round(30 + 2 * df$Viscosity + rnorm(nrow(df), 0, 2), 2)

        rv$data <- stamp_row_ids(df)
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

        rv$data <- stamp_row_ids(df)
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
        set.seed(123)
        base <- expand.grid(Temp = c(150, 200), pH = c(5, 9), Catalyst = c(2, 8))
        coded_T <- (base$Temp - 175) / 25
        coded_P <- (base$pH - 7) / 2
        coded_C <- (base$Catalyst - 5) / 3
        coded_S <- coded_T * coded_P * coded_C
        base$Speed <- ifelse(coded_S < 0, 100, 400)
        base$RunOrder <- sample(nrow(base))

        base$Yield <- round(40 + 5*coded_T + 3*coded_P - 2*coded_C +
                            1.5*coded_S + 2*coded_T*coded_P + rnorm(8, 0, 0.8), 2)

        rv$data <- stamp_row_ids(base)
        rv$roles <- list(
          Temp = "Factor", pH = "Factor", Catalyst = "Factor", Speed = "Factor",
          RunOrder = "Run Order", Yield = "Response"
        )
        rv$col_types <- list(
          Temp = "Numeric", pH = "Numeric", Catalyst = "Numeric", Speed = "Numeric",
          RunOrder = "Numeric", Yield = "Numeric"
        )
        rv$transforms <- list(
          Temp = "coding", pH = "coding", Catalyst = "coding", Speed = "coding",
          RunOrder = "none", Yield = "none"
        )
        rv$coding_values <- list(
          Temp = list(low = 150, high = 200),
          pH = list(low = 5, high = 9),
          Catalyst = list(low = 2, high = 8),
          Speed = list(low = 100, high = 400)
        )
        showNotification(
          paste0("2\u2074\u207b\u00b9 fractional factorial loaded: 8 runs, ",
                 "Temp / pH / Catalyst / Speed. ",
                 "Speed = T\u00d7P\u00d7C generator (Resolution IV). ",
                 "Auto-coded to \u00b11 in Data Roles."),
          type = "message")

      } else if (choice == "historical") {
        set.seed(314)
        n <- 40
        shift <- sample(c("Day","Night"), n, replace = TRUE, prob = c(0.6, 0.4))
        shift_offset <- ifelse(shift == "Night", 1, 0)

        temp_base <- rnorm(n, 180, 12)
        Temperature <- round(temp_base + 8 * shift_offset, 1)
        Pressure    <- round(2.5 + 0.035 * Temperature + rnorm(n, 0, 0.4), 2)

        FlowRate   <- round(runif(n, 50, 150), 1)
        Throughput <- round(0.85 * FlowRate + rnorm(n, 5, 3), 1)

        CatAge <- round(pmax(1, 20 + 15 * shift_offset + rnorm(n, 0, 8)), 0)

        true_yield <- 60 +
          0.3 * (Temperature - 180) -
          0.004 * (Temperature - 180)^2 +
          0.08 * (FlowRate - 100) -
          0.15 * (CatAge - 20) +
          2.5 * shift_offset +
          rnorm(n, 0, 1.8)

        df <- data.frame(
          Shift       = shift,
          Temperature = Temperature,
          Pressure    = Pressure,
          FlowRate    = FlowRate,
          Throughput  = Throughput,
          CatAge      = CatAge,
          Yield       = round(true_yield, 2),
          stringsAsFactors = FALSE
        )
        df$RunOrder <- seq_len(n)

        rv$data <- stamp_row_ids(df)
        rv$roles <- list(
          Shift       = "Factor",
          Temperature = "Factor",
          Pressure    = "Factor",
          FlowRate    = "Factor",
          Throughput  = "Factor",
          CatAge      = "Factor",
          RunOrder    = "Run Order",
          Yield       = "Response"
        )
        rv$col_types <- list(
          Shift       = "Factor",
          Temperature = "Numeric",
          Pressure    = "Numeric",
          FlowRate    = "Numeric",
          Throughput  = "Numeric",
          CatAge      = "Numeric",
          RunOrder    = "Numeric",
          Yield       = "Numeric"
        )
        showNotification(paste0(
          "Historical MLR loaded: 40 observations \u00b7 6 predictors. ",
          "Temperature\u2013Pressure and FlowRate\u2013Throughput are confounded. ",
          "Shift is a lurking variable. Compare full model vs. reduced model."),
          type = "message")

      } else if (choice == "ccd") {
        set.seed(456)
        fact <- expand.grid(x1 = c(-1, 1), x2 = c(-1, 1))
        axial <- data.frame(
          x1 = c(-1, 1, 0, 0),
          x2 = c(0, 0, -1, 1)
        )
        centre <- data.frame(x1 = rep(0, 3), x2 = rep(0, 3))
        df <- rbind(fact, axial, centre)
        df$RunOrder <- sample(nrow(df))

        df$Response <- round(80 + 4*df$x1 - 3*df$x2 + 2*df$x1*df$x2 -
                             5*df$x1^2 - 4*df$x2^2 + rnorm(nrow(df), 0, 0.6), 2)

        rv$data <- stamp_row_ids(df)
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

      } else if (choice == "unbalanced") {
        set.seed(2024)
        # 4-level treatment with deliberately unequal sample sizes
        # Control has most replication; TrtC has least — ideal for Dunnett testing
        n_ctrl <- 10; n_a <- 6; n_b <- 5; n_c <- 3
        n_tot  <- n_ctrl + n_a + n_b + n_c   # 24

        df <- data.frame(
          Treatment = c(rep("Control", n_ctrl), rep("TrtA", n_a),
                        rep("TrtB", n_b), rep("TrtC", n_c)),
          stringsAsFactors = FALSE
        )

        # True effects: Control=50, TrtA=+4, TrtB=+8, TrtC=+12
        trt_eff <- c(Control = 0, TrtA = 4, TrtB = 8, TrtC = 12)
        df$Response <- round(50 + trt_eff[df$Treatment] + rnorm(n_tot, 0, 2.5), 2)
        df$RunOrder <- sample(n_tot)

        rv$data <- stamp_row_ids(df)
        rv$roles <- list(
          Treatment = "Factor",
          RunOrder  = "Run Order",
          Response  = "Response"
        )
        rv$col_types <- list(
          Treatment = "Factor",
          RunOrder  = "Numeric",
          Response  = "Numeric"
        )
        showNotification(
          paste0("Unbalanced 1-way loaded: 24 runs \u00b7 ",
                 "4 treatments (n = 10, 6, 5, 3). ",
                 "Ideal for comparing Student / Dunnett / Tukey interval widths."),
          type = "message")
      }

      # ── Auto-populate default formulas for example datasets ────────────
      rv$skip_auto_formula <- TRUE
      facs <- names(rv$roles[rv$roles == "Factor"])
      blks <- names(rv$roles[rv$roles == "Block"])
      covs <- names(rv$roles[rv$roles == "Covariate"])
      resps <- names(rv$roles[rv$roles == "Response"])
      mode <- analysis_mode()
      r <- resps[1]

      default_formulas <- character(0)
      alias_full <- ""
      alias_check <- ""

      if (choice == "rcbd" && length(resps) > 0) {
        f1 <- paste0(r, " ~ ", paste(c(facs, "A:B", blks), collapse = " + "))
        default_formulas[f1] <- f1
        alias_full <- paste(c(facs, "A:B", blks), collapse = " + ")
        alias_check <- paste(c(facs, "A:B", blks, paste0(blks, ":", facs[1]),
                               paste0(blks, ":", facs[2])), collapse = " + ")

      } else if (choice == "latin" && length(resps) > 0) {
        f1 <- paste0(r, " ~ A + B + A:B + Row + Col")
        default_formulas[f1] <- f1
        alias_full <- "A + B + A:B + Row + Col"
        alias_check <- "A + B + A:B + Row + Col + Row:A + Row:B + Col:A + Col:B"

      } else if (choice == "mediator" && length(resps) > 0) {
        f1 <- paste0(r, " ~ Catalyst")
        default_formulas[f1] <- f1
        alias_full <- "Catalyst"
        alias_check <- "Catalyst"

      } else if (choice == "collinear" && length(resps) > 0) {
        f1 <- paste0(r, " ~ Fertilizer + Plot")
        default_formulas[f1] <- f1
        alias_full <- "Fertilizer + Plot"
        alias_check <- "Fertilizer + Plot + Plot:Fertilizer"

      } else if (choice == "historical" && length(resps) > 0) {
        cont_facs <- c("Temperature", "Pressure", "FlowRate", "Throughput", "CatAge")
        f_full <- paste0(r, " ~ Shift + ", paste(cont_facs, collapse = " + "))
        f_reduced <- paste0(r, " ~ Temperature + FlowRate + CatAge")
        default_formulas[f_full] <- f_full
        default_formulas[f_reduced] <- f_reduced
        alias_full <- paste(c("Shift", cont_facs), collapse = " + ")
        alias_check <- paste(c("Shift", cont_facs,
          "Shift:Temperature", "Shift:FlowRate", "Shift:CatAge",
          "Temperature:Pressure", "FlowRate:Throughput"), collapse = " + ")

      } else if (choice == "fracfact" && length(resps) > 0) {
        main_f <- paste0(r, " ~ ", paste(facs, collapse = " + "))
        default_formulas[main_f] <- main_f
        alias_full <- paste(facs, collapse = " + ")
        check_parts <- facs
        if (length(facs) >= 2) {
          combos2 <- combn(facs, 2, simplify = FALSE)
          check_parts <- c(check_parts, sapply(combos2, paste, collapse = ":"))
        }
        if (length(facs) >= 3) {
          combos3 <- combn(facs, 3, simplify = FALSE)
          check_parts <- c(check_parts, sapply(combos3, paste, collapse = ":"))
        }
        alias_check <- paste(check_parts, collapse = " + ")

      } else if (choice == "ccd" && length(resps) > 0) {
        quad_terms <- paste0("I(", facs, "^2)")
        fi2 <- if (length(facs) >= 2)
          sapply(combn(facs, 2, simplify = FALSE), paste, collapse = ":") else character(0)
        quad_f <- paste0(r, " ~ ", paste(c(facs, fi2, quad_terms), collapse = " + "))
        default_formulas[quad_f] <- quad_f
        alias_full <- paste(c(facs, fi2, quad_terms), collapse = " + ")
        alias_check <- paste(c(facs, fi2, quad_terms,
                               paste0("I(", facs, "^3)")), collapse = " + ")

      } else if (choice == "unbalanced" && length(resps) > 0) {
        f1 <- paste0(r, " ~ Treatment")
        default_formulas[f1] <- f1
        alias_full <- "Treatment"
        alias_check <- "Treatment"
      }

      if (length(default_formulas) > 0) {
        rv$formula_gen <- rv$formula_gen + 1L
        rv$formulas <- default_formulas
        rv$formula_aliases <- list()
        rv$alias_labels <- list()
        rv$inestimable_terms <- character()
        # Pre-populate custom formula input via models callback
        models_exports$set_custom_formula(unname(default_formulas[1]))
        # Auto-fit default models so results are immediately available
        fit_models(default_formulas)
      }
      if (nchar(alias_full) > 0)
        design_exports$set_alias_full_formula(alias_full)
      if (nchar(alias_check) > 0)
        design_exports$set_alias_check_formula(alias_check)

      navigate_to("Assign Roles")
    })
  })
}
