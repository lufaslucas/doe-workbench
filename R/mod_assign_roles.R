# R/mod_assign_roles.R — Assign Roles tab module (UI + server)

# ── UI ───────────────────────────────────────────────────────────────────────
mod_assign_roles_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
        wellPanel(
          h6("Batch role assignment"),
          fluidRow(
            column(3,
              selectInput(ns("batch_role"), "Set all columns to:",
                          choices = c("(choose)" = "", "Response" = "Response",
                                      "Factor" = "Factor", "Covariate" = "Covariate",
                                      "Block" = "Block", "Ignore" = "Ignore"),
                          selected = ""),
              actionButton(ns("apply_batch_role"), "Apply to all",
                           class = "btn-sm btn-outline-secondary", icon = icon("layer-group"))
            ),
            column(3,
              selectInput(ns("batch_type_role"), "Set all to Ignore by type:",
                          choices = c("(choose)" = "",
                                      "All factors \u2192 Ignore" = "factor_ignore",
                                      "All numeric \u2192 Ignore" = "numeric_ignore"),
                          selected = ""),
              actionButton(ns("apply_batch_type"), "Apply",
                           class = "btn-sm btn-outline-secondary", icon = icon("eraser"))
            ),
            column(6,
              p(class = "text-muted small mt-2",
                "Use these to reset roles before building up from scratch. ",
                "Individual column roles can still be changed below.")
            )
          )
        ),
        wellPanel(
          h6("Bulk transformations"),
          fluidRow(
            column(3,
              selectInput(ns("bulk_factor_transform"), "All numeric factors",
                          choices = c("None" = "none", "Centre" = "centre",
                                      "Centre & Scale (range/2)" = "centre_scale",
                                      "Coding (\u22121/+1 to min/max)" = "coding",
                                      "Coding (\u22121/+1 custom range)" = "coding_fixed"),
                          selected = "none"),
              actionButton(ns("apply_bulk_factor"), "Apply to all factors",
                           class = "btn-sm btn-outline-primary",
                           icon = icon("sync"))
            ),
            column(3,
              selectInput(ns("bulk_cov_transform"), "All numeric covariates",
                          choices = c("None" = "none", "Centre" = "centre",
                                      "Centre & Scale (range/2)" = "centre_scale",
                                      "Coding (\u22121/+1 to min/max)" = "coding",
                                      "Coding (\u22121/+1 custom range)" = "coding_fixed"),
                          selected = "none"),
              actionButton(ns("apply_bulk_cov"), "Apply to all covariates",
                           class = "btn-sm btn-outline-primary",
                           icon = icon("sync"))
            ),
            column(6,
              p(class = "text-muted small mt-2",
                tags$b("Coding (min/max):"), " maps data min \u2192 \u22121, max \u2192 +1. ",
                tags$br(),
                tags$b("Coding (custom):"), " same as min/max, then edit per-column \u22121/+1 levels below. ",
                tags$br(),
                tags$b("Centre:"), " subtracts column mean. ",
                tags$br(),
                tags$b("Centre & Scale:"), " (x \u2212 mean) / (range/2). Coefficients are per half-range unit.")
            )
          )
        ),
        uiOutput(ns("role_ui"))
      )
    )
  )
}

# ── Server ───────────────────────────────────────────────────────────────────
mod_assign_roles_server <- function(id, rv, analysis_mode) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Dynamic role/type/transform cards ──────────────────────────────────
    output$role_ui <- renderUI({
      req(rv$data)
      cols       <- setdiff(names(rv$data), ROW_ID_COL)
      role_ch    <- c("Response", "Factor", "Covariate", "Block", "Run Order", "Weight", "Ignore")
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

            # Namespaced IDs for conditionalPanel JS conditions
            type_id      <- ns(paste0("type_", cn))
            role_id      <- ns(paste0("role_", cn))
            transform_id <- ns(paste0("transform_", cn))

            column(4,
              tags$div(
                style = "border:1px solid #dee2e6; border-radius:4px; padding:8px; margin-bottom:8px;",
                tags$strong(cn),
                selectInput(ns(paste0("role_", cn)), "Role",
                            choices = role_ch, selected = cur_role),
                selectInput(ns(paste0("type_", cn)), "Type",
                            choices = type_ch, selected = cur_type),
                conditionalPanel(
                  condition = paste0(
                    "input['", type_id, "'] == 'Numeric' && ",
                    "input['", role_id, "'] != 'Response' && ",
                    "input['", role_id, "'] != 'Ignore' && ",
                    "input['", role_id, "'] != 'Weight'"),
                  selectInput(ns(paste0("transform_", cn)), "Transform",
                              choices = c("None" = "none", "Centre" = "centre",
                                          "Centre & Scale" = "centre_scale",
                                          "Coding" = "coding"),
                              selected = cur_transform),
                  conditionalPanel(
                    condition = paste0("input['", transform_id, "'] == 'coding'"),
                    fluidRow(
                      column(6,
                        numericInput(ns(paste0("code_low_", cn)), "\u22121 level",
                                     value = cur_code_low)
                      ),
                      column(6,
                        numericInput(ns(paste0("code_high_", cn)), "+1 level",
                                     value = cur_code_high)
                      )
                    )
                  )
                ),
                # Level labels for Factor-type columns
                conditionalPanel(
                  condition = paste0(
                    "(input['", type_id, "'] == 'Factor' || ",
                    "input['", role_id, "'] == 'Block') && ",
                    "input['", role_id, "'] != 'Response' && ",
                    "input['", role_id, "'] != 'Ignore' && ",
                    "input['", role_id, "'] != 'Weight'"),
                  {
                    ulevs <- as.character(sort(unique(rv$data[[cn]])))
                    if (length(ulevs) > 0 && length(ulevs) <= 20) {
                      cur_labels <- rv$level_labels[[cn]] %||% stats::setNames(ulevs, ulevs)
                      tagList(
                        tags$details(
                          tags$summary(
                            style = "cursor:pointer; font-size:0.85em; color:#6c757d;",
                            paste0("Level labels (", length(ulevs), ")")
                          ),
                          lapply(ulevs, function(lv) {
                            cur_lbl <- cur_labels[[lv]] %||% lv
                            textInput(ns(paste0("llbl_", cn, "___", lv)),
                                      label = lv, value = cur_lbl,
                                      width = "100%")
                          })
                        )
                      )
                    }
                  }
                )
              )
            )
          })
        )
      })

      tagList(rows)
    })

    # ── Read-only guard for bulk buttons ──────────────────────────────────
    observe({
      locked <- isTRUE(rv$read_only)
      toggle <- if (locked) shinyjs::disable else shinyjs::enable
      toggle(ns("apply_batch_role"))
      toggle(ns("apply_batch_type"))
      toggle(ns("apply_bulk_factor"))
      toggle(ns("apply_bulk_cov"))
    })

    # ── Batch role assignment: set all columns to one role ───────────────
    observeEvent(input$apply_batch_role, {
      if (is_locked(rv, "Batch role assignment")) return()
      req(rv$data)
      new_role <- input$batch_role
      if (is.null(new_role) || new_role == "") {
        showNotification("Select a role first.", type = "warning", duration = 3)
        return()
      }
      cols <- setdiff(names(rv$data), ROW_ID_COL)
      n <- 0
      for (cn in cols) {
        rv$roles[[cn]] <- new_role
        updateSelectInput(session, paste0("role_", cn), selected = new_role)
        n <- n + 1
      }
      apply_role_change(rv)
      showNotification(
        paste0("Set all ", n, " columns to '", new_role, "'."),
        type = "message", duration = 3)
    })

    # ── Batch ignore by detected type ────────────────────────────────────
    observeEvent(input$apply_batch_type, {
      if (is_locked(rv, "Batch role assignment")) return()
      req(rv$data)
      action <- input$batch_type_role
      if (is.null(action) || action == "") {
        showNotification("Select a type filter first.", type = "warning", duration = 3)
        return()
      }
      cols <- setdiff(names(rv$data), ROW_ID_COL)
      n <- 0
      for (cn in cols) {
        is_num <- is.numeric(rv$data[[cn]])
        match <- (action == "numeric_ignore" && is_num) ||
                 (action == "factor_ignore" && !is_num)
        if (match) {
          rv$roles[[cn]] <- "Ignore"
          updateSelectInput(session, paste0("role_", cn), selected = "Ignore")
          n <- n + 1
        }
      }
      apply_role_change(rv)
      type_label <- if (action == "factor_ignore") "factor/character" else "numeric"
      showNotification(
        paste0("Set ", n, " ", type_label, " column(s) to 'Ignore'."),
        type = "message", duration = 3)
    })

    # ── Sync role/type/transform inputs -> rv ─────────────────────────────
    observe({
      req(rv$data)
      cols <- setdiff(names(rv$data), ROW_ID_COL)
      for (col in cols) {
        local({
          cn <- col
          observeEvent(input[[paste0("role_", cn)]], {
            if (isTRUE(rv$read_only)) return()
            rv$roles[[cn]] <- input[[paste0("role_", cn)]]
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("type_", cn)]], {
            if (isTRUE(rv$read_only)) return()
            rv$col_types[[cn]] <- input[[paste0("type_", cn)]]
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("transform_", cn)]], {
            if (isTRUE(rv$read_only)) return()
            rv$transforms[[cn]] <- input[[paste0("transform_", cn)]]
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("code_low_", cn)]], {
            if (isTRUE(rv$read_only)) return()
            if (is.null(rv$coding_values[[cn]])) rv$coding_values[[cn]] <- list()
            rv$coding_values[[cn]]$low <- input[[paste0("code_low_", cn)]]
          }, ignoreInit = TRUE)
          observeEvent(input[[paste0("code_high_", cn)]], {
            if (isTRUE(rv$read_only)) return()
            if (is.null(rv$coding_values[[cn]])) rv$coding_values[[cn]] <- list()
            rv$coding_values[[cn]]$high <- input[[paste0("code_high_", cn)]]
          }, ignoreInit = TRUE)
          # Level label observers
          ulevs <- as.character(sort(unique(rv$data[[cn]])))
          for (lv in ulevs) {
            local({
              col_name <- cn
              level_val <- lv
              input_id <- paste0("llbl_", col_name, "___", level_val)
              observeEvent(input[[input_id]], {
                if (isTRUE(rv$read_only)) return()
                val <- input[[input_id]]
                if (is.null(val) || val == "") val <- level_val
                if (is.null(rv$level_labels[[col_name]])) {
                  all_levs <- as.character(sort(unique(rv$data[[col_name]])))
                  rv$level_labels[[col_name]] <- stats::setNames(all_levs, all_levs)
                }
                rv$level_labels[[col_name]][[level_val]] <- val
              }, ignoreInit = TRUE)
            })
          }
        })
      }
    })

    # ── Analysis mode change -> update factor types/transforms ────────────
    observeEvent(analysis_mode(), {
      req(rv$data, length(rv$roles) > 0)
      mode <- analysis_mode()
      new_type <- if (mode == "regression") "Numeric" else "Factor"
      for (cn in names(rv$roles)) {
        if (rv$roles[[cn]] == "Factor") {
          rv$col_types[[cn]] <- new_type
          updateSelectInput(session, paste0("type_", cn), selected = new_type)
          new_tr <- if (mode == "regression") "coding" else "none"
          rv$transforms[[cn]] <- new_tr
          updateSelectInput(session, paste0("transform_", cn), selected = new_tr)
        }
      }
      apply_role_change(rv)
    }, ignoreInit = TRUE)

    # ── Bulk transformation: factors ─────────────────────────────────────
    observeEvent(input$apply_bulk_factor, {
      if (is_locked(rv, "Bulk transform")) return()
      req(rv$data, length(rv$roles) > 0)
      tr <- input$bulk_factor_transform %||% "none"
      actual_tr <- if (tr == "coding_fixed") "coding" else tr
      for (cn in names(rv$roles)) {
        if (rv$roles[[cn]] != "Factor") next
        if ((rv$col_types[[cn]] %||% "Factor") != "Numeric") next
        rv$transforms[[cn]] <- actual_tr
        updateSelectInput(session, paste0("transform_", cn), selected = actual_tr)
        if (tr %in% c("coding", "coding_fixed")) {
          col_vals <- suppressWarnings(as.numeric(as.character(rv$data[[cn]])))
          col_vals <- col_vals[!is.na(col_vals)]
          if (length(col_vals) > 0) {
            rv$coding_values[[cn]] <- list(low = min(col_vals), high = max(col_vals))
            updateNumericInput(session, paste0("code_low_", cn), value = min(col_vals))
            updateNumericInput(session, paste0("code_high_", cn), value = max(col_vals))
          }
        }
      }
      apply_role_change(rv)
      label <- switch(tr, "coding_fixed" = "coding (\u22121/+1 custom range)", tr)
      showNotification(
        paste0("Applied '", label, "' transform to all numeric factors.",
               if (tr == "coding_fixed") " Edit per-column \u22121/+1 levels below to customise." else ""),
        type = "message")
    })

    # ── Bulk transformation: covariates ──────────────────────────────────
    observeEvent(input$apply_bulk_cov, {
      if (is_locked(rv, "Bulk transform")) return()
      req(rv$data, length(rv$roles) > 0)
      tr <- input$bulk_cov_transform %||% "none"
      actual_tr <- if (tr == "coding_fixed") "coding" else tr
      for (cn in names(rv$roles)) {
        if (!rv$roles[[cn]] %in% c("Covariate", "Run Order")) next
        if (!is.numeric(rv$data[[cn]])) next
        rv$transforms[[cn]] <- actual_tr
        updateSelectInput(session, paste0("transform_", cn), selected = actual_tr)
        if (tr %in% c("coding", "coding_fixed")) {
          col_vals <- rv$data[[cn]]
          col_vals <- col_vals[!is.na(col_vals)]
          if (length(col_vals) > 0) {
            rv$coding_values[[cn]] <- list(low = min(col_vals), high = max(col_vals))
            updateNumericInput(session, paste0("code_low_", cn), value = min(col_vals))
            updateNumericInput(session, paste0("code_high_", cn), value = max(col_vals))
          }
        }
      }
      apply_role_change(rv)
      label <- switch(tr, "coding_fixed" = "coding (\u22121/+1 custom range)", tr)
      showNotification(
        paste0("Applied '", label, "' transform to all numeric covariates.",
               if (tr == "coding_fixed") " Edit per-column \u22121/+1 levels below to customise." else ""),
        type = "message")
    })
  })
}
