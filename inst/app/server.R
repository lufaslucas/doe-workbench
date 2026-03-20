# server.R — Module orchestrator
# All tab-level logic lives in R/mod_*.R modules.
# This file defines shared state (rv), colour theme, role selectors,
# session save/load, and wires modules together.

server <- function(input, output, session) {

  # ── Build Info ────────────────────────────────────────────────────────
  output$build_info_text <- renderText(get_build_info())

  # ── Shared State ───────────────────────────────────────────────────────
  defaults <- make_default_rv()
  rv <- do.call(reactiveValues, defaults)

  # ── Colour Theme Reactives ─────────────────────────────────────────────
  cat_palette <- reactive({
    theme <- input$cat_colour_theme %||% "shell"
    function(n = 8) get_cat_palette(theme, n)
  })

  cont_plotly_cs <- reactive({
    theme <- input$cont_colour_theme %||% "shell_warm"
    cont_plotly_colorscale(theme)
  })

  cat_scale_colour <- reactive({
    theme <- input$cat_colour_theme %||% "shell"
    function(...) scale_colour_manual(values = get_cat_palette(theme, 50), ...)
  })

  cat_scale_fill <- reactive({
    theme <- input$cat_colour_theme %||% "shell"
    function(...) scale_fill_manual(values = get_cat_palette(theme, 50), ...)
  })

  cont_scale_colour <- reactive({
    theme <- input$cont_colour_theme %||% "shell_warm"
    cols <- get_cont_colours(theme, 256)
    function(...) scale_colour_gradientn(colours = cols, ...)
  })

  cont_scale_fill <- reactive({
    theme <- input$cont_colour_theme %||% "shell_warm"
    cols <- get_cont_colours(theme, 256)
    function(...) scale_fill_gradientn(colours = cols, ...)
  })

  default_col <- reactive({ cat_palette()(1) })

  output$cat_colour_preview <- renderPlot({
    theme <- input$cat_colour_theme %||% "shell"
    cols <- get_cat_palette(theme, 12)
    par(mar = c(0.5, 0, 0, 0))
    image(matrix(seq_along(cols)), col = cols, axes = FALSE)
    text(seq(0, 1, length.out = length(cols)), rep(0.5, length(cols)),
         labels = seq_along(cols), cex = 0.8, col = "white", font = 2)
  })

  output$cont_colour_preview <- renderPlot({
    theme <- input$cont_colour_theme %||% "shell_warm"
    cols <- get_cont_colours(theme, 256)
    par(mar = c(0.5, 0, 0, 0))
    image(matrix(1:256), col = cols, axes = FALSE)
  })

  # ── Session Save/Load ──────────────────────────────────────────────────

  # Helper: build state list for saving
  build_save_state <- function(read_only = FALSE, hash = NULL) {
    list(
      data          = rv$data,
      roles         = rv$roles,
      col_types     = rv$col_types,
      transforms    = rv$transforms,
      coding_values = rv$coding_values,
      level_labels  = rv$level_labels,
      formulas      = rv$formulas,
      models        = rv$models,
      model_errors  = rv$model_errors,
      model_notes   = rv$model_notes,
      mc_results    = rv$mc_results,
      mc_on         = rv$mc_on,
      mc_alpha      = rv$mc_alpha,
      mc_terms      = rv$mc_terms,
      mc_methods    = rv$mc_methods,
      vif_df        = rv$vif_df,
      excluded_obs  = rv$excluded_obs,
      prune_notes   = rv$prune_notes,
      design_metadata      = rv$design_metadata,
      design_model_formula = rv$design_model_formula,
      design_alias_formula = rv$design_alias_formula,
      alias_threshold      = rv$alias_threshold,
      sim_data             = rv$sim_data,
      # Formula/alias state (previously lost on save)
      formula_aliases   = rv$formula_aliases,
      alias_labels      = rv$alias_labels,
      inestimable_terms = rv$inestimable_terms,
      # UI state
      report_items  = rv$report_items,
      settings      = list(
        cat_colour_theme  = input$cat_colour_theme,
        cont_colour_theme = input$cont_colour_theme
      ),
      read_only      = read_only,
      read_only_hash = hash
    )
  }

  # Save modal: choose editable (server-side) or read-only (browser download)
  observeEvent(input$save_btn, {
    default_path <- rv$session_path %||%
      file.path(path.expand("~"), paste0("DoE_Session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
    showModal(modalDialog(
      title = "Save Session",
      fluidRow(
        column(6,
          wellPanel(
            h5(icon("file-export"), "Save Editable"),
            p(class = "text-muted small", "Saves directly to the file path below."),
            textInput("save_path", "File path", value = default_path, width = "100%"),
            actionButton("save_editable_btn", "Save", class = "btn-primary",
                         icon = icon("download"))
          )
        ),
        column(6,
          wellPanel(
            h5(icon("lock"), "Save Read-Only"),
            p(class = "text-muted small",
              "Password-protected. Recipients can view but not modify."),
            textInput("save_final_path", "File path",
                      value = sub("\\.rds$", "_FINAL.rds", default_path), width = "100%"),
            passwordInput("final_password", "Set unlock password"),
            passwordInput("final_password_confirm", "Confirm password"),
            actionButton("save_readonly_btn", "Save Read-Only", class = "btn-danger",
                         icon = icon("lock"))
          )
        )
      ),
      footer = modalButton("Cancel"),
      size = "l",
      easyClose = TRUE
    ))
  })

  # Server-side save: editable session
  observeEvent(input$save_editable_btn, {
    req(input$save_path)
    path <- trimws(input$save_path)
    if (nchar(path) == 0) {
      showNotification("Please enter a file path.", type = "error")
      return()
    }
    # Ensure .rds extension
    if (!grepl("\\.rds$", path, ignore.case = TRUE)) path <- paste0(path, ".rds")
    tryCatch({
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(build_save_state(
        read_only = isTRUE(rv$read_only),
        hash      = rv$read_only_hash
      ), path)
      rv$session_path <- path
      removeModal()
      showNotification(paste("Session saved to", path), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message), type = "error")
    })
  })

  # Server-side save: read-only session
  observeEvent(input$save_readonly_btn, {
    req(input$save_final_path)
    path <- trimws(input$save_final_path)
    pw   <- input$final_password
    pw2  <- input$final_password_confirm
    if (nchar(path) == 0) {
      showNotification("Please enter a file path.", type = "error")
      return()
    }
    if (is.null(pw) || nchar(pw) == 0) {
      showNotification("Password cannot be empty.", type = "error")
      return()
    }
    if (!identical(pw, pw2)) {
      showNotification("Passwords do not match.", type = "error")
      return()
    }
    if (!grepl("\\.rds$", path, ignore.case = TRUE)) path <- paste0(path, ".rds")
    tryCatch({
      hash <- digest::digest(pw, algo = READONLY_HASH_ALGO)
      dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(build_save_state(read_only = TRUE, hash = hash), path)
      removeModal()
      showNotification(paste("Read-only session saved to", path), type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message), type = "error")
    })
  })

  # Load modal: enter file path
  observeEvent(input$load_btn, {
    default_path <- rv$session_path %||% file.path(path.expand("~"), "")
    showModal(modalDialog(
      title = "Load Session",
      textInput("load_path", "File path (.rds)", value = default_path, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("load_confirm_btn", "Load", class = "btn-primary", icon = icon("upload"))
      ),
      easyClose = TRUE
    ))
  })

  # Server-side load: read directly from disk
  observeEvent(input$load_confirm_btn, {
    req(input$load_path)
    path <- trimws(input$load_path)
    if (nchar(path) == 0 || !file.exists(path)) {
      showNotification("File not found. Check the path and try again.", type = "error")
      return()
    }
    state <- tryCatch(readRDS(path), error = function(e) {
      showNotification(paste("Error loading session:", e$message), type = "error")
      NULL
    })
    if (is.null(state)) return()

    rv$skip_auto_formula <- TRUE
    rv$data          <- stamp_row_ids(state$data)
    rv$roles         <- state$roles %||% list()
    rv$col_types     <- state$col_types %||% list()
    rv$transforms    <- state$transforms %||% list()
    rv$coding_values <- state$coding_values %||% list()
    rv$level_labels  <- state$level_labels %||% list()
    rv$formulas      <- state$formulas %||% character(0)
    rv$models        <- state$models %||% list()
    rv$model_errors  <- state$model_errors %||% list()
    rv$model_notes   <- state$model_notes %||% list()
    rv$mc_results    <- state$mc_results %||% list()
    rv$mc_on         <- state$mc_on %||% FALSE
    rv$mc_alpha      <- state$mc_alpha %||% ALPHA_DEFAULT
    rv$mc_terms      <- state$mc_terms %||% character(0)
    rv$mc_methods    <- state$mc_methods %||% character(0)
    rv$vif_df        <- state$vif_df %||% data.frame()
    rv$excluded_obs  <- state$excluded_obs %||% list()
    rv$prune_notes   <- state$prune_notes %||% list()
    rv$sim_data              <- state$sim_data
    rv$design_metadata       <- state$design_metadata %||% list()
    rv$design_model_formula  <- state$design_model_formula %||% ""
    rv$design_alias_formula  <- state$design_alias_formula %||% ""
    rv$alias_threshold       <- state$alias_threshold %||% ALIAS_CORR_THRESH
    # Formula/alias state (backward-compatible: defaults for old saves)
    rv$formula_aliases          <- state$formula_aliases %||% list()
    rv$alias_labels             <- state$alias_labels %||% list()
    rv$inestimable_terms        <- state$inestimable_terms %||% character()
    rv$pending_alias_resolution <- NULL
    # UI state
    rv$report_items  <- state$report_items %||% list()
    rv$selected_obs  <- NULL
    rv$formula_gen   <- rv$formula_gen + 1L
    rv$session_path  <- path

    # Detect read-only session
    if (isTRUE(state$read_only)) {
      rv$read_only      <- TRUE
      rv$read_only_hash <- state$read_only_hash
    } else {
      rv$read_only      <- FALSE
      rv$read_only_hash <- NULL
    }

    if (!is.null(state$settings)) {
      if (!is.null(state$settings$cat_colour_theme))
        updateSelectInput(session, "cat_colour_theme", selected = state$settings$cat_colour_theme)
      if (!is.null(state$settings$cont_colour_theme))
        updateSelectInput(session, "cont_colour_theme", selected = state$settings$cont_colour_theme)
    }

    # Sync module UIs to loaded rv state
    tryCatch(models_exports$sync_ui_from_rv(), error = function(e) NULL)
    tryCatch(design_exports$sync_ui_from_rv(), error = function(e) NULL)

    removeModal()
    msg <- if (isTRUE(state$read_only)) "Session restored (read-only mode)."
           else paste("Session loaded from", basename(path))
    showNotification(msg, type = "message", duration = 5)
  })

  # ── Read-Only Mode Coordinator ──────────────────────────────────────────
  observe({
    locked <- isTRUE(rv$read_only)

    # Toggle body CSS class + bulk-disable dynamic inputs via JS
    session$sendCustomMessage("toggle_readonly", list(locked = locked))

    # Top-level inputs
    toggle <- if (locked) shinyjs::disable else shinyjs::enable
    toggle("analysis_mode")
    toggle("load_btn")

    # Banner + save group visibility
    if (locked) {
      shinyjs::show("readonly_banner")
      shinyjs::hide("save_group")
    } else {
      shinyjs::hide("readonly_banner")
      shinyjs::show("save_group")
    }
  })

  # ── Unlock Flow ─────────────────────────────────────────────────────────
  observeEvent(input$unlock_btn, {
    showModal(modalDialog(
      title = "Unlock Session",
      passwordInput("unlock_password", "Enter unlock password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("unlock_confirm", "Unlock", class = "btn-primary",
                     icon = icon("unlock"))
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$unlock_confirm, {
    req(input$unlock_password)
    entered_hash <- digest::digest(input$unlock_password, algo = READONLY_HASH_ALGO)
    if (identical(entered_hash, rv$read_only_hash)) {
      rv$read_only      <- FALSE
      rv$read_only_hash <- NULL
      removeModal()
      showNotification("Session unlocked.", type = "message", duration = 3)
    } else {
      showNotification("Incorrect password.", type = "error", duration = 3)
    }
  })

  # ── Full App Reset ────────────────────────────────────────────────────
  # Note: references models_exports/design_exports which are defined later,
  # but this function is only called at runtime from observeEvent handlers.
  reset_app_to_defaults <- function() {
    defs <- make_default_rv()
    for (nm in names(defs)) {
      rv[[nm]] <- defs[[nm]]
    }
    # Bump formula_gen to trigger downstream reactives
    rv$formula_gen <- 1L
    # Reset top-level UI controls
    updateRadioButtons(session, "analysis_mode", selected = "comparative")
    updateSelectInput(session, "cat_colour_theme", selected = "shell")
    updateSelectInput(session, "cont_colour_theme", selected = "shell_warm")
    updateNavbarPage(session, "main_nav", selected = "Data")
    # Reset module-local UI inputs
    tryCatch(models_exports$reset_ui(), error = function(e) NULL)
    tryCatch(design_exports$reset_ui(), error = function(e) NULL)
  }

  observeEvent(input$reset_btn, {
    showModal(modalDialog(
      title = "Reset Session",
      p("This will clear all data, roles, models, and results."),
      p(tags$b("This action cannot be undone.")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("reset_confirm_btn", "Reset Everything",
                     class = "btn-danger", icon = icon("rotate-left"))
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$reset_confirm_btn, {
    reset_app_to_defaults()
    removeModal()
    showNotification("Session reset to defaults.", type = "message", duration = 3)
  })

  # ── Role-Based Reactive Helpers ────────────────────────────────────────
  reset_downstream <- function() {
    defs <- make_default_rv()
    clear_formula_state(rv)
    clear_model_state(rv)
    # Design layer
    rv$design_metadata       <- defs$design_metadata
    rv$design_model_formula  <- defs$design_model_formula
    rv$design_alias_formula  <- defs$design_alias_formula
    rv$alias_threshold       <- defs$alias_threshold
    rv$sim_data              <- defs$sim_data
    # UI layer
    rv$selected_obs    <- defs$selected_obs
  }

  responses      <- reactive({ names(Filter(function(r) r == "Response",  rv$roles)) })
  factors_       <- reactive({ names(Filter(function(r) r == "Factor",    rv$roles)) })
  covariates     <- reactive({ names(Filter(function(r) r == "Covariate", rv$roles)) })
  blocks         <- reactive({ names(Filter(function(r) r == "Block",     rv$roles)) })
  run_orders     <- reactive({ names(Filter(function(r) r == "Run Order", rv$roles)) })
  weights_col    <- reactive({ names(Filter(function(r) r == "Weight",    rv$roles)) })
  all_covariates <- reactive({ unique(c(covariates(), run_orders())) })

  treatment <- reactive({
    facs <- factors_()
    if (length(facs) == 0 || is.null(rv$data)) return(NULL)
    facs_in <- intersect(facs, names(rv$data))
    if (length(facs_in) == 0) return(NULL)
    as.factor(apply(rv$data[, facs_in, drop = FALSE], 1,
                    function(r) paste(r, collapse = ":")))
  })

  treatment_label <- reactive({
    facs_in <- intersect(factors_(), names(rv$data %||% data.frame()))
    if (length(facs_in) > 0) paste(facs_in, collapse = ":") else "Treatment"
  })

  # ── Shared Parameter Lists ─────────────────────────────────────────────
  role_selectors <- list(
    responses      = responses,
    factors_       = factors_,
    covariates     = covariates,
    blocks         = blocks,
    run_orders     = run_orders,
    all_covariates = all_covariates,
    weights_col    = weights_col
  )

  shared_reactives <- list(
    treatment       = treatment,
    treatment_label = treatment_label,
    analysis_mode   = reactive(input$analysis_mode %||% "comparative")
  )

  colour_theme <- list(
    cat_palette       = cat_palette,
    default_col       = default_col,
    cat_scale_colour  = cat_scale_colour,
    cat_scale_fill    = cat_scale_fill,
    cont_scale_colour = cont_scale_colour,
    cont_scale_fill   = cont_scale_fill,
    cont_plotly_cs    = cont_plotly_cs
  )

  # ── Available Terms (shared by Design + Models formula choosers) ───────
  available_terms <- reactive({
    facs <- factors_()
    blks <- blocks()
    covs <- all_covariates()
    mode <- input$analysis_mode %||% "comparative"
    terms <- list()
    terms$factors <- facs
    terms$blocks <- blks
    terms$covariates <- covs
    if (length(facs) >= 2)
      terms$fac_2fi <- sapply(combn(facs, 2, simplify = FALSE), paste, collapse = ":")
    if (length(facs) >= 3)
      terms$fac_3fi <- sapply(combn(facs, 3, simplify = FALSE), paste, collapse = ":")
    if (mode == "regression" && length(facs) > 0) {
      terms$quadratic <- paste0("I(", facs, "^2)")
      terms$cubic     <- paste0("I(", facs, "^3)")
    }
    if (length(blks) > 0 && length(facs) > 0)
      terms$blk_fac <- as.vector(outer(blks, facs, function(b, f) paste0(b, ":", f)))
    if (length(covs) > 0 && length(facs) > 0)
      terms$cov_fac <- as.vector(outer(covs, facs, function(c, f) paste0(c, ":", f)))
    terms
  })

  # ── Wire Modules ───────────────────────────────────────────────────────
  # Order matters: Models returns do_fit_models, Results returns active_models

  # Data modules
  mod_assign_roles_server("assign_roles", rv = rv,
                          analysis_mode = reactive(input$analysis_mode),
                          reset_downstream = reset_downstream)

  # Shared navigation callback
  navigate_to <- function(tab) {
    updateNavbarPage(session, "main_nav", selected = tab)
  }
  set_analysis_mode <- function(mode) {
    updateRadioButtons(session, "analysis_mode", selected = mode)
  }
  observe_main_nav <- reactive({ input$main_nav })

  # Models tab (returns exports: do_fit_models, set/get_custom_formula, get_active_response)
  models_exports <- mod_models_server("models", rv = rv,
                                      role_selectors = role_selectors,
                                      shared_reactives = shared_reactives,
                                      analysis_mode = reactive(input$analysis_mode),
                                      reset_downstream = reset_downstream,
                                      colour_theme = colour_theme,
                                      available_terms = available_terms)

  # Design tab (returns exports: set_alias_full_formula, set_alias_check_formula)
  design_exports <- mod_design_server("design", rv = rv,
                                      colour_theme = colour_theme,
                                      role_selectors = role_selectors,
                                      shared_reactives = shared_reactives,
                                      analysis_mode = reactive(input$analysis_mode),
                                      models_exports = models_exports,
                                      navigate_to = navigate_to,
                                      observe_main_nav = observe_main_nav,
                                      available_terms = available_terms)

  # Data Upload (needs models + design exports for cross-module updates)
  mod_data_upload_server("data_upload", rv = rv,
                         analysis_mode = reactive(input$analysis_mode),
                         navigate_to = navigate_to,
                         set_analysis_mode = set_analysis_mode,
                         reset_downstream = reset_downstream,
                         fit_models = function(formulas) models_exports$do_fit_models(formulas),
                         models_exports = models_exports,
                         design_exports = design_exports)

  mod_data_generate_server("data_generate", rv = rv,
                           analysis_mode = reactive(input$analysis_mode),
                           navigate_to = navigate_to,
                           set_analysis_mode = set_analysis_mode,
                           reset_downstream = reset_downstream,
                           colour_theme = colour_theme,
                           design_exports = design_exports)

  # Explore tab
  mod_explore_server("explore", rv = rv,
                     colour_theme = colour_theme,
                     role_selectors = role_selectors,
                     shared_reactives = shared_reactives)

  # Results tab (returns active_models)
  results_exports <- mod_results_server("results", rv = rv,
                                        colour_theme = colour_theme,
                                        role_selectors = role_selectors,
                                        shared_reactives = shared_reactives,
                                        mc_state = list(
                                          mc_on      = reactive(rv$mc_on %||% FALSE),
                                          mc_alpha   = reactive(rv$mc_alpha %||% 0.05),
                                          mc_terms   = reactive(rv$mc_terms),
                                          mc_methods = reactive(rv$mc_methods)
                                        ))

  # Report tab (needs active_models from Results)
  mod_report_server("report", rv = rv,
                    role_selectors = role_selectors,
                    active_models = results_exports$active_models,
                    mc_state = list(
                      mc_on      = reactive(rv$mc_on %||% FALSE),
                      mc_terms   = reactive(rv$mc_terms),
                      mc_methods = reactive(rv$mc_methods)
                    ))

  # ── Sync Models formulas → Design alias formula ────────────────────
  observeEvent(rv$formulas, {
    if (length(rv$formulas) == 0) return()
    # Use the largest formula (most terms) as the Design model formula
    n_terms <- sapply(rv$formulas, function(f) {
      rhs <- sub("^[^~]+~\\s*", "", f)
      length(trimws(strsplit(rhs, "\\+")[[1]]))
    })
    largest <- rv$formulas[[which.max(n_terms)]]
    rhs <- trimws(sub("^[^~]+~\\s*", "", largest))
    design_exports$set_alias_full_formula(rhs)
  }, ignoreInit = TRUE)

  # ── Linked Selection: global observers ───────────────────────────────
  # event_data() must be called inside observe() to properly set up its
  # internal plotlyInputStore reactive. observeEvent(event_data(...)) does
  # not establish the dependency chain reliably.

  # Lasso / box select → set selection
  observe({
    ed <- tryCatch(event_data("plotly_selected", source = SEL_SOURCE),
                    warning = function(w) NULL)
    if (is.null(ed) || nrow(ed) == 0) return()
    new_ids <- as.integer(ed$key)
    new_ids <- new_ids[!is.na(new_ids)]
    if (length(new_ids) > 0) isolate(rv$selected_obs <- unique(new_ids))
  })

  # Single click → toggle point in/out of selection
  observe({
    ed <- tryCatch(event_data("plotly_click", source = SEL_SOURCE),
                    warning = function(w) NULL)
    if (is.null(ed) || nrow(ed) == 0) return()
    clicked_id <- as.integer(ed$key[1])
    if (is.na(clicked_id)) return()
    isolate({
      current <- rv$selected_obs
      if (is.null(current)) {
        rv$selected_obs <- clicked_id
      } else if (clicked_id %in% current) {
        remaining <- setdiff(current, clicked_id)
        rv$selected_obs <- if (length(remaining) == 0) NULL else remaining
      } else {
        rv$selected_obs <- c(current, clicked_id)
      }
    })
  })

  # Double-click / deselect → clear selection
  observe({
    ed <- event_data("plotly_deselect", source = SEL_SOURCE)
    if (!is.null(ed)) isolate(rv$selected_obs <- NULL)
  })

  # Clear button
  observeEvent(input$clear_selection, { rv$selected_obs <- NULL })

  # Clear on data change
  observeEvent(rv$data, { rv$selected_obs <- NULL }, priority = -10)

  # Update selection indicator bar via JS custom message
  observe({
    n <- length(rv$selected_obs)
    session$sendCustomMessage("update_selection_bar", list(
      bar_id   = "global_selection_bar",
      count_id = "global_selection_count",
      n        = n
    ))
  })
}
