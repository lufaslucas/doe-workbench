# R/mod_explore.R — Explore tab module (UI + server)

# ── Needle plot builder (testable, pure function) ────────────────────────────
#' Build a needle plot ggplot object
#'
#' @param df Data frame with observations
#' @param resp_col Response column name
#' @param fac_order Character vector of factor columns for treatment ID
#' @param sort_mode "factors" or "response"
#' @param colour_vec Optional colour vector (same length as nrow(df)), or NULL
#' @param dcol Default point colour (hex string)
#' @param colour_label Label for colour legend, or NULL
#' @param facet_var Facet variable name, or NULL
#' @param facet_mode "wrap" or "grid"
#' @return A ggplot object (not yet converted to plotly)
build_needle_plot <- function(df, resp_col, fac_order,
                              sort_mode = "factors",
                              colour_vec = NULL,
                              dcol = "#404040",
                              colour_label = NULL,
                              facet_var = NULL,
                              facet_mode = "wrap",
                              colour_scale = NULL) {
  df$.trt_id <- apply(df[, fac_order, drop = FALSE], 1,
                       function(r) paste(r, collapse = ":"))

  grand_mean <- mean(df[[resp_col]], na.rm = TRUE)

  if (sort_mode == "response") {
    trt_order <- aggregate(df[[resp_col]],
                            by = list(.trt_id = df$.trt_id), FUN = mean, na.rm = TRUE)
    df$.trt_id <- factor(df$.trt_id,
                          levels = trt_order$.trt_id[order(trt_order$x)])
  } else {
    df$.trt_id <- factor(df$.trt_id)
  }

  has_colour <- !is.null(colour_vec)
  has_facet  <- !is.null(facet_var) && facet_var != "none" && facet_var %in% names(df)

  # Compute group mean per treatment (and per facet panel if faceted)
  # ave() naturally respects facet grouping
  if (has_facet) {
    group_key <- paste(df$.trt_id, df[[facet_var]], sep = "||")
  } else {
    group_key <- as.character(df$.trt_id)
  }
  df$.group_mean <- ave(df[[resp_col]], group_key, FUN = function(x) mean(x, na.rm = TRUE))

  if (has_colour) {
    df$.colour <- colour_vec
    is_cont <- is.numeric(colour_vec)
    if (!is_cont) df$.colour <- as.factor(df$.colour)
  }

  p <- ggplot(df, aes(x = .trt_id, y = .data[[resp_col]])) +
    # Grand mean reference line
    geom_hline(yintercept = grand_mean, colour = "grey40", linewidth = 0.8) +
    # Group-mean needles (grand mean -> group mean)
    geom_segment(aes(xend = .trt_id, y = grand_mean, yend = .group_mean),
                 colour = dcol, linewidth = 1.5) +
    # Group mean diamonds
    stat_summary(fun = mean, geom = "point",
                 shape = 18, size = 4, colour = dcol) +
    # Individual needles (point -> group mean)
    geom_segment(aes(xend = .trt_id, yend = .group_mean),
                 colour = "grey70", linewidth = 0.5)

  # Individual points
  if (has_colour) {
    p <- p + geom_point(aes(colour = .colour), size = 2.5, alpha = 0.8)
    if (!is.null(colour_scale)) {
      p <- p + colour_scale
    }
  } else {
    p <- p + geom_point(colour = dcol, size = 2.5, alpha = 0.8)
  }

  if (has_facet) {
    p <- apply_facets(p, facet_var = facet_var, mode = facet_mode, scales = "free_x")
  }

  p <- p +
    annotate("text", x = levels(df$.trt_id)[1], y = grand_mean,
             label = paste("mean =", round(grand_mean, 2)),
             vjust = -0.5, hjust = 0, colour = "grey40", size = 3) +
    labs(title = paste("Needle plot \u2014", resp_col),
         x = paste(fac_order, collapse = " : "),
         y = resp_col,
         colour = colour_label) +
    theme_app() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p
}

# ── UI ───────────────────────────────────────────────────────────────────────
mod_explore_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      fluidRow(
        column(2, selectInput(ns("explore_response"), "Response variable", choices = NULL)),
        column(2,
          selectInput(ns("explore_colour_by"), "Colour by",
                      choices = c("None" = "none"), selected = "none")
        ),
        column(2,
          selectInput(ns("explore_facet_by"), "Facet by",
                      choices = c("None" = "none"), selected = "none"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Needle Plot' || input['%s'] == 'Parallel Plot'",
                                ns("explore_tabs"), ns("explore_tabs")),
            tags$small(class = "text-muted", icon("info-circle"), " Uses own facet control below")
          )
        ),
        column(2,
          radioButtons(ns("explore_facet_mode"), "Facet mode",
                       choices = c("Wrap" = "wrap", "Row" = "row", "Col" = "col"),
                       selected = "wrap", inline = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Needle Plot' || input['%s'] == 'Parallel Plot'",
                                ns("explore_tabs"), ns("explore_tabs")),
            tags$small(class = "text-muted", icon("info-circle"), " Not linked to current tab")
          )
        ),
        column(2,
          radioButtons(ns("explore_smooth"), "Fit / connect",
                       choices = c("Linear" = "lm", "Smoother" = "loess",
                                   "Connected" = "connected"),
                       selected = "lm", inline = TRUE),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'Covariate Plots'", ns("explore_tabs")),
            tags$small(class = "text-muted", icon("info-circle"), " Only applies to Covariate Plots")
          )
        ),
        column(2,
          sliderInput(ns("explore_line_width"), "Line width",
                      min = 0.5, max = 5, value = 1, step = 0.5),
          conditionalPanel(
            condition = sprintf(
              "input['%s'] != 'Covariate Plots' && input['%s'] != 'Parallel Plot'",
              ns("explore_tabs"), ns("explore_tabs")),
            tags$small(class = "text-muted", icon("info-circle"), " Applies to Covariate & Parallel Plots")
          )
        )
      ),
      fluidRow(
        column(3,
          sliderInput(ns("explore_jitter"), "Jitter amount",
                      min = 0, max = 0.4, value = 0.05, step = 0.01),
          conditionalPanel(
            condition = sprintf("input['%s'] != 'By Factor' && input['%s'] != 'By Block'",
                                ns("explore_tabs"), ns("explore_tabs")),
            tags$small(class = "text-muted", icon("info-circle"), " Only applies to By Factor / By Block")
          )
        ),
        column(3, div(style = "margin-top: 28px;",
                      checkboxInput(ns("explore_jitter_replicated"),
                                    "Only jitter replicated points", value = TRUE)))
      )
    ),
    fluidRow(
      column(12,
        checkboxInput(ns("explore_grid_mode"), "Grid view (all plots)", value = FALSE),
        tags$style(HTML(paste0(
          "#", ns("explore_tabs_grid"), ".grid-mode > .nav { display: none !important; }",
          "#", ns("explore_tabs_grid"), ".grid-mode > .tab-content { display: grid !important; ",
          "  grid-template-columns: 1fr 1fr; gap: 16px; }",
          "#", ns("explore_tabs_grid"), ".grid-mode > .tab-content > .tab-pane { ",
          "  display: block !important; opacity: 1 !important; ",
          "  border: 1px solid #dee2e6; border-radius: 8px; padding: 12px; }"
        )))
      )
    ),
    div(id = ns("explore_tabs_grid"),
    tabsetPanel(id = ns("explore_tabs"),
        tabPanel("Distribution",
          br(),
          plotlyOutput(ns("dist_plot"), height = "450px")
        ),
        tabPanel("By Factor",
          br(),
          fluidRow(
            column(6, selectInput(ns("explore_factor"), "Factor", choices = NULL)),
            column(6, div(id = ns("boxplot_toggle_div"),
                         checkboxInput(ns("explore_show_boxplot"), "Show boxplots", value = TRUE)))
          ),
          plotlyOutput(ns("by_factor_plot"), height = "450px")
        ),
        tabPanel("By Block",
          br(),
          uiOutput(ns("by_block_ui"))
        ),
        tabPanel("Covariate Plots",
          br(),
          selectInput(ns("explore_covariate"), "Covariate", choices = NULL),
          plotlyOutput(ns("covariate_plot"), height = "500px")
        ),
        tabPanel("Needle Plot",
          br(),
          p(class = "text-muted",
            "Response vs treatment combinations. Needles show deviation from the ",
            "overall mean. Reorder factors or sort by response to identify patterns."),
          fluidRow(
            column(4, uiOutput(ns("needle_factor_order_ui"))),
            column(4, radioButtons(ns("needle_sort"), "Sort by",
                                   choices = c("Factor levels" = "factors",
                                               "Response value" = "response"),
                                   selected = "response", inline = TRUE))
          ),
          fluidRow(
            column(3, selectInput(ns("needle_facet"), "Facet by",
                                  choices = c("None" = "none"))),
            column(3, radioButtons(ns("needle_facet_mode"), "Facet mode",
                                   choices = c("Wrap" = "wrap", "Row" = "row", "Col" = "col"),
                                   selected = "wrap", inline = TRUE))
          ),
          plotlyOutput(ns("needle_plot"), height = "500px")
        ),
        tabPanel("Parallel Plot",
          br(),
          p(class = "text-muted",
            "Parallel coordinates plot: each vertical axis represents a variable. ",
            "Lines connect each observation\u2019s values across axes to reveal patterns. ",
            "Click and drag on any axis to filter/highlight lines through a value range."),
          uiOutput(ns("parallel_controls_ui")),
          plotlyOutput(ns("parallel_plot"), height = "500px")
        )
      )
    )  # close grid wrapper div
  )
}

# ── Server ───────────────────────────────────────────────────────────────────

mod_explore_server <- function(id, rv, colour_theme, role_selectors,
                               shared_reactives) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Convenience aliases
    responses      <- role_selectors$responses
    factors_       <- role_selectors$factors_
    covariates     <- role_selectors$covariates
    blocks         <- role_selectors$blocks
    all_covariates <- role_selectors$all_covariates
    treatment      <- shared_reactives$treatment
    treatment_label <- shared_reactives$treatment_label
    analysis_mode  <- shared_reactives$analysis_mode

    cat_palette       <- colour_theme$cat_palette
    default_col       <- colour_theme$default_col
    cat_scale_colour  <- colour_theme$cat_scale_colour
    cat_scale_fill    <- colour_theme$cat_scale_fill
    cont_scale_colour <- colour_theme$cont_scale_colour
    cont_plotly_cs    <- colour_theme$cont_plotly_cs

    # Grid mode toggle: add/remove CSS class on wrapper div
    observeEvent(input$explore_grid_mode, {
      grid_id <- ns("explore_tabs_grid")
      if (isTRUE(input$explore_grid_mode)) {
        shinyjs::addClass(id = "explore_tabs_grid", class = "grid-mode", asis = FALSE)
      } else {
        shinyjs::removeClass(id = "explore_tabs_grid", class = "grid-mode", asis = FALSE)
      }
    }, ignoreInit = TRUE)

    # ── Dropdown updates ───────────────────────────────────────────────────
    observe({
      updateSelectInput(session, "explore_response",  choices = responses())
      # In comparative mode, put treatment combination first in factor choices
      fac_choices <- factors_()
      if (analysis_mode() == "comparative" && length(fac_choices) > 0) {
        trt_lab <- treatment_label()
        fac_choices <- c(setNames(".treatment", trt_lab), fac_choices)
      }
      updateSelectInput(session, "explore_factor",    choices = fac_choices)
      updateSelectInput(session, "explore_covariate", choices = all_covariates())

      cols <- names(rv$data %||% data.frame())
      trt_lab <- treatment_label()
      mode <- analysis_mode()
      colour_ch <- build_colour_choices(
        all_cols = cols, treatment_label = trt_lab,
        include_treatment = (mode == "comparative"))
      updateSelectInput(session, "explore_colour_by", choices = colour_ch)

      facet_ch <- c("None" = "none")
      facs_in <- intersect(factors_(), names(rv$data %||% data.frame()))
      if (mode == "comparative" && length(facs_in) > 1)
        facet_ch <- c(facet_ch, setNames(".treatment", trt_lab))
      for (b in blocks()) facet_ch <- c(facet_ch, setNames(b, b))
      for (f in factors_()) facet_ch <- c(facet_ch, setNames(f, f))
      for (cv in covariates()) facet_ch <- c(facet_ch, setNames(cv, paste0(cv, " (cov)")))
      updateSelectInput(session, "explore_facet_by", choices = facet_ch)

      # Hide boxplot toggle in regression mode (boxplots only for comparative)
      shinyjs::toggle("boxplot_toggle_div", condition = (mode == "comparative"))
    })

    # ── Colour helper ──────────────────────────────────────────────────────
    explore_colour <- reactive({
      cb <- input$explore_colour_by
      if (is.null(cb) || cb == "none") return(NULL)
      if (cb == ".treatment") return(treatment())
      if (cb %in% names(rv$data)) {
        v <- rv$data[[cb]]
        if (is.numeric(v)) return(v)
        return(as.factor(v))
      }
      NULL
    })

    # ── Facet helper ───────────────────────────────────────────────────────
    explore_facet <- function(p) {
      fb   <- input$explore_facet_by
      mode <- input$explore_facet_mode %||% "wrap"
      if (is.null(fb) || fb == "none") return(p)
      valid <- (fb == ".treatment" && !is.null(treatment())) || fb %in% names(rv$data)
      if (!valid) return(p)
      apply_facets(p, facet_var = fb, mode = mode)
    }

    # ── Distribution ───────────────────────────────────────────────────────
    output$dist_plot <- renderPlotly({
      req(rv$data, input$explore_response)
      y_col <- input$explore_response
      req(y_col %in% names(rv$data))
      df <- rv$data
      cv <- explore_colour()
      pal <- cat_palette()
      if (!is.null(cv)) {
        if (is.numeric(cv)) {
          n_uniq <- length(unique(cv))
          df$.colour <- if (n_uniq <= 1) as.factor(cv)
                        else cut(cv, breaks = min(5, n_uniq), ordered_result = TRUE)
        } else {
          df$.colour <- cv
        }
        p <- ggplot(df, aes_string(x = y_col, fill = ".colour")) +
          geom_histogram(aes(y = after_stat(density)), bins = 30,
                         alpha = 0.5, colour = "white", position = "identity") +
          geom_density(aes_string(colour = ".colour"), linewidth = 1) +
          cat_scale_fill()() + cat_scale_colour()() +
          labs(fill = input$explore_colour_by, colour = input$explore_colour_by)
      } else {
        cols <- pal(2)
        p <- ggplot(df, aes_string(x = y_col)) +
          geom_histogram(aes(y = after_stat(density)), bins = 30,
                         fill = cols[1], alpha = 0.6, colour = "white") +
          geom_density(colour = cols[2], linewidth = 1)
      }
      p <- p + labs(title = paste("Distribution of", y_col), x = y_col, y = "Density") +
        theme_app()
      p <- explore_facet(p)
      ggplotly(p)
    })

    # ── By Factor ──────────────────────────────────────────────────────────
    output$by_factor_plot <- renderPlotly({
      req(rv$data, input$explore_response, input$explore_factor)
      resp <- input$explore_response; fac <- input$explore_factor
      df <- rv$data
      trt <- treatment()

      # Handle .treatment as a virtual factor column
      if (fac == ".treatment") {
        req(!is.null(trt))
        df$.treatment <- trt
      }
      req(fac %in% names(df))
      df[[fac]] <- as.factor(df[[fac]])
      cv  <- explore_colour()
      show_box <- isTRUE(input$explore_show_boxplot)

      jit <- input$explore_jitter %||% 0.05
      rep_only <- isTRUE(input$explore_jitter_replicated)
      jw <- smart_jitter_width(df, fac, jit, rep_only)

      p <- ggplot(df, aes_string(x = fac, y = resp))

      if (show_box) {
        p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
      }

      if (!is.null(trt)) {
        df$.treatment <- trt
        trt_lab <- treatment_label()
        p <- ggplot(df, aes_string(x = fac, y = resp))
        if (show_box) {
          p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
        }
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          p <- ggplot(df, aes_string(x = fac, y = resp))
          if (show_box) {
            p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
          }
          p <- p + geom_jitter(aes(shape = .treatment, colour = .colour,
                                   key = .data[[ROW_ID_COL]]),
                        width = jw, alpha = 0.7, size = 2) +
            labs(colour = input$explore_colour_by, shape = trt_lab)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- p + geom_jitter(aes(shape = .treatment, key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2) +
            labs(shape = trt_lab)
        }
      } else {
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          p <- ggplot(df, aes_string(x = fac, y = resp))
          if (show_box) {
            p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
          }
          p <- p + geom_jitter(aes(colour = .colour, key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2) +
            labs(colour = input$explore_colour_by)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- p + geom_jitter(aes(key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2)
        }
      }
      p <- p + labs(title = paste(resp, "by", fac), x = fac, y = resp) + theme_app()
      p <- explore_facet(p)
      ggplotly(p, source = SEL_SOURCE) %>%
        plotly::event_register("plotly_selected") %>%
        plotly::event_register("plotly_click") %>%
        plotly::event_register("plotly_deselect") %>%
        apply_selection_style(df[[ROW_ID_COL]], rv$selected_obs)
    })

    # ── By Block ───────────────────────────────────────────────────────────
    output$by_block_ui <- renderUI({
      if (length(blocks()) == 0) {
        p("No block variable assigned.", class = "text-muted p-3")
      } else {
        tagList(
          fluidRow(
            column(6, selectInput(ns("explore_block"), "Block variable", choices = blocks())),
            column(6, checkboxInput(ns("block_connect_trt"), "Connect by treatment", value = FALSE))
          ),
          plotlyOutput(ns("by_block_plot"), height = "500px")
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
      if (!is.null(trt)) df$.treatment <- trt
      connect_trt <- isTRUE(input$block_connect_trt) && !is.null(trt)

      jit <- input$explore_jitter %||% 0.05
      rep_only <- isTRUE(input$explore_jitter_replicated)
      jw <- smart_jitter_width(df, blk, jit, rep_only)

      p <- ggplot(df, aes_string(x = blk, y = resp))

      if (!is.null(trt)) {
        trt_lab <- treatment_label()
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          p <- ggplot(df, aes_string(x = blk, y = resp))
          if (connect_trt) {
            p <- p + geom_line(aes(group = .treatment, linetype = .treatment),
                               colour = "grey60", linewidth = 0.5, alpha = 0.6)
          }
          p <- p + geom_jitter(aes(shape = .treatment, colour = .colour,
                                   key = .data[[ROW_ID_COL]]),
                        width = jw, alpha = 0.7, size = 2) +
            labs(colour = input$explore_colour_by, shape = trt_lab)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- ggplot(df, aes_string(x = blk, y = resp))
          if (connect_trt) {
            p <- p + geom_line(aes(group = .treatment, linetype = .treatment),
                               colour = "grey60", linewidth = 0.5, alpha = 0.6)
          }
          p <- p + geom_jitter(aes(shape = .treatment, key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2) +
            labs(shape = trt_lab)
        }
      } else {
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          p <- ggplot(df, aes_string(x = blk, y = resp))
          p <- p + geom_jitter(aes(colour = .colour, key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2) +
            labs(colour = input$explore_colour_by)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- ggplot(df, aes_string(x = blk, y = resp))
          p <- p + geom_jitter(aes(key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2)
        }
      }
      if (connect_trt) p <- p + labs(linetype = treatment_label())
      p <- p + labs(title = paste(resp, "by block:", blk), x = blk, y = resp) + theme_app()
      p <- explore_facet(p)
      ggplotly(p, source = SEL_SOURCE) %>%
        plotly::event_register("plotly_selected") %>%
        plotly::event_register("plotly_click") %>%
        plotly::event_register("plotly_deselect") %>%
        apply_selection_style(df[[ROW_ID_COL]], rv$selected_obs)
    })

    # ── Covariate Plot ─────────────────────────────────────────────────────
    output$covariate_plot <- renderPlotly({
      req(rv$data, input$explore_response, input$explore_covariate)
      resp <- input$explore_response; cov_name <- input$explore_covariate
      req(resp %in% names(rv$data), cov_name %in% names(rv$data))
      df <- rv$data
      trt <- treatment()
      cv  <- explore_colour()
      if (!is.null(trt)) df$.treatment <- trt

      sm  <- input$explore_smooth %||% "lm"
      lw  <- input$explore_line_width %||% 2
      show_connected <- (sm == "connected")
      show_smooth    <- !show_connected

      if (!is.null(trt)) {
        trt_lab <- treatment_label()
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          colour_aligns <- colour_nests_in_group(df, ".colour", ".treatment")
          p <- ggplot(df, aes_string(x = cov_name, y = resp))
          p <- p + geom_point(aes(shape = .treatment, colour = .colour,
                                  key = .data[[ROW_ID_COL]]), alpha = 0.7, size = 2)
          if (colour_aligns && !is_cont_cv) {
            if (show_connected)
              p <- p + geom_line(aes(group = .treatment, linetype = .treatment,
                                      colour = .colour), linewidth = lw)
            if (show_smooth)
              p <- p + geom_smooth(aes(group = .treatment, linetype = .treatment,
                                        colour = .colour),
                                   method = sm, se = FALSE, linewidth = lw)
          } else {
            if (show_connected)
              p <- p + geom_line(aes(group = .treatment, linetype = .treatment),
                                 colour = "grey50", linewidth = lw)
            if (show_smooth)
              p <- p + geom_smooth(aes(group = .treatment, linetype = .treatment),
                                   method = sm, se = TRUE, colour = "grey30", linewidth = lw)
          }
          p <- p + labs(colour = input$explore_colour_by, shape = trt_lab,
                        linetype = trt_lab)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- ggplot(df, aes_string(x = cov_name, y = resp,
                                      colour = ".treatment", shape = ".treatment"))
          p <- p + geom_point(aes(key = .data[[ROW_ID_COL]]), alpha = 0.7, size = 2)
          if (show_connected)
            p <- p + geom_line(linewidth = lw)
          if (show_smooth)
            p <- p + geom_smooth(method = sm, se = FALSE, linewidth = lw)
          p <- p + labs(colour = trt_lab, shape = trt_lab) + cat_scale_colour()()
        }
      } else {
        dcol <- default_col()
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          colour_aligns <- !is_cont_cv
          p <- ggplot(df, aes_string(x = cov_name, y = resp))
          p <- p + geom_point(aes(colour = .colour, key = .data[[ROW_ID_COL]]),
                               alpha = 0.7, size = 2)
          if (show_connected && colour_aligns)
            p <- p + geom_line(aes(colour = .colour), linewidth = lw, alpha = 0.6)
          else if (show_connected)
            p <- p + geom_line(colour = "grey50", linewidth = lw, alpha = 0.6)
          if (show_smooth)
            p <- p + geom_smooth(method = sm, se = TRUE, colour = "grey30", linewidth = lw)
          p <- p + labs(colour = input$explore_colour_by)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- ggplot(df, aes_string(x = cov_name, y = resp))
          p <- p + geom_point(aes(key = .data[[ROW_ID_COL]]),
                               alpha = 0.6, colour = dcol, size = 2)
          if (show_connected)
            p <- p + geom_line(colour = dcol, linewidth = lw, alpha = 0.6)
          if (show_smooth)
            p <- p + geom_smooth(method = sm, se = TRUE, colour = dcol, linewidth = lw)
        }
      }
      p <- p + labs(title = paste(resp, "vs", cov_name), x = cov_name, y = resp) +
        theme_app()
      p <- explore_facet(p)
      ggplotly(p, source = SEL_SOURCE) %>%
        plotly::event_register("plotly_selected") %>%
        plotly::event_register("plotly_click") %>%
        plotly::event_register("plotly_deselect") %>%
        apply_selection_style(df[[ROW_ID_COL]], rv$selected_obs)
    })

    # ── Needle Plot ────────────────────────────────────────────────────────
    observe({
      facs  <- factors_()
      blks  <- blocks()
      facet_ch <- c("None" = "none")
      if (length(facs) > 1) {
        facet_ch <- c(facet_ch, setNames(".treatment", treatment_label()))
      }
      for (f in facs) facet_ch <- c(facet_ch, setNames(f, f))
      for (b in blks) facet_ch <- c(facet_ch, setNames(b, paste0(b, " (block)")))
      updateSelectInput(session, "needle_facet", choices = facet_ch)
    })

    output$needle_factor_order_ui <- renderUI({
      facs <- factors_()
      blks <- blocks()
      all_choices <- c(facs, blks)
      if (length(all_choices) == 0) return(NULL)
      selectInput(ns("needle_factor_order"), "Factor nesting order",
                  choices = all_choices, selected = facs, multiple = TRUE)
    })

    output$needle_plot <- renderPlotly({
      tryCatch({
      req(rv$data, input$explore_response)
      req(length(factors_()) > 0 || length(blocks()) > 0)
      resp_col <- input$explore_response
      req(resp_col %in% names(rv$data))

      df <- rv$data
      trt <- treatment()
      if (!is.null(trt)) df$.treatment <- trt
      fac_order <- input$needle_factor_order %||% c(factors_(), blocks())
      fac_order <- intersect(fac_order, names(df))
      if (length(fac_order) == 0) fac_order <- c(factors_(), blocks())

      cv <- explore_colour()
      colour_var <- input$explore_colour_by %||% "none"
      colour_label <- if (!is.null(cv)) {
        if (colour_var == ".treatment") treatment_label() else colour_var
      } else NULL

      # Resolve colour scale for the plot
      cs <- if (!is.null(cv)) {
        if (is.numeric(cv)) cont_scale_colour()() else cat_scale_colour()()
      } else NULL

      p <- build_needle_plot(
        df           = df,
        resp_col     = resp_col,
        fac_order    = fac_order,
        sort_mode    = input$needle_sort %||% "factors",
        colour_vec   = cv,
        dcol         = default_col(),
        colour_label = colour_label,
        facet_var    = input$needle_facet,
        facet_mode   = input$needle_facet_mode %||% "wrap",
        colour_scale = cs
      )
      ggplotly(p)
      }, error = function(e) {
        showNotification(paste("Needle plot error:", e$message), type = "error", duration = 10)
        plotly_empty() %>% layout(title = list(text = "Error rendering plot", font = list(color = "red")))
      })
    })

    # ── Parallel Plot ──────────────────────────────────────────────────────
    output$parallel_controls_ui <- renderUI({
      req(rv$data)
      facs  <- factors_()
      resps <- responses()
      blks  <- blocks()
      covs  <- covariates()
      all_vars <- c(facs, blks, covs, resps)
      fluidRow(
        column(12, selectInput(ns("parallel_vars"), "Variables to include",
                               choices = all_vars, selected = all_vars, multiple = TRUE))
      )
    })

    output$parallel_plot <- renderPlotly({
      req(rv$data, length(input$parallel_vars) >= 2)
      df <- rv$data
      vars <- intersect(input$parallel_vars, names(df))
      if (length(vars) < 2) return(plotly_empty())

      cb <- input$explore_colour_by %||% "none"
      if (cb == ".treatment") {
        trt <- treatment()
        if (!is.null(trt)) {
          df$.treatment <- trt
          colour_var <- ".treatment"
          has_colour <- TRUE
        } else {
          has_colour <- FALSE
          colour_var <- NULL
        }
      } else {
        colour_var <- cb
        has_colour <- !is.null(colour_var) && colour_var != "none" && colour_var %in% names(df)
      }

      par_lw <- (input$explore_line_width %||% 1) * 3

      if (has_colour && is.numeric(df[[colour_var]])) {
        cs <- cont_plotly_cs()
        line_spec <- list(color = df[[colour_var]], colorscale = cs,
                          showscale = TRUE, colorbar = list(title = colour_var),
                          width = par_lw)
      } else if (has_colour) {
        lvl_num <- as.numeric(as.factor(df[[colour_var]]))
        pal <- cat_palette()(length(unique(lvl_num)))
        cs <- lapply(seq_along(pal), function(i) {
          list((i - 1) / (length(pal) - 1), pal[i])
        })
        line_spec <- list(color = lvl_num, colorscale = cs,
                          showscale = TRUE,
                          colorbar = list(title = if (colour_var == ".treatment")
                                          treatment_label() else colour_var),
                          width = par_lw)
      } else {
        line_spec <- list(color = default_col(), width = par_lw)
      }

      dims <- lapply(vars, function(v) {
        vals <- df[[v]]
        if (is.numeric(vals)) {
          list(label = v, values = vals,
               range = c(min(vals, na.rm = TRUE), max(vals, na.rm = TRUE)))
        } else {
          lvls <- sort(unique(as.character(vals)))
          list(label = v, values = match(as.character(vals), lvls),
               tickvals = seq_along(lvls), ticktext = lvls,
               range = c(0.5, length(lvls) + 0.5))
        }
      })

      plot_ly(type = "parcoords", line = line_spec, dimensions = dims) %>%
        layout(title = plotly_title("Parallel Coordinates Plot"))
    })
  })
}
