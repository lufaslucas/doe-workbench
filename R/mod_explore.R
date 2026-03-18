# R/mod_explore.R — Explore tab module (UI + server)

# ── UI ───────────────────────────────────────────────────────────────────────
mod_explore_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput(ns("explore_response"), "Response variable", choices = NULL),
      selectInput(ns("explore_colour_by"), "Colour by",
                  choices = c("None" = "none"), selected = "none"),
      selectInput(ns("explore_facet_by"), "Facet by",
                  choices = c("None" = "none"), selected = "none"),
      radioButtons(ns("explore_facet_mode"), "Facet mode",
                   choices = c("Wrap" = "wrap", "Row" = "row", "Col" = "col"),
                   selected = "wrap", inline = TRUE),
      radioButtons(ns("explore_smooth"), "Fit / connect",
                   choices = c("Linear" = "lm", "Smoother" = "loess",
                               "Connected" = "connected"),
                   selected = "lm", inline = TRUE),
      sliderInput(ns("explore_line_width"), "Line width",
                  min = 0.5, max = 5, value = 1, step = 0.5),
      hr(),
      sliderInput(ns("explore_jitter"), "Jitter amount",
                  min = 0, max = 0.4, value = 0.05, step = 0.01),
      checkboxInput(ns("explore_jitter_replicated"), "Only jitter replicated points", value = TRUE)
    ),
    mainPanel(width = 9,
      tabsetPanel(id = ns("explore_tabs"),
        tabPanel("Distribution",
          br(),
          plotlyOutput(ns("dist_plot"), height = "450px")
        ),
        tabPanel("By Factor",
          br(),
          fluidRow(
            column(6, selectInput(ns("explore_factor"), "Factor", choices = NULL)),
            column(6, checkboxInput(ns("explore_show_boxplot"), "Show boxplots", value = TRUE))
          ),
          plotlyOutput(ns("by_factor_plot"), height = "450px")
        ),
        tabPanel("Covariate Plots",
          br(),
          selectInput(ns("explore_covariate"), "Covariate", choices = NULL),
          plotlyOutput(ns("covariate_plot"), height = "500px")
        ),
        tabPanel("By Block",
          br(),
          uiOutput(ns("by_block_ui"))
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
    )
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

    cat_palette       <- colour_theme$cat_palette
    default_col       <- colour_theme$default_col
    cat_scale_colour  <- colour_theme$cat_scale_colour
    cat_scale_fill    <- colour_theme$cat_scale_fill
    cont_scale_colour <- colour_theme$cont_scale_colour
    cont_plotly_cs    <- colour_theme$cont_plotly_cs

    # ── Dropdown updates ───────────────────────────────────────────────────
    observe({
      updateSelectInput(session, "explore_response",  choices = responses())
      updateSelectInput(session, "explore_factor",    choices = factors_())
      updateSelectInput(session, "explore_covariate", choices = all_covariates())

      cols <- names(rv$data %||% data.frame())
      trt_lab <- treatment_label()
      colour_ch <- c("None" = "none", setNames(".treatment", trt_lab))
      for (cn in cols) colour_ch <- c(colour_ch, setNames(cn, cn))
      updateSelectInput(session, "explore_colour_by", choices = colour_ch)

      facet_ch <- c("None" = "none")
      facs_in <- intersect(factors_(), names(rv$data %||% data.frame()))
      if (length(facs_in) > 1)
        facet_ch <- c(facet_ch, setNames(".treatment", trt_lab))
      for (b in blocks()) facet_ch <- c(facet_ch, setNames(b, b))
      for (f in factors_()) facet_ch <- c(facet_ch, setNames(f, f))
      for (cv in covariates()) facet_ch <- c(facet_ch, setNames(cv, paste0(cv, " (cov)")))
      updateSelectInput(session, "explore_facet_by", choices = facet_ch)
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
          df$.colour <- cut(cv, breaks = min(5, length(unique(cv))),
                            ordered_result = TRUE)
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
      req(resp %in% names(rv$data), fac %in% names(rv$data))
      df <- rv$data; df[[fac]] <- as.factor(df[[fac]])
      trt <- treatment()
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
          selectInput(ns("explore_block"), "Block variable", choices = blocks()),
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

      jit <- input$explore_jitter %||% 0.05
      rep_only <- isTRUE(input$explore_jitter_replicated)
      jw <- smart_jitter_width(df, blk, jit, rep_only)

      if (!is.null(trt)) {
        trt_lab <- treatment_label()
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          p <- ggplot(df, aes_string(x = blk, y = resp))
          p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
          p <- p + geom_jitter(aes(shape = .treatment, colour = .colour,
                                   key = .data[[ROW_ID_COL]]),
                        width = jw, alpha = 0.7, size = 2) +
            labs(colour = input$explore_colour_by, shape = trt_lab)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- ggplot(df, aes_string(x = blk, y = resp))
          p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
          p <- p + geom_jitter(aes(shape = .treatment, key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2) +
            labs(shape = trt_lab)
        }
      } else {
        if (!is.null(cv)) {
          df$.colour <- cv
          is_cont_cv <- is.numeric(cv)
          p <- ggplot(df, aes_string(x = blk, y = resp))
          p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
          p <- p + geom_jitter(aes(colour = .colour, key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2) +
            labs(colour = input$explore_colour_by)
          if (is_cont_cv) p <- p + cont_scale_colour()() else p <- p + cat_scale_colour()()
        } else {
          p <- ggplot(df, aes_string(x = blk, y = resp))
          p <- p + geom_boxplot(colour = "black", fill = NA, alpha = 0.6, outlier.shape = NA)
          p <- p + geom_jitter(aes(key = .data[[ROW_ID_COL]]),
                               width = jw, alpha = 0.7, size = 2)
        }
      }
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
      sort_mode <- input$needle_sort %||% "factors"

      df$.trt_id <- apply(df[, fac_order, drop = FALSE], 1,
                           function(r) paste(r, collapse = ":"))

      grand_mean <- mean(df[[resp_col]], na.rm = TRUE)

      # Always compute group means — needles extend to group mean, not raw point
      trt_means <- aggregate(df[[resp_col]],
                              by = list(.trt_id = df$.trt_id), FUN = mean, na.rm = TRUE)
      names(trt_means)[2] <- ".group_mean"

      if (sort_mode == "response") {
        trt_order <- trt_means$.trt_id[order(trt_means$.group_mean)]
        df$.trt_id <- factor(df$.trt_id, levels = trt_order)
        trt_means$.trt_id <- factor(trt_means$.trt_id, levels = trt_order)
      } else {
        df$.trt_id <- factor(df$.trt_id)
        trt_means$.trt_id <- factor(trt_means$.trt_id, levels = levels(df$.trt_id))
      }

      cv <- explore_colour()
      colour_var <- input$explore_colour_by %||% "none"
      has_colour <- !is.null(cv)

      df$.grand_mean <- grand_mean
      # Merge group mean onto df for tooltip access; needle uses trt_means directly
      df <- merge(df, trt_means, by = ".trt_id", all.x = TRUE)

      if (has_colour) {
        df$.colour <- cv
        is_cont <- is.numeric(cv)
        if (!is_cont) df$.colour <- as.factor(df$.colour)
        dcol <- default_col()
        p <- ggplot(df, aes(x = .trt_id, y = .data[[resp_col]])) +
          geom_hline(yintercept = grand_mean, colour = "grey40", linewidth = 0.8) +
          geom_segment(data = trt_means,
                       aes(x = .trt_id, xend = .trt_id,
                           y = grand_mean, yend = .group_mean),
                       colour = dcol, linewidth = 1.2) +
          geom_point(aes(colour = .colour), size = 2.5, alpha = 0.8)
        if (is_cont) {
          p <- p + cont_scale_colour()()
        } else {
          p <- p + cat_scale_colour()()
        }
      } else {
        dcol <- default_col()
        p <- ggplot(df, aes_string(x = ".trt_id", y = resp_col)) +
          geom_hline(yintercept = grand_mean, colour = "grey40", linewidth = 0.8) +
          geom_segment(data = trt_means,
                       aes(x = .trt_id, xend = .trt_id,
                           y = grand_mean, yend = .group_mean),
                       colour = dcol, linewidth = 1.2) +
          geom_point(colour = dcol, size = 2.5, alpha = 0.8)
      }

      facet_var  <- input$needle_facet
      facet_mode <- input$needle_facet_mode %||% "wrap"
      has_facet  <- !is.null(facet_var) && facet_var != "none" && facet_var %in% names(df)
      if (has_facet) {
        p <- apply_facets(p, facet_var = facet_var, mode = facet_mode, scales = "free_x")
      }

      p <- p +
        annotate("text", x = 1, y = grand_mean, label = paste("mean =", round(grand_mean, 2)),
                 vjust = -0.5, hjust = 0, colour = "grey40", size = 3) +
        labs(title = paste("Needle plot \u2014", resp_col),
             x = paste(fac_order, collapse = " : "),
             y = resp_col,
             colour = if (has_colour) {
               cb <- input$explore_colour_by %||% "none"
               if (cb == ".treatment") treatment_label() else cb
             } else NULL) +
        theme_app() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      ggplotly(p)
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

      fb <- input$explore_facet_by %||% "none"
      has_facet <- fb != "none" && (fb == ".treatment" || fb %in% names(df))

      if (has_facet) {
        if (fb == ".treatment") {
          trt <- treatment()
          if (!is.null(trt)) df$.facet <- trt else has_facet <- FALSE
        } else {
          df$.facet <- as.factor(df[[fb]])
        }
      }

      if (has_facet) {
        facet_lvls <- levels(as.factor(df$.facet))
        plots <- lapply(facet_lvls, function(lvl) {
          sub_df <- df[df$.facet == lvl, , drop = FALSE]
          sub_dims <- lapply(vars, function(v) {
            vals <- sub_df[[v]]
            if (is.numeric(vals)) {
              list(label = v, values = vals,
                   range = c(min(df[[v]], na.rm = TRUE), max(df[[v]], na.rm = TRUE)))
            } else {
              lvls_all <- sort(unique(as.character(df[[v]])))
              list(label = v, values = match(as.character(vals), lvls_all),
                   tickvals = seq_along(lvls_all), ticktext = lvls_all,
                   range = c(0.5, length(lvls_all) + 0.5))
            }
          })
          sub_line <- if (has_colour && colour_var %in% names(sub_df)) {
            if (is.numeric(sub_df[[colour_var]])) {
              list(color = sub_df[[colour_var]], colorscale = cs,
                   showscale = FALSE, width = par_lw)
            } else {
              list(color = as.numeric(as.factor(sub_df[[colour_var]])),
                   colorscale = cs, showscale = FALSE, width = par_lw)
            }
          } else {
            list(color = default_col(), width = par_lw)
          }
          plot_ly(type = "parcoords", line = sub_line, dimensions = sub_dims) %>%
            layout(title = plotly_title(lvl, size = 12))
        })
        subplot(plots, nrows = length(facet_lvls), shareX = TRUE) %>%
          layout(title = plotly_title(paste("Parallel Coordinates by", fb)))
      } else {
        plot_ly(type = "parcoords", line = line_spec, dimensions = dims) %>%
          layout(title = plotly_title("Parallel Coordinates Plot"))
      }
    })
  })
}
