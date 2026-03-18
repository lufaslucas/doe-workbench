# ui.R — navbarPage with module tabs

ui <- navbarPage(
  title = "DoE Workbench",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  id = "main_nav",
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(src = "app.js")
    ),
    div(class = "session-controls", id = "save_group",
      actionButton("save_btn", "Save", class = "btn-sm",
                   icon = icon("download")),
      actionButton("load_btn", "Load", class = "btn-sm",
                   icon = icon("upload"))
    ),
    div(id = "readonly_banner", class = "readonly-banner", style = "display:none;",
      icon("lock"),
      span("Read-Only Session"),
      actionButton("unlock_btn", "Unlock", class = "btn-sm btn-outline-light ms-2",
                   icon = icon("unlock"))
    ),
    div(id = "global_selection_bar", class = "selection-bar", style = "display:none;",
      span(id = "global_selection_count", class = "selection-count"),
      actionButton("clear_selection", "Clear",
                   class = "btn-sm btn-outline-secondary ms-2",
                   icon = icon("times"))
    )
  ),

  # ── Tab 1: Data ─────────────────────────────────────────────────────────
  tabPanel(
    "Data",
    # Analysis mode at the top (shared across Upload and Generate)
    wellPanel(
      fluidRow(
        column(6,
          radioButtons("analysis_mode", "Analysis mode",
                       choices = c("Comparative (ANOVA)" = "comparative",
                                   "Regression (MLR)"    = "regression"),
                       selected = "comparative", inline = TRUE)
        ),
        column(6,
          p(class = "text-muted small mt-2",
            tags$b("Comparative:"), " factors as categorical \u2014 LS means, pairwise comparisons.",
            tags$br(),
            tags$b("Regression:"), " factors as numeric \u2014 polynomial models, coefficients.")
        )
      )
    ),
    tabsetPanel(id = "data_tabs",
      mod_data_upload_ui("data_upload"),
      mod_data_generate_ui("data_generate")
    )
  ),

  # ── Tab 2: Assign Roles ──────────────────────────────────────────────────
  tabPanel("Assign Roles", mod_assign_roles_ui("assign_roles")),

  # ── Tab 3: Design (module) ──────────────────────────────────────────────
  tabPanel("Design", mod_design_ui("design")),

  # ── Tab 4: Explore (module) ──────────────────────────────────────────────
  tabPanel("Explore", mod_explore_ui("explore")),

  # ── Tab 5: Models (module) ─────────────────────────────────────────────
  tabPanel("Models", mod_models_ui("models")),

  # ── Tab 6: Results (module) ────────────────────────────────────────────
  tabPanel("Results", mod_results_ui("results")),

  # ── Tab 7: Report (module) ─────────────────────────────────────────────
  tabPanel("Report", mod_report_ui("report")),

  # ── Settings (gear icon nav menu) ────────────────────────────────────────
  navbarMenu(
    "\u2699 Settings",
    tabPanel(
      "Colour Themes",
      fluidRow(
        column(6,
          wellPanel(
            h4("Categorical Factor Colours"),
            p(class = "text-muted",
              "Select a colour palette for categorical variables (factors, blocks, treatments). ",
              "Palettes are designed to scale gracefully to many levels."),
            selectInput("cat_colour_theme", "Categorical palette",
                        choices = c("Shell" = "shell",
                                    "Shell Muted" = "shell_muted",
                                    "Default (Tableau 10)" = "default",
                                    "Viridis (discrete)" = "viridis",
                                    "Brewer Set2" = "set2",
                                    "Brewer Dark2" = "dark2"),
                        selected = "shell"),
            plotOutput("cat_colour_preview", height = "60px")
          )
        ),
        column(6,
          wellPanel(
            h4("Continuous Colour Scale"),
            p(class = "text-muted",
              "Select a colour scale for continuous variables (heatmaps, numeric colour-by)."),
            selectInput("cont_colour_theme", "Continuous scale",
                        choices = c("Shell Warm" = "shell_warm",
                                    "Shell Cool" = "shell_cool",
                                    "Viridis" = "viridis",
                                    "Inferno" = "inferno",
                                    "Plasma" = "plasma",
                                    "Blue-Red" = "bluered"),
                        selected = "shell_warm"),
            plotOutput("cont_colour_preview", height = "60px")
          )
        )
      )
    )
  )
)
