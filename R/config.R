# R/config.R — Centralised configuration constants
# All magic numbers and defaults referenced across the app.

# ── Observation Identity ──────────────────────────────────────────────────
ROW_ID_COL     <- "._row_id"   # Stable row identifier (survives subsetting)

# ── Statistical Thresholds ───────────────────────────────────────────────
ALPHA_DEFAULT      <- 0.05   # Default significance level
ALPHA_LENIENT      <- 0.10   # Lenient threshold (e.g., backward elimination)
ALIAS_CORR_THRESH  <- 0.99   # Correlation threshold for alias detection

# ── VIF (Variance Inflation Factor) ─────────────────────────────────────
VIF_MODERATE  <- sqrt(5)     # ~2.236 — moderate collinearity (amber)
VIF_HIGH      <- sqrt(10)    # ~3.162 — high collinearity (red)

# ── Model Builder Defaults ───────────────────────────────────────────────
MAX_WAY_DEFAULT   <- 2L      # Default max interaction order
MAX_WAY_LIMIT     <- 5L      # Absolute upper bound for interaction order
POLY_DEGREE_DEFAULT <- 2L    # Default polynomial degree (regression mode)
POLY_DEGREE_LIMIT   <- 5L    # Absolute upper bound for polynomial degree

# ── Jitter Defaults ─────────────────────────────────────────────────────
JITTER_DEFAULT     <- 0.05   # Standard jitter amount for plots
JITTER_DENSE       <- 0.15   # Jitter for high-density plots (splom, design 2D)
JITTER_MAX         <- 0.40   # Maximum jitter slider value
JITTER_STEP        <- 0.01   # Jitter slider step

# ── DataTable Page Lengths ───────────────────────────────────────────────
PAGE_LEN_PREVIEW  <- 10L     # Small preview tables
PAGE_LEN_DEFAULT  <- 20L     # Standard tables
PAGE_LEN_DETAIL   <- 30L     # Detailed analytical tables
PAGE_LEN_LARGE    <- 50L     # Large datasets (simulation, outliers)

# ── Plot Heights (px) ───────────────────────────────────────────────────
PLOT_HEIGHT_SM     <- 320L    # Compact plots (residual sub-panels)
PLOT_HEIGHT_MD     <- 400L    # Standard single-plot tabs
PLOT_HEIGHT_LG     <- 500L    # Default explorations
PLOT_HEIGHT_XL     <- 600L    # Large/3D plots

# ── Shell Brand Colours ─────────────────────────────────────────────────
SHELL_RED     <- "#DD1D21"
SHELL_YELLOW  <- "#FBCE07"
SHELL_GREY    <- "#404040"
SHELL_NAVY    <- "#003B73"
SHELL_ORANGE  <- "#E87722"
SHELL_GREEN   <- "#00A651"
SHELL_BROWN   <- "#8C6A32"
SHELL_LTBLUE  <- "#6CACE4"

# ── P-value Formatting ────────────────────────────────────────────────
PVALUE_DIGITS      <- 4L       # Default decimal places for p-values
PVALUE_FLOOR_LABEL <- "<0.0001"  # Display string when p rounds to 0
PVALUE_FOOTNOTE    <- "P-values shown to 4 decimal places. Values below 0.0001 displayed as <0.0001. Values near the significance threshold are rounded conservatively (away from rejection)."

# ── UI Styling ──────────────────────────────────────────────────────────
PVALUE_GREEN   <- "#c8f7c5"   # Background for significant p-values
VIF_AMBER      <- "#fff3cd"   # Background for moderate VIF
VIF_RED        <- "#f8d7da"   # Background for high VIF
ALIASED_BG     <- "#fff8e1"   # Background for aliased/confounded rows

# ── Robustness Tab ────────────────────────────────────────────────────────
ROBUST_GREEN   <- "#e8f5e9"   # Robust verdict background
ROBUST_AMBER   <- "#fff8e1"   # Fragile verdict background
ROBUST_RED     <- "#ffebee"   # Warning/mediator verdict background
MEDIATION_R2_THRESH <- 0.30   # R² above which covariate flagged as likely mediator
COOK_D_THRESH_MULT  <- 4      # Cook's D threshold = COOK_D_THRESH_MULT / n
DFBETA_THRESH_MULT  <- 2      # |DFBETA| threshold = DFBETA_THRESH_MULT / sqrt(n)
OUTLIER_MAX_ITER    <- 5L     # Max iterations for iterative outlier removal

# ── Linked Selection ──────────────────────────────────────────────────
SEL_SOURCE          <- "linked_sel"   # plotly source ID for selection events
SEL_ALPHA_DIMMED    <- 0.12           # alpha for unselected points
SEL_ALPHA_NORMAL    <- 0.70           # default alpha (no selection active)
SEL_ALPHA_SELECTED  <- 1.0            # alpha for selected points
SEL_SIZE_BOOST      <- 1.3            # size multiplier for selected points

# ── Model Types ──────────────────────────────────────────────────────────
MODEL_TYPES <- c("OLS (lm)" = "lm")   # Extensible: add "WLS" = "wls", "GLM" = "glm", etc.

# ── Read-Only Mode ──────────────────────────────────────────────────────
READONLY_HASH_ALGO  <- "sha256"       # digest algorithm for password hashing

# ── Default State Constructor ──────────────────────────────────────────
#' Canonical default values for every rv field.
#' Used by reactiveValues() init, reset_app_to_defaults(), and save/load.
make_default_rv <- function() {
  list(
    # DATA LAYER
    data          = NULL,
    roles         = list(),
    col_types     = list(),
    transforms    = list(),
    coding_values = list(),
    level_labels  = list(),

    # MODEL LAYER
    formulas      = character(0),
    formula_gen   = 0L,
    models        = list(),
    mc_results    = list(),
    mc_on         = FALSE,
    mc_alpha      = ALPHA_DEFAULT,
    mc_terms      = character(0),
    mc_methods    = character(0),
    vif_df        = data.frame(),
    prune_notes   = list(),
    model_notes   = list(),
    model_errors  = list(),
    excluded_obs  = list(),

    # MODEL SPEC (builder settings)
    model_active_response            = NULL,
    model_custom_formula             = "",
    model_max_way                    = MAX_WAY_DEFAULT,
    model_poly_degree                = POLY_DEGREE_DEFAULT,
    model_include_covariates         = TRUE,
    model_formula_covariates         = character(0),
    model_max_covariates_per_formula = 1L,
    model_include_cov_fac            = FALSE,
    model_include_blocks             = TRUE,
    model_include_block_fac          = FALSE,
    model_append_formulas            = FALSE,

    # DESIGN LAYER
    design_metadata          = list(),
    design_model_formula     = "",
    design_alias_formula     = "",
    alias_threshold          = ALIAS_CORR_THRESH,
    sim_data                 = NULL,
    formula_aliases          = list(),
    alias_labels             = list(),
    inestimable_terms        = character(),
    pending_alias_resolution = NULL,
    skip_auto_formula        = FALSE,

    # UI LAYER
    selected_obs  = NULL,
    report_items  = list(),

    # SESSION LAYER
    session_path  = NULL,

    # READ-ONLY LAYER
    read_only      = FALSE,
    read_only_hash = NULL
  )
}

# ── Build Info ──────────────────────────────────────────────────────────
get_build_info <- function() {
  # In dev mode, read live from git
  pkg_root <- Sys.getenv("DOE_DEV_ROOT", unset = "")
  if (nzchar(pkg_root) && dir.exists(file.path(pkg_root, ".git"))) {
    branch <- tryCatch(
      trimws(system2("git", c("-C", pkg_root, "rev-parse", "--abbrev-ref", "HEAD"),
                      stdout = TRUE, stderr = FALSE)),
      error = function(e) "unknown")
    commit <- tryCatch(
      trimws(system2("git", c("-C", pkg_root, "log", "-1", "--format=%h"),
                      stdout = TRUE, stderr = FALSE)),
      error = function(e) "unknown")
    date <- tryCatch(
      trimws(system2("git", c("-C", pkg_root, "log", "-1", "--format=%ci"),
                      stdout = TRUE, stderr = FALSE)),
      error = function(e) "unknown")
    return(paste0(branch, " @ ", commit, " (", substr(date, 1, 10), ")"))
  }
  # Installed mode: read baked-in file
  info_file <- system.file("build_info.txt", package = "doe.workbench")
  if (info_file != "" && file.exists(info_file)) {
    return(trimws(readLines(info_file, n = 1, warn = FALSE)))
  }
  # Fallback: package version
  paste0("v", utils::packageVersion("doe.workbench"))
}
