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

# ── Read-Only Mode ──────────────────────────────────────────────────────
READONLY_HASH_ALGO  <- "sha256"       # digest algorithm for password hashing
