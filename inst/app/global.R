# global.R — Bootstrap for doe.workbench Shiny app
# Dev mode: load_all() sources R/ files directly (no install needed).
# Installed mode: falls back to library(doe.workbench).

# Show full error messages in-app instead of grey disconnect overlay
options(shiny.sanitize.errors = FALSE)

suppressPackageStartupMessages({
  # Use load_all() in dev (when DESCRIPTION + R/ exist at package root), else installed package
  pkg_root <- tryCatch(here::here(), error = function(e) NULL)
  if (!is.null(pkg_root) && file.exists(file.path(pkg_root, "DESCRIPTION")) &&
      file.exists(file.path(pkg_root, "R"))) {
    pkgload::load_all(pkg_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
  } else {
    library(doe.workbench)
  }
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(DT)
  library(plotly)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(car)
  library(emmeans)
  library(multcomp)
  library(readxl)
  library(rmarkdown)
  library(flextable)
  library(RColorBrewer)
  library(base64enc)
  library(digest)
  library(openxlsx)
})

# Optional: officedown for styled Word reports (falls back to plain Word)
has_officedown <- requireNamespace("officedown", quietly = TRUE)
