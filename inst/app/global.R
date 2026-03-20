# global.R — Bootstrap for doe.workbench Shiny app

# Show full error messages in-app instead of grey disconnect overlay
options(shiny.sanitize.errors = FALSE)

suppressPackageStartupMessages({
  # In dev mode, set DOE_DEV_ROOT env var before running:
  #   Sys.setenv(DOE_DEV_ROOT = "/path/to/doe-workbench"); shiny::runApp("inst/app")
  # Otherwise loads the installed package.
  dev_root <- Sys.getenv("DOE_DEV_ROOT", unset = "")
  if (nzchar(dev_root) && file.exists(file.path(dev_root, "DESCRIPTION"))) {
    pkgload::load_all(dev_root, export_all = FALSE, helpers = FALSE, quiet = TRUE)
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
