# global.R — Bootstrap for DoEWorkbench Shiny app
# When installed as a package, R/ files are auto-loaded by the namespace.
# This file attaches the package + dependency packages that ui.R/server.R
# reference directly (e.g. useShinyjs(), plotlyOutput(), etc.)

suppressPackageStartupMessages({
  library(DoEWorkbench)
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
})

# Optional: officedown for styled Word reports (falls back to plain Word)
has_officedown <- requireNamespace("officedown", quietly = TRUE)
