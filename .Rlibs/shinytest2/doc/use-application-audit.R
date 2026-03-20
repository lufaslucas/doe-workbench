## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
# library(shinytest2)
# library(shiny)

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot of a shinyloadtest performance report showing session duration metrics with multiple tabs displaying load testing results, indicating that the app struggles to handle 5 simultaneous users due to large computations"----
knitr::include_graphics("images/shinytest2-loadtest.png")

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot of an optimized shinyloadtest performance report showing dramatically improved performance with caching enabled, demonstrating that shinycannon was able to start over 1000 sessions in 2 minutes"----
knitr::include_graphics("images/shinytest2-shinyloadtest-optimized.png")

