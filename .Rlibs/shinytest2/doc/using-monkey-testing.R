## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(shinytest2)
library(shiny)

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot of a browser developer tools window with three panels: the Shiny application on the left showing a slider input, the DOM elements inspector in the top-right panel, and the JavaScript console in the bottom-right panel"----
knitr::include_graphics("images/gremlins-start.png")

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot of the JavaScript console showing successful injection of the gremlins.js library, displaying the gremlins horde object with various methods and properties available for monkey testing"----
knitr::include_graphics("images/gremlins-inject.png")

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Animated GIF showing gremlins.js monkey testing in action, with random automated interactions being performed on a Shiny app including clicks, scrolls, and other user actions simulated by the gremlins"----
knitr::include_graphics("images/gremlins-attack.gif")

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot of a Shiny app with a slider input being highlighted, demonstrating how to target specific UI elements for focused monkey testing with the toucher species"----
knitr::include_graphics("images/gremlins-slider-handle.png")

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot showing console.log output from gremlins.js monkey testing, displaying detailed logging information that can be captured by shinytest2's get_logs() method for debugging and refining test scripts"----
knitr::include_graphics("images/gremlins-logs.png")

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Animated GIF showing refined and optimized gremlins.js monkey testing focused on specific UI elements, demonstrating more targeted automated testing with customized gremlin species configurations"----
knitr::include_graphics("images/gremlins-attack-refined.gif")

