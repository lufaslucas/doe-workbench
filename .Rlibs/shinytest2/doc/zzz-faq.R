## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## -----------------------------------------------------------------------------
# if (isTRUE(getOption("shiny.testmode"))) {
#    # Do something special here
# }

## -----------------------------------------------------------------------------
# shinyApp(
#   ui = fluidPage(
#     verbatimTextOutput("random")
#   ),
#   server = function(input, output, session) {
#     output$random <- snapshotPreprocessOutput(
#       renderText({
#         paste("This is a random number:", rnorm(1))
#       }),
#       function(value) {
#         sub("[0-9.]+$", "<a random number>", value)
#       }
#     )
#   }
# )

## -----------------------------------------------------------------------------
# test_that("Bookmark works", {
#   # Start local app in the background in test mode
#   bg_app <- shinytest2::AppDriver$new("path/to/shiny/app")
#   # Capture the background app's URL and add appropriate query parameters
#   bookmark_url <- paste0(bg_app$get_url(), "?_inputs_&n=10")
#   # Open the bookmark URL in a new AppDriver object
#   app <- shinytest2::AppDriver$new(bookmark_url)
# 
#   # Run your tests on the bookmarked `app`
#   app$expect_values()
# })

