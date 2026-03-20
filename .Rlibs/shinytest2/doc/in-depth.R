## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, eval = FALSE)

## -----------------------------------------------------------------------------
# app <- AppDriver$new()

## -----------------------------------------------------------------------------
# app$set_inputs(check_group = c("1", "2"))
# app$set_inputs(check_group = c("1", "2", "3"))
# app$click("action")
# app$expect_values()
# 
# app$click("action")
# app$expect_values()

## -----------------------------------------------------------------------------
# app$set_inputs(check_group = c("1", "2"))
# app$set_inputs(check_group = c("1", "2", "3"))
# app$click("action")

## -----------------------------------------------------------------------------
# app$set_inputs(check_group = c("1", "2", "3"))
# app$click("action")

## -----------------------------------------------------------------------------
# app$expect_values()

## -----------------------------------------------------------------------------
# app <- AppDriver$new(expect_values_screenshot_args = FALSE)

## -----------------------------------------------------------------------------
# app$expect_values(screenshot_args = FALSE)

## -----------------------------------------------------------------------------
# app$expect_values(output = c("a", "b"))

## -----------------------------------------------------------------------------
# app$expect_values(
#   input = "n",
#   output = c("a", "b"),
#   export = c("e1", "e2")
# )

## -----------------------------------------------------------------------------
# app$expect_values(output = TRUE)

## -----------------------------------------------------------------------------
# app$expect_values(output = TRUE, export = TRUE)

## -----------------------------------------------------------------------------
# shinyApp(
#   fluidPage(
#     numericInput("x", "x", 4),
#     numericInput("y", "y", 10),
#     numericInput("z", "z", 100),
#     verbatimTextOutput("result", placeholder = TRUE)
#   ),
#   function(input, output, session) {
#     xy <- reactive(input$x * input$y)
#     yz <- reactive(input$y + input$z)
# 
#     output$result <- renderText({
#       xy() / yz()
#     })
# 
#     exportTestValues(
#       xy = {
#         xy()
#       },
#       yz = {
#         yz()
#       }
#     )
#   }
# )

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot of a Shiny app showing the shinytest2 test recorder with exported values visible in the app's code, demonstrating how exportTestValues() is used to expose internal reactive values for testing"----
knitr::include_graphics("images/screenshot-exports-app.png")

## -----------------------------------------------------------------------------
#     exportTestValues(
#       xy = {
#         xy()
#       },
#       yz = {
#         yz()
#       }
#     )

## -----------------------------------------------------------------------------
# Sys.sleep(0.5)

## ----echo=FALSE, eval=TRUE, out.width='100%', fig.align='center', fig.alt="Screenshot of the test recorder interface showing the random seed configuration field where users can set a seed value to ensure reproducible test results for applications using random number generation"----
knitr::include_graphics("images/screenshot-recorder-random-seed.png")

## -----------------------------------------------------------------------------
# app <- AppDriver$new(seed = 4323)

## -----------------------------------------------------------------------------
# tabsetPanel(id = "tabs", ....)

## -----------------------------------------------------------------------------
# navbarPage(id = "tabs", ....)

## -----------------------------------------------------------------------------
# app$uploadFile(file1 = "mtcars.csv")

## -----------------------------------------------------------------------------
# vals <- app$get_values()
# 
# str(vals)
# #> List of 3
# #>  $ input :List of 4
# #>   ..$ action    :Classes 'integer', 'shinyActionButtonValue'  int 0
# #>   ..$ checkbox  : logi TRUE
# #>   ..$ check_group: chr "1"
# #>   ..$ text      : chr "Enter text..."
# #>  $ output:List of 12
# #>   ..$ action_out    : chr "[1] 0\nattr(,\"class\")\n[1] \"integer\"                #> \"shinyActionButtonValue\""
# #>   ..$ checkbox_out  : chr "[1] TRUE"
# #>   ..$ check_group_out: chr "[1] \"1\""
# #>   ..$ text_out      : chr "[1] \"Enter text...\""
# #>  $ export: Named list()

## -----------------------------------------------------------------------------
# vals <- app$get_values()
# expect_identical(vals$output$checkbox_out, "[1] TRUE")
# 
# # Or in a single line:
# expect_identical(app$get_value(output = "checkbox_out"), "[1] TRUE")

## -----------------------------------------------------------------------------
# if (isTRUE(getOption("shiny.testmode"))) {
#   # Load static/dummy data here
# } else {
#   # Load normal dynamic data here
# }

## -----------------------------------------------------------------------------
# app$set_inputs(table_rows_selected = 1, allow_no_input_binding_ = TRUE)
# app$set_inputs(table_row_last_clicked = 1, allow_no_input_binding_ = TRUE)

## -----------------------------------------------------------------------------
# app$set_inputs(
#   table_rows_selected = 1,
#   table_row_last_clicked = 1,
#   allow_no_input_binding_ = TRUE
# )

## -----------------------------------------------------------------------------
# # helper function that wraps `set_inputs`, with  the default value for the 'dataset' component
# update_dataset <- function(app, value, component_id = "dataset") {
#   checkmate::assert_choice(value, c("rock", "pressure", "cars"))
#   ml <- rlang::list2()
#   ml[[component_id]] <- value
#   app$set_inputs(!!!ml)
# }
# 
# app_dir <- system.file("examples/02_text", package = "shiny")
# app <- shinytest2::AppDriver$new(app_dir)
# update_dataset(app, "rock")

