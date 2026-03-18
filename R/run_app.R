#' Launch the DoE Workbench Shiny App
#'
#' @param port Port to run on (default: 4564)
#' @param launch.browser Whether to open a browser window (default: TRUE)
#' @param ... Additional arguments passed to [shiny::runApp()]
#' @export
run_doe_workbench <- function(port = 4564L, launch.browser = TRUE, ...) {
  app_dir <- system.file("app", package = "DoEWorkbench")
  if (app_dir == "") {
    # Development mode: run from source
    app_dir <- here::here()
  }
  shiny::runApp(app_dir, port = port, launch.browser = launch.browser, ...)
}

#' @rdname run_doe_workbench
#' @export
run_app <- run_doe_workbench
