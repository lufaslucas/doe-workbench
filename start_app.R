here::i_am("start_app.R")
port <- as.integer(Sys.getenv("PORT", "4562"))
shiny::runApp(here::here(), port = port, launch.browser = TRUE)
