setwd("/Users/dipesh/Documents/AI/DataAnalysisApp")
port <- as.integer(Sys.getenv("PORT", "4562"))
shiny::runApp("/Users/dipesh/Documents/AI/DataAnalysisApp", port = port, launch.browser = FALSE)
