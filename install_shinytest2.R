if (!requireNamespace("shinytest2", quietly = TRUE)) {
  install.packages("shinytest2", repos = "https://cran.r-project.org")
}
if (!requireNamespace("chromote", quietly = TRUE)) {
  install.packages("chromote", repos = "https://cran.r-project.org")
}
cat("shinytest2:", requireNamespace("shinytest2", quietly = TRUE), "\n")
cat("chromote:", requireNamespace("chromote", quietly = TRUE), "\n")
