# tests/testthat.R — Bootstrap for testthat test suite
library(testthat)

# Source all R files from the package
pkg_dir <- normalizePath(file.path(dirname(dirname(sys.frame(1)$ofile %||% ".")), "."))
r_files <- list.files(file.path(pkg_dir, "R"), pattern = "\\.R$", full.names = TRUE)
for (f in r_files) source(f, local = FALSE)

test_check("DoEWorkbench")
