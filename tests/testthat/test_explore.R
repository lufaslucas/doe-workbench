# tests/testthat/test_explore.R
# Runs all explore sub-tab tests

for (f in list.files("explore", pattern = "^test_.*\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}
