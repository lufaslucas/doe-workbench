.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "DoE Workbench v", utils::packageVersion(pkgname), "\n",
    "Run the app with:  doe.workbench::run_app()"
  )
}
