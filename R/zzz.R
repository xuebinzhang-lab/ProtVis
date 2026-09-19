# Apply the upload limit as soon as the package namespace is loaded. This is
# required for launchers that do not call run_ProtVis() explicitly.
.onLoad <- function(libname, pkgname) {
  current <- getOption("shiny.maxRequestSize", 0)
  if (!is.numeric(current) || length(current) != 1L || !is.finite(current)) {
    current <- 0
  }
  options(shiny.maxRequestSize = max(current, 2 * 1024^3))
}
