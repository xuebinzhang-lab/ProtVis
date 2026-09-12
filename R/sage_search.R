# Helpers for locating the bundled Sage database-search executable.

#' Locate Sage for database-search workflows.
#' @param path Optional user-selected executable path.
#' @return An executable path, or NULL when Sage is not available.
#' @export
protvis_sage_executable <- function(path = NULL) {
  candidates <- c(
    if (!is.null(path) && length(path) == 1L) as.character(path),
    if (.Platform$OS.type == "windows")
      system.file("extdata", "sage", "windows", "sage.exe", package = "ProtVis"),
    Sys.which("sage")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates)) return(NULL)
  normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
}
