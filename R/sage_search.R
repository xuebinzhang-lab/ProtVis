# Helpers for locating the bundled Sage database-search executable.

.protvis_sage_path <- function(path, label, must_work = TRUE) {
  if (is.null(path) || length(path) != 1L || is.na(path)) {
    stop(label, " path is invalid. Select exactly one readable path.",
         call. = FALSE)
  }
  path <- trimws(as.character(path))
  if (!nzchar(path)) {
    stop(label, " path is empty. Select a readable path.", call. = FALSE)
  }
  path <- path.expand(path)
  if (must_work && !file.exists(path)) {
    stop(label, " path does not exist: ", path, call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = must_work)
}

#' Locate Sage for database-search workflows.
#' @param path Optional user-selected executable path.
#' @return An executable path, or NULL when Sage is not available.
#' @export
protvis_sage_executable <- function(path = NULL) {
  candidates <- c(
    if (!is.null(path) && length(path) == 1L && !is.na(path)) as.character(path),
    if (.Platform$OS.type == "windows")
      system.file("extdata", "sage", "windows", "sage.exe", package = "ProtVis"),
    Sys.which("sage")
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates) &
                           file.exists(candidates)]
  if (!length(candidates)) return(NULL)
  .protvis_sage_path(candidates[[1L]], "Sage executable")
}
