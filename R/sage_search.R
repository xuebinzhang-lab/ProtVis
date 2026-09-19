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

.protvis_sage_bundled_candidates <- function() {
  sysname <- unname(Sys.info()[["sysname"]])
  machine <- tolower(unname(Sys.info()[["machine"]]))
  root <- function(...) system.file("extdata", "sage", ..., package = "ProtVis")
  if (identical(.Platform$OS.type, "windows")) {
    return(root("windows", "sage.exe"))
  }
  if (identical(sysname, "Darwin")) {
    architecture <- if (grepl("arm|aarch", machine)) "ARM64" else "Intel"
    return(root("macOS", architecture, "sage"))
  }
  if (identical(sysname, "Linux")) return(root("Linux", "sage"))
  character()
}

.protvis_prepare_sage_executable <- function(path) {
  if (!identical(.Platform$OS.type, "windows") && file.exists(path)) {
    # Source packages can lose the executable bit during transfer.
    try(Sys.chmod(path, mode = "0755"), silent = TRUE)
  }
  path
}

#' Locate Sage for database-search workflows.
#' @param path Optional user-selected executable path.
#' @return An executable path, or NULL when Sage is not available.
#' @export
protvis_sage_executable <- function(path = NULL) {
  path_candidate <- if (!is.null(path) && length(path) == 1L && !is.na(path)) {
    as.character(path)
  } else character()
  candidates <- c(path_candidate, .protvis_sage_bundled_candidates(),
                  unname(Sys.which("sage")), unname(Sys.which("sage.exe")))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates) &
                           file.exists(candidates)]
  if (!length(candidates)) return(NULL)
  selected <- .protvis_prepare_sage_executable(candidates[[1L]])
  .protvis_sage_path(selected, "Sage executable")
}
