#' Cross-platform roots for the working-directory chooser
#'
#' @param exclude A character vector of root labels to exclude.
#'
#' @return A named character vector containing accessible directories.
#' @description
#' Returns stable roots for `shinyFiles`. On Windows, drive roots are detected
#' directly instead of parsing the localized output of WMIC or PowerShell.
#' @export
getVolumes_win <- function(exclude = NULL) {
  os_system <- unname(base::Sys.info()[["sysname"]])

  volumes <- switch(
    os_system,
    Darwin = {
      mounted <- tryCatch(
        fs::dir_ls("/Volumes", type = "directory", fail = FALSE),
        error = function(e) character()
      )
      stats::setNames(as.character(mounted), base::basename(mounted))
    },
    Linux = {
      mounted <- character()
      if (isTRUE(fs::dir_exists("/media"))) {
        mounted <- tryCatch(
          fs::dir_ls("/media", type = "directory", recurse = 1L,
                     fail = FALSE),
          error = function(e) character()
        )
      }
      c(Computer = "/",
        stats::setNames(as.character(mounted), base::basename(mounted)))
    },
    Windows = {
      candidates <- paste0(LETTERS, ":/")
      accessible <- vapply(candidates, base::dir.exists, logical(1L))
      drives <- candidates[accessible]
      stats::setNames(drives, paste0(sub("/$", "", drives), " drive"))
    },
    stop("Unsupported operating system: ", os_system, call. = FALSE)
  )

  volumes <- .protvis_sanitize_directory_roots(volumes)
  if (!is.null(exclude)) {
    volumes <- volumes[!names(volumes) %in% as.character(exclude)]
  }
  volumes
}

# Keep only roots that shinyFiles can browse. This also protects the chooser
# from duplicate paths and malformed platform-specific values.
.protvis_sanitize_directory_roots <- function(roots) {
  if (is.null(roots) || !length(roots)) {
    return(stats::setNames(character(), character()))
  }

  root_names <- names(roots)
  roots <- as.character(roots)
  if (is.null(root_names)) root_names <- rep.int("", length(roots))

  keep <- !is.na(roots) & nzchar(roots)
  roots <- path.expand(roots[keep])
  root_names <- root_names[keep]
  accessible <- vapply(roots, base::dir.exists, logical(1L))
  roots <- roots[accessible]
  root_names <- root_names[accessible]
  if (!length(roots)) {
    return(stats::setNames(character(), character()))
  }

  roots <- vapply(
    roots,
    base::normalizePath,
    character(1L),
    winslash = "/",
    mustWork = TRUE,
    USE.NAMES = FALSE
  )
  dedupe_key <- if (identical(.Platform$OS.type, "windows")) {
    tolower(roots)
  } else {
    roots
  }
  unique_root <- !duplicated(dedupe_key)
  roots <- roots[unique_root]
  root_names <- root_names[unique_root]

  unnamed <- is.na(root_names) | !nzchar(root_names)
  root_names[unnamed] <- base::basename(roots[unnamed])
  root_names[!nzchar(root_names)] <- roots[!nzchar(root_names)]
  stats::setNames(roots, make.unique(root_names, sep = " "))
}

.protvis_directory_roots <- function(
    home = fs::path_home(),
    r_home = R.home(),
    system_roots = getVolumes_win()) {
  roots <- .protvis_sanitize_directory_roots(
    c(Home = as.character(home), "R installation" = r_home, system_roots)
  )
  if (!length(roots)) {
    roots <- .protvis_sanitize_directory_roots(c("Current directory" = getwd()))
  }
  roots
}
