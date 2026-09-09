# Clean installer for ProtVis -------------------------------------------------
# Run this file in a fresh R session before calling library(ProtVis).

if ("ProtVis" %in% loadedNamespaces()) {
  stop(
    "ProtVis is loaded in this R session. Restart R, do not call ",
    "library(ProtVis), and run this installer again.",
    call. = FALSE
  )
}

protvis_library <- Sys.getenv("PROTVIS_LIBRARY", unset = "")
if (!nzchar(protvis_library)) {
  existing_install <- find.package("ProtVis", quiet = TRUE)
  protvis_library <- if (nzchar(existing_install)) {
    dirname(existing_install)
  } else {
    .libPaths()[[1L]]
  }
}
protvis_library <- normalizePath(
  protvis_library, winslash = "/", mustWork = FALSE
)

if (!dir.exists(protvis_library)) {
  dir.create(protvis_library, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(protvis_library) || file.access(protvis_library, 2L) != 0L) {
  stop("R library is not writable: ", protvis_library, call. = FALSE)
}

protvis_target <- file.path(protvis_library, "ProtVis")
protvis_lock <- file.path(protvis_library, "00LOCK-ProtVis")

# Guard the recursive deletion so it can never target the library itself.
if (!identical(basename(protvis_target), "ProtVis") ||
    identical(normalizePath(protvis_target, winslash = "/", mustWork = FALSE),
              protvis_library)) {
  stop("Refusing to clean an unexpected installation path.", call. = FALSE)
}

if (dir.exists(protvis_target)) {
  unlink(protvis_target, recursive = TRUE, force = TRUE)
}
if (dir.exists(protvis_lock)) {
  unlink(protvis_lock, recursive = TRUE, force = TRUE)
}
if (dir.exists(protvis_target) || dir.exists(protvis_lock)) {
  stop(
    "Could not remove the previous ProtVis installation. Close all R/RStudio ",
    "sessions that use it and retry.",
    call. = FALSE
  )
}

if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", lib = protvis_library)
}

remotes::install_github(
  "xuebinzhang-lab/ProtVis",
  ref = "dev",
  lib = protvis_library,
  force = TRUE,
  upgrade = "never"
)

installed_path <- find.package(
  "ProtVis", lib.loc = protvis_library, quiet = TRUE
)
if (!nzchar(installed_path)) {
  stop("ProtVis installation did not produce an installed package.",
       call. = FALSE)
}

lazy_databases <- file.path(installed_path, "R", c("ProtVis.rdb", "ProtVis.rdx"))
if (!all(file.exists(lazy_databases))) {
  stop(
    "Installation verification failed: the package lazy-load database is ",
    "incomplete. Please report the R version and installation log.",
    call. = FALSE
  )
}

# Verify the new database in a clean process. Calling body() forces the
# exported launcher promise to be decompressed instead of merely registering
# the namespace, which detects the exact R_decompress1 failure reported by
# interrupted or overwritten installations.
probe_file <- tempfile("protvis_install_probe_", fileext = ".R")
on.exit(unlink(probe_file, force = TRUE), add = TRUE)
writeLines(
  c(
    paste0("protvis_library <- ", deparse(protvis_library)),
    "library(ProtVis, lib.loc = protvis_library)",
    "launcher <- getExportedValue('ProtVis', 'run_ProtVis')",
    "stopifnot(is.function(launcher), is.call(body(launcher)) || is.expression(body(launcher)))"
  ),
  probe_file,
  useBytes = TRUE
)
rscript <- file.path(
  R.home("bin"),
  if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
)
probe_output <- system2(
  rscript,
  c("--vanilla", shQuote(probe_file)),
  stdout = TRUE,
  stderr = TRUE
)
probe_status <- attr(probe_output, "status")
if (is.null(probe_status)) probe_status <- 0L
if (!identical(as.integer(probe_status), 0L)) {
  stop(
    "ProtVis was installed but failed the clean-process load test:\n",
    paste(probe_output, collapse = "\n"),
    call. = FALSE
  )
}

installed_description <- read.dcf(file.path(installed_path, "DESCRIPTION"))
message(
  "ProtVis ", installed_description[1L, "Version"],
  " installed successfully in ", installed_path,
  ". The lazy-load database passed a clean-process verification. Restart R ",
  "before loading the package."
)
