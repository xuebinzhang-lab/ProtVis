# FragPipe integration for ProtVis.
#
# FragPipe itself and several tools in its ecosystem are distributed under
# separate licenses. ProtVis therefore installs/locates the official release at
# runtime instead of redistributing licensed binaries inside the R package.

.protvis_fragpipe_version <- "24.0"

.protvis_fragpipe_default_threads <- function() {
  n <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if (length(n) != 1L || is.na(n) || n < 2L) return(1L)
  max(1L, as.integer(n) - 1L)
}

.protvis_fragpipe_scalar <- function(x, default = "") {
  if (is.null(x) || !length(x) || is.na(x[[1L]])) return(default)
  value <- trimws(as.character(x[[1L]]))
  if (nzchar(value)) value else default
}

.protvis_fragpipe_normalize <- function(path, must_work = FALSE) {
  path <- path.expand(.protvis_fragpipe_scalar(path))
  if (!nzchar(path)) return("")
  normalizePath(path, winslash = "/", mustWork = must_work)
}

.protvis_fragpipe_candidate_roots <- function() {
  version <- .protvis_fragpipe_version
  roots <- c(
    Sys.getenv("PROTVIS_FRAGPIPE_HOME", ""),
    file.path(path.expand("~"), ".protvis", "tools", "fragpipe", version),
    file.path(path.expand("~"), "FragPipe"),
    file.path(path.expand("~"), paste0("FragPipe-", version))
  )
  if (.Platform$OS.type == "windows") {
    roots <- c(
      roots,
      file.path(Sys.getenv("LOCALAPPDATA", ""), "Programs", "FragPipe"),
      file.path(Sys.getenv("PROGRAMFILES", ""), "FragPipe"),
      file.path(Sys.getenv("PROGRAMFILES(X86)", ""), "FragPipe")
    )
  }
  unique(roots[nzchar(roots)])
}

#' Locate the FragPipe headless executable.
#'
#' @param path Optional executable or FragPipe installation directory.
#' @return Normalized executable path or NULL.
#' @export
protvis_fragpipe_executable <- function(path = NULL) {
  supplied <- .protvis_fragpipe_scalar(path)
  if (nzchar(supplied)) {
    supplied <- path.expand(supplied)
    if (file.exists(supplied) && !dir.exists(supplied)) {
      return(normalizePath(supplied, winslash = "/", mustWork = TRUE))
    }
  }

  exe_names <- if (.Platform$OS.type == "windows") {
    c("fragpipe.bat", "fragpipe.cmd", "fragpipe.exe")
  } else {
    c("fragpipe")
  }

  roots <- c(if (nzchar(supplied) && dir.exists(supplied)) supplied,
             .protvis_fragpipe_candidate_roots())
  for (root in unique(roots)) {
    if (!dir.exists(root)) next
    direct <- unique(c(
      file.path(root, "bin", exe_names),
      file.path(root, exe_names)
    ))
    direct <- direct[file.exists(direct) & !dir.exists(direct)]
    if (length(direct)) {
      return(normalizePath(direct[[1L]], winslash = "/", mustWork = TRUE))
    }

    # Limit recursive discovery to FragPipe-specific candidate roots.
    hits <- tryCatch(
      list.files(root, pattern = if (.Platform$OS.type == "windows") {
        "^fragpipe[.](bat|cmd|exe)$"
      } else {
        "^fragpipe$"
      }, recursive = TRUE, full.names = TRUE, ignore.case = TRUE),
      error = function(e) character()
    )
    hits <- hits[file.exists(hits) & !dir.exists(hits)]
    if (length(hits)) {
      score <- order(!grepl("/bin/", gsub("\\\\", "/", hits)),
                     nchar(hits))
      return(normalizePath(hits[score[[1L]]], winslash = "/", mustWork = TRUE))
    }
  }

  on_path <- unname(Sys.which(exe_names))
  on_path <- on_path[nzchar(on_path)]
  if (length(on_path)) {
    return(normalizePath(on_path[[1L]], winslash = "/", mustWork = TRUE))
  }
  NULL
}

.protvis_fragpipe_home <- function(path = NULL) {
  exe <- protvis_fragpipe_executable(path)
  if (is.null(exe)) return(NULL)
  parent <- dirname(exe)
  if (tolower(basename(parent)) == "bin") parent <- dirname(parent)
  normalizePath(parent, winslash = "/", mustWork = TRUE)
}

.protvis_fragpipe_find_one <- function(root, pattern) {
  if (is.null(root) || !dir.exists(root)) return("")
  hits <- tryCatch(
    list.files(root, pattern = pattern, recursive = TRUE,
               full.names = TRUE, ignore.case = TRUE),
    error = function(e) character()
  )
  hits <- hits[file.exists(hits) & !dir.exists(hits)]
  if (!length(hits)) return("")
  normalizePath(hits[[1L]], winslash = "/", mustWork = TRUE)
}

.protvis_fragpipe_version_from_path <- function(path) {
  value <- paste(c(path, dirname(path), basename(dirname(dirname(path)))),
                 collapse = " ")
  hit <- regmatches(value, regexpr("[0-9]+[.][0-9]+([.][0-9]+)?", value))
  if (length(hit) && nzchar(hit)) hit else NA_character_
}

#' Inspect the local FragPipe runtime.
#'
#' @param path Optional executable or installation directory.
#' @return A list describing FragPipe, workflow, and companion-tool status.
#' @export
protvis_fragpipe_status <- function(path = NULL) {
  exe <- protvis_fragpipe_executable(path)
  home <- if (is.null(exe)) NULL else .protvis_fragpipe_home(exe)
  tools_folder <- if (!is.null(home) && dir.exists(file.path(home, "tools"))) {
    normalizePath(file.path(home, "tools"), winslash = "/", mustWork = TRUE)
  } else ""
  workflows <- if (!is.null(home)) {
    list.files(home, pattern = "[.]workflow$", recursive = TRUE,
               full.names = TRUE, ignore.case = TRUE)
  } else character()

  java <- unname(Sys.which("java"))
  python <- unname(Sys.which(c("python3", "python")))
  python <- python[nzchar(python)]
  docker <- unname(Sys.which("docker"))

  msfragger <- .protvis_fragpipe_find_one(tools_folder, "MSFragger.*[.]jar$")
  ionquant <- .protvis_fragpipe_find_one(tools_folder, "IonQuant.*[.]jar$")
  diatracer <- .protvis_fragpipe_find_one(tools_folder, "diaTracer")
  diann <- .protvis_fragpipe_find_one(tools_folder, "(DiaNN[.]exe$|^diann[^/]*$)")

  list(
    installed = !is.null(exe),
    executable = exe %||% "",
    home = home %||% "",
    version = if (!is.null(exe)) .protvis_fragpipe_version_from_path(exe) else NA_character_,
    workflows = normalizePath(workflows, winslash = "/", mustWork = FALSE),
    tools_folder = tools_folder,
    msfragger = msfragger,
    ionquant = ionquant,
    diatracer = diatracer,
    diann = diann,
    java = .protvis_fragpipe_scalar(java),
    python = if (length(python)) normalizePath(python[[1L]], winslash = "/",
                                               mustWork = TRUE) else "",
    docker = .protvis_fragpipe_scalar(docker),
    platform = Sys.info()[["sysname"]] %||% .Platform$OS.type,
    macos_docker_only = identical(Sys.info()[["sysname"]], "Darwin")
  )
}

.protvis_fragpipe_release_asset <- function(version = .protvis_fragpipe_version) {
  api <- paste0(
    "https://api.github.com/repos/Nesvilab/FragPipe/releases/tags/",
    utils::URLencode(as.character(version), reserved = TRUE)
  )
  release <- tryCatch(
    jsonlite::fromJSON(api, simplifyVector = TRUE),
    error = function(e) stop(
      "Unable to query the official FragPipe release metadata: ",
      conditionMessage(e), call. = FALSE
    )
  )
  assets <- release$assets
  if (!is.data.frame(assets) || !nrow(assets)) {
    stop("No downloadable assets were found for FragPipe ", version, ".",
         call. = FALSE)
  }
  pattern <- if (.Platform$OS.type == "windows") {
    paste0("^FragPipe-", version, "-installer[.]exe$")
  } else if (identical(Sys.info()[["sysname"]], "Linux")) {
    paste0("^FragPipe-", version, "-linux[.]zip$")
  } else {
    stop(
      "FragPipe does not provide a native macOS release. Use the official ",
      "FragPipe Docker route or supply a compatible wrapper executable.",
      call. = FALSE
    )
  }
  hit <- grep(pattern, assets$name, ignore.case = TRUE)
  if (!length(hit)) {
    stop("Official FragPipe asset was not found for this platform.", call. = FALSE)
  }
  list(
    name = assets$name[hit[[1L]]],
    url = assets$browser_download_url[hit[[1L]]],
    version = version
  )
}

#' Download/install the official FragPipe release for ProtVis.
#'
#' Linux releases are downloaded and unpacked under ~/.protvis/tools by
#' default. On Windows the official installer is downloaded and launched; the
#' user completes the vendor installer, after which ProtVis detects it.
#'
#' @param version FragPipe release tag.
#' @param directory Parent directory for the ProtVis-managed runtime.
#' @param launch_windows Whether to launch the official Windows installer.
#' @return Installation result and refreshed status.
#' @export
protvis_install_fragpipe <- function(
    version = .protvis_fragpipe_version,
    directory = file.path(path.expand("~"), ".protvis", "tools", "fragpipe"),
    launch_windows = TRUE) {
  asset <- .protvis_fragpipe_release_asset(version)
  directory <- path.expand(directory)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  downloads <- file.path(path.expand("~"), ".protvis", "downloads")
  dir.create(downloads, recursive = TRUE, showWarnings = FALSE)
  archive <- file.path(downloads, asset$name)

  utils::download.file(asset$url, archive, mode = "wb", quiet = FALSE)
  if (!file.exists(archive) || file.info(archive)$size <= 0) {
    stop("FragPipe download failed.", call. = FALSE)
  }

  if (.Platform$OS.type == "windows") {
    launched <- FALSE
    if (isTRUE(launch_windows)) {
      shell.exec(normalizePath(archive, winslash = "\\", mustWork = TRUE))
      launched <- TRUE
    }
    return(list(
      status = if (launched) "installer_launched" else "downloaded",
      installer = normalizePath(archive, winslash = "/", mustWork = TRUE),
      message = paste0(
        "Official FragPipe ", version,
        " Windows installer downloaded",
        if (launched) " and launched." else "."
      ),
      fragpipe = protvis_fragpipe_status()
    ))
  }

  target <- file.path(directory, version)
  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  utils::unzip(archive, exdir = target)
  exe <- protvis_fragpipe_executable(target)
  if (is.null(exe)) {
    stop("FragPipe was unpacked but the headless executable was not found.",
         call. = FALSE)
  }
  Sys.chmod(exe, mode = "0755")
  Sys.setenv(PROTVIS_FRAGPIPE_HOME = .protvis_fragpipe_home(exe))
  list(
    status = "installed",
    executable = exe,
    directory = target,
    message = paste0("FragPipe ", version, " installed for ProtVis."),
    fragpipe = protvis_fragpipe_status(exe)
  )
}

#' Common official FragPipe workflow presets.
#' @export
protvis_fragpipe_workflow_presets <- function() {
  c(
    "LFQ + MBR" = "LFQ-MBR.workflow",
    "Basic search" = "Basic-Search.workflow",
    "LFQ phosphoproteomics" = "LFQ-phospho.workflow",
    "Open search" = "Open.workflow",
    "Quick open search" = "Open-quickscan.workflow",
    "TMT10 phosphoproteomics" = "TMT10-phospho.workflow",
    "DIA spectral library + quantification" = "DIA_SpecLib_Quant.workflow",
    "DIA phosphoproteomics" = "DIA_SpecLib_Quant_Phospho.workflow"
  )
}

#' List installed FragPipe workflow files.
#' @export
protvis_fragpipe_workflows <- function(path = NULL) {
  status <- protvis_fragpipe_status(path)
  files <- status$workflows
  if (!length(files)) {
    return(data.frame(name = character(), path = character(),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    name = basename(files),
    path = files,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.protvis_fragpipe_resolve_workflow <- function(
    workflow, fragpipe_path = NULL,
    cache_dir = file.path(path.expand("~"), ".protvis", "workflows", "fragpipe")) {
  workflow <- .protvis_fragpipe_scalar(workflow, "LFQ-MBR.workflow")
  if (file.exists(path.expand(workflow))) {
    return(normalizePath(path.expand(workflow), winslash = "/", mustWork = TRUE))
  }

  presets <- protvis_fragpipe_workflow_presets()
  if (workflow %in% names(presets)) workflow <- unname(presets[[workflow]])
  if (!grepl("[.]workflow$", workflow, ignore.case = TRUE)) {
    exact <- presets[tolower(names(presets)) == tolower(workflow)]
    if (length(exact)) workflow <- unname(exact[[1L]])
  }

  installed <- protvis_fragpipe_workflows(fragpipe_path)
  hit <- which(tolower(installed$name) == tolower(basename(workflow)))
  if (length(hit)) return(installed$path[hit[[1L]]])

  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  target <- file.path(cache_dir, basename(workflow))
  url <- paste0(
    "https://raw.githubusercontent.com/Nesvilab/FragPipe/master/workflows/",
    utils::URLencode(basename(workflow), reserved = TRUE)
  )
  ok <- tryCatch({
    utils::download.file(url, target, mode = "wb", quiet = TRUE)
    file.exists(target) && file.info(target)$size > 0
  }, error = function(e) FALSE)
  if (!ok) {
    stop(
      "FragPipe workflow was not found locally and could not be downloaded: ",
      workflow, call. = FALSE
    )
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

.protvis_fragpipe_prepare_workflow <- function(
    workflow, fasta, output_directory, fragpipe_path = NULL) {
  fasta <- .protvis_fragpipe_normalize(fasta, must_work = TRUE)
  workflow <- .protvis_fragpipe_resolve_workflow(workflow, fragpipe_path)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  output_directory <- .protvis_fragpipe_normalize(output_directory, TRUE)
  target <- file.path(output_directory, basename(workflow))
  lines <- readLines(workflow, warn = FALSE, encoding = "UTF-8")
  db_line <- paste0("database.db-path=", fasta)
  hit <- grep("^database[.]db-path=", lines)
  if (length(hit)) {
    lines[hit[[1L]]] <- db_line
    if (length(hit) > 1L) lines <- lines[-hit[-1L]]
  } else {
    insert_after <- if (length(lines) && startsWith(lines[[1L]], "#")) 1L else 0L
    lines <- append(lines, db_line, after = insert_after)
  }
  writeLines(lines, target, useBytes = TRUE)
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

.protvis_fragpipe_spectra <- function(directory) {
  directory <- .protvis_fragpipe_normalize(directory, must_work = TRUE)
  files <- list.files(
    directory,
    pattern = "[.](mzML|mzXML|raw|mgf)$",
    full.names = TRUE, ignore.case = TRUE
  )
  dirs <- list.dirs(directory, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[grepl("[.]d$", dirs, ignore.case = TRUE)]
  paths <- unique(c(files, dirs))
  if (!length(paths)) {
    stop(
      "No supported spectra files were found. FragPipe accepts mzML/mzXML, ",
      "vendor RAW formats supported by its runtime, and compatible .d inputs.",
      call. = FALSE
    )
  }
  normalizePath(paths, winslash = "/", mustWork = TRUE)
}

.protvis_fragpipe_match_sample_rows <- function(sample_info, spectra_paths) {
  sample_info <- as.data.frame(sample_info %||% data.frame(),
                               stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(sample_info)) return(rep(NA_integer_, length(spectra_paths)))
  file_col <- names(sample_info)[tolower(names(sample_info)) %in%
    c("mzml_file", "raw_file", "file", "filename", "spectra_file")][1L]
  if (is.na(file_col) || !nzchar(file_col)) return(rep(NA_integer_, length(spectra_paths)))
  match(tolower(basename(spectra_paths)),
        tolower(basename(as.character(sample_info[[file_col]]))))
}

#' Write a FragPipe headless manifest.
#'
#' @param spectra_paths LC-MS file paths.
#' @param path Output manifest path.
#' @param sample_info Optional sample metadata.
#' @param data_type FragPipe manifest data type, usually DDA or DIA.
#' @return Normalized manifest path.
#' @export
protvis_fragpipe_write_manifest <- function(
    spectra_paths, path, sample_info = NULL, data_type = "DDA") {
  spectra_paths <- as.character(spectra_paths %||% character())
  if (!length(spectra_paths) || any(!file.exists(spectra_paths))) {
    stop("FragPipe manifest requires existing spectra paths.", call. = FALSE)
  }
  spectra_paths <- normalizePath(spectra_paths, winslash = "/", mustWork = TRUE)
  sample_info <- as.data.frame(sample_info %||% data.frame(),
                               stringsAsFactors = FALSE, check.names = FALSE)
  idx <- .protvis_fragpipe_match_sample_rows(sample_info, spectra_paths)

  sample_col <- if (nrow(sample_info)) {
    names(sample_info)[tolower(names(sample_info)) %in%
      c("sample_id", "sample", "run")][1L]
  } else NA_character_
  group_col <- if (nrow(sample_info)) {
    names(sample_info)[tolower(names(sample_info)) %in%
      c("group", "condition", "treatment", "experiment")][1L]
  } else NA_character_
  rep_col <- if (nrow(sample_info)) {
    names(sample_info)[tolower(names(sample_info)) %in%
      c("replicate", "bioreplicate", "rep")][1L]
  } else NA_character_

  fallback_sample <- tools::file_path_sans_ext(basename(spectra_paths))
  sample_id <- fallback_sample
  matched <- !is.na(idx)
  if (!is.na(sample_col) && nzchar(sample_col) && any(matched)) {
    sample_id[matched] <- as.character(sample_info[[sample_col]][idx[matched]])
  }

  experiment <- sample_id
  if (!is.na(group_col) && nzchar(group_col) && any(matched)) {
    candidate <- as.character(sample_info[[group_col]][idx[matched]])
    candidate[is.na(candidate) | !nzchar(candidate)] <- sample_id[matched][
      is.na(candidate) | !nzchar(candidate)
    ]
    experiment[matched] <- candidate
  }

  replicate <- ave(seq_along(experiment), experiment, FUN = seq_along)
  if (!is.na(rep_col) && nzchar(rep_col) && any(matched)) {
    candidate <- suppressWarnings(as.integer(sample_info[[rep_col]][idx[matched]]))
    keep <- is.finite(candidate)
    replicate[which(matched)[keep]] <- candidate[keep]
  }

  manifest <- data.frame(
    spectra = spectra_paths,
    experiment = experiment,
    bioreplicate = replicate,
    data_type = rep(toupper(.protvis_fragpipe_scalar(data_type, "DDA")),
                    length(spectra_paths)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(
    manifest, path, sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = FALSE, na = ""
  )
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.protvis_read_fragpipe_table <- function(path) {
  if (is.null(path) || !length(path) || !file.exists(path[[1L]])) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  data.table::fread(path[[1L]], sep = "\t", data.table = FALSE,
                    check.names = FALSE, showProgress = FALSE)
}

.protvis_fragpipe_result_files <- function(output_directory) {
  files <- list.files(output_directory, recursive = TRUE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  by_name <- function(pattern) files[grepl(pattern, basename(files),
                                           ignore.case = TRUE, perl = TRUE)]
  prefer <- function(x, preferred) {
    if (!length(x)) return(character())
    hit <- x[tolower(basename(x)) == tolower(preferred)]
    if (length(hit)) hit[[1L]] else x[[1L]]
  }
  list(
    all = files,
    protein = prefer(by_name("(^|_)protein[.]tsv$|combined_protein[.]tsv$"),
                     "combined_protein.tsv"),
    peptide = prefer(by_name("(^|_)peptide[.]tsv$|combined_peptide[.]tsv$"),
                     "combined_peptide.tsv"),
    psm = prefer(by_name("^psm[.]tsv$"), "psm.tsv")
  )
}

#' Summarize FragPipe search output for the Project Dashboard.
#' @export
protvis_fragpipe_qc <- function(bundle) {
  if (is.null(bundle) || !is.list(bundle)) {
    return(list(
      summary = data.frame(Metric = "Search status", Value = "Not available"),
      per_run = data.frame(stringsAsFactors = FALSE)
    ))
  }
  psms <- bundle$psms %||% data.frame()
  peptides <- bundle$peptides %||% data.frame()
  proteins <- bundle$proteins %||% data.frame()
  generic <- tryCatch(protvis_sage_qc(psms), error = function(e) NULL)
  summary <- data.frame(
    Metric = c("Engine", "Status", "PSMs", "Peptide rows", "Protein rows"),
    Value = c(
      paste0("FragPipe ", bundle$fragpipe_version %||% ""),
      bundle$status %||% "",
      nrow(psms), nrow(peptides), nrow(proteins)
    ),
    stringsAsFactors = FALSE
  )
  if (!is.null(generic) && nrow(generic$summary)) {
    extra <- generic$summary[!generic$summary$Metric %in% summary$Metric, , drop = FALSE]
    summary <- rbind(summary, extra)
  }
  list(
    summary = summary,
    per_run = if (!is.null(generic)) generic$per_run else
      data.frame(stringsAsFactors = FALSE)
  )
}

#' Run FragPipe in official headless mode.
#'
#' @param fasta Protein FASTA path.
#' @param spectra_paths LC-MS input paths.
#' @param output_directory Result directory.
#' @param workflow Workflow path or one of protvis_fragpipe_workflow_presets().
#' @param sample_info Optional sample metadata used to build the manifest.
#' @param data_type FragPipe manifest type (DDA or DIA).
#' @param fragpipe_path Optional executable/installation path.
#' @param tools_folder Optional folder containing MSFragger/IonQuant/diaTracer.
#' @param python Optional Python executable/directory.
#' @param diann Optional DIA-NN executable.
#' @param threads Number of threads.
#' @param ram Maximum RAM in GB; zero lets FragPipe decide.
#' @param dry_run Use FragPipe --dry-run.
#' @return Search bundle with logs, tables, paths, and runtime metadata.
#' @export
run_fragpipe_search <- function(
    fasta, spectra_paths, output_directory,
    workflow = "LFQ + MBR", sample_info = NULL, data_type = "DDA",
    fragpipe_path = NULL, tools_folder = NULL, python = NULL, diann = NULL,
    threads = .protvis_fragpipe_default_threads(),
    ram = 0L, dry_run = FALSE) {
  fasta <- .protvis_fragpipe_normalize(fasta, must_work = TRUE)
  spectra_paths <- as.character(spectra_paths %||% character())
  if (!length(spectra_paths) || any(!file.exists(spectra_paths))) {
    stop("At least one existing LC-MS input is required.", call. = FALSE)
  }
  spectra_paths <- normalizePath(spectra_paths, winslash = "/", mustWork = TRUE)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  output_directory <- .protvis_fragpipe_normalize(output_directory, TRUE)

  exe <- protvis_fragpipe_executable(fragpipe_path)
  if (is.null(exe)) {
    stop(
      "FragPipe headless executable was not found. Install FragPipe from the ",
      "Search > FragPipe tab or provide fragpipe_path.",
      call. = FALSE
    )
  }
  runtime <- protvis_fragpipe_status(exe)
  prepared_workflow <- .protvis_fragpipe_prepare_workflow(
    workflow, fasta, output_directory, exe
  )
  manifest <- protvis_fragpipe_write_manifest(
    spectra_paths,
    file.path(output_directory, "protvis.fp-manifest"),
    sample_info = sample_info, data_type = data_type
  )

  tools_folder <- .protvis_fragpipe_scalar(tools_folder, runtime$tools_folder)
  python <- .protvis_fragpipe_scalar(python, runtime$python)
  diann <- .protvis_fragpipe_scalar(diann, runtime$diann)

  threads <- suppressWarnings(as.integer(threads[[1L]]))
  if (length(threads) != 1L || is.na(threads) || threads < 1L) {
    threads <- .protvis_fragpipe_default_threads()
  }
  ram <- suppressWarnings(as.integer(ram[[1L]]))
  if (length(ram) != 1L || is.na(ram) || ram < 0L) ram <- 0L

  args <- c(
    "--headless",
    "--workflow", shQuote(prepared_workflow),
    "--manifest", shQuote(manifest),
    "--workdir", shQuote(output_directory),
    "--threads", as.character(threads)
  )
  if (ram > 0L) args <- c(args, "--ram", as.character(ram))
  if (nzchar(tools_folder)) {
    args <- c(args, "--config-tools-folder", shQuote(tools_folder))
  }
  if (nzchar(diann)) args <- c(args, "--config-diann", shQuote(diann))
  if (nzchar(python)) args <- c(args, "--config-python", shQuote(python))
  if (isTRUE(dry_run)) args <- c(args, "--dry-run")

  started <- Sys.time()
  log <- tryCatch(
    system2(exe, args = args, stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  exit_status <- attr(log, "status", exact = TRUE) %||% 0L
  log <- as.character(log %||% character())
  result_files <- .protvis_fragpipe_result_files(output_directory)

  bundle <- list(
    status = if (identical(as.integer(exit_status), 0L)) "success" else "failed",
    exit_status = as.integer(exit_status),
    fragpipe_path = exe,
    fragpipe_home = runtime$home,
    fragpipe_version = .protvis_fragpipe_scalar(runtime$version, .protvis_fragpipe_version),
    runtime = runtime,
    workflow = prepared_workflow,
    manifest = manifest,
    fasta = fasta,
    spectra_paths = spectra_paths,
    output_directory = output_directory,
    data_type = data_type,
    parameters = list(
      workflow = basename(prepared_workflow),
      tools_folder = tools_folder,
      python = python,
      diann = diann,
      threads = as.integer(threads),
      ram = as.integer(ram),
      dry_run = isTRUE(dry_run)
    ),
    files = result_files,
    log = log,
    started_at = as.character(started),
    finished_at = as.character(Sys.time()),
    psms = .protvis_read_fragpipe_table(result_files$psm),
    peptides = .protvis_read_fragpipe_table(result_files$peptide),
    proteins = .protvis_read_fragpipe_table(result_files$protein)
  )
  bundle
}

.protvis_attach_fragpipe_bundle <- function(
    dataset, bundle, parameters = list(), fasta = "", spectra_paths = character()) {
  dataset <- as_protvis_dataset(dataset)
  dataset$analysis_results$FragPipe_database_search <- bundle

  dataset <- .protvis_record_software(
    dataset, "FragPipe",
    version = .protvis_fragpipe_scalar(bundle$fragpipe_version, .protvis_fragpipe_version),
    path = bundle$fragpipe_path %||% "",
    parameters = parameters
  )
  if (nzchar(fasta) && file.exists(fasta)) {
    dataset <- .protvis_record_file(
      dataset, fasta, name = basename(fasta), kind = "FASTA database"
    )
  }
  for (path in spectra_paths) {
    if (file.exists(path)) {
      dataset <- .protvis_record_file(
        dataset, path, name = basename(path), kind = "LC-MS search input"
      )
    }
  }
  for (path in c(bundle$workflow, bundle$manifest, bundle$files$all %||% character())) {
    if (length(path) && file.exists(path)) {
      dataset <- .protvis_record_file(
        dataset, path, name = basename(path), kind = "FragPipe search artifact"
      )
    }
  }
  if (is.data.frame(bundle$psms) && nrow(bundle$psms)) {
    dataset <- register_protvis_assay(
      dataset, level = "psm", data = bundle$psms, source = "FragPipe"
    )
  }
  if (is.data.frame(bundle$peptides) && nrow(bundle$peptides)) {
    dataset <- register_protvis_assay(
      dataset, level = "peptide", data = bundle$peptides, source = "FragPipe"
    )
  }
  dataset$metadata$raw_search <- list(
    engine = paste0("FragPipe ", bundle$fragpipe_version %||% ""),
    fasta = fasta,
    spectra_paths = spectra_paths,
    workflow = bundle$workflow,
    manifest = bundle$manifest,
    output_directory = bundle$output_directory
  )
  dataset <- .protvis_append_process(
    dataset, "FragPipe_database_search", status = bundle$status,
    parameters = parameters,
    message = if (identical(bundle$status, "success")) {
      "FragPipe headless workflow completed and search outputs were retained."
    } else {
      "FragPipe headless workflow failed."
    },
    error = if (!identical(bundle$status, "success")) {
      paste(bundle$log, collapse = "\n")
    } else NULL
  )
  protvis_standardize_dataset(dataset)
}

.protvis_create_search_staging_dataset <- function(
    sample_info, fasta = "", spectra_paths = character(), output_directory = getwd()) {
  sample_info <- as.data.frame(sample_info %||% data.frame(),
                               stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(sample_info)) {
    stop("Raw search staging requires sample information.", call. = FALSE)
  }
  spectra_paths <- as.character(spectra_paths %||% character())
  sample_col <- names(sample_info)[tolower(names(sample_info)) == "sample_id"][1L]
  if (is.na(sample_col) || !nzchar(sample_col)) sample_col <- names(sample_info)[[1L]]
  sample_names <- as.character(sample_info[[sample_col]])
  if (anyNA(sample_names) || any(!nzchar(sample_names)) || anyDuplicated(sample_names)) {
    stop("Search staging requires unique sample identifiers.", call. = FALSE)
  }
  placeholder <- matrix(
    NA_real_, nrow = 1L, ncol = length(sample_names),
    dimnames = list("__SEARCH_PENDING__", sample_names)
  )
  metadata <- list(
    source = "Raw database search",
    workflow_stage = "Search_staging",
    output_directory = output_directory
  )
  if (nzchar(fasta)) metadata$raw_fasta <- list(name = basename(fasta), path = fasta)
  if (length(spectra_paths)) {
    metadata$raw_directory <- dirname(spectra_paths[[1L]])
    metadata$raw_spectra_paths <- spectra_paths
  }
  dataset <- create_protvis_dataset(
    placeholder, sample_info = sample_info, metadata = metadata
  )
  dataset$expression_data <- data.frame(row.names = character())
  dataset$variable_info <- .protvis_normalise_variable_info(NULL, character())
  dataset$variable_info_note <- .protvis_normalise_note(
    NULL, names(dataset$variable_info), "variable"
  )
  dataset$metadata$object_name <- "ProtVis_dataset__project_init__Search_staging__v1"
  dataset$metadata$object_version <- 1L
  dataset <- .protvis_append_process(
    dataset, "project_init_search_staging", status = "success",
    parameters = list(fasta = fasta, spectra_paths = spectra_paths),
    message = "Raw search inputs registered; quantitative data will be added after Search."
  )
  validate_protvis_dataset(dataset)
  dataset
}

.protvis_create_fragpipe_dataset <- function(
    bundle, sample_info = NULL, parent = NULL, fasta = "",
    spectra_paths = character(), output_directory = NULL) {
  protein_file <- bundle$files$protein %||% character()
  if (!length(protein_file) || !file.exists(protein_file[[1L]])) {
    if (!inherits(parent, "ProtVis_dataset")) {
      stop(
        "FragPipe completed but no protein report was found for creating ProtVis_dataset.",
        call. = FALSE
      )
    }
    dataset <- .protvis_attach_fragpipe_bundle(
      parent, bundle, bundle$parameters, fasta, spectra_paths
    )
    dataset$metadata$workflow_stage <- "FragPipe_search_complete_no_protein_matrix"
    return(dataset)
  }

  dataset <- tryCatch(
    import_protvis(
      protein_file[[1L]], source = "FragPipe",
      sample_info = sample_info, filename = basename(protein_file[[1L]]),
      auto_export = FALSE
    ),
    error = function(e) import_protvis(
      protein_file[[1L]], source = "FragPipe",
      sample_info = NULL, filename = basename(protein_file[[1L]]),
      auto_export = FALSE
    )
  )
  if (inherits(parent, "ProtVis_dataset")) {
    parent_metadata <- parent$metadata %||% list()
    dataset$metadata <- utils::modifyList(parent_metadata, dataset$metadata)
    dataset$process_info$history <- c(
      parent$process_info$history %||% list(),
      dataset$process_info$history %||% list()
    )
    dataset$other_files <- c(
      parent$other_files %||% list(),
      dataset$other_files %||% list()
    )
  }
  dataset <- .protvis_attach_fragpipe_bundle(
    dataset, bundle, bundle$parameters, fasta, spectra_paths
  )
  dataset$metadata$workflow_stage <- "FragPipe_complete"
  dataset$metadata$output_directory <- output_directory %||% bundle$output_directory
  dataset$metadata$parent_object_name <- if (inherits(parent, "ProtVis_dataset")) {
    parent$metadata$object_name %||% NA_character_
  } else NA_character_
  dataset$metadata$object_name <- "ProtVis_dataset__FragPipe_database_search__v2"
  dataset$metadata$object_version <- 2L
  validate_protvis_dataset(dataset)
  dataset
}

#' FragPipe Search UI.
#' @export
fragpipe_search_ui <- function(id) {
  ns <- shiny::NS(id)
  presets <- protvis_fragpipe_workflow_presets()
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      width = 410,
      tags$h4("FragPipe", class = "text-primary"),
      tags$p(
        "Run official FragPipe headless workflows from ProtVis. The runtime is installed or located separately so licensed companion binaries are not redistributed by ProtVis.",
        class = "text-muted small"
      ),
      shiny::textInput(
        ns("fragpipe_path"), "FragPipe path (optional)",
        placeholder = "Auto-detect, or paste FragPipe/bin/fragpipe(.bat)"
      ),
      bslib::layout_columns(
        shiny::actionButton(
          ns("refresh"), "Refresh", icon = bsicons::bs_icon("arrow-clockwise"),
          class = "btn btn-outline-secondary w-100"
        ),
        shiny::actionButton(
          ns("install"), "Install FragPipe", icon = bsicons::bs_icon("download"),
          class = "btn btn-outline-primary w-100"
        ),
        col_widths = c(6, 6)
      ),
      shiny::uiOutput(ns("runtime_status")),
      shiny::selectInput(
        ns("workflow"), "Workflow",
        choices = stats::setNames(unname(presets), names(presets)),
        selected = "LFQ-MBR.workflow"
      ),
      shiny::fileInput(
        ns("custom_workflow"), "Custom workflow (optional)",
        accept = c(".workflow")
      ),
      shiny::selectInput(ns("data_type"), "Manifest data type",
                         choices = c("DDA", "DIA"), selected = "DDA"),
      shiny::numericInput(
        ns("threads"), "Threads",
        value = .protvis_fragpipe_default_threads(),
        min = 1, step = 1
      ),
      shiny::numericInput(
        ns("ram"), "Maximum RAM (GB; 0 = automatic)",
        value = 0, min = 0, step = 1
      ),
      shiny::textInput(ns("tools_folder"), "Tools folder (optional)",
                       placeholder = "Auto-detect FragPipe/tools"),
      shiny::textInput(ns("python"), "Python (optional)",
                       placeholder = "Auto-detect python3/python"),
      shiny::textInput(ns("diann"), "DIA-NN executable (optional)",
                       placeholder = "Needed by DIA workflows"),
      shiny::checkboxInput(ns("dry_run"), "FragPipe dry run", FALSE),
      shiny::actionButton(
        ns("run"), "Run FragPipe Search",
        icon = bsicons::bs_icon("play-fill"),
        class = "btn-primary w-100 pv-run-button"
      )
    ),
    bslib::layout_columns(
      bslib::card(
        bslib::card_header("Search status"),
        bslib::card_body(shiny::uiOutput(ns("status")))
      ),
      bslib::card(
        bslib::card_header("Runtime components"),
        bslib::card_body(DT::DTOutput(ns("runtime_table")))
      ),
      bslib::card(
        bslib::card_header("FragPipe log"),
        bslib::card_body(shiny::verbatimTextOutput(ns("log")))
      ),
      bslib::card(
        bslib::card_header("Search results"),
        bslib::card_body(
          bslib::navset_tab(
            bslib::nav_panel("QC Summary", DT::DTOutput(ns("qc"))),
            bslib::nav_panel("PSMs", DT::DTOutput(ns("psms"))),
            bslib::nav_panel("Peptides", DT::DTOutput(ns("peptides"))),
            bslib::nav_panel("Proteins", DT::DTOutput(ns("proteins"))),
            bslib::nav_panel("Files", DT::DTOutput(ns("files")))
          )
        )
      ),
      col_widths = c(4, 8, 12, 12)
    )
  )
}

#' FragPipe Search server.
#' @export
fragpipe_search_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(bundle = NULL, install = NULL, refresh = 0L)

    requested_path <- shiny::reactive({
      rv$refresh
      .protvis_fragpipe_scalar(input$fragpipe_path)
    })
    runtime <- shiny::reactive({
      protvis_fragpipe_status(requested_path())
    })

    fasta_path <- shiny::reactive({
      value <- .protvis_fragpipe_scalar(shared_state$raw_fasta$path %||% "")
      if (!nzchar(value) && inherits(shared_state$dataset, "ProtVis_dataset")) {
        raw_fasta <- shared_state$dataset$metadata$raw_fasta %||% ""
        if (is.list(raw_fasta)) raw_fasta <- raw_fasta$path %||% ""
        value <- .protvis_fragpipe_scalar(raw_fasta)
      }
      value
    })
    input_directory <- shiny::reactive({
      value <- .protvis_fragpipe_scalar(shared_state$raw_directory %||% "")
      if (!nzchar(value) && inherits(shared_state$dataset, "ProtVis_dataset")) {
        value <- .protvis_fragpipe_scalar(
          shared_state$dataset$metadata$raw_directory %||% ""
        )
      }
      value
    })
    workdir <- shiny::reactive({
      candidates <- c(
        .protvis_fragpipe_scalar(shared_state$workdir %||% ""),
        if (inherits(shared_state$dataset, "ProtVis_dataset")) {
          .protvis_fragpipe_scalar(
            shared_state$dataset$metadata$output_directory %||% ""
          )
        } else "",
        input_directory(),
        getwd()
      )
      candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
      normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
    })

    output$runtime_status <- shiny::renderUI({
      s <- runtime()
      if (isTRUE(s$macos_docker_only) && !isTRUE(s$installed)) {
        return(tags$p(
          "macOS: official FragPipe support is Docker/headless. Configure a compatible FragPipe wrapper or Docker workflow.",
          class = "text-warning small"
        ))
      }
      if (isTRUE(s$installed)) {
        tags$p(
          paste0("Ready: ", s$executable),
          class = "text-success small"
        )
      } else {
        tags$p(
          "FragPipe was not detected. Click Install FragPipe or provide a path.",
          class = "text-warning small"
        )
      }
    })

    output$runtime_table <- DT::renderDT({
      s <- runtime()
      values <- data.frame(
        Component = c(
          "FragPipe", "Version", "Workflows", "Tools folder",
          "MSFragger", "IonQuant", "diaTracer", "DIA-NN", "Java", "Python"
        ),
        Status = c(
          if (s$installed) "Ready" else "Missing",
          s$version %||% "",
          length(s$workflows),
          if (nzchar(s$tools_folder)) "Found" else "Not found",
          if (nzchar(s$msfragger)) "Found" else "Not found",
          if (nzchar(s$ionquant)) "Found" else "Not found",
          if (nzchar(s$diatracer)) "Found" else "Not found",
          if (nzchar(s$diann)) "Found" else "Not found",
          if (nzchar(s$java)) "Found" else "Managed/not on PATH",
          if (nzchar(s$python)) "Found" else "Not found"
        ),
        Path = c(
          s$executable, "", paste(length(s$workflows), "workflow files"),
          s$tools_folder, s$msfragger, s$ionquant, s$diatracer,
          s$diann, s$java, s$python
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(values, rownames = FALSE,
                    options = list(dom = "t", scrollX = TRUE))
    })

    output$status <- shiny::renderUI({
      b <- rv$bundle
      if (is.null(b)) {
        return(tags$div(
          tags$p("Ready to run after Project init registers FASTA and LC-MS inputs.",
                 class = "text-muted"),
          tags$p(tags$b("FASTA: "), fasta_path(), class = "small"),
          tags$p(tags$b("Input directory: "), input_directory(), class = "small"),
          tags$p(tags$b("Output: "), file.path(workdir(), "FragPipe_search"),
                 class = "small")
        ))
      }
      tags$p(
        paste0(
          "Status: ", b$status,
          "; PSM rows: ", nrow(b$psms),
          "; peptide rows: ", nrow(b$peptides),
          "; protein rows: ", nrow(b$proteins)
        ),
        class = if (identical(b$status, "success")) "text-success" else "text-danger"
      )
    })

    output$log <- shiny::renderText({
      b <- rv$bundle
      if (is.null(b)) "No FragPipe search has been run." else
        paste(b$log, collapse = "\n")
    })
    output$qc <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b)) return(data.frame(Message = "Run FragPipe to display QC."))
      DT::datatable(protvis_fragpipe_qc(b)$summary, rownames = FALSE,
                    options = list(dom = "t"))
    })
    output$psms <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b) || !nrow(b$psms)) return(data.frame(Message = "No PSM table available."))
      DT::datatable(b$psms, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    output$peptides <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b) || !nrow(b$peptides)) return(data.frame(Message = "No peptide table available."))
      DT::datatable(b$peptides, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    output$proteins <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b) || !nrow(b$proteins)) return(data.frame(Message = "No protein table available."))
      DT::datatable(b$proteins, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    output$files <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b)) return(data.frame(Message = "No FragPipe outputs yet."))
      paths <- c(
        workflow = b$workflow, manifest = b$manifest,
        b$files$all %||% character()
      )
      DT::datatable(
        data.frame(file = basename(paths), path = paths,
                   stringsAsFactors = FALSE),
        rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$refresh, {
      rv$refresh <- rv$refresh + 1L
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$install, {
      tryCatch({
        result <- shiny::withProgress(
          message = "Preparing FragPipe", value = 0.2,
          {
            x <- protvis_install_fragpipe()
            shiny::incProgress(0.7)
            x
          }
        )
        rv$install <- result
        if (!is.null(result$executable) && nzchar(result$executable)) {
          shiny::updateTextInput(session, "fragpipe_path",
                                 value = result$executable)
        }
        rv$refresh <- rv$refresh + 1L
        shiny::showNotification(result$message, type = "message", duration = 8)
      }, error = function(e) {
        shiny::showNotification(
          paste("FragPipe installation failed:", conditionMessage(e)),
          type = "error", duration = NULL
        )
      })
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      if (!.protvis_begin_run(shared_state, "fragpipe_search", session, "run")) {
        shiny::showNotification(
          "FragPipe Search is already running; duplicate click ignored.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      on.exit(
        .protvis_end_run(shared_state, "fragpipe_search", session, "run"),
        add = TRUE
      )
      shared_state$fragpipe_workflow <- TRUE

      tryCatch({
        fasta <- fasta_path()
        directory <- input_directory()
        if (!nzchar(fasta) || !file.exists(fasta)) {
          stop("Register a readable protein FASTA before running FragPipe.",
               call. = FALSE)
        }
        if (!nzchar(directory) || !dir.exists(directory)) {
          stop("Register an accessible LC-MS input directory before running FragPipe.",
               call. = FALSE)
        }
        spectra <- if (isTRUE(shared_state$raw_check$valid %||% FALSE) &&
                       is.data.frame(shared_state$raw_manifest) &&
                       nrow(shared_state$raw_manifest)) {
          as.character(shared_state$raw_manifest$path)
        } else {
          .protvis_fragpipe_spectra(directory)
        }
        spectra <- spectra[file.exists(spectra)]
        sample_info <- shared_state$raw_sample_info %||% shared_state$sample_info
        workflow <- if (!is.null(input$custom_workflow) &&
                        file.exists(input$custom_workflow$datapath)) {
          input$custom_workflow$datapath
        } else {
          input$workflow
        }
        out <- file.path(workdir(), "FragPipe_search")

        bundle <- shiny::withProgress(
          message = "Running FragPipe headless workflow", value = 0.1,
          {
            value <- run_fragpipe_search(
              fasta = fasta,
              spectra_paths = spectra,
              output_directory = out,
              workflow = workflow,
              sample_info = sample_info,
              data_type = input$data_type,
              fragpipe_path = requested_path(),
              tools_folder = input$tools_folder,
              python = input$python,
              diann = input$diann,
              threads = input$threads,
              ram = input$ram,
              dry_run = isTRUE(input$dry_run)
            )
            shiny::incProgress(0.7)
            value
          }
        )
        rv$bundle <- bundle
        shared_state$fragpipe_search_bundle <- bundle
        shared_state$fragpipe_search_parameters <- bundle$parameters

        if (!identical(bundle$status, "success")) {
          stop(paste(c("FragPipe search failed.", bundle$log), collapse = "\n"),
               call. = FALSE)
        }

        parent <- shared_state$dataset
        dataset <- .protvis_create_fragpipe_dataset(
          bundle = bundle,
          sample_info = sample_info,
          parent = if (inherits(parent, "ProtVis_dataset")) parent else NULL,
          fasta = fasta,
          spectra_paths = spectra,
          output_directory = out
        )
        dataset <- protvis_auto_export_dataset(
          dataset, directory = workdir(), include_raw = FALSE
        )
        .protvis_ui_sync_state(dataset, shared_state)
        .protvis_save_stage_dataset(
          dataset, file.path(workdir(), "Step2_fragpipe_database_search.rda")
        )
        if (nrow(dataset$expression_data) > 0L) {
          .protvis_save_stage_dataset(
            dataset, file.path(workdir(), "Step2_remove_unreliable_peptide.rda")
          )
        }
        shared_state$fragpipe_search_bundle <- NULL
        shared_state$fragpipe_search_parameters <- list()

        shiny::showNotification(
          if (nrow(dataset$expression_data) > 0L) {
            "FragPipe completed; PSM, peptide and protein results were integrated into ProtVis_dataset."
          } else {
            "FragPipe completed, but this workflow did not produce a protein matrix for downstream quantification."
          },
          type = "message", duration = 7
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("FragPipe search failed:", conditionMessage(e)),
          type = "error", duration = NULL
        )
      })
    }, ignoreInit = TRUE)
  })
}
