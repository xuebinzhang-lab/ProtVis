# ProtVis_dataset schema, provenance, assay registry, and interoperability.

#' Return the current ProtVis_dataset schema version.
#' @export
protvis_schema_version <- function() "4.0.0"

#' Describe the canonical ProtVis_dataset schema.
#' @export
protvis_schema <- function() {
  list(
    name = "ProtVis_dataset",
    version = protvis_schema_version(),
    primary_assay = "protein",
    assay_levels = c("psm", "peptide", "protein"),
    core_fields = c(
      "expression_data", "sample_info", "variable_info", "annotation",
      "analysis_results", "process_info", "metadata", "other_files",
      "checkpoint_info"
    ),
    annotation_fields = c(
      "eggnog_output", "GO_annotation", "KEGG_annotation"
    ),
    analysis_contract = list(
      mode = "append_only",
      active_matrix = "expression_data",
      module_runs = "analysis_results$<module>$runs",
      latest_pointer = "analysis_results$<module>$latest_run_id",
      preserve_core_matrix = TRUE
    ),
    metaproteomics = list(
      protein_abundance = "expression_data",
      relation_layers = c(
        "psm", "peptide", "protein", "taxonomy", "function", "taxon_function"
      ),
      annotation_domains = c(
        "taxonomy", "GO", "KEGG", "eggNOG", "CAZy", "COG", "EC"
      ),
      peptide_centric = TRUE
    )
  )
}

.protvis_package_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("ProtVis")),
    error = function(e) "development"
  )
}

.protvis_runtime_snapshot <- function() {
  info <- Sys.info()
  session <- utils::sessionInfo()
  package_entries <- c(session$otherPkgs %||% list(), session$loadedOnly %||% list())
  packages <- if (length(package_entries)) {
    vapply(package_entries, function(x) {
      as.character(x$Version %||% NA_character_)
    }, character(1))
  } else character()
  packages <- packages[!duplicated(names(packages))]
  list(
    recorded_at = as.character(Sys.time()),
    R = R.version.string,
    platform = R.version$platform,
    os = paste(
      na.omit(c(
        unname(info[["sysname"]]),
        unname(info[["release"]]),
        unname(info[["machine"]])
      )),
      collapse = " "
    ),
    ProtVis = .protvis_package_version(),
    packages = packages
  )
}

.protvis_default_provenance <- function(source = "user") {
  list(
    schema_version = protvis_schema_version(),
    source = as.character(source %||% "user"),
    environment = .protvis_runtime_snapshot(),
    software = list(
      ProtVis = list(
        version = .protvis_package_version(),
        recorded_at = as.character(Sys.time())
      )
    ),
    files = list(),
    events = list()
  )
}

.protvis_file_fingerprint <- function(path, name = basename(path),
                                      kind = "input") {
  path <- as.character(path %||% "")
  if (length(path) != 1L || !nzchar(path) || !file.exists(path)) return(NULL)
  info <- file.info(path)
  list(
    name = as.character(name),
    kind = as.character(kind),
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    size_bytes = as.numeric(info$size),
    md5 = unname(tools::md5sum(path)),
    modified_at = as.character(info$mtime),
    recorded_at = as.character(Sys.time())
  )
}

.protvis_record_file <- function(dataset, path, name = basename(path),
                                 kind = "input") {
  entry <- .protvis_file_fingerprint(path, name = name, kind = kind)
  if (is.null(entry)) return(dataset)
  provenance <- dataset$metadata$provenance %||%
    .protvis_default_provenance(dataset$metadata$source %||% "user")
  files <- provenance$files %||% list()
  key <- paste(entry$kind, entry$path, sep = "|")
  old_keys <- vapply(files, function(x) {
    paste(x$kind %||% "", x$path %||% "", sep = "|")
  }, character(1))
  if (!key %in% old_keys) files[[length(files) + 1L]] <- entry
  provenance$files <- files
  dataset$metadata$provenance <- provenance
  dataset
}

.protvis_record_software <- function(dataset, name, version = NA_character_,
                                     path = NULL, parameters = list()) {
  provenance <- dataset$metadata$provenance %||%
    .protvis_default_provenance(dataset$metadata$source %||% "user")
  software <- provenance$software %||% list()
  software[[as.character(name)]] <- list(
    version = as.character(version %||% NA_character_),
    path = if (is.null(path)) NA_character_ else as.character(path),
    parameters = parameters %||% list(),
    recorded_at = as.character(Sys.time())
  )
  provenance$software <- software
  dataset$metadata$provenance <- provenance
  dataset
}

.protvis_append_provenance_event <- function(
    dataset, stage, status = "success", parameters = list(), error = NULL,
    message = NULL, started_at = NULL, finished_at = NULL) {
  if (is.null(started_at)) started_at <- Sys.time()
  if (is.null(finished_at)) finished_at <- Sys.time()
  provenance <- dataset$metadata$provenance %||%
    .protvis_default_provenance(dataset$metadata$source %||% "user")
  events <- provenance$events %||% list()
  events[[length(events) + 1L]] <- list(
    id = length(events) + 1L,
    stage = as.character(stage),
    status = as.character(status),
    started_at = as.character(started_at),
    finished_at = as.character(finished_at),
    duration_seconds = as.numeric(
      difftime(finished_at, started_at, units = "secs")
    ),
    parameters = parameters %||% list(),
    message = if (is.null(message)) NA_character_ else as.character(message),
    error = if (is.null(error)) NA_character_ else as.character(error),
    object_name = as.character(
      dataset$metadata$object_name %||% "ProtVis_dataset"
    )
  )
  provenance$events <- events
  provenance$schema_version <- protvis_schema_version()
  provenance$environment <- provenance$environment %||%
    .protvis_runtime_snapshot()
  dataset$metadata$provenance <- provenance
  dataset
}

#' Upgrade a ProtVis_dataset to the current canonical schema.
#'
#' This function is intentionally backward compatible. It enriches legacy
#' objects with schema/provenance metadata without discarding their data.
#' @export
protvis_standardize_dataset <- function(object) {
  object <- as_protvis_dataset(object)
  validate_protvis_dataset(object)

  metadata <- object$metadata %||% list()
  metadata$schema <- protvis_schema()
  metadata$schema_version <- protvis_schema_version()
  metadata$provenance <- metadata$provenance %||%
    .protvis_default_provenance(metadata$source %||% "user")
  metadata$provenance$schema_version <- protvis_schema_version()
  metadata$provenance$environment <-
    metadata$provenance$environment %||% .protvis_runtime_snapshot()
  metadata$provenance$software <-
    metadata$provenance$software %||% list()
  if (is.null(metadata$provenance$software$ProtVis)) {
    metadata$provenance$software$ProtVis <- list(
      version = .protvis_package_version(),
      recorded_at = as.character(Sys.time())
    )
  }
  metadata$provenance$files <- metadata$provenance$files %||% list()
  metadata$provenance$events <- metadata$provenance$events %||% list()
  object$metadata <- metadata
  if (!length(object$metadata$provenance$events) &&
      length(object$process_info$history %||% list())) {
    for (event in object$process_info$history) {
      object <- .protvis_append_provenance_event(
        object,
        stage = event$stage %||% "legacy",
        status = event$status %||% "success",
        parameters = event$parameters %||% list(),
        error = event$error %||% NULL,
        message = event$message %||% "Migrated from legacy process history.",
        started_at = event$started_at %||% event$time %||% Sys.time(),
        finished_at = event$finished_at %||% event$time %||% Sys.time()
      )
    }
  }

  assays <- object$analysis_results$assays %||% list()
  registry <- object$analysis_results$assay_registry %||% list()
  registry$protein <- list(
    level = "protein",
    storage = "expression_data",
    n_features = nrow(object$expression_data),
    n_samples = ncol(object$expression_data),
    updated_at = as.character(Sys.time())
  )
  if (!is.null(assays$peptide)) {
    registry$peptide <- list(
      level = "peptide", storage = "analysis_results$assays$peptide",
      n_rows = NROW(assays$peptide), updated_at = as.character(Sys.time())
    )
  }
  if (!is.null(assays$psm)) {
    registry$psm <- list(
      level = "psm", storage = "analysis_results$assays$psm",
      n_rows = NROW(assays$psm), updated_at = as.character(Sys.time())
    )
  }
  sage <- object$analysis_results$Sage_database_search %||% list()
  if (is.null(assays$psm) && is.data.frame(sage$psms)) {
    registry$psm <- list(
      level = "psm", storage = "analysis_results$Sage_database_search$psms",
      n_rows = nrow(sage$psms), updated_at = as.character(Sys.time())
    )
  }
  object$analysis_results$assay_registry <- registry
  object$version <- protvis_schema_version()
  validate_protvis_dataset(object)
  object
}

#' Extract a canonical assay from a ProtVis_dataset.
#' @param object A ProtVis_dataset.
#' @param level One of protein, peptide, or psm.
#' @export
protvis_assay <- function(object, level = c("protein", "peptide", "psm")) {
  object <- protvis_standardize_dataset(object)
  level <- match.arg(level)
  if (identical(level, "protein")) return(object$expression_data)
  assays <- object$analysis_results$assays %||% list()
  if (!is.null(assays[[level]])) return(assays[[level]])
  sage <- object$analysis_results$Sage_database_search %||% list()
  if (identical(level, "psm") && is.data.frame(sage$psms)) return(sage$psms)
  NULL
}

#' Register a PSM or peptide assay in a ProtVis_dataset.
#' @export
register_protvis_assay <- function(object, level = c("psm", "peptide"),
                                   data, source = NULL) {
  object <- protvis_standardize_dataset(object)
  level <- match.arg(level)
  if (!(is.data.frame(data) || is.matrix(data))) {
    stop("Assay data must be a data.frame or matrix.", call. = FALSE)
  }
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  assays <- object$analysis_results$assays %||% list()
  assays[[level]] <- data
  object$analysis_results$assays <- assays
  registry <- object$analysis_results$assay_registry %||% list()
  registry[[level]] <- list(
    level = level,
    storage = paste0("analysis_results$assays$", level),
    source = as.character(source %||% object$metadata$source %||% "user"),
    n_rows = nrow(data),
    n_columns = ncol(data),
    updated_at = as.character(Sys.time())
  )
  object$analysis_results$assay_registry <- registry
  object <- .protvis_append_process(
    object, paste0("register_", level, "_assay"), status = "success",
    parameters = list(source = source, n_rows = nrow(data), n_columns = ncol(data))
  )
  validate_protvis_dataset(object)
  object
}


# Append one immutable analysis run to a module-specific run store.
# The active expression_data matrix is deliberately untouched. The latest run
# pointer can change, but all previous run payloads remain available.
.protvis_append_analysis_run <- function(
    object, module, run, run_id = NULL, parameters = list()) {
  object <- protvis_standardize_dataset(object)
  module <- trimws(as.character(module %||% "")[[1L]])
  if (!nzchar(module)) {
    stop("module must be a non-empty analysis name.", call. = FALSE)
  }
  if (!is.list(run)) {
    stop("run must be a list.", call. = FALSE)
  }

  root <- object$analysis_results[[module]]
  if (is.null(root)) {
    root <- list()
  } else if (!is.list(root) || is.data.frame(root)) {
    root <- list(legacy = root)
  }
  runs <- root$runs %||% list()
  if (!is.list(runs)) runs <- list(legacy = runs)

  candidate <- as.character(
    run_id %||% run$run_id %||%
      paste0(module, "_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )[[1L]]
  candidate <- gsub("[^A-Za-z0-9_.-]+", "_", candidate)
  if (!nzchar(candidate)) candidate <- paste0(module, "_run")

  final_id <- candidate
  suffix <- 1L
  while (final_id %in% names(runs)) {
    suffix <- suffix + 1L
    final_id <- paste0(candidate, "_v", suffix)
  }

  run$run_id <- final_id
  run$created_at <- as.character(run$created_at %||% Sys.time())
  run$schema_version <- protvis_schema_version()
  runs[[final_id]] <- run

  root$runs <- runs
  root$latest_run_id <- final_id
  root$n_runs <- length(runs)
  root$updated_at <- as.character(Sys.time())
  object$analysis_results[[module]] <- root

  object <- .protvis_append_process(
    object,
    stage = module,
    status = "success",
    parameters = utils::modifyList(
      list(run_id = final_id, storage = "append_only"),
      parameters %||% list()
    )
  )
  object <- .protvis_append_provenance_event(
    object,
    stage = module,
    status = "success",
    parameters = utils::modifyList(
      list(run_id = final_id, storage = "append_only"),
      parameters %||% list()
    ),
    message = paste0("Appended analysis run ", final_id, ".")
  )
  object$metadata$schema <- protvis_schema()
  object$metadata$schema_version <- protvis_schema_version()
  object$version <- protvis_schema_version()
  validate_protvis_dataset(object)
  object
}

#' Return the structured provenance record of a ProtVis_dataset.
#' @export
protvis_provenance <- function(object) {
  object <- protvis_standardize_dataset(object)
  provenance <- object$metadata$provenance
  events <- provenance$events %||% list()
  event_table <- if (!length(events)) {
    data.frame(
      id = integer(), stage = character(), status = character(),
      started_at = character(), finished_at = character(),
      duration_seconds = numeric(), message = character(), error = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(events, function(x) {
      data.frame(
        id = as.integer(x$id %||% NA_integer_),
        stage = as.character(x$stage %||% ""),
        status = as.character(x$status %||% ""),
        started_at = as.character(x$started_at %||% ""),
        finished_at = as.character(x$finished_at %||% ""),
        duration_seconds = as.numeric(x$duration_seconds %||% NA_real_),
        message = as.character(x$message %||% NA_character_),
        error = as.character(x$error %||% NA_character_),
        stringsAsFactors = FALSE
      )
    }))
  }
  files <- provenance$files %||% list()
  file_table <- if (!length(files)) {
    data.frame(
      name = character(), kind = character(), path = character(),
      size_bytes = numeric(), md5 = character(), modified_at = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(files, function(x) {
      data.frame(
        name = as.character(x$name %||% ""),
        kind = as.character(x$kind %||% ""),
        path = as.character(x$path %||% ""),
        size_bytes = as.numeric(x$size_bytes %||% NA_real_),
        md5 = as.character(x$md5 %||% ""),
        modified_at = as.character(x$modified_at %||% ""),
        stringsAsFactors = FALSE
      )
    }))
  }
  list(
    schema = object$metadata$schema,
    environment = provenance$environment,
    software = provenance$software,
    events = event_table,
    files = file_table
  )
}

.protvis_require_qfeatures <- function() {
  needed <- c("QFeatures", "SummarizedExperiment", "S4Vectors")
  missing <- needed[!vapply(
    needed, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)
  )]
  if (length(missing)) {
    stop(
      "QFeatures interoperability requires: ",
      paste(missing, collapse = ", "),
      ". Install them with BiocManager::install().",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Convert a ProtVis_dataset to a Bioconductor QFeatures object.
#' @export
as_QFeatures <- function(object) {
  .protvis_require_qfeatures()
  object <- protvis_standardize_dataset(object)
  sample_info <- object$sample_info
  rownames(sample_info) <- as.character(sample_info$sample_id)
  variable_info <- object$variable_info
  rownames(variable_info) <- as.character(variable_info$protein_id)
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(abundance = as.matrix(object$expression_data)),
    rowData = S4Vectors::DataFrame(variable_info, check.names = FALSE)
  )
  QFeatures::QFeatures(
    list(protein = se),
    colData = S4Vectors::DataFrame(sample_info, check.names = FALSE)
  )
}

#' Convert a QFeatures object to a ProtVis_dataset.
#' @param object A QFeatures object.
#' @param assay Name of the protein-level assay.
#' @export
from_QFeatures <- function(object, assay = "protein") {
  .protvis_require_qfeatures()
  if (!methods::is(object, "QFeatures")) {
    stop("object must inherit from QFeatures.", call. = FALSE)
  }
  assay <- as.character(assay)[[1L]]
  if (!assay %in% names(object)) {
    stop("QFeatures assay not found: ", assay, call. = FALSE)
  }
  se <- object[[assay]]
  expression <- as.data.frame(
    SummarizedExperiment::assay(se),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  sample_info <- as.data.frame(
    SummarizedExperiment::colData(object),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!"sample_id" %in% names(sample_info)) {
    sample_info$sample_id <- rownames(sample_info)
  }
  sample_info <- sample_info[colnames(expression), , drop = FALSE]
  variable_info <- as.data.frame(
    SummarizedExperiment::rowData(se),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!"protein_id" %in% names(variable_info)) {
    variable_info$protein_id <- rownames(expression)
  }
  if (!"variable_id" %in% names(variable_info)) {
    variable_info$variable_id <- rownames(expression)
  }
  variable_info <- variable_info[rownames(expression), , drop = FALSE]
  create_protvis_dataset(
    expression_data = expression,
    sample_info = sample_info,
    variable_info = variable_info,
    metadata = list(
      source = "QFeatures",
      imported_at = as.character(Sys.time()),
      qfeatures_assay = assay
    )
  )
}
