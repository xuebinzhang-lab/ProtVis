# Checkpoint, recovery, and export helpers for ProtVis_dataset.

.protvis_checkpoint_stage_order <- c(
  "creation", "import", "noise_correction", "transformation", "imputation",
  "normalization", "dimensionality_reduction", "differential_analysis",
  "enrichment", "network"
)

.protvis_safe_file_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_.-]+", "_", as.character(x))
  x <- gsub("_+", "_", x)
  if (!nzchar(x)) "stage" else x
}

.protvis_checkpoint_files <- function(directory) {
  if (is.null(directory) || !nzchar(as.character(directory)) ||
      !dir.exists(directory)) return(character())
  list.files(
    directory,
    pattern = "^protvis_checkpoint_.*[.]rds$",
    full.names = TRUE,
    ignore.case = TRUE
  )
}

# Read a checkpoint written by the current format, while retaining a safe
# migration path for older releases that used save() with an .rds suffix.
.protvis_read_checkpoint_file <- function(path) {
  result <- tryCatch(
    as_protvis_dataset(readRDS(path)),
    error = function(e) NULL
  )
  if (!is.null(result)) return(result)

  workspace <- new.env(parent = emptyenv())
  loaded <- tryCatch(load(path, envir = workspace), error = function(e) NULL)
  if (is.null(loaded) || length(loaded) == 0L) {
    stop("The checkpoint is not a readable RDS or legacy workspace file.",
         call. = FALSE)
  }
  values <- mget(loaded, envir = workspace, inherits = FALSE)
  candidates <- values[vapply(values, function(x) {
    inherits(x, "ProtVis_dataset") || inherits(x, "mass_dataset")
  }, logical(1))]
  if (length(candidates) != 1L) {
    stop("The legacy checkpoint does not contain exactly one ProtVis_dataset.",
         call. = FALSE)
  }
  as_protvis_dataset(candidates[[1L]])
}

# Save one exact ProtVis_dataset per stage file. The temporary-file validation
# prevents a partially written workspace from replacing a valid stage.
.protvis_save_stage_dataset <- function(dataset, path) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  directory <- dirname(path)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Unable to create stage directory: ", directory, call. = FALSE)
  }
  temporary <- tempfile(pattern = ".protvis_stage_", tmpdir = directory,
                        fileext = ".rda")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  workspace <- new.env(parent = emptyenv())
  workspace$ProtVis_dataset <- dataset
  save(
    list = "ProtVis_dataset", envir = workspace, file = temporary,
    compress = TRUE, version = 3
  )

  check <- new.env(parent = emptyenv())
  loaded <- load(temporary, envir = check)
  if (!identical(loaded, "ProtVis_dataset")) {
    stop("Stage file must contain exactly one ProtVis_dataset object.",
         call. = FALSE)
  }
  if (!methods::is(check$ProtVis_dataset, "ProtVis_dataset")) {
    stop("Stage file is not a ProtVis_dataset.", call. = FALSE)
  }
  validate_protvis_dataset(check$ProtVis_dataset)
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) {
      stop("Unable to publish stage file: ", path, call. = FALSE)
    }
    unlink(temporary, force = TRUE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Read current one-object stages and transparently migrate historical stages
# that stored expression/sample data as separate workspace variables.
.protvis_load_stage_dataset <- function(
    path,
    expression_names = c(
      "normalized_data", "imputed_df", "transformed", "correct_noise_result",
      "expression_matrix_filtered", "expression_matrix"
    ),
    sample_info_names = "sample_info",
    metadata = list()) {
  if (!file.exists(path)) return(NULL)
  workspace <- new.env(parent = emptyenv())
  loaded <- load(path, envir = workspace)
  values <- mget(loaded, envir = workspace, inherits = FALSE)
  object_index <- vapply(values, function(value) {
    inherits(value, "ProtVis_dataset") || inherits(value, "mass_dataset")
  }, logical(1))
  if (any(object_index)) {
    preferred <- match("ProtVis_dataset", names(values))
    object <- if (!is.na(preferred) && object_index[[preferred]]) {
      values[[preferred]]
    } else {
      values[[which(object_index)[[1L]]]]
    }
    return(as_protvis_dataset(object))
  }

  expression_candidates <- expression_names[expression_names %in% loaded]
  expression_name <- if (length(expression_candidates)) {
    expression_candidates[[1L]]
  } else {
    NULL
  }
  if (is.null(expression_name)) return(NULL)
  sample_candidates <- sample_info_names[sample_info_names %in% loaded]
  sample_name <- if (length(sample_candidates)) sample_candidates[[1L]] else NULL
  sample_info <- if (is.null(sample_name)) NULL else values[[sample_name]]
  migration_metadata <- utils::modifyList(
    list(
      source = "legacy_stage",
      migrated_from = basename(path),
      object_name = paste0(
        "ProtVis_dataset__legacy_stage__",
        .protvis_object_label(tools::file_path_sans_ext(basename(path))),
        "__v1"
      ),
      object_version = 1L
    ),
    metadata
  )
  object <- create_protvis_dataset(
    expression_data = values[[expression_name]],
    sample_info = sample_info,
    metadata = migration_metadata
  )
  .protvis_append_process(
    object, "legacy_stage_migration", status = "success",
    parameters = list(file = basename(path), expression = expression_name)
  )
}

#' Resolve the output directory used by automatic dataset persistence.
#'
#' An explicitly supplied directory wins; otherwise the current R working
#' directory is used. The directory is created on demand.
#' @export
protvis_output_directory <- function(directory = NULL) {
  directory <- directory %||% getwd()
  if (length(directory) != 1L || is.na(directory) ||
      !nzchar(trimws(as.character(directory)))) directory <- getwd()
  directory <- normalizePath(path.expand(as.character(directory)),
                             winslash = "/", mustWork = FALSE)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE,
                                             showWarnings = FALSE)) {
    stop("Unable to create ProtVis output directory: ", directory,
         call. = FALSE)
  }
  directory
}

.protvis_checkpoint_metadata <- function(path) {
  info <- tryCatch({
    object <- .protvis_read_checkpoint_file(path)
    cp <- object$checkpoint_info %||% list()
    stage <- cp$stage %||% sub(
      "^protvis_checkpoint_([^_]+).*", "\\1", basename(path)
    )
    list(
      path = normalizePath(path, winslash = "/", mustWork = FALSE),
      name = basename(path),
      stage = as.character(stage),
      created_at = as.character(cp$created_at %||%
                                  file.info(path)$mtime),
      size_bytes = as.numeric(file.info(path)$size),
      md5 = unname(tools::md5sum(path)),
      valid = TRUE
    )
  }, error = function(e) {
    list(
      path = normalizePath(path, winslash = "/", mustWork = FALSE),
      name = basename(path),
      stage = "unreadable",
      created_at = as.character(file.info(path)$mtime),
      size_bytes = as.numeric(file.info(path)$size),
      md5 = NA_character_,
      valid = FALSE,
      error = conditionMessage(e)
    )
  })
  info
}

#' Save a validated dataset as an atomically published checkpoint.
#'
#' @param dataset A ProtVis_dataset.
#' @param directory Directory for checkpoint files.
#' @param stage Processing stage label.
#' @param keep Maximum number of checkpoint files retained.
#' @return The published checkpoint path.
#' @export
save_protvis_checkpoint <- function(dataset, directory = NULL, stage = "manual",
                                    keep = 20L) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  directory <- protvis_output_directory(directory)
  keep <- max(1L, as.integer(keep[[1L]] %||% 20L))
  stage <- as.character(stage[[1L]] %||% "manual")
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  path <- file.path(
    directory,
    paste0("protvis_checkpoint_", .protvis_safe_file_name(stage), "_",
           timestamp, "_", sprintf("%06d", sample.int(999999L, 1L)), ".rds")
  )
  cp <- dataset$checkpoint_info %||% list()
  cp$stage <- stage
  cp$created_at <- as.character(Sys.time())
  cp$directory <- directory
  cp$version <- as.integer((cp$version %||% 0L) + 1L)
  cp$last_successful_stage <- stage
  dataset$checkpoint_info <- cp
  temporary <- tempfile(pattern = ".protvis_checkpoint_", tmpdir = directory,
                        fileext = ".tmp")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(dataset, temporary, compress = TRUE)
  if (!file.rename(temporary, path)) {
    if (!file.copy(temporary, path, overwrite = TRUE)) {
      stop("Unable to publish checkpoint: ", path, call. = FALSE)
    }
    unlink(temporary, force = TRUE)
  }

  files <- .protvis_checkpoint_files(directory)
  if (length(files) > keep) {
    mtimes <- file.info(files)$mtime
    old <- files[order(mtimes, decreasing = TRUE)][seq.int(keep + 1L,
                                                             length(files))]
    unlink(old, force = TRUE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' List valid and unreadable ProtVis checkpoint files.
#' @export
list_protvis_checkpoints <- function(directory) {
  files <- .protvis_checkpoint_files(directory)
  if (length(files) == 0L) {
    return(data.frame(
      path = character(), name = character(), stage = character(),
      created_at = character(), size_bytes = numeric(), md5 = character(),
      valid = logical(), stringsAsFactors = FALSE
    ))
  }
  values <- lapply(files, .protvis_checkpoint_metadata)
  columns <- unique(unlist(lapply(values, names)))
  out <- lapply(columns, function(column) {
    values_col <- vapply(values, function(value) {
      current <- value[[column]]
      if (is.null(current) || length(current) == 0L) NA_character_
      else as.character(current[[1L]])
    }, character(1))
    if (column == "valid") as.logical(values_col) else values_col
  })
  names(out) <- columns
  out <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  out$size_bytes <- suppressWarnings(as.numeric(out$size_bytes))
  out$valid <- as.logical(out$valid)
  out <- out[order(as.POSIXct(out$created_at, tz = "UTC"), decreasing = TRUE,
                   na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.protvis_stage_rank <- function(stage) {
  rank <- match(as.character(stage), .protvis_checkpoint_stage_order)
  ifelse(is.na(rank), length(.protvis_checkpoint_stage_order) + 1L, rank)
}

#' Restore a ProtVis dataset from a checkpoint path or directory.
#'
#' @param path_or_directory A checkpoint RDS file or directory.
#' @param stage Optional stage. If unavailable, the latest earlier checkpoint
#'   is used so resume remains possible after an interrupted run.
#' @param version latest, or a positive version index from newest to oldest.
#' @return A validated ProtVis_dataset.
#' @export
restore_protvis_checkpoint <- function(path_or_directory, stage = NULL,
                                        version = "latest") {
  if (length(path_or_directory) != 1L || is.na(path_or_directory)) {
    stop("Provide one checkpoint path or directory.", call. = FALSE)
  }
  target <- path.expand(as.character(path_or_directory))
  if (dir.exists(target)) {
    table <- list_protvis_checkpoints(target)
    table <- table[table$valid %in% TRUE, , drop = FALSE]
    if (nrow(table) == 0L) stop("No readable ProtVis checkpoints found.",
                                call. = FALSE)
    if (!is.null(stage) && nzchar(as.character(stage))) {
      requested <- as.character(stage[[1L]])
      exact <- table[table$stage == requested, , drop = FALSE]
      if (nrow(exact) > 0L) {
        table <- exact
      } else {
        # Select the newest checkpoint at or before the requested stage.
        earlier <- table[.protvis_stage_rank(table$stage) <=
                           .protvis_stage_rank(requested), , drop = FALSE]
        if (nrow(earlier) > 0L) table <- earlier
      }
    }
    if (identical(version, "latest") || is.null(version)) {
      target <- table$path[[1L]]
    } else {
      index <- suppressWarnings(as.integer(version[[1L]]))
      if (is.na(index) || index < 1L || index > nrow(table)) {
        stop("Checkpoint version must be latest or a valid positive index.",
             call. = FALSE)
      }
      target <- table$path[[index]]
    }
  }
  if (!file.exists(target)) stop("Checkpoint file does not exist: ", target,
                                call. = FALSE)
  object <- tryCatch(.protvis_read_checkpoint_file(target), error = function(e) {
    stop("Unable to read checkpoint: ", conditionMessage(e), call. = FALSE)
  })
  validate_protvis_dataset(object)
  object$checkpoint_info$restored_from <- normalizePath(
    target, winslash = "/", mustWork = FALSE
  )
  object$checkpoint_info$restored_at <- as.character(Sys.time())
  object
}

#' Attach an imported file to the dataset provenance record.
#' @export
attach_protvis_file <- function(dataset, path, name = basename(path),
                                kind = "imported") {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  if (length(path) != 1L || !file.exists(path)) {
    stop("Attached file does not exist.", call. = FALSE)
  }
  entry <- list(
    path = normalizePath(path, winslash = "/", mustWork = FALSE),
    name = as.character(name),
    kind = as.character(kind),
    size_bytes = as.numeric(file.info(path)$size),
    md5 = unname(tools::md5sum(path)),
    attached_at = as.character(Sys.time())
  )
  files <- dataset$other_files %||% list()
  files[[length(files) + 1L]] <- entry
  dataset$other_files <- files
  .protvis_append_process(dataset, "attach_file", status = "success",
                          parameters = list(name = entry$name, kind = entry$kind))
}

.protvis_export_value <- function(value, path) {
  tryCatch({
    saveRDS(value, path, compress = TRUE)
    TRUE
  }, error = function(e) {
    writeLines(c("Unable to serialize this result:", conditionMessage(e)), path)
    FALSE
  })
}

#' Export a dataset and its provenance into a portable directory.
#' @export
export_protvis_dataset <- function(dataset, directory, include_raw = TRUE) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  if (is.null(directory) || !nzchar(as.character(directory))) {
    stop("An export directory is required.", call. = FALSE)
  }
  root <- file.path(
    path.expand(as.character(directory)),
    paste0(.protvis_safe_file_name(dataset$metadata$object_name %||%
                                     "ProtVis_dataset"), "_export_",
           format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  if (!dir.create(root, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(root)) {
    stop("Unable to create export directory.", call. = FALSE)
  }
  saveRDS(dataset, file.path(root, "ProtVis_dataset.rds"), compress = TRUE)
  utils::write.csv(protvis_expression_matrix(dataset),
                   file.path(root, "expression_data.csv"), row.names = FALSE)
  utils::write.csv(dataset$sample_info, file.path(root, "sample_info.csv"),
                   row.names = FALSE)
  utils::write.csv(dataset$variable_info, file.path(root, "variable_info.csv"),
                   row.names = FALSE)
  utils::write.csv(protvis_history(dataset),
                   file.path(root, "process_history.csv"), row.names = FALSE)
  dir.create(file.path(root, "analysis_results"), showWarnings = FALSE)
  result_names <- names(dataset$analysis_results)
  if (length(result_names) > 0L) {
    for (name in result_names) {
      .protvis_export_value(
        dataset$analysis_results[[name]],
        file.path(root, "analysis_results",
                  paste0(.protvis_safe_file_name(name), ".rds"))
      )
    }
  }
  if (isTRUE(include_raw) && length(dataset$other_files) > 0L) {
    raw_dir <- file.path(root, "other_files")
    dir.create(raw_dir, showWarnings = FALSE)
    for (entry in dataset$other_files) {
      source <- entry$path %||% ""
      if (nzchar(source) && file.exists(source)) {
        file.copy(source, file.path(raw_dir, basename(entry$name %||% source)),
                  overwrite = TRUE)
      }
    }
  }
  writeLines(c(
    "{",
    paste0('  "object": "ProtVis_dataset",'),
    paste0('  "created_at": "', as.character(Sys.time()), '",'),
    paste0('  "proteins": ', nrow(dataset$expression_data), ","),
    paste0('  "samples": ', ncol(dataset$expression_data)),
    "}"
  ), file.path(root, "metadata.json"))
  write_protvis_report(dataset, file.path(root, "report.html"))
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

#' Automatically persist a ProtVis_dataset and its portable export bundle.
#'
#' @param dataset A ProtVis_dataset.
#' @param directory Output directory; defaults to getwd().
#' @param include_raw Whether to copy the original imported files.
#' @return The updated dataset, invisibly carrying the output paths.
#' @export
protvis_auto_export_dataset <- function(dataset, directory = NULL,
                                        include_raw = FALSE) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  directory <- protvis_output_directory(directory)
  exported <- export_protvis_dataset(dataset, directory,
                                     include_raw = include_raw)
  dataset$metadata$output_directory <- directory
  dataset$metadata$auto_export_directory <- exported
  dataset$metadata$auto_exported_at <- as.character(Sys.time())
  dataset$checkpoint_info$output_directory <- directory
  dataset$checkpoint_info$latest_export <- exported
  saveRDS(dataset, file.path(exported, "ProtVis_dataset.rds"), compress = TRUE)
  dataset
}
