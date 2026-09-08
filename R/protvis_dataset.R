# ProtVis_dataset: a small, explicit, reproducible container for proteomics
# data, sample/variable metadata, analysis results, and provenance.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

.protvis_safe_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  value <- gsub(",", "", as.character(x), fixed = TRUE)
  value <- trimws(value)
  value[value %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  suppressWarnings(as.numeric(value))
}

.protvis_clean_sample_name <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^Reporter intensity corrected[[:space:]]*", "", x,
            ignore.case = TRUE)
  x <- gsub("^Reporter intensity[[:space:]]*", "", x,
            ignore.case = TRUE)
  x <- gsub("^LFQ intensity[[:space:]:_]*", "", x,
            ignore.case = TRUE)
  x <- gsub("^MSstats Area[[:space:]:_]*", "", x,
            ignore.case = TRUE)
  x <- gsub("^PG[.]Quantity[[:space:]:_]*", "", x, ignore.case = TRUE)
  x <- gsub("^PG[.]Normalized[[:space:]:_]*", "", x, ignore.case = TRUE)
  x <- gsub("^Precursor[.]Normalised[[:space:]:_]*", "", x,
            ignore.case = TRUE)
  x <- gsub("^Intensity[[:space:]:_]*", "", x, ignore.case = TRUE)
  x <- gsub("^map_[[:space:]:_]*", "", x, ignore.case = TRUE)
  x <- gsub("^Abundances?([[:space:]]*\\([^)]*\\))?[[:space:]:_]*", "",
            x, ignore.case = TRUE)
  x <- gsub("^Area[[:space:]:_]*", "", x, ignore.case = TRUE)
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("[^A-Za-z0-9_.-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^[_ .-]+|[_ .-]+$", "", x)
  x[x == "" | is.na(x)] <- "sample"
  x
}

.protvis_make_ids <- function(x, prefix = "P") {
  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_
  fallback <- paste0(prefix, seq_along(x))
  x[is.na(x)] <- fallback[is.na(x)]
  make.unique(x, sep = "_")
}

.protvis_find_column <- function(columns, patterns) {
  columns <- as.character(columns)
  for (pattern in patterns) {
    hit <- columns[grepl(pattern, columns, ignore.case = TRUE, perl = TRUE)]
    if (length(hit) > 0L) return(hit[[1L]])
  }
  NULL
}

.protvis_as_data_frame <- function(x) {
  if (is.data.frame(x)) return(as.data.frame(x, check.names = FALSE,
                                               stringsAsFactors = FALSE))
  if (is.matrix(x)) return(as.data.frame(x, check.names = FALSE,
                                         stringsAsFactors = FALSE))
  stop("Expression data must be a data.frame or matrix.", call. = FALSE)
}

.protvis_coerce_expression <- function(expression_data) {
  df <- .protvis_as_data_frame(expression_data)
  id_col <- .protvis_find_column(
    names(df),
    c("^ID$", "^protein_id$", "^protein[ ._-]*id$", "^Protein IDs?$",
      "^accession$", "^ProteinName$", "^Protein$")
  )
  if (!is.null(id_col)) {
    ids <- as.character(df[[id_col]])
    df[[id_col]] <- NULL
  } else {
    ids <- rownames(df)
    if (is.null(ids) || length(ids) != nrow(df) ||
        all(is.na(ids)) || anyDuplicated(ids)) {
      ids <- paste0("P", seq_len(nrow(df)))
    }
  }
  if (ncol(df) == 0L) {
    stop("Expression data must contain at least one sample column.",
         call. = FALSE)
  }
  names(df) <- make.unique(.protvis_clean_sample_name(names(df)), sep = "_")
  for (column in names(df)) df[[column]] <- .protvis_safe_numeric(df[[column]])
  ids <- trimws(as.character(ids))
  ids[is.na(ids) | ids == ""] <- NA_character_
  fallback <- paste0("P", seq_along(ids))
  ids[is.na(ids)] <- fallback[is.na(ids)]
  keep <- !is.na(ids) & nzchar(ids)
  df <- df[keep, , drop = FALSE]
  ids <- ids[keep]
  if (nrow(df) == 0L) stop("Expression data contain no valid protein IDs.",
                            call. = FALSE)

  if (!anyDuplicated(ids)) {
    rownames(df) <- ids
    return(df)
  }
  # Collapse duplicate protein identifiers without changing sample order.
  groups <- split(seq_len(nrow(df)), ids)
  out <- lapply(groups, function(index) {
    values <- lapply(df, function(column) {
      if (all(is.na(column[index]))) NA_real_
      else mean(column[index], na.rm = TRUE)
    })
    as.data.frame(values, check.names = FALSE, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, out)
  rownames(out) <- names(groups)
  out
}

.protvis_normalise_sample_info <- function(sample_info, samples) {
  samples <- as.character(samples)
  if (is.null(sample_info)) {
    return(data.frame(
      sample_id = samples,
      maxquant_id = samples,
      group = rep("Unassigned", length(samples)),
      batch = rep(NA_character_, length(samples)),
      condition = rep(NA_character_, length(samples)),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  info <- .protvis_as_data_frame(sample_info)
  if (nrow(info) == 0L) {
    return(.protvis_normalise_sample_info(NULL, samples))
  }
  sample_col <- .protvis_find_column(
    names(info),
    c("^sample_id$", "^sample$", "sample[ ._-]*name", "^run$", "^file",
      "raw[ ._-]*file", "replicate")
  )
  if (is.null(sample_col)) sample_col <- names(info)[[1L]]
  group_col <- .protvis_find_column(
    names(info),
    c("^group$", "experimental[ ._-]*group", "^condition$",
      "treatment", "class", "phenotype")
  )
  batch_col <- .protvis_find_column(names(info), c("^batch$", "batch[ ._-]*id"))
  condition_col <- .protvis_find_column(names(info), c("^condition$", "state"))
  maxquant_col <- .protvis_find_column(
    names(info),
    c("^maxquant_id$", "maxquant", "raw[ ._-]*file", "^file$", "^run$")
  )
  raw_ids <- trimws(as.character(info[[sample_col]]))
  cleaned_ids <- .protvis_clean_sample_name(raw_ids)
  if (!any(cleaned_ids %in% samples) &&
      !any(raw_ids %in% samples) && nrow(info) == length(samples)) {
    cleaned_ids <- samples
  }
  candidate_ids <- if (any(raw_ids %in% samples)) raw_ids else cleaned_ids
  row_index <- match(samples, candidate_ids)
  if (all(is.na(row_index)) && nrow(info) == length(samples)) {
    row_index <- seq_len(nrow(info))
  }

  result <- data.frame(
    sample_id = samples,
    maxquant_id = samples,
    group = rep("Unassigned", length(samples)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!is.null(maxquant_col)) {
    values <- trimws(as.character(info[[maxquant_col]]))
    values[is.na(values) | values == ""] <- raw_ids[is.na(values) | values == ""]
    result$maxquant_id <- ifelse(
      is.na(row_index), samples, values[row_index]
    )
  }
  if (!is.null(group_col)) {
    values <- as.character(info[[group_col]])
    result$group <- ifelse(
      is.na(row_index) | is.na(values[row_index]) | values[row_index] == "",
      "Unassigned", values[row_index]
    )
  }
  if (!is.null(batch_col)) {
    values <- as.character(info[[batch_col]])
    result$batch <- ifelse(is.na(row_index), NA_character_, values[row_index])
  } else {
    result$batch <- NA_character_
  }
  if (!is.null(condition_col)) {
    values <- as.character(info[[condition_col]])
    result$condition <- ifelse(is.na(row_index), NA_character_, values[row_index])
  } else {
    result$condition <- NA_character_
  }

  # Preserve additional metadata columns, aligned to expression columns.
  extras <- setdiff(names(info), c(sample_col, group_col, batch_col,
                                    condition_col, maxquant_col))
  for (column in extras) {
    values <- info[[column]]
    result[[column]] <- ifelse(is.na(row_index), NA, values[row_index])
  }
  result
}

.protvis_normalise_variable_info <- function(variable_info, protein_ids) {
  protein_ids <- as.character(protein_ids)
  if (is.null(variable_info)) {
    return(data.frame(
      protein_id = protein_ids,
      accession = protein_ids,
      gene = NA_character_,
      description = NA_character_,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  info <- .protvis_as_data_frame(variable_info)
  if (nrow(info) == 0L) return(.protvis_normalise_variable_info(NULL,
                                                                 protein_ids))
  id_col <- .protvis_find_column(
    names(info),
    c("^protein_id$", "^ID$", "^Protein IDs?$", "^accession$",
      "^ProteinName$", "^Protein$")
  )
  if (is.null(id_col)) {
    ids <- rownames(info)
    if (is.null(ids) || length(ids) != nrow(info)) ids <- protein_ids[seq_len(
      min(length(protein_ids), nrow(info))
    )]
  } else {
    ids <- as.character(info[[id_col]])
  }
  ids <- .protvis_make_ids(ids)
  if (length(ids) == length(protein_ids) &&
      !any(ids %in% protein_ids) && nrow(info) == length(protein_ids)) {
    ids <- protein_ids
  }
  info$protein_id <- ids
  if (!is.null(id_col) && id_col != "protein_id") info[[id_col]] <- NULL
  info <- info[!duplicated(info$protein_id), , drop = FALSE]
  index <- match(protein_ids, info$protein_id)
  result <- data.frame(protein_id = protein_ids, stringsAsFactors = FALSE,
                       check.names = FALSE)
  for (column in setdiff(names(info), "protein_id")) {
    values <- info[[column]]
    result[[column]] <- values[index]
  }
  if (!"accession" %in% names(result)) result$accession <- protein_ids
  if (!"gene" %in% names(result)) result$gene <- NA_character_
  if (!"description" %in% names(result)) result$description <- NA_character_
  result
}

.protvis_default_note <- function(kind) {
  if (identical(kind, "variable")) {
    return(data.frame(
      Name = c("protein_id", "accession", "gene", "description"),
      Meaning = c("Stable protein identifier", "Protein accession",
                  "Gene symbol", "Protein description"),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    Name = c("sample_id", "group", "batch", "condition"),
    Meaning = c("Sample identifier", "Experimental group", "Batch",
                "Experimental condition"),
    stringsAsFactors = FALSE
  )
}

.protvis_append_process <- function(dataset, stage, status = "success",
                                    parameters = list(), error = NULL,
                                    message = NULL, started_at = NULL,
                                    finished_at = NULL) {
  stopifnot(inherits(dataset, "ProtVis_dataset"))
  if (is.null(started_at)) started_at <- Sys.time()
  if (is.null(finished_at)) finished_at <- Sys.time()
  if (is.null(dataset$process_info) || !is.list(dataset$process_info)) {
    dataset$process_info <- list()
  }
  history <- dataset$process_info$history %||% list()
  event <- list(
    id = length(history) + 1L,
    stage = as.character(stage),
    status = as.character(status),
    parameters = parameters %||% list(),
    time = as.character(finished_at),
    started_at = as.character(started_at),
    finished_at = as.character(finished_at),
    duration_seconds = as.numeric(difftime(finished_at, started_at,
                                            units = "secs")),
    error = if (is.null(error)) NA_character_ else as.character(error),
    message = if (is.null(message)) NA_character_ else as.character(message)
  )
  history[[length(history) + 1L]] <- event
  dataset$process_info$history <- history
  dataset$process_info$parameters <- dataset$process_info$parameters %||% list()
  dataset$process_info$time <- dataset$process_info$time %||% list()
  dataset$process_info$parameters[[as.character(stage)]] <- parameters %||% list()
  dataset$process_info$time[[as.character(stage)]] <- finished_at
  dataset$process_info$active_stage <- as.character(stage)
  dataset$process_info$last_status <- as.character(status)
  if (!is.null(error)) {
    errors <- dataset$process_info$errors %||% list()
    errors[[length(errors) + 1L]] <- event
    dataset$process_info$errors <- errors
  }
  dataset
}

#' Create a ProtVis dataset object.
#'
#' @param expression_data A protein-by-sample data.frame or matrix. An ID
#'   column is accepted and is moved to row names.
#' @param sample_info Optional sample metadata.
#' @param variable_info Optional protein metadata.
#' @param variable_info_note, sample_info_note Optional documentation tables.
#' @param annotation Optional annotation list or table.
#' @param metadata Optional project metadata list.
#' @param other_files Optional list of imported-file metadata.
#' @return An object of class ProtVis_dataset.
#' @export
create_protvis_dataset <- function(expression_data, sample_info = NULL,
                                   variable_info = NULL,
                                   variable_info_note = NULL,
                                   sample_info_note = NULL,
                                   annotation = list(),
                                   metadata = list(),
                                   other_files = list()) {
  expression_data <- .protvis_coerce_expression(expression_data)
  ids <- rownames(expression_data)
  samples <- colnames(expression_data)
  sample_info <- .protvis_normalise_sample_info(sample_info, samples)
  variable_info <- .protvis_normalise_variable_info(variable_info, ids)
  if (!is.list(metadata)) stop("metadata must be a list.", call. = FALSE)
  object_metadata <- utils::modifyList(
    list(
      object = "ProtVis_dataset",
      created_at = as.character(Sys.time()),
      source = "user"
    ),
    metadata
  )
  object <- list(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info,
    variable_info_note = variable_info_note %||% .protvis_default_note("variable"),
    sample_info_note = sample_info_note %||% .protvis_default_note("sample"),
    annotation = annotation %||% list(),
    analysis_results = list(),
    process_info = list(parameters = list(), time = list(), history = list()),
    metadata = object_metadata,
    other_files = other_files %||% list(),
    checkpoint_info = list()
  )
  class(object) <- c("ProtVis_dataset", "list")
  object <- .protvis_append_process(
    object, "creation", status = "success",
    parameters = list(n_proteins = nrow(expression_data),
                      n_samples = ncol(expression_data))
  )
  validate_protvis_dataset(object)
  object
}

#' Alias for create_protvis_dataset.
#' @export
ProtVis_dataset <- function(...) create_protvis_dataset(...)

#' Short compatibility alias for create_protvis_dataset.
#' @export
create_dataset <- function(...) create_protvis_dataset(...)

#' Validate a ProtVis dataset object.
#'
#' @param object Object to validate.
#' @param strict Whether to reject malformed metadata instead of repairing only
#'   harmless names.
#' @return TRUE invisibly when validation succeeds.
#' @export
validate_protvis_dataset <- function(object, strict = TRUE) {
  if (!inherits(object, "ProtVis_dataset") || !is.list(object)) {
    stop("Object is not a ProtVis_dataset.", call. = FALSE)
  }
  required <- c("expression_data", "sample_info", "variable_info",
                "variable_info_note", "sample_info_note", "annotation",
                "analysis_results", "process_info", "metadata",
                "other_files", "checkpoint_info")
  missing <- setdiff(required, names(object))
  if (length(missing) > 0L) {
    stop("ProtVis dataset is missing fields: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  expression_data <- object$expression_data
  if (!is.data.frame(expression_data) || nrow(expression_data) < 1L ||
      ncol(expression_data) < 1L) {
    stop("expression_data must be a non-empty protein-by-sample data.frame.",
         call. = FALSE)
  }
  if (is.null(rownames(expression_data)) ||
      anyNA(rownames(expression_data)) ||
      any(!nzchar(rownames(expression_data))) ||
      anyDuplicated(rownames(expression_data))) {
    stop("expression_data row names must be unique protein identifiers.",
         call. = FALSE)
  }
  if (is.null(colnames(expression_data)) ||
      anyNA(colnames(expression_data)) ||
      any(!nzchar(colnames(expression_data))) ||
      anyDuplicated(colnames(expression_data))) {
    stop("expression_data column names must be unique sample identifiers.",
         call. = FALSE)
  }
  numeric_ok <- vapply(expression_data, is.numeric, logical(1))
  if (!all(numeric_ok)) {
    if (isTRUE(strict)) {
      stop("All expression_data columns must be numeric.", call. = FALSE)
    }
  }
  if (!is.data.frame(object$sample_info) ||
      !all(c("sample_id", "group") %in% names(object$sample_info))) {
    stop("sample_info must contain sample_id and group columns.", call. = FALSE)
  }
  if (!setequal(as.character(object$sample_info$sample_id),
                colnames(expression_data))) {
    stop("sample_info sample_id values must match expression_data columns.",
         call. = FALSE)
  }
  if (!is.data.frame(object$variable_info) ||
      !"protein_id" %in% names(object$variable_info) ||
      !setequal(as.character(object$variable_info$protein_id),
                rownames(expression_data))) {
    stop("variable_info protein_id values must match expression_data rows.",
         call. = FALSE)
  }
  if (!is.list(object$process_info) || !is.list(object$analysis_results) ||
      !is.list(object$metadata) || !is.list(object$other_files) ||
      !is.list(object$checkpoint_info)) {
    stop("process_info, analysis_results, metadata, other_files, and ",
         "checkpoint_info must be lists.", call. = FALSE)
  }
  invisible(TRUE)
}

#' Convert a ProtVis dataset into the legacy ID-plus-samples matrix format.
#' @export
protvis_expression_matrix <- function(object) {
  validate_protvis_dataset(object)
  out <- object$expression_data
  out <- cbind(ID = rownames(out), out, stringsAsFactors = FALSE)
  as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE)
}

#' Add a named analysis result and provenance event.
#' @export
add_protvis_result <- function(object, name, value, stage = name,
                               parameters = list()) {
  validate_protvis_dataset(object)
  if (!nzchar(as.character(name))) stop("Result name cannot be empty.",
                                        call. = FALSE)
  object$analysis_results[[as.character(name)]] <- value
  .protvis_append_process(object, stage, status = "success",
                          parameters = parameters)
}

#' Return process history as a compact data.frame.
#' @export
protvis_history <- function(object) {
  validate_protvis_dataset(object)
  history <- object$process_info$history %||% list()
  if (length(history) == 0L) {
    return(data.frame(stage = character(), status = character(),
                      time = character(), error = character(),
                      stringsAsFactors = FALSE))
  }
  data.frame(
    id = vapply(history, function(x) x$id %||% NA_integer_, integer(1)),
    stage = vapply(history, function(x) as.character(x$stage %||% ""),
                   character(1)),
    status = vapply(history, function(x) as.character(x$status %||% ""),
                    character(1)),
    time = vapply(history, function(x) as.character(x$time %||% ""),
                  character(1)),
    duration_seconds = vapply(
      history, function(x) as.numeric(x$duration_seconds %||% NA_real_),
      numeric(1)
    ),
    error = vapply(history, function(x) as.character(x$error %||% NA_character_),
                   character(1)),
    stringsAsFactors = FALSE
  )
}

#' Subset a ProtVis dataset while retaining metadata and provenance.
#' @export
subset_protvis_dataset <- function(object, variables = NULL, samples = NULL) {
  validate_protvis_dataset(object)
  if (is.null(variables)) variables <- rownames(object$expression_data)
  if (is.numeric(variables)) variables <- rownames(object$expression_data)[variables]
  if (is.null(samples)) samples <- colnames(object$expression_data)
  if (is.numeric(samples)) samples <- colnames(object$expression_data)[samples]
  variables <- intersect(as.character(variables), rownames(object$expression_data))
  samples <- intersect(as.character(samples), colnames(object$expression_data))
  if (length(variables) == 0L || length(samples) == 0L) {
    stop("Subset must retain at least one protein and one sample.",
         call. = FALSE)
  }
  object$expression_data <- object$expression_data[variables, samples, drop = FALSE]
  object$sample_info <- object$sample_info[
    match(samples, object$sample_info$sample_id), , drop = FALSE
  ]
  object$variable_info <- object$variable_info[
    match(variables, object$variable_info$protein_id), , drop = FALSE
  ]
  object$analysis_results <- list()
  .protvis_append_process(
    object, "subset", status = "success",
    parameters = list(n_proteins = length(variables), n_samples = length(samples))
  )
}

print.ProtVis_dataset <- function(x, ...) {
  validate_protvis_dataset(x)
  cat("<ProtVis_dataset>\\n")
  cat("  proteins:", nrow(x$expression_data),
      " samples:", ncol(x$expression_data), "\\n")
  cat("  source:", x$metadata$source %||% "unknown", "\\n")
  cat("  last stage:", x$process_info$active_stage %||% "none",
      " [", x$process_info$last_status %||% "unknown", "]\\n", sep = "")
  invisible(x)
}
