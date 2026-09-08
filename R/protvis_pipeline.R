# Ordered, restartable ProtVis_dataset processing workflow.

.protvis_stage_aliases <- c(
  noise = "noise_correction",
  noise_correction = "noise_correction",
  data_clean = "noise_correction",
  clean = "noise_correction",
  transformation = "transformation",
  transform = "transformation",
  data_transformed = "transformation",
  imputation = "imputation",
  impute = "imputation",
  missing_value_imputation = "imputation",
  normalization = "normalization",
  normalisation = "normalization",
  normalize = "normalization",
  dimensionality_reduction = "dimensionality_reduction",
  pca = "dimensionality_reduction",
  differential = "differential_analysis",
  differential_analysis = "differential_analysis",
  dep = "differential_analysis",
  enrichment = "enrichment",
  enrich = "enrichment",
  network = "network",
  network_analysis = "network"
)

.protvis_stage_order <- c(
  "noise_correction", "transformation", "imputation", "normalization",
  "dimensionality_reduction", "differential_analysis", "enrichment", "network"
)

#' Return the ordered ProtVis processing stages.
#' @export
protvis_stage_order <- function() .protvis_stage_order

#' Normalize a stage alias to its canonical name.
#' @export
normalise_protvis_stage <- function(stage) {
  if (length(stage) != 1L || is.na(stage) || !nzchar(as.character(stage))) {
    stop("A processing stage is required.", call. = FALSE)
  }
  key <- tolower(gsub("[^a-z0-9]+", "_", as.character(stage)))
  key <- gsub("^_|_$", "", key)
  result <- .protvis_stage_aliases[[key]]
  if (is.null(result)) stop("Unknown ProtVis stage: ", stage, call. = FALSE)
  result
}

#' American-spelling alias for normalise_protvis_stage.
#' @export
normalize_protvis_stage <- normalise_protvis_stage

protvis_stage_labels <- function() {
  c(
    noise_correction = "Noise correction / QC filter",
    transformation = "Transformation",
    imputation = "Missing-value imputation",
    normalization = "Normalization",
    dimensionality_reduction = "Dimensionality reduction",
    differential_analysis = "Differential analysis",
    enrichment = "Enrichment",
    network = "Network analysis"
  )
}

.protvis_stage_parameters <- function(stage, params = list(), method = NULL,
                                       dots = list()) {
  params <- params %||% list()
  if (!is.list(params)) stop("params must be a list.", call. = FALSE)
  if (!is.null(params[[stage]]) && is.list(params[[stage]])) {
    params <- params[[stage]]
  }
  if (!is.null(method)) params$method <- method
  if (length(dots) > 0L) {
    for (name in names(dots)) params[[name]] <- dots[[name]]
  }
  params
}

.protvis_matrix_with_medians <- function(expression_data) {
  matrix <- as.matrix(expression_data)
  storage.mode(matrix) <- "numeric"
  for (j in seq_len(ncol(matrix))) {
    values <- matrix[, j]
    if (anyNA(values)) {
      replacement <- if (all(is.na(values))) 0 else median(values, na.rm = TRUE)
      values[is.na(values)] <- replacement
      matrix[, j] <- values
    }
  }
  matrix
}

.protvis_replace_expression <- function(dataset, matrix) {
  result <- as.data.frame(matrix, check.names = FALSE,
                          stringsAsFactors = FALSE)
  rownames(result) <- rownames(dataset$expression_data)
  colnames(result) <- colnames(dataset$expression_data)
  dataset$expression_data <- result
  dataset
}

.protvis_clear_downstream <- function(dataset, stage) {
  index <- match(stage, .protvis_stage_order)
  if (is.na(index)) return(dataset)
  downstream <- .protvis_stage_order[seq.int(index, length(.protvis_stage_order))]
  for (name in downstream) dataset$analysis_results[[name]] <- NULL
  dataset
}

.protvis_noise_correction <- function(dataset, params) {
  max_missing <- as.numeric(params$max_missing %||% 0.5)
  if (length(max_missing) != 1L || is.na(max_missing) ||
      max_missing < 0 || max_missing >= 1) {
    stop("max_missing must be between 0 and 1.", call. = FALSE)
  }
  matrix <- as.matrix(dataset$expression_data)
  missing_fraction <- rowMeans(is.na(matrix) | !is.finite(matrix))
  keep <- missing_fraction <= max_missing
  if (!any(keep)) stop("Noise correction removed every protein.", call. = FALSE)
  dataset$expression_data <- dataset$expression_data[keep, , drop = FALSE]
  dataset$variable_info <- dataset$variable_info[
    match(rownames(dataset$expression_data),
          dataset$variable_info$protein_id), , drop = FALSE
  ]
  dataset$analysis_results$noise_correction <- data.frame(
    protein_id = rownames(dataset$expression_data),
    missing_fraction = missing_fraction[keep],
    stringsAsFactors = FALSE
  )
  dataset
}

.protvis_transformation <- function(dataset, params) {
  method <- tolower(as.character(params$method %||% "log2"))
  if (method %in% c("none", "identity")) return(dataset)
  matrix <- as.matrix(dataset$expression_data)
  storage.mode(matrix) <- "numeric"
  finite <- matrix[is.finite(matrix)]
  if (length(finite) == 0L) stop("No finite values available for transformation.",
                                  call. = FALSE)
  pseudocount <- as.numeric(params$pseudocount %||% 1)
  if (length(pseudocount) != 1L || is.na(pseudocount) || pseudocount <= 0) {
    stop("pseudocount must be a positive number.", call. = FALSE)
  }
  offset <- max(0, -min(finite, na.rm = TRUE))
  shifted <- matrix + offset
  transformed <- switch(
    method,
    log2 = log2(shifted + pseudocount),
    log10 = log10(shifted + pseudocount),
    ln = log(shifted + pseudocount),
    log = log(shifted + pseudocount),
    stop("Unknown transformation method: ", method, call. = FALSE)
  )
  .protvis_replace_expression(dataset, transformed)
}

.protvis_knn_fill <- function(matrix, k = 10L) {
  k <- max(1L, as.integer(k[[1L]] %||% 10L))
  complete <- .protvis_matrix_with_medians(matrix)
  missing <- is.na(matrix) | !is.finite(matrix)
  if (!any(missing)) return(matrix)
  n <- nrow(matrix)
  # Limit the reference set for large MaxQuant exports; the deterministic
  # nearest-neighbour rule remains responsive in a Shiny session.
  reference <- if (n > 1000L) seq_len(min(n, 500L)) else seq_len(n)
  for (i in which(rowSums(missing) > 0L)) {
    observed <- which(!missing[i, ])
    donors <- setdiff(reference, i)
    if (length(donors) > 0L && length(observed) > 0L) {
      distance <- rowSums((complete[donors, observed, drop = FALSE] -
                             matrix(complete[i, observed],
                                    nrow = length(donors),
                                    ncol = length(observed), byrow = TRUE))^2)
      donors <- donors[order(distance)][seq_len(min(k, length(donors)))]
      for (j in which(missing[i, ])) {
        matrix[i, j] <- mean(complete[donors, j], na.rm = TRUE)
      }
    }
    remaining <- which(is.na(matrix[i, ]) | !is.finite(matrix[i, ]))
    if (length(remaining) > 0L) matrix[i, remaining] <- complete[i, remaining]
  }
  matrix
}

.protvis_imputation <- function(dataset, params) {
  method <- tolower(as.character(params$method %||% "median"))
  matrix <- as.matrix(dataset$expression_data)
  storage.mode(matrix) <- "numeric"
  if (method %in% c("none", "skip")) return(dataset)
  if (method %in% c("knn", "k-nearest-neighbor", "k_nearest_neighbor")) {
    matrix <- .protvis_knn_fill(matrix, params$k %||% 10L)
  } else if (method %in% c("median", "min", "zero")) {
    for (j in seq_len(ncol(matrix))) {
      observed <- matrix[, j]
      observed <- observed[is.finite(observed)]
      replacement <- switch(
        method,
        median = if (length(observed)) median(observed) else NA_real_,
        min = if (length(observed)) min(observed) else NA_real_,
        zero = 0
      )
      if (is.na(replacement)) stop("A sample contains no observed values.",
                                   call. = FALSE)
      matrix[is.na(matrix[, j]) | !is.finite(matrix[, j]), j] <- replacement
    }
  } else {
    stop("Unknown imputation method: ", method, call. = FALSE)
  }
  if (anyNA(matrix) || any(!is.finite(matrix))) {
    stop("Imputation did not resolve all missing or non-finite values.",
         call. = FALSE)
  }
  .protvis_replace_expression(dataset, matrix)
}

.protvis_quantile_normalize <- function(matrix) {
  matrix <- .protvis_matrix_with_medians(matrix)
  sorted <- apply(matrix, 2L, sort)
  target <- rowMeans(sorted)
  result <- matrix
  for (j in seq_len(ncol(matrix))) {
    result[order(matrix[, j]), j] <- target
  }
  result
}

.protvis_normalization <- function(dataset, params) {
  method <- tolower(as.character(params$method %||% "median"))
  matrix <- as.matrix(dataset$expression_data)
  storage.mode(matrix) <- "numeric"
  if (method %in% c("none", "identity")) return(dataset)
  if (method %in% c("median", "mean")) {
    for (j in seq_len(ncol(matrix))) {
      observed <- matrix[, j]
      center <- if (method == "median") median(observed, na.rm = TRUE)
                else mean(observed, na.rm = TRUE)
      if (!is.finite(center)) center <- 0
      matrix[, j] <- observed - center
    }
  } else if (method %in% c("quantile", "quantile_normalization")) {
    matrix <- .protvis_quantile_normalize(matrix)
  } else if (method %in% c("zscore", "z_score", "standardize")) {
    for (j in seq_len(ncol(matrix))) {
      observed <- matrix[, j]
      center <- mean(observed, na.rm = TRUE)
      spread <- stats::sd(observed, na.rm = TRUE)
      if (!is.finite(center)) center <- 0
      if (!is.finite(spread) || spread == 0) spread <- 1
      matrix[, j] <- (observed - center) / spread
    }
  } else {
    stop("Unknown normalization method: ", method, call. = FALSE)
  }
  .protvis_replace_expression(dataset, matrix)
}

.protvis_dimensionality_reduction <- function(dataset, params) {
  matrix <- .protvis_matrix_with_medians(dataset$expression_data)
  if (nrow(matrix) < 2L || ncol(matrix) < 2L) {
    dataset$analysis_results$dimensionality_reduction <- list(
      status = "skipped", message = "At least two proteins and two samples are required."
    )
    return(dataset)
  }
  variable <- apply(matrix, 1L, stats::sd, na.rm = TRUE) > 0
  if (sum(variable) < 2L) {
    dataset$analysis_results$dimensionality_reduction <- list(
      status = "skipped", message = "At least two variable proteins are required."
    )
    return(dataset)
  }
  fit <- tryCatch(
    stats::prcomp(t(matrix[variable, , drop = FALSE]),
                  center = TRUE, scale. = TRUE),
    error = function(e) stop("PCA failed: ", conditionMessage(e), call. = FALSE)
  )
  scores <- as.data.frame(fit$x, stringsAsFactors = FALSE)
  scores$sample_id <- rownames(scores)
  scores$group <- dataset$sample_info$group[
    match(scores$sample_id, dataset$sample_info$sample_id)
  ]
  scores <- scores[, c("sample_id", "group",
                       setdiff(names(scores), c("sample_id", "group"))),
                   drop = FALSE]
  variance <- (fit$sdev^2) / sum(fit$sdev^2)
  dataset$analysis_results$dimensionality_reduction <- list(
    status = "success", scores = scores,
    variance_explained = data.frame(
      component = paste0("PC", seq_along(variance)),
      variance_explained = variance,
      stringsAsFactors = FALSE
    )
  )
  dataset
}

.protvis_differential_analysis <- function(dataset, params) {
  group_column <- as.character(params$group_column %||% "group")
  if (!group_column %in% names(dataset$sample_info)) {
    stop("Sample metadata has no group column named ", group_column, ".",
         call. = FALSE)
  }
  groups <- as.character(dataset$sample_info[[group_column]])
  groups[is.na(groups) | !nzchar(groups)] <- "Unassigned"
  levels <- unique(groups[groups != "Unassigned"])
  if (length(levels) < 2L) {
    dataset$analysis_results$differential_analysis <- list(
      status = "skipped",
      message = "Two assigned experimental groups are required.",
      groups = levels
    )
    return(dataset)
  }
  group1 <- as.character(params$group1 %||% levels[[1L]])
  group2 <- as.character(params$group2 %||% levels[[2L]])
  if (!group1 %in% levels || !group2 %in% levels || identical(group1, group2)) {
    stop("group1 and group2 must be two different assigned groups.",
         call. = FALSE)
  }
  matrix <- as.matrix(dataset$expression_data)
  index1 <- which(groups == group1)
  index2 <- which(groups == group2)
  p_value <- rep(NA_real_, nrow(matrix))
  mean1 <- rowMeans(matrix[, index1, drop = FALSE], na.rm = TRUE)
  mean2 <- rowMeans(matrix[, index2, drop = FALSE], na.rm = TRUE)
  for (i in seq_len(nrow(matrix))) {
    x <- matrix[i, index1]
    y <- matrix[i, index2]
    x <- x[is.finite(x)]
    y <- y[is.finite(y)]
    if (length(x) >= 2L && length(y) >= 2L) {
      p_value[i] <- tryCatch(stats::t.test(x, y)$p.value,
                             error = function(e) NA_real_)
    }
  }
  adj <- rep(NA_real_, length(p_value))
  valid <- is.finite(p_value)
  if (any(valid)) adj[valid] <- stats::p.adjust(p_value[valid], "BH")
  fdr <- as.numeric(params$fdr %||% 0.05)
  logfc <- as.numeric(params$logfc %||% 1)
  result <- data.frame(
    protein_id = rownames(matrix),
    mean_group1 = mean1,
    mean_group2 = mean2,
    log2FC = mean2 - mean1,
    p_value = p_value,
    adj_p_value = adj,
    significant = is.finite(adj) & adj <= fdr & abs(mean2 - mean1) >= logfc,
    stringsAsFactors = FALSE
  )
  result <- result[order(result$adj_p_value, na.last = TRUE), , drop = FALSE]
  rownames(result) <- NULL
  dataset$analysis_results$differential_analysis <- list(
    status = "success", group1 = group1, group2 = group2,
    parameters = list(fdr = fdr, logfc = logfc), table = result
  )
  dataset
}

.protvis_term_mapping <- function(annotation) {
  if (is.null(annotation) || !is.list(annotation)) return(NULL)
  candidates <- c("terms", "term_mapping", "GO", "go", "pathway",
                  "pathways", "KEGG", "kegg")
  for (name in candidates) {
    value <- annotation[[name]]
    if (is.data.frame(value) && nrow(value) > 0L) {
      id_col <- .protvis_find_column(names(value),
                                     c("^protein_id$", "^ID$", "protein",
                                       "accession"))
      term_col <- .protvis_find_column(names(value),
                                       c("^term$", "go", "pathway", "kegg",
                                         "category", "annotation"))
      if (!is.null(id_col) && !is.null(term_col)) {
        return(data.frame(
          protein_id = as.character(value[[id_col]]),
          term = as.character(value[[term_col]]),
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  NULL
}

.protvis_enrichment <- function(dataset, params) {
  mapping <- .protvis_term_mapping(dataset$annotation)
  if (is.null(mapping)) {
    dataset$analysis_results$enrichment <- list(
      status = "skipped",
      message = "No annotation term mapping was supplied in annotation$terms."
    )
    return(dataset)
  }
  mapping <- mapping[!is.na(mapping$protein_id) & !is.na(mapping$term) &
                       nzchar(mapping$term), , drop = FALSE]
  diff <- dataset$analysis_results$differential_analysis
  selected <- params$selected_ids
  if (is.null(selected) && is.list(diff) && is.data.frame(diff$table)) {
    selected <- diff$table$protein_id[diff$table$significant %in% TRUE]
  }
  selected <- intersect(as.character(selected %||% character()),
                        rownames(dataset$expression_data))
  universe <- intersect(unique(mapping$protein_id),
                        rownames(dataset$expression_data))
  mapping <- mapping[mapping$protein_id %in% universe, , drop = FALSE]
  if (length(selected) == 0L || length(universe) == 0L) {
    dataset$analysis_results$enrichment <- list(
      status = "skipped", message = "No selected proteins or annotation universe."
    )
    return(dataset)
  }
  terms <- unique(mapping$term)
  total_selected <- length(selected)
  total_universe <- length(universe)
  result <- lapply(terms, function(term) {
    members <- unique(mapping$protein_id[mapping$term == term])
    overlap <- length(intersect(members, selected))
    p <- if (overlap == 0L) 1 else stats::phyper(
      overlap - 1L, length(members), total_universe - length(members),
      total_selected, lower.tail = FALSE
    )
    data.frame(term = term, overlap = overlap, term_size = length(members),
               p_value = p, stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, result)
  result$adj_p_value <- stats::p.adjust(result$p_value, "BH")
  result <- result[order(result$adj_p_value, result$p_value), , drop = FALSE]
  rownames(result) <- NULL
  dataset$analysis_results$enrichment <- list(
    status = "success", selected = selected, universe = universe, table = result
  )
  dataset
}

.protvis_network <- function(dataset, params) {
  matrix <- .protvis_matrix_with_medians(dataset$expression_data)
  if (ncol(matrix) < 3L || nrow(matrix) < 2L) {
    dataset$analysis_results$network <- list(
      status = "skipped", message = "At least three samples are required."
    )
    return(dataset)
  }
  max_nodes <- max(2L, as.integer(params$max_nodes %||% 100L))
  threshold <- as.numeric(params$threshold %||% 0.8)
  variance <- apply(matrix, 1L, stats::var)
  selected <- order(variance, decreasing = TRUE)[
    seq_len(min(max_nodes, nrow(matrix)))
  ]
  selected_matrix <- matrix[selected, , drop = FALSE]
  correlation <- suppressWarnings(stats::cor(t(selected_matrix),
                                             use = "pairwise.complete.obs"))
  edge_index <- which(upper.tri(correlation) &
                        is.finite(correlation) &
                        abs(correlation) >= threshold, arr.ind = TRUE)
  edges <- if (nrow(edge_index) == 0L) {
    data.frame(from = character(), to = character(), correlation = numeric(),
               stringsAsFactors = FALSE)
  } else {
    data.frame(
      from = rownames(selected_matrix)[edge_index[, 1L]],
      to = rownames(selected_matrix)[edge_index[, 2L]],
      correlation = correlation[edge_index],
      stringsAsFactors = FALSE
    )
  }
  dataset$analysis_results$network <- list(
    status = "success", threshold = threshold,
    nodes = data.frame(protein_id = rownames(selected_matrix),
                       stringsAsFactors = FALSE),
    edges = edges
  )
  dataset
}

.protvis_run_stage <- function(dataset, stage, params) {
  switch(
    stage,
    noise_correction = .protvis_noise_correction(dataset, params),
    transformation = .protvis_transformation(dataset, params),
    imputation = .protvis_imputation(dataset, params),
    normalization = .protvis_normalization(dataset, params),
    dimensionality_reduction = .protvis_dimensionality_reduction(dataset, params),
    differential_analysis = .protvis_differential_analysis(dataset, params),
    enrichment = .protvis_enrichment(dataset, params),
    network = .protvis_network(dataset, params),
    stop("No runner exists for stage ", stage, ".", call. = FALSE)
  )
}

.protvis_last_event <- function(dataset) {
  history <- dataset$process_info$history %||% list()
  if (length(history) == 0L) return(NULL)
  history[[length(history)]]
}

#' Run one guarded processing node transactionally.
#'
#' Errors are recorded in process_info and the last valid object is returned,
#' keeping the Shiny session usable. Set stop_on_error=TRUE for scripts that
#' want an exception after the error has been recorded.
#' @export
run_protvis_step <- function(dataset, stage, params = list(),
                             checkpoint_dir = NULL, method = NULL,
                             stop_on_error = FALSE, ...) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  stage <- normalise_protvis_stage(stage)
  params <- .protvis_stage_parameters(stage, params, method, list(...))
  started <- Sys.time()
  candidate <- .protvis_new_analysis_dataset(
    .protvis_clear_downstream(dataset, stage), stage, params
  )
  result <- tryCatch(
    .protvis_run_stage(candidate, stage, params),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    message_text <- conditionMessage(result)
    failed <- .protvis_append_process(
      candidate, stage, status = "error", parameters = params,
      error = message_text, message = "Node failed; previous valid data retained.",
      started_at = started, finished_at = Sys.time()
    )
    failed$metadata$last_error <- message_text
    failed$metadata$last_error_stage <- stage
    failed$analysis_results$errors <- failed$process_info$errors
    directory <- protvis_output_directory(
      checkpoint_dir %||% failed$checkpoint_info$directory %||%
        failed$metadata$checkpoint_dir
    )
    failed <- tryCatch(
      protvis_auto_export_dataset(failed, directory = directory),
      error = function(e) .protvis_append_process(
        failed, "auto_export", status = "error",
        parameters = list(directory = directory), error = conditionMessage(e)
      )
    )
    if (isTRUE(stop_on_error)) stop(message_text, call. = FALSE)
    validate_protvis_dataset(failed)
    return(failed)
  }
  candidate <- .protvis_append_process(
    result, stage, status = "success", parameters = params,
    started_at = started, finished_at = Sys.time()
  )
  directory <- protvis_output_directory(
    checkpoint_dir %||% candidate$checkpoint_info$directory %||%
      candidate$metadata$checkpoint_dir
  )
  candidate <- tryCatch(
    protvis_auto_export_dataset(candidate, directory = directory),
    error = function(e) .protvis_append_process(
      candidate, "auto_export", status = "error",
      parameters = list(directory = directory), error = conditionMessage(e),
      message = "Automatic export failed; the in-memory dataset remains available."
    )
  )
  if (nzchar(as.character(directory))) {
    checkpoint <- tryCatch(
      save_protvis_checkpoint(candidate, directory, stage),
      error = function(e) e
    )
    if (inherits(checkpoint, "error")) {
      candidate <- .protvis_append_process(
        candidate, "checkpoint", status = "error",
        parameters = list(stage = stage, directory = directory),
        error = conditionMessage(checkpoint)
      )
    } else {
      candidate$checkpoint_info$directory <- normalizePath(
        directory, winslash = "/", mustWork = FALSE
      )
      candidate$checkpoint_info$latest_path <- checkpoint
      candidate$checkpoint_info$last_successful_stage <- stage
    }
  }
  validate_protvis_dataset(candidate)
  candidate
}

#' Run a sequence of guarded ProtVis processing nodes.
#' @export
run_protvis_pipeline <- function(dataset, stages = NULL, params = list(),
                                 checkpoint_dir = NULL,
                                 continue_on_error = FALSE,
                                 stop_on_error = FALSE, ...) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  if (is.null(stages)) stages <- .protvis_stage_order
  stages <- vapply(stages, normalise_protvis_stage, character(1))
  stages <- unique(stages)
  current <- dataset
  for (stage in stages) {
    current <- run_protvis_step(
      current, stage, params = params, checkpoint_dir = checkpoint_dir,
      stop_on_error = stop_on_error, ...
    )
    event <- .protvis_last_event(current)
    if (identical(event$status, "error") && !isTRUE(continue_on_error)) break
  }
  current
}

.protvis_last_failed_stage <- function(dataset) {
  history <- dataset$process_info$history %||% list()
  if (length(history) == 0L) return(NULL)
  failed <- vapply(history, function(event) identical(event$status, "error"),
                   logical(1))
  if (!any(failed)) return(NULL)
  as.character(history[[max(which(failed))]]$stage)
}

.protvis_last_success_stage <- function(dataset) {
  history <- dataset$process_info$history %||% list()
  if (length(history) == 0L) return(NULL)
  ok <- vapply(history, function(event) identical(event$status, "success") &&
                 event$stage %in% .protvis_stage_order, logical(1))
  if (!any(ok)) return(NULL)
  as.character(history[[max(which(ok))]]$stage)
}

#' Restore the nearest available checkpoint for a stage.
#' @export
restore_protvis_stage <- function(dataset_or_directory, stage = NULL,
                                   version = "latest") {
  if (inherits(dataset_or_directory, "ProtVis_dataset") ||
      inherits(dataset_or_directory, "mass_dataset")) {
    dataset_or_directory <- as_protvis_dataset(dataset_or_directory)
    directory <- dataset_or_directory$checkpoint_info$directory %||%
      dataset_or_directory$metadata$checkpoint_dir
    if (is.null(directory)) {
      if (is.null(stage)) return(dataset_or_directory)
      stop("The dataset has no checkpoint directory.", call. = FALSE)
    }
    return(restore_protvis_checkpoint(directory, stage = stage,
                                      version = version))
  }
  restore_protvis_checkpoint(dataset_or_directory, stage = stage,
                             version = version)
}

.protvis_next_stage <- function(stage) {
  if (is.null(stage)) return(.protvis_stage_order[[1L]])
  index <- match(stage, .protvis_stage_order)
  if (is.na(index) || index >= length(.protvis_stage_order)) return(NULL)
  .protvis_stage_order[[index + 1L]]
}

#' Resume a workflow after a failure or from a saved checkpoint.
#' @export
resume_protvis_pipeline <- function(dataset_or_directory, stages = NULL,
                                    from_stage = NULL, params = list(),
                                    checkpoint_dir = NULL,
                                    continue_on_error = FALSE, ...) {
  if (inherits(dataset_or_directory, "ProtVis_dataset") ||
      inherits(dataset_or_directory, "mass_dataset")) {
    current <- as_protvis_dataset(dataset_or_directory)
  } else {
    current <- restore_protvis_checkpoint(dataset_or_directory)
  }
  failed <- .protvis_last_failed_stage(current)
  from_stage <- from_stage %||% failed
  if (is.null(from_stage)) from_stage <- .protvis_next_stage(
    .protvis_last_success_stage(current)
  )
  if (is.null(from_stage)) return(current)
  from_stage <- normalise_protvis_stage(from_stage)
  if (is.null(stages)) {
    stages <- .protvis_stage_order[
      seq.int(match(from_stage, .protvis_stage_order), length(.protvis_stage_order))
    ]
  } else {
    stages <- vapply(stages, normalise_protvis_stage, character(1))
    stages <- stages[match(from_stage, stages):length(stages)]
  }
  directory <- checkpoint_dir %||% current$checkpoint_info$directory %||%
    current$metadata$checkpoint_dir
  run_protvis_pipeline(
    current, stages = stages, params = params, checkpoint_dir = directory,
    continue_on_error = continue_on_error, ...
  )
}

#' Re-run a selected node and every downstream node with new parameters.
#' @export
rerun_protvis_downstream <- function(dataset_or_directory, from_stage,
                                     params = list(), checkpoint_dir = NULL,
                                     ...) {
  from_stage <- normalise_protvis_stage(from_stage)
  if (inherits(dataset_or_directory, "ProtVis_dataset") ||
      inherits(dataset_or_directory, "mass_dataset")) {
    current <- as_protvis_dataset(dataset_or_directory)
    directory <- checkpoint_dir %||% current$checkpoint_info$directory %||%
      current$metadata$checkpoint_dir
    index <- match(from_stage, .protvis_stage_order)
    previous <- if (index > 1L) .protvis_stage_order[[index - 1L]] else NULL
    if (!is.null(directory) && !is.null(previous)) {
      current <- tryCatch(
        restore_protvis_checkpoint(directory, stage = previous),
        error = function(e) current
      )
    }
  } else {
    directory <- checkpoint_dir %||% dataset_or_directory
    index <- match(from_stage, .protvis_stage_order)
    previous <- if (index > 1L) .protvis_stage_order[[index - 1L]] else NULL
    current <- restore_protvis_checkpoint(directory, stage = previous)
  }
  stages <- .protvis_stage_order[
    seq.int(match(from_stage, .protvis_stage_order), length(.protvis_stage_order))
  ]
  run_protvis_pipeline(current, stages = stages, params = params,
                       checkpoint_dir = directory, ...)
}

#' Retry the most recently failed processing node.
#' @export
retry_protvis_step <- function(dataset, stage = NULL, params = list(),
                               checkpoint_dir = NULL, ...) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  stage <- stage %||% .protvis_last_failed_stage(dataset)
  if (is.null(stage)) stop("No failed ProtVis node is available to retry.",
                           call. = FALSE)
  stage <- normalise_protvis_stage(stage)
  index <- match(stage, .protvis_stage_order)
  directory <- checkpoint_dir %||% dataset$checkpoint_info$directory %||%
    dataset$metadata$checkpoint_dir
  current <- dataset
  if (!is.null(directory) && index > 1L) {
    current <- tryCatch(
      restore_protvis_checkpoint(directory,
                                 stage = .protvis_stage_order[[index - 1L]]),
      error = function(e) current
    )
  }
  run_protvis_step(current, stage, params = params,
                   checkpoint_dir = directory, ...)
}

#' Write a small self-contained HTML provenance report.
#' @export
write_protvis_report <- function(dataset, file) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  if (length(file) != 1L || !nzchar(as.character(file))) {
    stop("A report filename is required.", call. = FALSE)
  }
  directory <- dirname(path.expand(file))
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE,
                                         showWarnings = FALSE)
  history <- protvis_history(dataset)
  rows <- if (nrow(history) == 0L) "" else paste(
    apply(history, 1L, function(row) paste0(
      "<tr><td>", row[["id"]], "</td><td>", row[["stage"]],
      "</td><td>", row[["status"]], "</td><td>", row[["time"]],
      "</td><td>", row[["error"]], "</td></tr>"
    )), collapse = "\n"
  )
  html <- c(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<title>ProtVis_dataset report</title>",
    "<style>body{font-family:system-ui;margin:2rem;color:#1f3447}",
    "table{border-collapse:collapse}td,th{border:1px solid #dbe8f3;padding:.4rem}",
    "th{background:#e8f5fc}</style></head><body>",
    "<h1>ProtVis_dataset</h1>",
    paste0("<p>Proteins: ", nrow(dataset$expression_data),
           " &nbsp; Samples: ", ncol(dataset$expression_data), "</p>"),
    "<h2>Process history</h2><table><tr><th>ID</th><th>Stage</th>",
    "<th>Status</th><th>Time</th><th>Error</th></tr>",
    rows, "</table></body></html>"
  )
  writeLines(html, file)
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

# Convenience aliases matching the stage names in the workflow diagrams.
data_clean <- function(dataset, ...) run_protvis_step(dataset, "noise_correction", ...)
transform_data <- function(dataset, ...) run_protvis_step(dataset, "transformation", ...)
impute_missing <- function(dataset, ...) run_protvis_step(dataset, "imputation", ...)
normalize_data <- function(dataset, ...) run_protvis_step(dataset, "normalization", ...)
differential_analysis <- function(dataset, ...) {
  run_protvis_step(dataset, "differential_analysis", ...)
}
enrich_network <- function(dataset, ...) run_protvis_pipeline(
  dataset, stages = c("enrichment", "network"), ...
)
