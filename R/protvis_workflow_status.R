# Workflow dependency graph, invalidation, and state inspection.

#' Return dependencies for the canonical ProtVis workflow.
#' @export
protvis_stage_dependencies <- function() {
  stages <- protvis_stage_order()
  dependencies <- vector("list", length(stages))
  names(dependencies) <- stages
  for (i in seq_along(stages)) {
    dependencies[[i]] <- if (i == 1L) character() else stages[[i - 1L]]
  }
  dependencies
}

#' Invalidate a workflow node and its downstream dependants.
#' @export
protvis_invalidate_downstream <- function(dataset, stage, reason = NULL,
                                          include_stage = TRUE,
                                          clear_results = TRUE) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  stage <- normalise_protvis_stage(stage)
  stages <- protvis_stage_order()
  index <- match(stage, stages)
  start <- if (isTRUE(include_stage)) index else index + 1L
  if (start > length(stages)) return(dataset)
  affected <- stages[seq.int(start, length(stages))]
  invalidated <- dataset$metadata$workflow_invalidated %||% list()
  timestamp <- as.character(Sys.time())
  reason <- as.character(reason %||% paste0(stage, " changed"))
  for (name in affected) {
    invalidated[[name]] <- list(
      stage = name,
      invalidated_at = timestamp,
      triggered_by = stage,
      reason = reason
    )
    if (isTRUE(clear_results)) dataset$analysis_results[[name]] <- NULL
  }
  dataset$metadata$workflow_invalidated <- invalidated
  dataset
}

.protvis_mark_stage_complete <- function(dataset, stage) {
  stage <- normalise_protvis_stage(stage)
  invalidated <- dataset$metadata$workflow_invalidated %||% list()
  invalidated[[stage]] <- NULL
  dataset$metadata$workflow_invalidated <- invalidated
  completed <- dataset$metadata$workflow_completed %||% list()
  completed[[stage]] <- list(
    completed_at = as.character(Sys.time()),
    object_name = dataset$metadata$object_name %||% "ProtVis_dataset"
  )
  dataset$metadata$workflow_completed <- completed
  dataset
}

#' Summarize the status of every workflow node.
#' @export
protvis_workflow_status <- function(dataset) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  stages <- protvis_stage_order()
  labels <- protvis_stage_labels()
  deps <- protvis_stage_dependencies()
  history <- dataset$process_info$history %||% list()
  invalidated <- dataset$metadata$workflow_invalidated %||% list()

  rows <- lapply(stages, function(stage) {
    events <- history[vapply(history, function(x) {
      identical(as.character(x$stage %||% ""), stage)
    }, logical(1))]
    last <- if (length(events)) events[[length(events)]] else NULL
    status <- "pending"
    reason <- ""
    if (!is.null(invalidated[[stage]])) {
      status <- "invalidated"
      reason <- as.character(invalidated[[stage]]$reason %||% "")
    } else if (!is.null(last)) {
      status <- if (identical(last$status, "success")) "complete" else "failed"
      reason <- if (identical(status, "failed")) {
        as.character(last$error %||% "")
      } else ""
    }
    data.frame(
      stage = stage,
      label = unname(labels[[stage]] %||% stage),
      depends_on = paste(deps[[stage]], collapse = ", "),
      status = status,
      last_run = if (is.null(last)) "" else as.character(last$finished_at %||% last$time %||% ""),
      reason = reason,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
