# Reusable error guard for Shiny handlers and scripts.

#' Evaluate an expression without taking down the surrounding Shiny session.
#'
#' @param expr Expression to evaluate.
#' @param label Human-readable node label.
#' @return A list with ok, value, error, and label fields.
#' @export
protvis_guard <- function(expr, label = "ProtVis operation") {
  label <- as.character(label[[1L]] %||% "ProtVis operation")
  result <- tryCatch(
    list(ok = TRUE, value = force(expr), error = NULL, label = label),
    error = function(e) {
      list(ok = FALSE, value = NULL, error = conditionMessage(e), label = label)
    }
  )
  result
}

#' Format a guarded error for a notification or report.
#' @export
protvis_error_message <- function(error, label = NULL) {
  text <- if (inherits(error, "condition")) conditionMessage(error)
          else as.character(error %||% "Unknown error")
  if (!is.null(label) && nzchar(as.character(label))) {
    paste0(as.character(label), ": ", text)
  } else {
    text
  }
}

#' Return recorded node errors from a ProtVis_dataset.
#' @export
protvis_error_log <- function(dataset) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  events <- dataset$process_info$errors %||% list()
  if (length(events) == 0L) {
    return(data.frame(
      stage = character(), time = character(), error = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    stage = vapply(events, function(x) as.character(x$stage %||% ""),
                   character(1)),
    time = vapply(events, function(x) as.character(x$time %||% ""),
                  character(1)),
    error = vapply(events, function(x) as.character(x$error %||% ""),
                   character(1)),
    stringsAsFactors = FALSE
  )
}
