# Shared three-layer guard for Shiny actions that must not run twice.

.protvis_run_key <- function(key) {
  if (is.null(key) || !length(key)) return("operation")
  key <- as.character(key[[1L]] %||% "operation")
  if (!nzchar(key)) "operation" else key
}

.protvis_run_locks <- function(shared_state) {
  locks <- shared_state$run_locks
  if (!is.list(locks)) list() else locks
}

#' Start a guarded ProtVis operation.
#'
#' The lock is the server-side second layer.  The UI class and JavaScript
#' handler provide the first layer; the dataset checkpoint written by the
#' caller is the third layer and is reached only after successful completion.
#' @param shared_state Application reactive state.
#' @param key Stable operation name.
#' @param session Optional Shiny module session.
#' @param button_id Optional module-local action button id.
#' @return TRUE when this invocation owns the lock, otherwise FALSE.
#' @noRd
.protvis_begin_run <- function(shared_state, key, session = NULL,
                                button_id = NULL) {
  key <- .protvis_run_key(key)
  locks <- .protvis_run_locks(shared_state)
  if (isTRUE(locks[[key]])) return(FALSE)

  locks[[key]] <- TRUE
  shared_state$run_locks <- locks
  if (!is.null(session) && !is.null(button_id)) {
    try(shinyjs::disable(session$ns(button_id)), silent = TRUE)
  }
  TRUE
}

#' Release a guarded ProtVis operation.
#' @noRd
.protvis_end_run <- function(shared_state, key, session = NULL,
                              button_id = NULL) {
  key <- .protvis_run_key(key)
  locks <- .protvis_run_locks(shared_state)
  locks[[key]] <- FALSE
  shared_state$run_locks <- locks
  if (!is.null(session) && !is.null(button_id)) {
    try(shinyjs::enable(session$ns(button_id)), silent = TRUE)
  }
  invisible(NULL)
}
