#' OpenMS User Interface Module
#'
#' This function creates an OpenMS-compatible tabular importer.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the OpenMS processing module.
#' @import shiny
#' @import bslib
#' @name OpenMS_ui
#' @export
OpenMS_ui <- function(id) {
  .protvis_tabular_source_ui(id, "OpenMS")
}

#' OpenMS Server Module
#'
#' @param id Module ID.
#' @param shared_state A reactiveValues object shared across modules.
#' @return No return value. Called for Shiny side effects.
#' @import shiny
#' @name OpenMS_server
#' @export
OpenMS_server <- function(id, shared_state) {
  .protvis_tabular_source_server(id, shared_state, "OpenMS")
}
