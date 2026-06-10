#' PD User Interface Module
#'
#' This function creates a Proteome Discoverer-compatible tabular importer.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the PD processing module.
#' @import shiny
#' @import bslib
#' @name PD_ui
#' @export
PD_ui <- function(id) {
  .protvis_tabular_source_ui(id, "Proteome Discoverer")
}

#' PD Server Module
#'
#' @param id Module ID.
#' @param shared_state A reactiveValues object shared across modules.
#' @return No return value. Called for Shiny side effects.
#' @import shiny
#' @name PD_server
#' @export
PD_server <- function(id, shared_state) {
  .protvis_tabular_source_server(id, shared_state, "ProteomeDiscoverer")
}
