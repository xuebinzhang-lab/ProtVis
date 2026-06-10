#' Skyline User Interface Module
#'
#' This function creates a Skyline-compatible tabular importer.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the Skyline processing module.
#' @import shiny
#' @import bslib
#' @name skyline_ui
#' @export
skyline_ui <- function(id) {
  .protvis_tabular_source_ui(id, "Skyline")
}

#' Skyline Server Module
#'
#' @param id Module ID.
#' @param shared_state A reactiveValues object shared across modules.
#' @return No return value. Called for Shiny side effects.
#' @import shiny
#' @name skyline_server
#' @export
skyline_server <- function(id, shared_state) {
  .protvis_tabular_source_server(id, shared_state, "Skyline")
}
