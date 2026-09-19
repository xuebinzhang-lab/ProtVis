#' Skyline User Interface Module
#' This function creates the user interface for processing proteomics data
#' from Skyline output files.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the Skyline processing module.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @name skyline_ui
#' @export
#'
skyline_ui <- function(id) {
  tabular_data_source_ui(
    id = id,
    source_name = "Skyline",
    source_label = "Upload Skyline report/MSstats export (csv, tsv, txt, xlsx or xls)",
    source_accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt")
  )
}

#' Skyline Server Logic Module
#'
#' Parses Skyline MSstats-style long reports or wide protein abundance exports
#' into a ProtVis expression matrix.
#' @param id Character. Module ID used for namespacing server inputs/outputs.
#' @param shared_state A reactiveValues object shared across modules.
#' @return None. Called for side effects in the Shiny session.
#' @import shiny
#' @importFrom DT renderDT datatable
#' @name skyline_server
#' @export
skyline_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(id, "Skyline", parse_skyline_output, shared_state)
}
