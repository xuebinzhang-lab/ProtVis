#' OpenMS User Interface Module
#' This function creates the user interface for processing proteomics data
#' from OpenMS output files.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the OpenMS processing module.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @name OpenMS_ui
#' @export
#'
OpenMS_ui <- function(id) {
  tabular_data_source_ui(
    id = id,
    source_name = "OpenMS",
    source_label = "Upload OpenMS consensus/ProteinQuantifier table (csv, tsv, txt, xlsx or xls)",
    source_accept = c(".xlsx", ".xls", ".csv", ".tsv", ".txt")
  )
}

#' OpenMS Server Logic Module
#'
#' Parses OpenMS consensus or ProteinQuantifier tables into a ProtVis expression
#' matrix using protein accession and intensity/abundance columns.
#' @param id Character. Module ID used for namespacing server inputs/outputs.
#' @param shared_state A reactiveValues object shared across modules.
#' @return None. Called for side effects in the Shiny session.
#' @import shiny
#' @importFrom DT renderDT datatable
#' @name OpenMS_server
#' @export
OpenMS_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(id, "OpenMS", parse_openms_output, shared_state)
}
