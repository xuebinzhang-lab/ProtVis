#' PD User Interface Module
#' This function creates the user interface for processing proteomics data
#' from Proteome Discoverer (PD) output files.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the PD processing module.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @name PD_ui
#' @export
#'
PD_ui <- function(id) {
  tabular_data_source_ui(
    id = id,
    source_name = "Proteome Discoverer",
    source_label = "Upload Proteome Discoverer protein/peptide groups export (xlsx, xls or csv)",
    source_accept = c(".xlsx", ".xls", ".csv")
  )
}

#' Proteome Discoverer Server Logic Module
#'
#' Parses Proteome Discoverer protein or peptide group exports into a ProtVis
#' expression matrix using accession and Abundance/Area columns.
#' @param id Character. Module ID used for namespacing server inputs/outputs.
#' @param shared_state A reactiveValues object shared across modules.
#' @return None. Called for side effects in the Shiny session.
#' @import shiny
#' @importFrom DT renderDT datatable
#' @name PD_server
#' @export
PD_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(id, "Proteome Discoverer", parse_proteome_discoverer_output, shared_state)
}
