#' Mascot User Interface Module
#'
#' Creates the Mascot data-source UI so the Mascot option in Project init opens
#' a stable module instead of failing with a missing-function error.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the Mascot data-source module.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @name Mascot_ui
#' @export
Mascot_ui <- function(id) {
  tabular_data_source_ui(
    id = id,
    source_name = "Mascot",
    source_label = "Upload Mascot CSV/export results (xlsx, xls, csv, txt or tsv)",
    source_accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
  )
}

#' Mascot Server Logic Module
#'
#' Parses Mascot CSV/export tables into a ProtVis expression matrix using
#' protein accession and numeric quantitation columns.
#' @param id Character. Module ID used for namespacing server inputs/outputs.
#' @param shared_state A reactiveValues object shared across modules.
#' @return None. Called for side effects in the Shiny session.
#' @import shiny
#' @importFrom DT renderDT datatable
#' @name Mascot_server
#' @export
Mascot_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(id, "Mascot", parse_mascot_output, shared_state)
}
