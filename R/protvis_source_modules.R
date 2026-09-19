# Legacy MaxQuant output preparation adapters. The canonical import implementation lives
# in import_protvis(); these modules keep the existing tabular UI usable.

.protvis_legacy_parsed <- function(data, source) {
  parsed <- .protvis_parse_table(data, source)
  list(
    expression_matrix = parsed$expression,
    note = paste0("Parsed ", source, " output into the ProtVis matrix.")
  )
}

parse_dia_nn_output <- function(df) .protvis_legacy_parsed(df, "DIA-NN")
parse_spectronaut_output <- function(df) {
  .protvis_legacy_parsed(df, "Spectronaut")
}
parse_fragpipe_output <- function(df) .protvis_legacy_parsed(df, "FragPipe")
parse_user_defined_matrix <- function(df) {
  .protvis_legacy_parsed(df, "User-defined matrix")
}

#' DIA-NN input UI.
#' @export
DIA_NN_ui <- function(id) {
  tabular_data_source_ui(
    id, "DIA-NN",
    "Upload DIA-NN report (parquet, tsv, txt or csv)",
    c(".parquet", ".tsv", ".txt", ".csv")
  )
}

#' DIA-NN input server.
#' @export
DIA_NN_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(
    id, "DIA-NN", parse_dia_nn_output, shared_state
  )
}

#' Spectronaut input UI.
#' @export
Spectronaut_ui <- function(id) {
  tabular_data_source_ui(
    id, "Spectronaut",
    "Upload Spectronaut report (csv, tsv, xlsx or xls)",
    c(".csv", ".tsv", ".xlsx", ".xls")
  )
}

#' Spectronaut input server.
#' @export
Spectronaut_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(
    id, "Spectronaut", parse_spectronaut_output, shared_state
  )
}

#' FragPipe input UI.
#' @export
FragPipe_ui <- function(id) {
  tabular_data_source_ui(
    id, "FragPipe",
    "Upload FragPipe report (tsv, txt or csv)",
    c(".tsv", ".txt", ".csv")
  )
}

#' FragPipe input server.
#' @export
FragPipe_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(
    id, "FragPipe", parse_fragpipe_output, shared_state
  )
}

#' User-defined matrix input UI.
#' @export
User_defined_matrix_ui <- function(id) {
  tabular_data_source_ui(
    id, "User-defined matrix",
    "Upload user-defined matrix (csv, tsv, xlsx or xls)",
    c(".csv", ".tsv", ".xlsx", ".xls", ".parquet")
  )
}

#' User-defined matrix input server.
#' @export
User_defined_matrix_server <- function(id, shared_state = NULL) {
  register_tabular_data_source_server(
    id, "User-defined matrix", parse_user_defined_matrix, shared_state
  )
}
