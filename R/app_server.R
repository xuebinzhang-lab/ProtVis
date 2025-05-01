#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  bslib::bs_themer()
  TMT_server(id = "TMT_ui")
  DEP_visualize_server(id = "DEP_visualize")
  mv_noise_server(id = "mv_noise")
  mv_imputation_server(id = "mv_imputation")
  mv_summary_server(id = "mv_summary")
  DEP_analysis_server(id = "DEP_analysis")
  veen_server(id = "veen")
  protein_structure_server(id = "protein_structure")
  GO_and_KEGG_server(id = "GO_and_KEGG")
  # # PPI_network_server(id = "PPI_network")
  DR_analysis_server(id = "DR_analysis")
  Expression_profile_server(id = "Expression_profile")
}
