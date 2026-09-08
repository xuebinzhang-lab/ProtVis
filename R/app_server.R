#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'   DO NOT REMOVE.
#'
#' @import shiny
#' @name app_server
#' @export
#'
app_server <- function(input, output, session) {
  shared_state <- reactiveValues(
    workdir = NULL,
    sample_info = NULL,
    expression_matrix = NULL,
    expression_matrix_filtered = NULL,
    data_source = NULL,
    dataset = NULL,
    dataset_history = list(),
    dataset_name = NULL
  )
  project_init_server("project_init", shared_state = shared_state)
  data_input_server(
    "data_input",
    data_source_reactive = reactive(shared_state$data_source),
    shared_state = shared_state
  )
  data_imputation_server("data_imputation", shared_state = shared_state)
  correct_noise_server("correct_noise", shared_state = shared_state)
  data_transformed_server("data_transformed", shared_state = shared_state)
  missing_value_server("missing_value", shared_state = shared_state)
  data_normalization_server("data_normalization", shared_state = shared_state)
  # -------------------------------------------------------------------------
  overview_server("overview", shared_state)
  DEP_analysis_server("DEP_analysis", shared_state = shared_state)
  enrichment_analysis_server("enrichment_analysis", shared_state = shared_state)
  gsea_server("gsea")
  pathview_server("pathview")
  PTM_server("PTM")
  pd_strict_module_server("pd_strict")
  # protein_fun_server("protein_fun", shared_state)
  release_data_server("release_data1", shared_state)
  stringdb_ppi_server("stringdb_ppi")
  # -------------------------------------------------------------------------
  protein_extract_server("protein_extract")
  background_make_server("background_make")
  protein_links_server("prot_links")
  Expression_profile_server("Expression_profile")
  wgcna_server("wgcna", shared_state)
  metaproteomics_server("metaproteomics")
  co_enrichment_server("co_enrichment")
  nine_quadrant_server("nine")
  venn_server("venn")
  protein_structure_server("protein_structure")
  boxplot_module_server("box1")
  swissmodel_server("swissmodel")
  stacked_column_chart_server("stacked_column_chart")
  correlation_chord_server("correlation_chord")
  DEG_server("DEG")
}
