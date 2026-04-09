#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @import shiny
#' @importFrom bslib bs_themer
#' @noRd
#' @name app_server
#' @export
#'
app_server <- function(input, output, session) {
  bslib::bs_themer()

  shared_state <- reactiveValues(
    workdir = NULL,
    sample_info = NULL,
    expression_matrix = NULL,
    data_source = NULL
  )
  project_init_server("project_init", shared_state = shared_state)
  data_input_server(
    "data_input",
    data_source_reactive = reactive(shared_state$data_source),
    shared_state = shared_state
  )
  MaxQuant_server("MaxQuant", shared_state = shared_state)
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
  protein_fun_server("protein_fun", shared_state)
  release_data_server("release_data1", shared_state)
  # -------------------------------------------------------------------------
  protein_extract_server("protein_extract")
  background_make_server("background_make")
  protein_links_server("prot_links")
  Expression_profile_server("Expression_profile")
  nine_quadrant_server("nine")
  venn_server("venn")
  protein_structure_server("protein_structure")
  boxplot_module_server("box1")
  swissmodel_server("swissmodel")
  stacked_column_chart_server("stacked_column_chart")
  DEG_server("DEG")
}
