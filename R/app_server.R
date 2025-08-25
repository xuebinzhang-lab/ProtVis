options(shiny.maxRequestSize = 500*1024^2)
app_server <- function(input, output, session) {
  bslib::bs_themer()

  shared_state <- reactiveValues(
    workdir = NULL,
    sample_info = NULL,
    expression_matrix = NULL,
    data_source = NULL
  )

  # get_workdir <- function() {
  #   shared_state$workdir
  # }

  project_init_server("project_init", shared_state = shared_state)

  data_input_server(
    "data_input",
    data_source_reactive = reactive(shared_state$data_source),
    shared_state = shared_state
  )

  # ✅ 传同一个 shared_state
  MaxQuant_server("MaxQuant", shared_state = shared_state)
  data_imputation_server("data_imputation", shared_state = shared_state)

  correct_noise_server("correct_noise", shared_state = shared_state)
  data_transformed_server("data_transformed", shared_state = shared_state)
  data_normalization_server("data_normalization", shared_state = shared_state)

  # -------------------------------------------------------------------------
  overview_server("overview", shared_state)
  DEP_analysis_server("DEP_analysis", shared_state = shared_state)

  release_data_server("release_data1", shared_state)

  # -------------------------------------------------------------------------
  protein_extract_server("protein_extract")
  background_make_server("background_make")
  # -------------------------------------------------------------------------


  missing_value_server("missing_value", shared_state = shared_state)

  # -------------------------------------------------------------------------


  # mv_noise_server("mv_noise")
  # mv_imputation_server("mv_imputation")
  # mv_summary_server("mv_summary")
  # DEP_visualize_server("DEP_visualize")
  # veen_server("veen")
  # protein_structure_server("protein_structure")
  # GO_and_KEGG_server("GO_and_KEGG")
  # DR_analysis_server("DR_analysis")
  # Expression_profile_server("Expression_profile")


}
