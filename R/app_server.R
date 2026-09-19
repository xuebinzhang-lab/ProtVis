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
    raw_directory = NULL,
    raw_sample_info = NULL,
    raw_fasta = NULL,
    raw_manifest = NULL,
    raw_check = NULL,
    sage_search_bundle = NULL,
    sage_search_parameters = list(),
    sage_workflow = FALSE,
    fragpipe_search_bundle = NULL,
    fragpipe_search_parameters = list(),
    fragpipe_workflow = FALSE,
    data_source = "Raw",
    dataset = NULL,
    dataset_history = list(),
    dataset_name = NULL,
    # Per-session operation locks prevent duplicate Shiny events from starting
    # the same long-running or state-changing task twice.
    run_locks = list(),
    # Keep the current DEP results available to downstream modules without
    # requiring a legacy Step7_DEP_result.rda file on disk.
    dep_results = list()
  )
  # Search is the Raw-data workflow. FASTA and mzML are validated when the
  # search is run, while the navigation item follows the selected source.
  shiny::observe({
    source <- as.character(shared_state$data_source %||% "Raw")
    visible <- identical(source, "Raw")
    session$sendCustomMessage("protvis-sage-nav", list(visible = visible))
  })
  shiny::observe({
    source <- as.character(shared_state$data_source %||% "Raw")
    session$sendCustomMessage(
      "protvis-data-input-nav",
      list(visible = identical(source, "MaxQuant"))
    )
  })
  project_init_server("project_init", shared_state = shared_state)
  sage_search_server("sage_search", shared_state = shared_state)
  fragpipe_search_server("fragpipe_search", shared_state = shared_state)
  protvis_dashboard_server("project_dashboard", shared_state = shared_state)
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
  gsea_server("gsea", shared_state = shared_state)
  pathview_server("pathview", shared_state = shared_state)
  PTM_server("PTM", shared_state = shared_state)
  psm_explorer_server("psm_explorer", shared_state = shared_state)
  # protein_fun_server("protein_fun", shared_state)
  release_data_server("release_data1", shared_state)
  stringdb_ppi_server("stringdb_ppi", shared_state = shared_state)
  # -------------------------------------------------------------------------
  protein_workbench_server("protein_workbench", shared_state = shared_state)
  protein_extract_server("protein_extract", shared_state = shared_state)
  plant_mploc_server("plant_mploc", shared_state = shared_state)
  background_make_server("background_make", shared_state = shared_state)
  protein_links_server("prot_links", shared_state = shared_state)
  Expression_profile_server("Expression_profile", shared_state = shared_state)
  wgcna_server("wgcna", shared_state)
  metaproteomics_server("metaproteomics", shared_state = shared_state)
  co_enrichment_server("co_enrichment", shared_state = shared_state)
  nine_quadrant_server("nine", shared_state = shared_state)
  venn_server("venn", shared_state = shared_state)
  protein_structure_server("protein_structure", shared_state = shared_state)
  boxplot_module_server("box1", shared_state = shared_state)
  swissmodel_server("swissmodel", shared_state = shared_state)
  stacked_column_chart_server("stacked_column_chart", shared_state = shared_state)
  correlation_chord_server("correlation_chord", shared_state = shared_state)
  DEG_server("DEG", shared_state = shared_state)
}
