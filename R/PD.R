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
  ns <- NS(id)
  bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('protein_file'),
            label = 'Upload Proteome Discoverer protein/peptide groups export (xlsx or csv)',
            multiple = FALSE,
            accept = c(".xlsx", ".xls", ".csv")
          ),
          shiny::actionButton(ns("load_data"), "Parse Proteome Discoverer Output", class = "btn btn-primary w-100"),
          shiny::fileInput(
            inputId = ns('sample_info'),
            label = 'Upload Sample Info (xlsx or csv)',
            multiple = FALSE,
            accept = c(".xlsx", ".xls", ".csv")
          )
        ),
        bslib::accordion_panel(
          title = "Preprocessing Options",
          icon = bsicons::bs_icon("gear"),
          shiny::checkboxInput(ns("log2_transform"), "Log2 Transformation", TRUE),
          shiny::checkboxInput(ns("knn_impute"), "KNN Imputation for Missing Values", TRUE),
          shiny::numericInput(ns("knn_k"), "K for KNN Imputation", value = 10, min = 1, max = 50, step = 1),
          shiny::checkboxInput(ns("normalize"), "Tissue-centered Normalization", TRUE)
        ),
        bslib::accordion_panel(
          title = "Differential Expression Settings",
          icon = bs_icon("filter-circle"),
          shiny::uiOutput(ns("group_select")),
          shiny::numericInput(ns("logFC_cutoff"), "Log2 Fold Change Threshold", value = 1, min = 0, step = 0.1),
          shiny::numericInput(ns("adj_pval"), "Adjusted P-value (FDR) Threshold", value = 0.05, min = 0, max = 1, step = 0.01)
        ),
        bslib::accordion_panel(
          title = "Theme and Colors",
          icon = bsicons::bs_icon("palette"),
          shiny::selectInput(
            ns("theme_choice"),
            "Select Theme",
            choices = c("default", "darkly", "flatly", "cerulean", "cosmo"),
            selected = "default"
          )
        ),
        bslib::accordion_panel(
          title = "Export Results",
          icon = bsicons::bs_icon("download"),
          shiny::downloadButton(ns("download_cleaned"), "Download Cleaned Data"),
          shiny::downloadButton(ns("download_diff"), "Download DE Results")
        )
      ),
      # 👇 只改这里：删掉 mainPanel，直接放 tabsetPanel
      shiny::tabsetPanel(
        shiny::tabPanel("PCA", shiny::plotOutput(ns("umap_plot"))),
        shiny::tabPanel("Heatmap", shiny::plotOutput(ns("heatmap_plot"))),
        shiny::tabPanel("Boxplot", shiny::plotOutput(ns("boxplot"))),
        shiny::tabPanel("DE Table", DT::DTOutput(ns("de_table")))
      )
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
