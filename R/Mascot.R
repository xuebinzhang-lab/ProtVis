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
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::accordion(
      bslib::accordion_panel(
        title = "File Upload",
        icon = bsicons::bs_icon("upload"),
        shiny::fileInput(
          inputId = ns("protein_file"),
          label = "Upload Mascot CSV/export results (xlsx, xls, csv, txt or tsv)",
          multiple = FALSE,
          accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
        ),
        shiny::actionButton(ns("load_data"), "Parse Mascot Output", class = "btn btn-primary w-100"),
        shiny::fileInput(
          inputId = ns("sample_info"),
          label = "Upload Sample Info (xlsx, xls, csv, txt or tsv)",
          multiple = FALSE,
          accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
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
        icon = bsicons::bs_icon("filter-circle"),
        shiny::uiOutput(ns("group_select")),
        shiny::numericInput(ns("logFC_cutoff"), "Log2 Fold Change Threshold", value = 1, min = 0, step = 0.1),
        shiny::numericInput(ns("adj_pval"), "Adjusted P-value (FDR) Threshold", value = 0.05, min = 0, max = 1, step = 0.01)
      ),
      bslib::accordion_panel(
        title = "Export Results",
        icon = bsicons::bs_icon("download"),
        shiny::downloadButton(ns("download_cleaned"), "Download Cleaned Data"),
        shiny::downloadButton(ns("download_diff"), "Download DE Results")
      )
    ),
    shiny::tabsetPanel(
      shiny::tabPanel("UMAP", shiny::plotOutput(ns("umap_plot"))),
      shiny::tabPanel("Heatmap", shiny::plotOutput(ns("heatmap_plot"))),
      shiny::tabPanel("Boxplot", shiny::plotOutput(ns("boxplot"))),
      shiny::tabPanel("DE Table", DT::DTOutput(ns("de_table")))
    )
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
