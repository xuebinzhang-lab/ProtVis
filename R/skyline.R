skyline_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'skyline',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('protein_file'),
            label = 'Upload ProteinGroups (xlsx or csv)',
            multiple = FALSE,
            accept = c(".xlsx", ".xls", ".csv")
          ),
          fileInput(
            inputId = ns('sample_info'),
            label = 'Upload Sample Info (xlsx or csv)',
            multiple = FALSE,
            accept = c(".xlsx", ".xls", ".csv")
          )
        ),

        accordion_panel(
          title = "Preprocessing Options",
          icon = bs_icon("gear"),
          checkboxInput(ns("log2_transform"), "Log2 Transformation", TRUE),
          checkboxInput(ns("knn_impute"), "KNN Imputation for Missing Values", TRUE),
          numericInput(ns("knn_k"), "K for KNN Imputation", value = 10, min = 1, max = 50, step = 1),
          checkboxInput(ns("normalize"), "Tissue-centered Normalization", TRUE)
        ),

        accordion_panel(
          title = "Differential Expression Settings",
          icon = bs_icon("filter-circle"),
          uiOutput(ns("group_select")),
          numericInput(ns("logFC_cutoff"), "Log2 Fold Change Threshold", value = 1, min = 0, step = 0.1),
          numericInput(ns("adj_pval"), "Adjusted P-value (FDR) Threshold", value = 0.05, min = 0, max = 1, step = 0.01)
        ),

        accordion_panel(
          title = "Theme and Colors",
          icon = bs_icon("palette"),
          selectInput(
            ns("theme_choice"),
            "Select Theme",
            choices = c("default", "darkly", "flatly", "cerulean", "cosmo"),
            selected = "default"
          )
        ),

        accordion_panel(
          title = "Export Results",
          icon = bs_icon("download"),
          downloadButton(ns("download_cleaned"), "Download Cleaned Data"),
          downloadButton(ns("download_diff"), "Download DE Results")
        )
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("UMAP", plotOutput(ns("umap_plot"))),
          tabPanel("Heatmap", plotOutput(ns("heatmap_plot"))),
          tabPanel("Boxplot", plotOutput(ns("boxplot"))),
          tabPanel("DE Table", DT::dataTableOutput(ns("de_table")))
        )
      )
    )
  )
}
