#' UI for Data Transformation Module
#'
#' Creates the user interface for the data transformation module which includes:
#' - Data loading controls
#' - Transformation method selection
#' - Data preview tabs
#' - Export functionality
#' @param id Character string module ID for namespacing
#' @return A Shiny UI layout with sidebar controls and main display area
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs
#' @importFrom colourpicker colourInput
#' @name data_transformed_ui
#' @export

data_transformed_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::div(style = "margin-bottom: 15px;",
                   shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold")
        ),
        shiny::uiOutput(ns("load_status_panel")),
        shiny::hr(),
        shiny::div(style = "margin-top: 15px;",
                   shiny::actionButton(ns("run_normalization"), "Run Normalization", class = "btn btn-primary")
        ),
        shiny::hr(),
        shiny::div(style = "margin-top: 15px;",
            colourpicker::colourInput(ns("original_boxplot_color"), "Original Data Boxplot Color", value = "#1f77b4")
        ),
        shiny::div(style = "margin-top: 15px;",
            colourpicker::colourInput(ns("normalized_boxplot_color"), "Normalized Data Boxplot Color", value = "#ff7f0e")
        ),
        shiny::div(style = "margin-top: 15px;",
                   shiny::numericInput(ns("plot_width"), "Download Plot Width (inches)", value = 7, min = 0, max = 200)
        ),
        shiny::div(style = "margin-top: 15px;",
                   shiny::numericInput(ns("plot_height"), "Download Plot Height (inches)", value = 10, min = 0, max = 200)
        ),
        shiny::div(style = "margin-top: 15px;",
                   shiny::downloadButton(ns("download_original_plot"), "Download Original Plot (PDF)")
        ),
        shiny::div(style = "margin-top: 15px;",
                   shiny::downloadButton(ns("download_normalized_plot"), "Download Normalized Plot (PDF)")
        )
      ),
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,
          height = 600,
          bslib::card(
            height = "800px",
            bslib::card_header("Original Data"),
            bslib::card_body(
              DT::DTOutput(ns("originalData"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Original Data visualize"),
            bslib::card_body(
              shiny::plotOutput(ns("originalPlot"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Normalized Data"),
            bslib::card_body(
              DT::DTOutput(ns("dataNormalization"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Normalized Data Visualization"),
            bslib::card_body(
              shiny::plotOutput(ns("dataNormalizationPlot"))
            )
          )
        )
      )
    )
  )
}

#' Server Logic for Data Transformation Module
#' Handles the server-side processing for data transformation including:
#' - Loading input data
#' - Applying selected transformations (log10, log2, scaling, etc.)
#' - Data previews
#' - Export functionality
#' @param id Character string module ID for namespacing
#' @param shared_state Reactive values list for sharing data between modules
#' @return Server logic for the data transformation module
#' @import shiny
#' @importFrom DT renderDT datatable
#' @importFrom tibble column_to_rownames
#' @name data_transformed_server
#' @export
#'

data_transformed_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(
      correct_noise_result = NULL,
      sample_info = NULL,
      load_success = FALSE,
      transformed = NULL
    )
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      rda_path <- base::file.path(shared_state$workdir, "Step3_correct_noise.rda")
      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        base::load(rda_path, envir = e)
        if (base::exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (base::exists("correct_noise_result", envir = e)) {
          rv$correct_noise_result <- e$correct_noise_result
        }
        rv$load_success <- TRUE
        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("❌ Step3_correct_noise.rda not found.", type = "error")
      }
    })
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    output$tbl_sample_info <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE))
    })
    output$tbl_expression_matrix <- DT::renderDT({
      shiny::req(rv$correct_noise_result)
      DT::datatable(rv$correct_noise_result, options = list(scrollX = TRUE))
    })
    shiny::observe({
      shiny::req(input$data_transformed, rv$correct_noise_result)
      df <- rv$correct_noise_result
      # Ensure ID column is the row name
      df_mat <- df %>% tibble::column_to_rownames("ID")
      rv$transformed <- base::switch(input$data_transformed,
                               "None" = df_mat,
                               "log10" = log10(df_mat + 1e-8),
                               "log2" = log2(df_mat + 1e-8),
                               "Standardization" = scale(df_mat, center = TRUE, scale = TRUE),
                               "Z-Score" = scale(df_mat, center = TRUE, scale = TRUE),
                               "scale" = scale(df_mat, center = FALSE, scale = TRUE),
                               "center" = scale(df_mat, center = TRUE, scale = FALSE),
                               "scale-center" = scale(df_mat, center = TRUE, scale = TRUE),
                               df_mat
      )
    })
    shiny::observeEvent(input$export_remove_noise_data, {
      shiny::req(shared_state$workdir)
      save_path <- base::file.path(shared_state$workdir, "Step4_data_transformed.rda")
      sample_info <- rv$sample_info
      correct_noise_result <- rv$correct_noise_result
      transformed <- rv$transformed
      base::save(sample_info, correct_noise_result, transformed, file = save_path)
      shiny::showNotification(paste0("✅ Exported to: ", save_path), type = "message")
      output$export_remove_noise_data_status_panel <- shiny::renderUI({
        shiny::span(paste0("✅ Data exported to: ", base::basename(save_path)), style = "color: green;")
      })
    })
    output$table_data_transformed <- DT::renderDT({
      shiny::req(rv$transformed)
      DT::datatable(rv$transformed, options = list(scrollX = TRUE))
    })
    base::return(rv)
  })
}
