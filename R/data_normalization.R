#' Perform median subtraction normalization on a data matrix
#'
#' This function subtracts the median value (ignoring NAs) from each column of the input data matrix.
#' It is commonly used for sample normalization in omics data analysis.
#'
#' @param data A numeric matrix or data frame containing the data to be normalized
#' @return A matrix with the same dimensions as input where each column has been median-centered
#' @importFrom stats median
#' @name sample_subtract
#' @export
#'
sample_subtract <- function(data) {
  # Median subtraction (ignoring NA)
  data_median_subtracted <- base::apply(data, 2, function(x) x - stats::median(x, na.rm = TRUE))
  return(data_median_subtracted)
}

#' UI module for data normalization
#'
#' Creates the user interface for data normalization module which includes:
#' - Data loading controls
#' - Visualization of original and normalized data
#' - Normalization execution button
#'
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing the module interface
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs
#' @importFrom colourpicker colourInput
#' @name data_normalization_ui
#' @export
#'

data_normalization_ui <- function(id) {
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
              plotOutput(ns("originalPlot"))
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

#' Server module for data normalization
#'
#' Handles the server-side logic for data normalization including:
#' - Loading input data
#' - Performing median subtraction normalization
#' - Generating visualizations
#' - Saving results
#'
#' @param id The namespace identifier for the module
#' @param shared_state A reactiveValues object containing shared state between modules
#' @return A module server function
#' @import shiny
#' @importFrom DT renderDT datatable
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot aes xlab ylab geom_boxplot coord_flip scale_fill_manual theme_bw
#' @importFrom grDevices pdf dev.off
#' @name data_normalization_server
#' @export
#'

data_normalization_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE,
      normalized_matrix = NULL
    )
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      rda_path <- base::file.path(shared_state$workdir, "Step5_data_imputation.rda")
      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        base::load(rda_path, envir = e)
        if (base::exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (base::exists("imputed_df", envir = e)) {
          rv$expression_matrix <- e$imputed_df
        } else {
          rv$expression_matrix <- NULL
          shiny::showNotification("Step5_data_imputation.rda does not exist. Expression matrix cannot be loaded.", type = "warning")
        }
        rv$load_success <- TRUE
        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("Step5_data_imputation.rda not found.", type = "error")
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    output$originalData <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = list(scrollX = TRUE))
    })
    output$originalPlot <- shiny::renderPlot({
      shiny::req(rv$sample_info)
      shiny::req(rv$expression_matrix)
      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix
      expmat_before <- expression_matrix
      expmat_before.long <-
        expmat_before %>%
        tibble::rownames_to_column("ID") %>%
        tidyr::pivot_longer(!ID, names_to = "sample_id", values_to = "intensity") %>%
        dplyr::left_join(sample_info)
      unique_groups <- base::unique(sample_info$group)
      n_groups <- base::length(unique_groups)
      colors <- RColorBrewer::brewer.pal(n_groups, "Set3")
      ggplot2::ggplot(data = expmat_before.long, mapping = ggplot2::aes(x = sample_id, y = intensity, fill = group)) +
        ggplot2::xlab("") +
        ggplot2::ylab("Relative intensity") +
        ggplot2::geom_boxplot(outlier.size = 0.1, linewidth = 0.5, staplewidth = 0.5, fatten = 0.5) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::theme_bw()
    })
    shiny::observeEvent(input$run_normalization, {
      shiny::req(rv$expression_matrix)
      shiny::req(rv$sample_info)
      sample_info <- rv$sample_info
      normalized_data <- sample_subtract(rv$expression_matrix)
      rv$normalized_matrix <- base::as.data.frame(normalized_data)
      base::save(sample_info, normalized_data, file = base::file.path(shared_state$workdir, "Step6_data_normalization.rda"))
      shiny::showNotification("Normalization completed", type = "message")
    })
    output$dataNormalization <- DT::renderDT({
      shiny::req(rv$normalized_matrix)
      DT::datatable(rv$normalized_matrix, options = list(scrollX = TRUE))
    })
    output$dataNormalizationPlot <- shiny::renderPlot({
      shiny::req(rv$sample_info)
      shiny::req(rv$normalized_matrix)
      sample_info <- rv$sample_info
      normalized_matrix <- rv$normalized_matrix
      expmat_before <- normalized_matrix
      expmat_before.long <-
        expmat_before %>%
        tibble::rownames_to_column("ID") %>%
        tidyr::pivot_longer(!ID, names_to = "sample_id", values_to = "intensity") %>%
        dplyr::left_join(sample_info)
      unique_groups <- base::unique(sample_info$group)
      n_groups <- base::length(unique_groups)
      colors <- RColorBrewer::brewer.pal(n_groups, "Set3")
      ggplot2::ggplot(data = expmat_before.long, mapping = ggplot2::aes(x = sample_id, y = intensity, fill = group)) +
        ggplot2::xlab("") +
        ggplot2::ylab("Relative intensity") +
        ggplot2::geom_boxplot(outlier.size = 0.1, linewidth = 0.5, staplewidth = 0.5, fatten = 0.5) +
        ggplot2::coord_flip() +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::theme_bw()
    })
    output$download_original_plot <- shiny::downloadHandler(
      filename = function() {
        paste("original_data_boxplot", ".pdf", sep = "")
      },
      content = function(file) {
        grDevices::pdf(file, width = input$plot_width, height = input$plot_height)
        print({
          shiny::req(rv$sample_info)
          shiny::req(rv$expression_matrix)

          sample_info <- rv$sample_info
          expression_matrix <- rv$expression_matrix

          expmat_before <- expression_matrix
          expmat_before.long <-
            expmat_before %>%
            tibble::rownames_to_column("ID") %>%
            tidyr::pivot_longer(!ID, names_to = "sample_id", values_to = "intensity") %>%
            dplyr::left_join(sample_info)
          unique_groups <- base::unique(sample_info$group)
          n_groups <- base::length(unique_groups)
          colors <- RColorBrewer::brewer.pal(n_groups, "Set3")
          ggplot2::ggplot(data = expmat_before.long, mapping = ggplot2::aes(x = sample_id, y = intensity, fill = group)) +
            ggplot2::xlab("") +
            ggplot2::ylab("Relative intensity") +
            ggplot2::geom_boxplot(outlier.size = 0.1, linewidth = 0.5, staplewidth = 0.5, fatten = 0.5) +
            ggplot2::coord_flip() +
            ggplot2::scale_fill_manual(values = colors) +
            ggplot2::theme_bw()
        })
        grDevices::dev.off()
      }
    )
    output$download_normalized_plot <- shiny::downloadHandler(
      filename = function() {
        paste("normalized_data_boxplot", ".pdf", sep = "")
      },
      content = function(file) {
        grDevices::pdf(file, width = input$plot_width, height = input$plot_height)
        print({
          shiny::req(rv$sample_info)
          shiny::req(rv$normalized_matrix)
          sample_info <- rv$sample_info
          normalized_matrix <- rv$normalized_matrix
          expmat_before <- normalized_matrix
          expmat_before.long <-
            expmat_before %>%
            tibble::rownames_to_column("ID") %>%
            tidyr::pivot_longer(!ID, names_to = "sample_id", values_to = "intensity") %>%
            dplyr::left_join(sample_info)
          unique_groups <- base::unique(sample_info$group)
          n_groups <- base::length(unique_groups)
          colors <- RColorBrewer::brewer.pal(n_groups, "Set3")
          ggplot2::ggplot(data = expmat_before.long, mapping = ggplot2::aes(x = sample_id, y = intensity, fill = group)) +
            ggplot2::xlab("") +
            ggplot2::ylab("Relative intensity") +
            ggplot2::geom_boxplot(outlier.size = 0.1, linewidth = 0.5, staplewidth = 0.5, fatten = 0.5) +
            ggplot2::coord_flip() +
            ggplot2::scale_fill_manual(values = colors) +
            ggplot2::theme_bw()
        })
        grDevices::dev.off()
      }
    )
  })
}
