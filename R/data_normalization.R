#' Perform median subtraction normalization on a data matrix
#'
#' This function subtracts the median value of each numeric column
#' (ignoring NA values) from the corresponding column.
#'
#' @param data A numeric matrix or data frame containing expression values.
#'
#' @return A numeric matrix with the same dimensions as the input,
#'   where each column has been median-centered.
#'
#' @importFrom stats median
#' @name sample_subtract
#' @export
sample_subtract <- function(data) {
  data_median_subtracted <- base::apply(
    data,
    2,
    function(x) x - stats::median(x, na.rm = TRUE)
  )
  data_median_subtracted <- base::as.data.frame(
    data_median_subtracted,
    stringsAsFactors = FALSE
  )
  return(data_median_subtracted)
}



#' UI module for data normalization
#'
#' Creates the user interface for the data normalization module, including:
#' - loading imputed data
#' - running normalization
#' - viewing original and normalized data
#' - downloading boxplots
#'
#' @param id Character string. Module namespace ID.
#'
#' @return A Shiny UI object for the normalization module.
#'
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs
#' @importFrom colourpicker colourInput
#' @importFrom DT DTOutput
#' @name data_normalization_ui
#' @export
data_normalization_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    protvis_data_input_style(),
    bslib::layout_sidebar(
      class = "pv-mq-shell",
      sidebar = bslib::sidebar(
        width = 320,
        class = "pv-sidebar-card",

        shiny::actionButton(
          ns("load_data"),
          "Load data",
          class = "btn btn-primary fw-bold pv-load-button"
        ),

        shiny::uiOutput(ns("load_status_panel")),
        shiny::hr(),

        shiny::actionButton(
          ns("run_normalization"),
          "Run normalization",
          class = "btn btn-success fw-bold pv-load-button"
        ),

        shiny::uiOutput(ns("normalization_status_panel")),
        shiny::hr(),

        colourpicker::colourInput(
          ns("original_boxplot_color"),
          "Original Data Boxplot Color",
          value = "#B51F9C"
        ),

        colourpicker::colourInput(
          ns("normalized_boxplot_color"),
          "Normalized Data Boxplot Color",
          value = "#FF7F0E"
        ),

        shiny::numericInput(
          ns("plot_width"),
          "Download Plot Width (inches)",
          value = 7,
          min = 1,
          max = 200
        ),

        shiny::numericInput(
          ns("plot_height"),
          "Download Plot Height (inches)",
          value = 10,
          min = 1,
          max = 200
        ),

        shiny::downloadButton(
          ns("download_original_plot"),
          "Download Original Plot (PDF)"
        ),

        shiny::downloadButton(
          ns("download_normalized_plot"),
          "Download Normalized Plot (PDF)"
        )
      ),

      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1 / 2,
          height = 600,

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Original Data"),
            bslib::card_body(
              DT::DTOutput(ns("originalData"))
            )
          ),

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Original Data Visualization"),
            bslib::card_body(
              shiny::plotOutput(ns("originalPlot"))
            )
          ),

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Normalized Data"),
            bslib::card_body(
              DT::DTOutput(ns("normalizedData"))
            )
          ),

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Normalized Data Visualization"),
            bslib::card_body(
              shiny::plotOutput(ns("normalizedPlot"))
            )
          )
        )
      )
    )
  )
}



#' Server module for data normalization
#'
#' Handles the server-side logic for data normalization using
#' median subtraction normalization.
#'
#' This module:
#' - loads imputed data from `Step5_data_imputation.rda`
#' - applies column-wise median subtraction normalization
#' - displays original and normalized data
#' - saves normalized results to `Step6_data_normalization.rda`
#' - provides downloadable PDF boxplots
#'
#' @param id Character string. Module namespace ID.
#' @param shared_state A reactiveValues object shared between modules.
#'   It must contain at least `workdir`.
#'
#' @return No direct return value. This function generates Shiny outputs and
#'   writes `Step6_data_normalization.rda`.
#'
#' @import shiny
#' @importFrom DT renderDT datatable
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes xlab ylab geom_boxplot coord_flip theme_bw
#' @importFrom grDevices pdf dev.off
#' @name data_normalization_server
#' @export
data_normalization_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      normalization_done = FALSE
    )

    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)

      rda_path <- base::file.path(
        shared_state$workdir,
        "Step5_data_imputation.rda"
      )

      if (base::file.exists(rda_path)) {
        dataset <- .protvis_load_stage_dataset(
          rda_path, expression_names = "imputed_df"
        )
      } else if (inherits(shared_state$dataset, "ProtVis_dataset") &&
                 identical(shared_state$dataset$process_info$active_stage,
                           "imputation")) {
        dataset <- shared_state$dataset
      } else {
        dataset <- NULL
      }

      if (!base::is.null(dataset)) {
        shared_state$dataset <- dataset
        rv$sample_info <- dataset$sample_info
        rv$expression_matrix <- dataset$expression_data

        rv$normalized_matrix <- NULL
        rv$normalization_done <- FALSE
        rv$load_success <- TRUE

        shiny::showNotification(
          "✅ Imputed mass_dataset loaded successfully.",
          type = "message"
        )
      } else {
        rv$load_success <- FALSE
        shiny::showNotification(
          "❌ Step5_data_imputation.rda not found.",
          type = "error"
        )
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (isTRUE(rv$load_success)) {
        shiny::div(class = "pv-status pv-status-ready", "✓ Data loaded")
      } else {
        shiny::div(class = "pv-status pv-status-empty", "× Data not loaded")
      }
    })

    output$normalization_status_panel <- shiny::renderUI({
      if (isTRUE(rv$normalization_done)) {
        shiny::div(class = "pv-status pv-status-ready", "✓ Median subtraction normalization completed")
      } else {
        shiny::div(class = "pv-status pv-status-empty", "Normalization not run yet")
      }
    })

    original_matrix_numeric <- shiny::reactive({
      shiny::req(rv$expression_matrix)

      df <- base::as.data.frame(
        rv$expression_matrix,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if ("ID" %in% base::colnames(df)) {
        ids <- df$ID
        mat <- df[, base::setdiff(base::colnames(df), "ID"), drop = FALSE]
      } else {
        ids <- base::rownames(df)
        mat <- df
      }

      mat <- base::as.data.frame(
        base::lapply(mat, function(x) base::as.numeric(base::as.character(x))),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (base::is.null(ids) || length(ids) != base::nrow(mat)) {
        ids <- base::as.character(base::seq_len(base::nrow(mat)))
      }

      base::rownames(mat) <- ids
      mat
    })

    output$originalData <- DT::renderDT({
      shiny::req(original_matrix_numeric())

      DT::datatable(
        original_matrix_numeric(),
        options = list(
          scrollX = TRUE,
          pageLength = 10
        ),
        rownames = TRUE
      )
    })

    shiny::observeEvent(input$run_normalization, {
      shiny::req(original_matrix_numeric(), shared_state$workdir)

      expr_df <- original_matrix_numeric()

      normalized_data <- sample_subtract(expr_df)
      normalized_data <- base::as.data.frame(
        normalized_data,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (!base::is.null(base::rownames(expr_df))) {
        base::rownames(normalized_data) <- base::rownames(expr_df)
      }

      rv$normalized_matrix <- normalized_data
      rv$normalization_done <- TRUE

      dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        shared_state$dataset
      } else {
        create_protvis_dataset(
          expr_df, sample_info = rv$sample_info
        )
      }
      dataset <- .protvis_update_expression(dataset, normalized_data)
      dataset <- .protvis_new_analysis_dataset(
        dataset, "normalization", list(method = "median_subtraction")
      )
      dataset$analysis_results$normalization <- list(
        status = "success", method = "median_subtraction"
      )
      dataset <- .protvis_append_process(
        dataset, "normalization", status = "success",
        parameters = list(method = "median_subtraction")
      )
      .protvis_ui_sync_state(dataset, shared_state)
      .protvis_save_stage_dataset(
        dataset,
        base::file.path(
          shared_state$workdir, "Step6_data_normalization.rda"
        )
      )

      shiny::showNotification("✅ Normalization completed", type = "message")
    })

    output$normalizedData <- DT::renderDT({
      shiny::req(rv$normalized_matrix)

      DT::datatable(
        rv$normalized_matrix,
        options = list(
          scrollX = TRUE,
          pageLength = 10
        ),
        rownames = TRUE
      )
    })

    output$originalPlot <- shiny::renderPlot({
      shiny::req(original_matrix_numeric())

      expmat_before <- original_matrix_numeric()

      expmat_before_long <- .protvis_rownames_to_column(expmat_before, "ID")
      expmat_before_long <- tidyr::pivot_longer(
        expmat_before_long,
        cols = -ID,
        names_to = "sample_id",
        values_to = "intensity"
      )

      plot_color <- input$original_boxplot_color

      ggplot2::ggplot(
        data = expmat_before_long,
        mapping = ggplot2::aes(x = sample_id, y = intensity)
      ) +
        ggplot2::xlab("") +
        ggplot2::ylab("Relative intensity") +
        ggplot2::geom_boxplot(
          fill = plot_color,
          outlier.size = 0.1,
          linewidth = 0.5,
          staplewidth = 0.5,
          fatten = 0.5
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_bw()
    })

    output$normalizedPlot <- shiny::renderPlot({
      shiny::req(rv$normalized_matrix)

      plot_df <- rv$normalized_matrix

      plot_long <- .protvis_rownames_to_column(plot_df, "ID")
      plot_long <- tidyr::pivot_longer(
        plot_long,
        cols = -ID,
        names_to = "sample_id",
        values_to = "intensity"
      )

      plot_color <- input$normalized_boxplot_color

      ggplot2::ggplot(
        data = plot_long,
        mapping = ggplot2::aes(x = sample_id, y = intensity)
      ) +
        ggplot2::xlab("") +
        ggplot2::ylab("Relative intensity") +
        ggplot2::geom_boxplot(
          fill = plot_color,
          outlier.size = 0.1,
          linewidth = 0.5,
          staplewidth = 0.5,
          fatten = 0.5
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_bw()
    })

    output$download_original_plot <- shiny::downloadHandler(
      filename = function() {
        "original_data_boxplot.pdf"
      },
      content = function(file) {
        shiny::req(original_matrix_numeric())

        grDevices::pdf(
          file = file,
          width = input$plot_width,
          height = input$plot_height
        )

        expmat_before <- original_matrix_numeric()

        expmat_before_long <- .protvis_rownames_to_column(expmat_before, "ID")
        expmat_before_long <- tidyr::pivot_longer(
          expmat_before_long,
          cols = -ID,
          names_to = "sample_id",
          values_to = "intensity"
        )

        plot_color <- input$original_boxplot_color

        print(
          ggplot2::ggplot(
            data = expmat_before_long,
            mapping = ggplot2::aes(x = sample_id, y = intensity)
          ) +
            ggplot2::xlab("") +
            ggplot2::ylab("Relative intensity") +
            ggplot2::geom_boxplot(
              fill = plot_color,
              outlier.size = 0.1,
              linewidth = 0.5,
              staplewidth = 0.5,
              fatten = 0.5
            ) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw()
        )

        grDevices::dev.off()
      }
    )

    output$download_normalized_plot <- shiny::downloadHandler(
      filename = function() {
        "normalized_data_boxplot.pdf"
      },
      content = function(file) {
        shiny::req(rv$normalized_matrix)

        grDevices::pdf(
          file = file,
          width = input$plot_width,
          height = input$plot_height
        )

        plot_df <- rv$normalized_matrix

        plot_long <- .protvis_rownames_to_column(plot_df, "ID")
        plot_long <- tidyr::pivot_longer(
          plot_long,
          cols = -ID,
          names_to = "sample_id",
          values_to = "intensity"
        )

        plot_color <- input$normalized_boxplot_color

        print(
          ggplot2::ggplot(
            data = plot_long,
            mapping = ggplot2::aes(x = sample_id, y = intensity)
          ) +
            ggplot2::xlab("") +
            ggplot2::ylab("Relative intensity") +
            ggplot2::geom_boxplot(
              fill = plot_color,
              outlier.size = 0.1,
              linewidth = 0.5,
              staplewidth = 0.5,
              fatten = 0.5
            ) +
            ggplot2::coord_flip() +
            ggplot2::theme_bw()
        )

        grDevices::dev.off()
      }
    )
  })
}
