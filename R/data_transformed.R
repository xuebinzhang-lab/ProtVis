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
#' @importFrom DT DTOutput
#' @name data_transformed_ui
#' @export
data_transformed_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    protvis_data_input_style(),
    bslib::layout_sidebar(
      class = "pv-mq-shell",
      sidebar = bslib::sidebar(
        width = 320,
        class = "pv-sidebar-card",

        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::actionButton(ns("load_data"), "Load data", class = "btn btn-primary fw-bold pv-load-button")
        ),

        shiny::uiOutput(ns("load_status_panel")),
        shiny::hr(),

        shiny::selectInput(
          ns("data_transformed"),
          "Transformation Method",
          choices = c(
            "None",
            "log10",
            "log2",
            "Standardization",
            "Z-Score",
            "scale",
            "center",
            "scale-center"
          ),
          selected = "None"
        ),

        shiny::div(
          style = "margin-top: 15px;",
          shiny::actionButton(ns("run_transformation"), "Run transformation", class = "btn btn-success fw-bold pv-load-button")
        ),

        shiny::uiOutput(ns("transformation_status_panel")),
        shiny::hr(),

        shiny::div(
          style = "margin-top: 15px;",
          colourpicker::colourInput(
            ns("original_boxplot_color"),
            "Original Data Boxplot Color",
            value = "#1f77b4"
          )
        ),

        shiny::div(
          style = "margin-top: 15px;",
          colourpicker::colourInput(
            ns("transformed_boxplot_color"),
            "Transformed Data Boxplot Color",
            value = "#ff7f0e"
          )
        ),

        shiny::div(
          style = "margin-top: 15px;",
          shiny::numericInput(
            ns("plot_width"),
            "Download Plot Width (inches)",
            value = 7,
            min = 1,
            max = 200
          )
        ),

        shiny::div(
          style = "margin-top: 15px;",
          shiny::numericInput(
            ns("plot_height"),
            "Download Plot Height (inches)",
            value = 10,
            min = 1,
            max = 200
          )
        ),

        shiny::div(
          style = "margin-top: 15px;",
          shiny::downloadButton(ns("download_original_plot"), "Download Original Plot (PDF)")
        ),

        shiny::div(
          style = "margin-top: 15px;",
          shiny::downloadButton(ns("download_transformed_plot"), "Download Transformed Plot (PDF)")
        ),

        shiny::hr(),
        shiny::div(
          class = "pv-status pv-status-ready",
          "Results are saved automatically to ProtVis_dataset."
        )
      ),

      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Original Data"),
            bslib::card_body(
              shiny::tableOutput(ns("originalData"))
            )
          ),

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Original Data Visualization"),
            bslib::card_body(
              shiny::plotOutput(ns("originalPlot"), height = "720px")
            )
          ),

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Transformed Data"),
            bslib::card_body(
              shiny::tableOutput(ns("transformedData"))
            )
          ),

          bslib::card(
            class = "pv-preview-card",
            height = "800px",
            bslib::card_header("Transformed Data Visualization"),
            bslib::card_body(
              shiny::plotOutput(ns("transformedPlot"), height = "720px")
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
#' - Applying selected transformations
#' - Data previews
#' - Plot downloads
#' - Export functionality
#' @param id Character string module ID for namespacing
#' @param shared_state Reactive values list for sharing data between modules
#' @return Server logic for the data transformation module
#' @import shiny
#' @importFrom DT renderDT datatable
#' @importFrom tibble column_to_rownames rownames_to_column
#' @name data_transformed_server
#' @export
data_transformed_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    # DT requires a data.frame with no row.names attribute. Keep row names
    # internally for matrix calculations, but strip them at the display edge.
    display_data_frame <- function(x) {
      result <- as.data.frame(x, check.names = FALSE,
                              stringsAsFactors = FALSE)
      rownames(result) <- NULL
      result
    }

    rv <- shiny::reactiveValues(
      correct_noise_result = NULL,
      sample_info = NULL,
      protein_ids = NULL,
      load_success = FALSE,
      transformed = NULL,
      transformation_done = FALSE
    )

    shiny::observeEvent(input$load_data, {
      # Use the canonical in-memory dataset first. The RDA branch is retained
      # only as a compatibility path for projects created before ProtVis_dataset.
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        rv$sample_info <- shared_state$dataset$sample_info
        rv$correct_noise_result <- protvis_expression_matrix(shared_state$dataset)
        rv$transformed <- NULL
        rv$transformation_done <- FALSE
        rv$load_success <- TRUE
        shiny::showNotification("✅ ProtVis_dataset loaded successfully.", type = "message")
        return(invisible(NULL))
      }
      shiny::req(shared_state$workdir)

      rda_path <- base::file.path(shared_state$workdir, "Step3_correct_noise.rda")

      if (base::file.exists(rda_path)) {
        dataset <- .protvis_load_stage_dataset(
          rda_path, expression_names = "correct_noise_result"
        )
        if (!base::is.null(dataset)) {
          shared_state$dataset <- dataset
          rv$sample_info <- dataset$sample_info
          rv$correct_noise_result <- protvis_expression_matrix(dataset)
        }

        rv$transformed <- NULL
        rv$transformation_done <- FALSE
        rv$load_success <- !base::is.null(dataset)

        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("❌ Step3_correct_noise.rda not found.", type = "error")
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (isTRUE(rv$load_success)) {
        shiny::div(class = "pv-status pv-status-ready", "✓ Data loaded")
      } else {
        shiny::div(class = "pv-status pv-status-empty", "× Data not loaded")
      }
    })

    output$transformation_status_panel <- shiny::renderUI({
      if (isTRUE(rv$transformation_done)) {
        shiny::div(
          class = "pv-status pv-status-ready",
          paste0("✓ Current transformation method: ", input$data_transformed)
        )
      } else {
        shiny::div(class = "pv-status pv-status-empty", "Data transformation not run yet")
      }
    })

    original_matrix_numeric <- shiny::reactive({
      shiny::req(rv$correct_noise_result)

      mat <- rv$correct_noise_result

      if (base::is.data.frame(mat) && "ID" %in% base::colnames(mat)) {
        # tibble::column_to_rownames() rejects data frames that already have
        # row names. ProtVis_dataset exports may retain row names, so convert
        # explicitly and safely at this boundary.
        ids <- base::as.character(mat[["ID"]])
        mat[["ID"]] <- NULL
        rv$protein_ids <- ids
      } else {
        rv$protein_ids <- base::rownames(mat)
      }

      mat <- as.data.frame(mat, check.names = FALSE)
      base::rownames(mat) <- NULL

      num_df <- base::as.data.frame(
        lapply(mat, function(x) base::as.numeric(as.character(x))),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      base::rownames(num_df) <- NULL

      num_df
    })

    original_matrix_show <- shiny::reactive({
      shiny::req(original_matrix_numeric())
      mat <- original_matrix_numeric()
      ids <- rv$protein_ids %||% base::seq_len(base::nrow(mat))
      result <- base::data.frame(ID = ids, mat,
                                 check.names = FALSE,
                                 stringsAsFactors = FALSE)
      base::rownames(result) <- NULL
      base::as.data.frame(result, check.names = FALSE,
                          stringsAsFactors = FALSE)
    })

    shiny::observeEvent(input$run_transformation, {
      shiny::req(original_matrix_numeric())

      df_mat <- original_matrix_numeric()

      rv$transformed <- base::switch(
        input$data_transformed,
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

      rv$transformed <- as.data.frame(
        rv$transformed,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      base::rownames(rv$transformed) <- NULL

      # Commit one completed analysis to the canonical object.  Legacy RDA
      # files remain a read-only compatibility path for old projects.
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        ids <- rv$protein_ids %||% base::paste0(
          "P", base::seq_len(base::nrow(rv$transformed))
        )
        transformed_matrix <- as.matrix(rv$transformed)
        storage.mode(transformed_matrix) <- "numeric"
        base::rownames(transformed_matrix) <- as.character(ids)
        base::colnames(transformed_matrix) <- base::colnames(
          shared_state$dataset$expression_data
        )
        dataset <- .protvis_new_analysis_dataset(
          shared_state$dataset, "transformation",
          list(method = input$data_transformed)
        )
        dataset <- .protvis_replace_expression(dataset, transformed_matrix)
        dataset$analysis_results$transformation <- list(
          status = "success", method = input$data_transformed
        )
        dataset <- .protvis_append_process(
          dataset, "transformation", status = "success",
          parameters = list(method = input$data_transformed)
        )
        directory <- protvis_output_directory(shared_state$workdir %||% getwd())
        dataset <- tryCatch(
          protvis_auto_export_dataset(dataset, directory = directory),
          error = function(e) dataset
        )
        .protvis_ui_sync_state(dataset, shared_state)
        .protvis_save_stage_dataset(
          dataset,
          base::file.path(directory, "Step4_data_transformed.rda")
        )
      }

      rv$transformation_done <- TRUE

      shiny::showNotification(
        paste0("✅ Data transformation completed: ", input$data_transformed),
        type = "message"
      )
    })

    output$originalData <- shiny::renderTable({
      shiny::req(original_matrix_show())
      utils::head(display_data_frame(original_matrix_show()), 100L)
    })

    output$transformedData <- shiny::renderTable({
      shiny::req(rv$load_success)

      show_df <- if (!base::is.null(rv$transformed)) {
        transformed <- base::as.data.frame(rv$transformed, check.names = FALSE,
                                           stringsAsFactors = FALSE)
        ids <- rv$protein_ids %||% base::seq_len(base::nrow(transformed))
        result <- base::data.frame(ID = ids, transformed,
                                   check.names = FALSE,
                                   stringsAsFactors = FALSE)
        base::rownames(result) <- NULL
        base::as.data.frame(result, check.names = FALSE,
                            stringsAsFactors = FALSE)
      } else {
        original_matrix_show()
      }

      utils::head(display_data_frame(show_df), 100L)
    })

    output$originalPlot <- shiny::renderPlot({
      shiny::req(original_matrix_numeric())

      plot_df <- original_matrix_numeric()

      graphics::boxplot(
        plot_df,
        las = 2,
        col = input$original_boxplot_color,
        main = "Original Data",
        ylab = "Intensity",
        cex.axis = 0.8,
        outline = FALSE
      )
    })

    output$transformedPlot <- shiny::renderPlot({
      shiny::req(rv$load_success)

      plot_df <- if (!base::is.null(rv$transformed)) {
        rv$transformed
      } else {
        original_matrix_numeric()
      }

      graphics::boxplot(
        plot_df,
        las = 2,
        col = input$transformed_boxplot_color,
        main = if (!base::is.null(rv$transformed)) {
          paste0("Transformed Data (", input$data_transformed, ")")
        } else {
          "Transformed Data"
        },
        ylab = "Value",
        cex.axis = 0.8,
        outline = FALSE
      )
    })

    output$download_original_plot <- shiny::downloadHandler(
      filename = function() {
        "original_data_boxplot.pdf"
      },
      content = function(file) {
        grDevices::pdf(
          file,
          width = input$plot_width,
          height = input$plot_height
        )

        plot_df <- original_matrix_numeric()

        graphics::boxplot(
          plot_df,
          las = 2,
          col = input$original_boxplot_color,
          main = "Original Data",
          ylab = "Intensity",
          cex.axis = 0.8,
          outline = FALSE
        )

        grDevices::dev.off()
      }
    )

    output$download_transformed_plot <- shiny::downloadHandler(
      filename = function() {
        "transformed_data_boxplot.pdf"
      },
      content = function(file) {
        grDevices::pdf(
          file,
          width = input$plot_width,
          height = input$plot_height
        )

        plot_df <- if (!base::is.null(rv$transformed)) {
          rv$transformed
        } else {
          original_matrix_numeric()
        }

        graphics::boxplot(
          plot_df,
          las = 2,
          col = input$transformed_boxplot_color,
          main = if (!base::is.null(rv$transformed)) {
            paste0("Transformed Data (", input$data_transformed, ")")
          } else {
            "Transformed Data"
          },
          ylab = "Value",
          cex.axis = 0.8,
          outline = FALSE
        )

        grDevices::dev.off()
      }
    )

    base::return(rv)
  })
}
