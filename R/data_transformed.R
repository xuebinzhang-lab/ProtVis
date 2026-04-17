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
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,

        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold")
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
          shiny::actionButton(ns("run_transformation"), "Run Transformation", class = "btn btn-primary")
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
          style = "margin-top: 15px;",
          shiny::actionButton(ns("export_transformed_data"), "Export Data", class = "btn btn-light fw-bold")
        ),

        shiny::uiOutput(ns("export_transformed_data_status_panel"))
      ),

      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,

          bslib::card(
            height = "800px",
            bslib::card_header("Original Data"),
            bslib::card_body(
              DT::DTOutput(ns("originalData"))
            )
          ),

          bslib::card(
            height = "800px",
            bslib::card_header("Original Data Visualization"),
            bslib::card_body(
              shiny::plotOutput(ns("originalPlot"), height = "720px")
            )
          ),

          bslib::card(
            height = "800px",
            bslib::card_header("Transformed Data"),
            bslib::card_body(
              DT::DTOutput(ns("transformedData"))
            )
          ),

          bslib::card(
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
    rv <- shiny::reactiveValues(
      correct_noise_result = NULL,
      sample_info = NULL,
      load_success = FALSE,
      transformed = NULL,
      transformation_done = FALSE
    )

    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)

      rda_path <- base::file.path(shared_state$workdir, "Step3_correct_noise.rda")

      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        base::load(rda_path, envir = e)

        if (base::exists("sample_info", envir = e)) {
          rv$sample_info <- e$sample_info
        }

        if (base::exists("correct_noise_result", envir = e)) {
          rv$correct_noise_result <- e$correct_noise_result
        }

        rv$transformed <- NULL
        rv$transformation_done <- FALSE
        rv$load_success <- TRUE

        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("❌ Step3_correct_noise.rda not found.", type = "error")
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (isTRUE(rv$load_success)) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })

    output$transformation_status_panel <- shiny::renderUI({
      if (isTRUE(rv$transformation_done)) {
        shiny::span(
          paste0("✅ Current transformation method: ", input$data_transformed),
          style = "color: green;"
        )
      } else {
        shiny::span("ℹ️ Data transformation not run yet", style = "color: #666666;")
      }
    })

    original_matrix <- shiny::reactive({
      shiny::req(rv$correct_noise_result)
      rv$correct_noise_result
    })

    original_matrix_numeric <- shiny::reactive({
      shiny::req(rv$correct_noise_result)
      rv$correct_noise_result %>%
        tibble::column_to_rownames("ID")
    })

    shiny::observeEvent(input$run_transformation, {
      shiny::req(rv$correct_noise_result)

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

      rv$transformation_done <- TRUE

      shiny::showNotification(
        paste0("✅ Data transformation completed: ", input$data_transformed),
        type = "message"
      )
    })

    output$originalData <- DT::renderDT({
      shiny::req(original_matrix())
      DT::datatable(
        original_matrix(),
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    output$transformedData <- DT::renderDT({
      shiny::req(rv$load_success)

      show_df <- if (!is.null(rv$transformed)) {
        rv$transformed %>%
          as.data.frame() %>%
          tibble::rownames_to_column("ID")
      } else {
        original_matrix()
      }

      DT::datatable(
        show_df,
        options = list(scrollX = TRUE, pageLength = 10)
      )
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

      plot_df <- if (!is.null(rv$transformed)) {
        rv$transformed
      } else {
        original_matrix_numeric()
      }

      graphics::boxplot(
        plot_df,
        las = 2,
        col = input$transformed_boxplot_color,
        main = if (!is.null(rv$transformed)) {
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

        plot_df <- if (!is.null(rv$transformed)) {
          rv$transformed
        } else {
          original_matrix_numeric()
        }

        graphics::boxplot(
          plot_df,
          las = 2,
          col = input$transformed_boxplot_color,
          main = if (!is.null(rv$transformed)) {
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

    shiny::observeEvent(input$export_transformed_data, {
      shiny::req(shared_state$workdir, rv$sample_info, rv$correct_noise_result)

      export_data <- if (!is.null(rv$transformed)) {
        rv$transformed %>%
          as.data.frame() %>%
          tibble::rownames_to_column("ID")
      } else {
        rv$correct_noise_result
      }

      save_path <- base::file.path(shared_state$workdir, "Step4_data_transformed.rda")

      sample_info <- rv$sample_info
      correct_noise_result <- rv$correct_noise_result
      transformed <- export_data

      base::save(sample_info, correct_noise_result, transformed, file = save_path)

      shiny::showNotification(
        paste0("✅ Exported to: ", save_path),
        type = "message"
      )

      output$export_transformed_data_status_panel <- shiny::renderUI({
        shiny::span(
          paste0("✅ Data exported to: ", base::basename(save_path)),
          style = "color: green;"
        )
      })
    })

    base::return(rv)
  })
}
