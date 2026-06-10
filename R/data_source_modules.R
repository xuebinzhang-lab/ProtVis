#' Shared table reader for proteomics input modules
#'
#' @param file_info Shiny file input metadata for one uploaded file.
#' @return A data frame converted from csv, txt/tsv, or Excel input.
#' @keywords internal
.protvis_read_uploaded_table <- function(file_info) {
  shiny::req(file_info)
  ext <- base::tolower(tools::file_ext(file_info$name))

  if (ext == "csv") {
    return(utils::read.csv(
      file_info$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (ext %in% c("txt", "tsv")) {
    return(utils::read.delim(
      file_info$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  if (ext %in% c("xlsx", "xls")) {
    return(base::as.data.frame(readxl::read_excel(file_info$datapath)))
  }

  base::stop("Unsupported file type: ", ext)
}

#' Reusable UI for vendor-neutral proteomics tabular inputs
#'
#' @param id Module ID.
#' @param title Data source label shown in the UI.
#' @return A Shiny UI tag list.
#' @keywords internal
.protvis_tabular_source_ui <- function(id, title) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::card(
      bslib::card_header(base::paste(title, "data import")),
      bslib::card_body(
        shiny::p(
          "Upload a quantitative protein/peptide table and optional sample metadata. Supported formats: csv, txt/tsv, xlsx, and xls."
        ),
        shiny::fileInput(
          ns("protein_file"),
          "Upload quantitative table",
          multiple = FALSE,
          accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")
        ),
        shiny::fileInput(
          ns("sample_info"),
          "Upload sample information (optional)",
          multiple = FALSE,
          accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls")
        ),
        shiny::actionButton(ns("load_data"), "Load into shared project", class = "btn btn-primary"),
        shiny::hr(),
        shiny::uiOutput(ns("import_status"))
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Quantitative table preview"),
        DT::DTOutput(ns("protein_preview"))
      ),
      bslib::card(
        bslib::card_header("Sample information preview"),
        DT::DTOutput(ns("sample_preview"))
      )
    )
  )
}

#' Reusable server for vendor-neutral proteomics tabular inputs
#'
#' @param id Module ID.
#' @param shared_state A reactiveValues object used by the workflow.
#' @param data_source Data source label saved into shared_state.
#' @return No return value. Called for Shiny side effects.
#' @keywords internal
.protvis_tabular_source_server <- function(id, shared_state, data_source) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      expression_matrix = NULL,
      sample_info = NULL,
      status = "Upload a quantitative table, then click Load into shared project."
    )

    shiny::observeEvent(input$load_data, {
      shiny::req(input$protein_file)

      tryCatch({
        rv$expression_matrix <- .protvis_read_uploaded_table(input$protein_file)

        if (!base::is.null(input$sample_info)) {
          rv$sample_info <- .protvis_read_uploaded_table(input$sample_info)
          shared_state$sample_info <- rv$sample_info
        }

        shared_state$expression_matrix <- rv$expression_matrix
        shared_state$data_source <- data_source
        rv$status <- base::paste0(
          "Loaded ", base::nrow(rv$expression_matrix), " rows and ",
          base::ncol(rv$expression_matrix), " columns from ", data_source, "."
        )
        shiny::showNotification(rv$status, type = "message")
      }, error = function(e) {
        rv$status <- base::paste("Import failed:", e$message)
        shiny::showNotification(rv$status, type = "error")
      })
    })

    output$import_status <- shiny::renderUI({
      status_class <- if (base::startsWith(rv$status, "Import failed")) "alert alert-danger" else "alert alert-info"
      shiny::div(class = status_class, rv$status)
    })

    output$protein_preview <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = base::list(scrollX = TRUE, pageLength = 5))
    })

    output$sample_preview <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(rv$sample_info, options = base::list(scrollX = TRUE, pageLength = 5))
    })
  })
}

#' Mascot User Interface Module
#'
#' @param id Module ID.
#' @return A Shiny UI tag list for Mascot-compatible tabular imports.
#' @import shiny
#' @import bslib
#' @name Mascot_ui
#' @export
Mascot_ui <- function(id) {
  .protvis_tabular_source_ui(id, "Mascot")
}

#' Mascot Server Module
#'
#' @param id Module ID.
#' @param shared_state A reactiveValues object shared across modules.
#' @return No return value. Called for Shiny side effects.
#' @import shiny
#' @name Mascot_server
#' @export
Mascot_server <- function(id, shared_state) {
  .protvis_tabular_source_server(id, shared_state, "Mascot")
}
