#' Raw Data Upload Module UI
#' @description
#' Provides a simple user interface for uploading raw data files. It includes
#' a file input selector and a display area to show basic metadata about the
#' uploaded file.
#' @param id A character string representing the Shiny module namespace ID.
#' @return A Shiny UI tag list containing a file input and a text output.
#' @import shiny
#' @name Raw_ui
#' @export
Raw_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fileInput(ns("file"), "Upload Raw Data File"),
    shiny::verbatimTextOutput(ns("file_info"))
  )
}
#' Raw Data Upload Module Server
#' @description
#' Handles the server-side logic for raw data uploads. It renders file
#' metadata (name, size, and extension) to the UI and returns a reactive
#' object containing the file path and name for use in other modules.
#' @param id A character string representing the Shiny module namespace ID.
#' @return A reactive expression that returns a list containing:
#' @import shiny
#' @importFrom tools file_ext
#' @name Raw_server
#' @export
Raw_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$file_info <- shiny::renderPrint({
      shiny::req(input$file)
      base::cat("Raw data file uploaded:\n")
      base::cat("Name:", input$file$name, "\n")
      base::cat("Size:", base::format(input$file$size, units = "auto"), "\n")
      base::cat("Type:", tools::file_ext(input$file$name), "\n")
    })

    shiny::reactive({
      shiny::req(input$file)
      base::list(
        path = input$file$datapath,
        name = input$file$name
      )
    })
  })
}
