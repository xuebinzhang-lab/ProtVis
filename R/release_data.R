#' Release Data UI Module
#'
#' Creates the user interface for the release data module.
#' This module allows users to load .rda files and export their contents to xlsx or csv format.
#'
#' @param id The namespace identifier for the module
#' @return A tagList containing the UI elements
#' @import shiny
#' @export
#' @examples
#' \dontrun{
#' ui <- release_data_ui("release_data")
#' }
release_data_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::verbatimTextOutput(ns("workdir_display")),
    shiny::actionButton(ns("load_files"), "Load .rda Files"),
    shiny::uiOutput(ns("rda_files_ui")),
    shiny::selectInput(ns("output_format"), "Output Format", choices = c("xlsx", "csv")),
    shiny::actionButton(ns("export_btn"), "Export")
  )
}

#' Release Data Server Module
#'
#' Server-side logic for the release data module.
#' Handles loading .rda files and exporting their contents to the selected format.
#'
#' @param id The namespace identifier for the module
#' @param shared_state A reactive list containing shared state variables (must include 'workdir')
#' @return None (server-side module)
#' @import shiny
#' @import openxlsx
#' @import tools
#' @export
#' @name release_data_server
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   release_data_server("release_data", shared_state)
#' }
#' }
release_data_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Display current working directory
    output$workdir_display <- shiny::renderText({
      wd <- shared_state$workdir
      if (is.null(wd) || wd == "") {
        "Working directory not set"
      } else {
        paste("Current working directory:", wd)
      }
    })

    # Load all .rda files in the working directory
    rda_files <- shiny::eventReactive(input$load_files, {
      wd <- shared_state$workdir
      shiny::req(wd)
      if (!dir.exists(wd)) {
        shiny::showNotification("Directory does not exist", type = "error")
        return(NULL)
      }
      files <- list.files(wd, pattern = "\\.rda$", full.names = FALSE)
      if (length(files) == 0) {
        shiny::showNotification("No .rda files found", type = "warning")
        return(NULL)
      }
      files
    })

    # Render UI for selecting .rda files to export
    output$rda_files_ui <- shiny::renderUI({
      files <- rda_files()
      shiny::req(files)
      shiny::checkboxGroupInput(ns("selected_rda"), "Select .rda files to export", choices = files)
    })

    # Handle export button click
    shiny::observeEvent(input$export_btn, {
      wd <- shared_state$workdir
      shiny::req(wd)
      shiny::req(input$selected_rda)
      format <- input$output_format

      # Process each selected .rda file
      for (rda_file in input$selected_rda) {
        rda_path <- file.path(wd, rda_file)
        env <- new.env()
        load(rda_path, envir = env)

        # Create output directory named after the .rda file (without extension)
        folder_name <- tools::file_path_sans_ext(rda_file)
        out_dir <- file.path(wd, folder_name)
        if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

        # Export each object from the .rda file
        obj_names <- ls(env)
        for (obj_name in obj_names) {
          obj <- env[[obj_name]]
          out_file <- file.path(out_dir, paste0(obj_name, ".", format))

          if (format == "xlsx") {
            if (is.data.frame(obj)) {
              openxlsx::write.xlsx(obj, out_file)
            } else {
              # Try to convert to data.frame before exporting
              tryCatch({
                df <- as.data.frame(obj)
                openxlsx::write.xlsx(df, out_file)
              }, error = function(e) {
                shiny::showNotification(paste("Cannot export object", obj_name, "- not a data.frame"), type = "warning")
              })
            }
          } else if (format == "csv") {
            if (is.data.frame(obj)) {
              utils::write.csv(obj, out_file, row.names = FALSE)
            } else {
              tryCatch({
                df <- as.data.frame(obj)
                utils::write.csv(df, out_file, row.names = FALSE)
              }, error = function(e) {
                shiny::showNotification(paste("Cannot export object", obj_name, "- not a data.frame"), type = "warning")
              })
            }
          }
        }
      }

      shiny::showNotification("Export completed", type = "message")
    })
  })
}
