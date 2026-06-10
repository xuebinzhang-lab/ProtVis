#' Data Input UI Module
#' This UI module dynamically displays a header showing the current data source
#' and renders the corresponding UI for the selected data source module.
#' @param id Module ID for namespacing.
#' @return A Shiny UI tag list.
#' @import shiny
#' @name data_input_ui
#' @export
data_input_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("dynamic_header")),
    shiny::uiOutput(ns("dynamic_ui"))
  )
}
#' Data Input Server Module
#' This server module observes the reactive data source selection, renders the
#' appropriate UI dynamically, and loads the corresponding server logic module,
#' passing along a shared state object.
#' @param id Module ID for namespacing.
#' @param data_source_reactive A reactive expression returning the current data source as a string.
#' @param shared_state A reactiveValues object shared across modules, used for sharing state and data.
#' @return None. This module manages UI rendering and server logic dynamically.
#' @import shiny
#' @name data_input_server
#' @export
utils::globalVariables(c("Cluster", "Cluster_Count", "variable",
                         "index", "Cluster", "Var2", "Var1"))

data_input_server <- function(id, data_source_reactive, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamic_header <- shiny::renderUI({
      shiny::req(data_source_reactive())
      tags$h4(
        paste("Current Data Source:", data_source_reactive()),
        class = "text-primary",
        style = "margin-top: 20px; margin-bottom: 20px;"
      )
    })

    output$dynamic_ui <- shiny::renderUI({
      shiny::req(data_source_reactive())
      tryCatch({
        switch(
          data_source_reactive(),
          "Raw" = Raw_ui(ns("Raw")),
          "MaxQuant" = MaxQuant_ui(ns("MaxQuant")),
          "ProteomeDiscoverer" = PD_ui(ns("PD")),
          "Skyline" = skyline_ui(ns("skyline")),
          "Mascot" = Mascot_ui(ns("Mascot")),
          "OpenMS" = OpenMS_ui(ns("OpenMS")),
          tags$div(class = "alert alert-warning", "Unknown data source type")
        )
      }, error = function(e) {
        tags$div(class = "alert alert-danger", paste("Module loading error:", e$message))
      })
    })

    # Register each source module once. The rendered UI switches dynamically,
    # but Shiny module servers should not be re-registered every time the
    # source selector changes.
    Raw_server("Raw", shared_state = shared_state)
    MaxQuant_server("MaxQuant", shared_state = shared_state)
    PD_server("PD", shared_state = shared_state)
    skyline_server("skyline", shared_state = shared_state)
    Mascot_server("Mascot", shared_state = shared_state)
    OpenMS_server("OpenMS", shared_state = shared_state)
  })
}
