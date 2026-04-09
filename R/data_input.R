#' Data Input UI Module
#' This UI module dynamically displays a header showing the current data source
#' and renders the corresponding UI for the selected data source module.
#' @param id Module ID for namespacing.
#' @return A Shiny UI tag list.
#' @name data_input_ui
#' @export
data_input_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns("dynamic_header")),  # Dynamic header displaying current data source
    shiny::uiOutput(ns("dynamic_ui"))       # Dynamic UI for selected data source module
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
#' @name data_input_server
#' @export
utils::globalVariables(c("Cluster", "Cluster_Count", "variable",
                         "index","Cluster","Var2","Var1"))
data_input_server <- function(id, data_source_reactive, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Render dynamic header showing current data source
    output$dynamic_header <- shiny::renderUI({
      shiny::req(data_source_reactive())
      tags$h4(
        paste("Current Data Source:", data_source_reactive()),
        class = "text-primary",
        style = "margin-top: 20px; margin-bottom: 20px;"
      )
    })

    # Render the UI for the selected data source module dynamically
    output$dynamic_ui <- shiny::renderUI({
      shiny::req(data_source_reactive())
      tryCatch({
        switch(data_source_reactive(),
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

    # Dynamically load the server logic for the selected data source module,
    # passing the shared_state reactiveValues
    shiny::observeEvent(data_source_reactive(), {
      shiny::req(data_source_reactive())
      tryCatch({
        switch(data_source_reactive(),
               "Raw" = Raw_server("Raw", shared_state = shared_state),
               "MaxQuant" = MaxQuant_server("MaxQuant", shared_state = shared_state),
               "ProteomeDiscoverer" = PD_server("PD", shared_state = shared_state),
               "Skyline" = skyline_server("skyline", shared_state = shared_state),
               "Mascot" = Mascot_server("Mascot", shared_state = shared_state),
               "OpenMS" = OpenMS_server("OpenMS", shared_state = shared_state)
        )
      }, error = function(e) {
        shiny::showNotification(paste("Server module error:", e$message), type = "error")
      })
    })
  })
}
