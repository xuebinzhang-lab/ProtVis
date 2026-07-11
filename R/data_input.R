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

    source_modules <- base::list(
      Raw = base::list(ui = Raw_ui, server = Raw_server, id = "Raw"),
      MaxQuant = base::list(ui = MaxQuant_ui, server = MaxQuant_server, id = "MaxQuant"),
      ProteomeDiscoverer = base::list(ui = PD_ui, server = PD_server, id = "PD"),
      Skyline = base::list(ui = skyline_ui, server = skyline_server, id = "skyline"),
      Mascot = base::list(ui = Mascot_ui, server = Mascot_server, id = "Mascot"),
      OpenMS = base::list(ui = OpenMS_ui, server = OpenMS_server, id = "OpenMS")
    )

    register_data_source_server <- function(module) {
      server_args <- base::names(base::formals(module$server))
      if ("shared_state" %in% server_args) {
        module$server(module$id, shared_state = shared_state)
      } else {
        module$server(module$id)
      }
    }

    registered_sources <- shiny::reactiveVal(character())

    shiny::observeEvent(data_source_reactive(), {
      selected_source <- data_source_reactive()
      module <- source_modules[[selected_source]]
      if (base::is.null(module)) return(invisible(NULL))
      if (!selected_source %in% registered_sources()) {
        register_data_source_server(module)
        registered_sources(base::c(registered_sources(), selected_source))
      }
    }, ignoreInit = FALSE)

    output$dynamic_header <- shiny::renderUI({
      shiny::req(data_source_reactive())
      shiny::div(
        class = "pv-data-input-header",
        shiny::div(
          shiny::span("Data input", class = "pv-section-eyebrow"),
          shiny::tags$h2("Prepare proteomics data", class = "pv-page-title"),
          shiny::tags$p(
            "Load the selected search-engine output, remove unreliable peptide evidence, and preview each processing result before downstream analysis.",
            class = "pv-page-subtitle"
          )
        ),
        shiny::div(
          class = "pv-source-pill",
          shiny::span("Current data source", class = "pv-source-label"),
          shiny::strong(data_source_reactive())
        )
      )
    })

    output$dynamic_ui <- shiny::renderUI({
      shiny::req(data_source_reactive())
      selected_source <- data_source_reactive()
      module <- source_modules[[selected_source]]

      if (base::is.null(module)) {
        return(shiny::tags$div(
          class = "alert alert-warning",
          base::paste("Unknown data source type:", selected_source)
        ))
      }

      module$ui(ns(module$id))
    })
  })
}
