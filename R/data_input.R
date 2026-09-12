#' MaxQuant Output Preparation UI Module
#' This module prepares MaxQuant output for the ProtVis preprocessing workflow.
#' It is exposed in the navigation only when MaxQuant is the selected source.
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
#' MaxQuant Output Preparation Server Module
#' The server retains legacy adapters for existing imports, while the visible
#' navigation exposes this preparation step only for MaxQuant projects.
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
    source_modules[["DIA-NN"]] <- base::list(
      ui = DIA_NN_ui, server = DIA_NN_server, id = "DIA_NN"
    )
    source_modules[["Proteome Discoverer"]] <- source_modules[["ProteomeDiscoverer"]]
    source_modules[["Spectronaut"]] <- base::list(
      ui = Spectronaut_ui, server = Spectronaut_server, id = "Spectronaut"
    )
    source_modules[["FragPipe"]] <- base::list(
      ui = FragPipe_ui, server = FragPipe_server, id = "FragPipe"
    )
    source_modules[["User-defined matrix"]] <- base::list(
      ui = User_defined_matrix_ui, server = User_defined_matrix_server,
      id = "User_defined_matrix"
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
          shiny::span("MaxQuant output preparation", class = "pv-section-eyebrow"),
          shiny::tags$h2("Prepare MaxQuant output", class = "pv-page-title"),
          shiny::tags$p(
            "Load MaxQuant output, remove unreliable peptide evidence, and preview the quantitative matrix before downstream analysis.",
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
