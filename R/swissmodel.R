#' Swiss-Model UI Function
#' Creates the user interface for the Swiss-Model protein structure prediction module.
#' Includes input fields for protein sequences and API tokens, as well as display panels
#' for model results including project information, PDB data, quality metrics, and 3D visualization.
#' @param id The namespace ID for the Shiny module.
#' @return A `tagList` containing the UI elements for the Swiss-Model workflow.
#' @import shiny
#' @import bslib
#' @name swissmodel_ui
#' @export
#'
swissmodel_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        style = "margin-bottom: 15px;",
        # Protein sequence input
        shiny::textAreaInput(ns("sequence"), "Protein sequence",
                             value = "VLSPADKTNVKAAWAKVGNHAADFGAEALERMFMSFPSTKTYFSHFDLGHNSTQVKGHGKKVADALTKAVGHLDTLPDALSDLSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPGDFTPSVHASLDKFLASVSTVLTSKYR",
                             rows = 10),
        # API token input
        shiny::textInput(ns("api_token"), "API Token",
                         value = "",
                         placeholder = "Enter your API token"),
        # External link for getting API token (with smaller text)
        shiny::tags$small(
          shiny::p("How to get an API token? ",
                   shiny::tags$a(href = "https://github.com/anhuikylin/", "Click here to get the token", target = "_blank"))
        ),
        # Action button to run the model
        shiny::actionButton(ns("run_model"), "Run Model"),
        # Display project info after the action button
        shiny::uiOutput(ns("project_info_view_url"))
      ),
      # Main display panel
      bslib::card(
        height = "600px",
        bslib::card_header("Swiss-Model Results"),
        bslib::navset_card_tab(
          full_screen = TRUE,
          bslib::nav_panel(
            "Project Information File",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              shiny::verbatimTextOutput(ns("project_info_file"))
            )
          ),
          bslib::nav_panel(
            "PDB Information",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              shiny::verbatimTextOutput(ns("pdb_information"))
            )
          ),
          bslib::nav_panel(
            "Model Quality",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              shiny::dataTableOutput(ns("model_quality"))
            )
          ),
          bslib::nav_panel(
            "Ramachandran Plot",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              shiny::plotOutput(ns("Ramachandran_plot"), height = "100%")
            )
          ),
          bslib::nav_panel(
            "Residue Composition",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              shiny::plotOutput(ns("residue_composition"), height = "100%")
            )
          ),
          bslib::nav_panel(
            "PDB Plot",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              r3dmol::r3dmolOutput(ns("pdb_plot"), height = "100%")
            )
          )
        )
      )
    )
  )
}

#' Swiss-Model Server Function
#' Defines the server-side logic for handling user inputs, running the Swiss-Model
#' prediction workflow, and rendering the results. Handles protein sequence input,
#' API token validation, error handling, and displaying model results including
#' quality metrics, Ramachandran plots, and 3D structure visualization.
#' @param id The namespace ID for the Shiny module.
#' @return A `moduleServer` call which binds server-side logic to the UI components.
#' @import shiny
#' @importFrom bio3d read.pdb
#' @importFrom utils capture.output
#' @importFrom jsonlite fromJSON
#' @importFrom DT renderDT
#' @importFrom r3dmol renderR3dmol
#' @name swissmodel_server
#' @export
#'
swissmodel_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shiny::observeEvent(input$run_model, {
      sequence <- input$sequence
      api_token <- input$api_token
      # Validate protein sequence input
      if (base::nchar(sequence) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "Please enter a protein sequence.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      # Validate API token input
      if (base::nchar(api_token) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "Please enter a valid API token.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      # Set API token and run workflow
      swissmodel::set_swissmodel_token(api_token)
      result <- base::tryCatch({
        swissmodel::run_automodel_workflow(sequence)
      }, error = function(e) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          base::paste("Model running failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
      if (base::is.null(result)) return()
      # Process results
      pdb_file <- result$downloaded_files[[1]]
      pdb <- bio3d::read.pdb(pdb_file)
      # Render project info URL
      output$project_info_view_url <- shiny::renderUI({
        project_info <- jsonlite::fromJSON(result$project_info_file)
        view_url <- project_info$view_url[1]
        output_text <- base::paste(
          base::paste(shiny::tags$a(href = view_url, "Swiss-Model Results Url", target = "_blank")),
          sep = "\n\n"
        )
        shiny::HTML(output_text)
      })
      # Render project info file content
      output$project_info_file <- shiny::renderText({
        project_info <- jsonlite::fromJSON(result$project_info_file)
        project_info_text <- utils::capture.output(base::print(project_info))
        view_url <- project_info$view_url[1]
        base::paste(
          base::paste(project_info_text, collapse = "\n"),
          sep = "\n\n"
        )
      })
      # Render PDB information
      output$pdb_information <- shiny::renderText({
        base::print(swissmodel::pdb_info(pdb))
        base::paste(utils::capture.output(base::print(swissmodel::pdb_info(pdb))), collapse = "\n")
      })
      # Render model quality table
      output$model_quality <- DT::renderDT({
        model_quality <- swissmodel::analyze_model_quality(pdb)
        base::data.frame(Value = base::unlist(model_quality))
      })
      # Render Ramachandran plot
      output$Ramachandran_plot <- shiny::renderPlot({
        swissmodel::plot_ramachandran(pdb)
      })
      # Render residue composition plot
      output$residue_composition <- shiny::renderPlot({
        swissmodel::plot_residue_composition(pdb)
      })
      # Render 3D PDB structure
      output$pdb_plot <- r3dmol::renderR3dmol({
        swissmodel::plot_pdb(pdb)
      })
    })
  })
}
