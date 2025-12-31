#' Swiss-Model UI Function
#'
#' This function creates the user interface for the Swiss-Model protein structure prediction app. It includes input fields for protein sequences and API tokens, as well as a series of display panels for model results.
#'
#' @param id The namespace ID for the Shiny module.
#' @return A `tagList` containing the UI elements for the Swiss-Model workflow.
#' @noRd
swissmodel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        style = "margin-bottom: 15px;",

        # Protein sequence input
        textAreaInput(ns("sequence"), "Protein sequence",
                      value = "VLSPADKTNVKAAWAKVGNHAADFGAEALERMFMSFPSTKTYFSHFDLGHNSTQVKGHGKKVADALTKAVGHLDTLPDALSDLSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPGDFTPSVHASLDKFLASVSTVLTSKYR",
                      rows = 10),
        # API token input
        textInput(ns("api_token"), "API Token",
                  value = "",
                  placeholder = "Enter your API token"),
        # External link for getting API token (with smaller text)
        tags$small(
          p("How to get an API token? ",
            tags$a(href = "https://github.com/anhuikylin/", "Click here to get the token", target = "_blank"))
        ),
        # Action button to run the model
        actionButton(ns("run_model"), "Run Model"),
        # Display project info after the action button
        uiOutput(ns("project_info_view_url"))
      ),
      # Main display panel
      bslib::card(
        height = "600px",
        bslib::card_header("Swiss-Model Results"),
        bslib::navset_card_tab(
          full_screen = TRUE,
          bslib::nav_panel(
            "Project Information File",
            div(
              style = "height: 500px; overflow: auto;",
              verbatimTextOutput(ns("project_info_file"))
            )
          ),
          bslib::nav_panel(
            "PDB Information",
            div(
              style = "height: 500px; overflow: auto;",
              verbatimTextOutput(ns("pdb_information"))
            )
          ),
          bslib::nav_panel(
            "Model Quality",
            div(
              style = "height: 500px; overflow: auto;",
              DT::dataTableOutput(ns("model_quality"))
            )
          ),
          bslib::nav_panel(
            "Ramachandran Plot",
            div(
              style = "height: 500px; overflow: auto;",
              plotOutput(ns("Ramachandran_plot"), height = "100%")  # 用 plotOutput 来渲染 Ramachandran 图
            )
          ),
          bslib::nav_panel(
            "Residue Composition",
            div(
              style = "height: 500px; overflow: auto;",
              plotOutput(ns("residue_composition"), height = "100%")  # 用 plotOutput 来渲染残基组成图
            )
          ),
          bslib::nav_panel(
            "PDB Plot",
            div(
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
#'
#' This function defines the server-side logic for handling user inputs, running the Swiss-Model prediction workflow, and rendering the results. It handles the protein sequence input, API token, error handling, and displaying the model results.
#'
#' @param id The namespace ID for the Shiny module.
#' @return A `moduleServer` call which binds server-side logic to the UI components.
#' @noRd
swissmodel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$run_model, {
      sequence <- input$sequence
      api_token <- input$api_token
      if (nchar(sequence) == 0) {
        showModal(modalDialog(
          title = "Error",
          "Please enter a protein sequence.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      if (nchar(api_token) == 0) {
        showModal(modalDialog(
          title = "Error",
          "Please enter a valid API token.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      swissmodel::set_swissmodel_token(api_token)
      result <- tryCatch({
        swissmodel::run_automodel_workflow(sequence)
      }, error = function(e) {
        showModal(modalDialog(
          title = "Error",
          paste("Model running failed:", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
      if (is.null(result)) return()
      pdb_file <- result$downloaded_files[[1]]
      pdb <- bio3d::read.pdb(pdb_file)
      output$project_info_view_url <- renderUI({
        project_info <- jsonlite::fromJSON(result$project_info_file)
        view_url <- project_info$view_url[1]
        output_text <- paste(
          paste(tags$a(href = view_url, "Swiss-Model Results Url",target = "_blank")),
          sep = "\n\n"
        )
        HTML(output_text)
      })
      output$project_info_file <- renderText({
        project_info <- jsonlite::fromJSON(result$project_info_file)
        project_info_text <- capture.output(print(project_info))
        view_url <- project_info$view_url[1]

        paste(
          paste(project_info_text, collapse = "\n"),
          sep = "\n\n"
        )
      })
      output$pdb_information <- renderText({
        print(swissmodel::pdb_info(pdb))
        paste(utils::capture.output(print(swissmodel::pdb_info(pdb))), collapse = "\n")
      })
      output$model_quality <- renderDT({
        model_quality <- swissmodel::analyze_model_quality(pdb)
        data.frame(Value = unlist(model_quality))
      })
      output$Ramachandran_plot <- renderPlot({
        swissmodel::plot_ramachandran(pdb)
      })
      output$residue_composition <- renderPlot({
        swissmodel::plot_residue_composition(pdb)
      })
      output$pdb_plot <- r3dmol::renderR3dmol({
        swissmodel::plot_pdb(pdb)
      })
    })
  })
}
