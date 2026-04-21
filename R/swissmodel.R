#' Swiss-Model UI Function
#' Creates the user interface for the Swiss-Model protein structure prediction module.
#' Includes input fields for protein sequences and API tokens, as well as display panels
#' for model results including project information, PDB data, quality metrics, and 3D visualization.
#' @param id The namespace ID for the Shiny module.
#' @return A `tagList` containing the UI elements for the Swiss-Model workflow.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @importFrom r3dmol r3dmolOutput
#' @name swissmodel_ui
#' @export
#'
swissmodel_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        open = "open",
        gap = "12px",

        shiny::div(
          style = "margin-bottom: 8px;",
          shiny::h4("Swiss-Model", style = "margin-bottom: 6px;"),
          shiny::p(
            "Predict protein structure and inspect project information, model quality, and 3D visualization.",
            style = "color: #666; font-size: 13px; margin-bottom: 0;"
          )
        ),

        bslib::accordion(
          open = c("Input Parameters", "Project Link"),

          bslib::accordion_panel(
            "Input Parameters",
            shiny::div(
              style = "padding-top: 4px;",
              shiny::textAreaInput(
                ns("sequence"),
                "Protein sequence",
                value = "",
                rows = 10,
                width = "100%",
                placeholder = "Enter protein sequence here"
              ),

              shiny::textInput(
                ns("api_token"),
                "API Token",
                value = "",
                placeholder = "Enter your API token"
              ),

              shiny::div(
                style = "font-size: 12px; color: #666; margin-top: -6px; margin-bottom: 12px;",
                "Need a token? ",
                shiny::tags$a(
                  href = "https://github.com/anhuikylin/",
                  target = "_blank",
                  "Click here"
                )
              ),

              shiny::div(
                style = "display: flex; gap: 8px; flex-wrap: wrap; margin-top: 8px;",
                shiny::actionButton(
                  ns("run_model"),
                  "Run",
                  icon = bsicons::bs_icon("play-fill"),
                  class = "btn-primary"
                ),
                shiny::actionButton(
                  ns("example_seq"),
                  "Example",
                  icon = bsicons::bs_icon("stars")
                ),
                shiny::actionButton(
                  ns("clear_all"),
                  "Clear",
                  icon = bsicons::bs_icon("x-circle")
                )
              )
            )
          ),

          bslib::accordion_panel(
            "Project Link",
            shiny::uiOutput(ns("project_info_view_url"))
          ),

          bslib::accordion_panel(
            "Uploaded / Predicted File Info",
            shiny::verbatimTextOutput(ns("file_info"))
          ),

          bslib::accordion_panel(
            "Download",
            shiny::div(
              style = "display: flex; flex-direction: column; gap: 8px;",
              shiny::downloadButton(ns("download_project_info"), "Download Project Info"),
              shiny::downloadButton(ns("download_pdb"), "Download PDB File"),
              shiny::downloadButton(ns("download_model_quality"), "Download Model Quality")
            )
          )
        )
      ),

      bslib::card(
        full_screen = TRUE,
        min_height = 760,
        style = "border-radius: 14px;",
        bslib::card_header(
          shiny::div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            shiny::span("Swiss-Model Results"),
            shiny::tags$span(
              style = "font-size: 12px; color: #888;",
              "Project information, model quality, plots, and 3D structure"
            )
          )
        ),

        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(6, 6),

            bslib::card(
              full_screen = TRUE,
              style = "border-radius: 12px;",
              bslib::card_header("Project Information"),
              bslib::card_body(
                shiny::div(
                  style = "height: 280px; overflow-y: auto;",
                  shiny::verbatimTextOutput(ns("project_info_file"))
                )
              )
            ),

            bslib::card(
              full_screen = TRUE,
              style = "border-radius: 12px;",
              bslib::card_header("Model Quality"),
              bslib::card_body(
                shiny::div(
                  style = "height: 280px; overflow-y: auto;",
                  DT::DTOutput(ns("model_quality"))
                )
              )
            )
          ),

          shiny::br(),

          bslib::navset_card_tab(
            id = ns("result_tabs"),
            full_screen = TRUE,

            bslib::nav_panel(
              "PDB Information",
              shiny::div(
                style = "height: 420px; overflow-y: auto; padding: 8px;",
                shiny::verbatimTextOutput(ns("pdb_information"))
              )
            ),

            bslib::nav_panel(
              "Ramachandran Plot",
              shiny::div(
                style = "height: 420px; overflow-y: auto; padding: 8px;",
                shiny::plotOutput(ns("Ramachandran_plot"), height = "380px")
              )
            ),

            bslib::nav_panel(
              "Residue Composition",
              shiny::div(
                style = "height: 420px; overflow-y: auto; padding: 8px;",
                shiny::plotOutput(ns("residue_composition"), height = "380px")
              )
            ),

            bslib::nav_panel(
              "3D Structure",
              shiny::div(
                style = "height: 420px; overflow-y: auto; padding: 8px;",
                r3dmol::r3dmolOutput(ns("pdb_plot"), height = "380px")
              )
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
#' @importFrom utils capture.output write.csv
#' @importFrom jsonlite fromJSON write_json
#' @importFrom DT renderDT datatable
#' @importFrom r3dmol renderR3dmol r3dmol
#' @name swissmodel_server
#' @export
#'
swissmodel_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    example_sequence <- "VLSPADKTNVKAAWAKVGNHAADFGAEALERMFMSFPSTKTYFSHFDLGHNSTQVKGHGKKVADALTKAVGHLDTLPDALSDLSDLHAHKLRVDPVNFKLLSHCLLVTLAAHLPGDFTPSVHASLDKFLASVSTVLTSKYR"

    rv <- shiny::reactiveValues(
      result = NULL,
      pdb = NULL,
      pdb_file = NULL,
      project_info = NULL,
      model_quality_df = NULL
    )

    shiny::observeEvent(input$example_seq, {
      shiny::updateTextAreaInput(
        session,
        "sequence",
        value = example_sequence
      )

      shiny::showNotification(
        "Example protein sequence loaded.",
        type = "message",
        duration = 2
      )
    })

    shiny::observeEvent(input$clear_all, {
      shiny::updateTextAreaInput(session, "sequence", value = "")
      shiny::updateTextInput(session, "api_token", value = "")

      rv$result <- NULL
      rv$pdb <- NULL
      rv$pdb_file <- NULL
      rv$project_info <- NULL
      rv$model_quality_df <- NULL

      shiny::showNotification(
        "Inputs and results have been cleared.",
        type = "message",
        duration = 2
      )
    })

    output$project_info_view_url <- shiny::renderUI({
      if (base::is.null(rv$project_info)) {
        shiny::div(
          style = "color:#888; font-size:13px;",
          "No project result yet."
        )
      } else {
        view_url <- rv$project_info$view_url[1]
        shiny::div(
          shiny::tags$a(
            href = view_url,
            target = "_blank",
            "Open Swiss-Model result page"
          )
        )
      }
    })

    output$file_info <- shiny::renderText({
      if (base::is.null(rv$result)) {
        return("No predicted file available yet.")
      }

      downloaded_files <- rv$result$downloaded_files

      paste(
        c(
          paste0("Number of downloaded files: ", length(downloaded_files)),
          "",
          paste0("PDB file: ", ifelse(base::is.null(rv$pdb_file), "NA", rv$pdb_file)),
          "",
          "Downloaded files:",
          paste(downloaded_files, collapse = "\n")
        ),
        collapse = "\n"
      )
    })

    output$project_info_file <- shiny::renderText({
      if (base::is.null(rv$project_info)) {
        return("Click 'Run' to generate Swiss-Model results.")
      }

      project_info_text <- utils::capture.output(base::print(rv$project_info))
      base::paste(project_info_text, collapse = "\n")
    })

    output$pdb_information <- shiny::renderText({
      if (base::is.null(rv$pdb)) {
        return("PDB information will be displayed here after running the model.")
      }

      base::paste(
        utils::capture.output(base::print(swissmodel::pdb_info(rv$pdb))),
        collapse = "\n"
      )
    })

    output$model_quality <- DT::renderDT({
      if (base::is.null(rv$model_quality_df)) {
        return(
          DT::datatable(
            data.frame(Message = "Model quality metrics will be displayed here after running the model."),
            options = list(dom = "t", paging = FALSE),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        rv$model_quality_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
    })

    output$Ramachandran_plot <- shiny::renderPlot({
      if (base::is.null(rv$pdb)) {
        plot.new()
        text(0.5, 0.5, "Ramachandran plot will be displayed here after running the model.")
        return()
      }

      swissmodel::plot_ramachandran(rv$pdb)
    })

    output$residue_composition <- shiny::renderPlot({
      if (base::is.null(rv$pdb)) {
        plot.new()
        text(0.5, 0.5, "Residue composition plot will be displayed here after running the model.")
        return()
      }

      swissmodel::plot_residue_composition(rv$pdb)
    })

    output$pdb_plot <- r3dmol::renderR3dmol({
      if (base::is.null(rv$pdb)) {
        return(r3dmol::r3dmol())
      }

      swissmodel::plot_pdb(rv$pdb)
    })

    shiny::observeEvent(input$run_model, {
      sequence <- gsub("\\s+", "", input$sequence)
      api_token <- input$api_token

      if (base::nchar(sequence) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "Please enter a protein sequence.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      if (base::nchar(api_token) == 0) {
        shiny::showModal(shiny::modalDialog(
          title = "Error",
          "Please enter a valid API token.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }

      shiny::withProgress(message = "Running Swiss-Model workflow...", value = 0, {

        shiny::incProgress(0.10, detail = "Checking inputs...")
        Sys.sleep(0.2)

        swissmodel::set_swissmodel_token(api_token)

        shiny::incProgress(0.20, detail = "Submitting sequence to Swiss-Model...")
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

        if (base::is.null(result)) {
          return()
        }

        shiny::incProgress(0.30, detail = "Reading predicted structure...")
        pdb_file <- result$downloaded_files[[1]]
        pdb <- bio3d::read.pdb(pdb_file)

        shiny::incProgress(0.20, detail = "Parsing project information...")
        project_info <- jsonlite::fromJSON(result$project_info_file)

        shiny::incProgress(0.15, detail = "Analyzing model quality...")
        model_quality <- swissmodel::analyze_model_quality(pdb)
        model_quality_df <- data.frame(
          Metric = names(base::unlist(model_quality)),
          Value = base::unlist(model_quality),
          check.names = FALSE
        )

        shiny::incProgress(0.05, detail = "Updating outputs...")

        rv$result <- result
        rv$pdb <- pdb
        rv$pdb_file <- pdb_file
        rv$project_info <- project_info
        rv$model_quality_df <- model_quality_df
      })

      shiny::showNotification(
        "Swiss-Model workflow completed successfully.",
        type = "message",
        duration = 3
      )
    })

    output$download_project_info <- shiny::downloadHandler(
      filename = function() {
        "swissmodel_project_info.json"
      },
      content = function(file) {
        if (base::is.null(rv$project_info)) {
          writeLines("No project information available.", con = file)
        } else {
          jsonlite::write_json(
            rv$project_info,
            path = file,
            pretty = TRUE,
            auto_unbox = TRUE
          )
        }
      }
    )

    output$download_pdb <- shiny::downloadHandler(
      filename = function() {
        "swissmodel_model.pdb"
      },
      content = function(file) {
        if (base::is.null(rv$pdb_file) || !base::file.exists(rv$pdb_file)) {
          writeLines("No PDB file available.", con = file)
        } else {
          base::file.copy(rv$pdb_file, file, overwrite = TRUE)
        }
      }
    )

    output$download_model_quality <- shiny::downloadHandler(
      filename = function() {
        "swissmodel_model_quality.csv"
      },
      content = function(file) {
        if (base::is.null(rv$model_quality_df)) {
          utils::write.csv(
            data.frame(Message = "No model quality available."),
            file,
            row.names = FALSE
          )
        } else {
          utils::write.csv(rv$model_quality_df, file, row.names = FALSE)
        }
      }
    )
  })
}
