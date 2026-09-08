#' Project Initialization UI Module
#' @param id Module ID
#' @return UI elements for project initialization including working directory setup,
#'   sample info and expression matrix uploads, data source selection, and data preview.
#' @import shiny
#' @import bslib
#' @importFrom shinyFiles shinyDirButton
#' @importFrom bsicons bs_icon
#' @name project_init_ui
#' @export
#'
project_init_ui <- function(id) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = list(
      tags$h4("Setup", class = "text-primary"),
      shinyFiles::shinyDirButton(
        id = ns("prj_wd"),
        label = "Set working directory",
        title = "Select working directory",
        icon = bs_icon("folder")
      ),
      shiny::textOutput(ns("raw_wd_path")),
      shiny::hr(),
      shiny::fileInput(
        inputId = ns("SampleInfo"),
        label = 'Upload Sample Information (.csv, .xlsx, .xls)',
        accept = c(".csv", ".xlsx", ".xls")
      ),
      tags$small("Confirm sample information", style = "color: #6c757d"),
      shiny::fileInput(
        inputId = ns("expression_matrix"),
        label = 'Upload Expression Matrix (.csv, .xlsx, .xls)',
        accept = c(".csv", ".xlsx", ".xls")
      ),
      tags$small("Confirm expression matrix", style = "color: #6c757d"),
      shiny::selectInput(
        inputId = ns("builtin_dataset"),
        label = "Built-in example",
        choices = stats::setNames(
          protvis_builtin_datasets()$file,
          paste(protvis_builtin_datasets()$source,
                "—", protvis_builtin_datasets()$file)
        ),
        selected = protvis_builtin_datasets()$file[[1L]]
      ),
      shiny::actionButton(
        ns("load_builtin"), "Load selected built-in example",
        class = "btn btn-outline-primary w-100"
      ),
      shiny::selectInput(
        inputId = ns("data_source"),
        label = "Select data source",
        choices = c(
          "Raw", "MaxQuant", "ProteomeDiscoverer", "Proteome Discoverer",
          "DIA-NN", "Spectronaut", "FragPipe", "Skyline", "Mascot", "OpenMS",
          "User-defined matrix"
        ),
        selected = "MaxQuant"
      )
    ),
    shiny::actionButton(ns("run_button"), "Project init"),
    bslib::card(
      bslib::card_header("Preview Sample Info and Expression Matrix"),
      bslib::card_body(
        bslib::navset_tab(
          id = ns("preview_tabs"),
          header = NULL,
          bslib::nav_panel("Sample Info",
                           shiny::htmlOutput(ns("file_check_init")),
                    DT::DTOutput(ns("tbl_sample_info"))
          ),
          bslib::nav_panel("Expression Matrix",
                           shiny::htmlOutput(ns("matrix_check")),
                    DT::DTOutput(ns("tbl_expression_matrix"))
          )
        )
      )
    )
  )
}

#' Project Initialization Server Module
#' @param id Module ID
#' @param shared_state A reactiveValues object for sharing state (workdir, sample info, etc.)
#' @import shiny
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @importFrom fs path_home
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#' @importFrom DT renderDT
#' @name project_init_server
#' @export
#'
project_init_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Define available volumes for directory selection using your custom cross-platform function getVolumes_win()
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes_win())
    shinyFiles::shinyDirChoose(input, "prj_wd", roots = volumes, session = session)
    # Listen to directory selection and update shared_state$workdir
    shiny::observeEvent(input$prj_wd, {
      shiny::req(input$prj_wd)
      selected_dir <- shinyFiles::parseDirPath(volumes, input$prj_wd)
      shared_state$workdir <- selected_dir
      shiny::showNotification(paste("Workdir set to:", selected_dir), type = "message")
    })
    # Display selected working directory path
    output$raw_wd_path <- renderText({
      shiny::req(shared_state$workdir)
      base::paste("Working directory:", shared_state$workdir)
    })
    # Upload and read sample info, then store it in shared_state
    shiny::observeEvent(input$SampleInfo, {
      shiny::req(input$SampleInfo)
      tryCatch({
        sample_info <- protvis_read_table(
          input$SampleInfo$datapath, filename = input$SampleInfo$name
        )
        shared_state$sample_info <- sample_info
        shiny::showNotification("Sample info uploaded", type = "message")
      }, error = function(e) {
        shiny::showNotification(
          paste0("Sample info upload failed: ", conditionMessage(e)),
          type = "error"
        )
      })
    })
    # Upload and read expression matrix, then store it in shared_state
    shiny::observeEvent(input$expression_matrix, {
      shiny::req(input$expression_matrix)
      tryCatch({
        expression_matrix <- protvis_read_table(
          input$expression_matrix$datapath,
          filename = input$expression_matrix$name
        )
        shared_state$expression_matrix <- expression_matrix
        shiny::showNotification("Expression matrix uploaded", type = "message")
      }, error = function(e) {
        shiny::showNotification(
          paste0("Expression matrix upload failed: ", conditionMessage(e)),
          type = "error"
        )
      })
    })
    # Sync data source selection to shared_state
    shiny::observeEvent(input$data_source, {
      shared_state$data_source <- input$data_source
    })
    # Load a bundled, source-specific example into the shared project state.
    shiny::observeEvent(input$load_builtin, {
      tryCatch({
        selected <- as.character(input$builtin_dataset %||% "")
        manifest <- protvis_builtin_datasets()
        row <- manifest[manifest$file == selected, , drop = FALSE]
        if (nrow(row) != 1L) stop("Please select a valid built-in example.", call. = FALSE)
        dataset <- load_protvis_builtin_data(
          source = row$source[[1L]], file = row$file[[1L]], auto_export = FALSE
        )
        shared_state$sample_info <- dataset$sample_info
        shared_state$expression_matrix <- dataset$expression_data
        shared_state$expression_matrix_filtered <- dataset$expression_data
        shared_state$data_source <- row$source[[1L]]
        shared_state$workdir <- protvis_output_directory(shared_state$workdir %||% getwd())
        shiny::showNotification(
          paste(row$source[[1L]], "built-in example loaded; click Project init to create ProtVis_dataset."),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(paste("Built-in example failed:", conditionMessage(e)),
                                type = "error")
      })
    }, ignoreInit = TRUE)

    # On clicking init, validate the current inputs and create one canonical
    # ProtVis_dataset for the project. Later analyses create new versions only.
    shiny::observeEvent(input$run_button, {
      tryCatch({
        directory <- protvis_output_directory(shared_state$workdir %||% getwd())
        shiny::req(shared_state$sample_info, shared_state$expression_matrix,
                   shared_state$data_source)
        validated <- validate_protvis_data(
          shared_state$expression_matrix, shared_state$sample_info
        )
        sample_info <- validated$sample_info
        expression_matrix <- validated$expression_matrix
        shared_state$sample_info <- sample_info
        shared_state$expression_matrix <- expression_matrix
        data_source <- shared_state$data_source
        shared_state$workdir <- directory
        dataset <- create_protvis_dataset(
          expression_data = expression_matrix,
          sample_info = sample_info,
          metadata = list(source = data_source, output_directory = directory)
        )
        dataset$metadata$object_name <- paste0(
          "ProtVis_dataset__project_init__", .protvis_object_label(data_source), "__v1"
        )
        dataset$metadata$object_version <- 1L
        dataset <- protvis_auto_export_dataset(dataset, directory = directory)
        shared_state$dataset <- dataset
        shared_state$dataset_name <- protvis_dataset_name(dataset)
        history <- shared_state$dataset_history %||% list()
        shared_state$dataset_history <- c(history, list(dataset))
        save_path <- file.path(directory, "Step1_project_init.rda")
        base::save(sample_info, expression_matrix, data_source, file = save_path)
        shiny::showNotification(
          paste("Project initialized:", protvis_dataset_name(dataset)), type = "message"
        )
        message("✅ Step1_project_init.rda saved to: ", save_path)
      }, error = function(e) {
        shiny::showNotification(paste("❌ Save failed:", e$message), type = "error")
      })
    })
    # Preview sample info table
    output$tbl_sample_info <- DT::renderDT({
      shiny::req(shared_state$sample_info)
      shared_state$sample_info
    })
    # Preview expression matrix table
    output$tbl_expression_matrix <- DT::renderDT({
      shiny::req(shared_state$expression_matrix)
      shared_state$expression_matrix
    })
    # Confirm sample info upload UI
    output$file_check_init <- renderUI({
      shiny::req(input$SampleInfo)
      tags$p("✅ Sample info uploaded:", input$SampleInfo$name, class = "text-success")
    })
    # Confirm expression matrix upload UI
    output$matrix_check <- renderUI({
      shiny::req(input$expression_matrix)
      tags$p("✅ Expression matrix uploaded:", input$expression_matrix$name, class = "text-success")
    })
  })
}
