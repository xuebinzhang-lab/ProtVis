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
        inputId = ns("data_source"),
        label = "Select data source",
        choices = c("Raw", "MaxQuant", "ProteomeDiscoverer", "Skyline", "Mascot", "OpenMS"),
        selected = "MaxQuant"
      )
    ),
    shiny::actionButton(ns("run_button"), "Project init"),
    bslib::card(
      bslib::card_header("Preview Sample Info and Expression Matrix"),
      bslib::card_body(
        bslib::navset_tab(
          id = ns("preview_tabs"),
          bslib::nav_panel("Sample Info",
                           shiny::htmlOutput(ns("file_check_init")),
                    shiny::dataTableOutput(ns("tbl_sample_info"))
          ),
          bslib::nav_panel("Expression Matrix",
                           shiny::htmlOutput(ns("matrix_check")),
                    shiny::dataTableOutput(ns("tbl_expression_matrix"))
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
#' @importFrom DT renderDataTable
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
      ext <- tools::file_ext(input$SampleInfo$name)
      sample_info <- if (ext == "csv") {
        utils::read.csv(input$SampleInfo$datapath, stringsAsFactors = FALSE)
      } else {
        readxl::read_excel(input$SampleInfo$datapath)
      }
      shared_state$sample_info <- sample_info
      shiny::showNotification("Sample info uploaded", type = "message")
    })
    # Upload and read expression matrix, then store it in shared_state
    shiny::observeEvent(input$expression_matrix, {
      shiny::req(input$expression_matrix)
      ext <- tools::file_ext(input$expression_matrix$name)
      expression_matrix <- if (ext == "csv") {
        utils::read.csv(input$expression_matrix$datapath, stringsAsFactors = FALSE)
      } else {
        readxl::read_excel(input$expression_matrix$datapath)
      }
      shared_state$expression_matrix <- expression_matrix
      shiny::showNotification("Expression matrix uploaded", type = "message")
    })
    # Sync data source selection to shared_state
    shiny::observeEvent(input$data_source, {
      shared_state$data_source <- input$data_source
    })
    # On clicking the init button, save all data to Step1_project_init.rda in selected workdir
    shiny::observeEvent(input$run_button, {
      shiny::req(shared_state$workdir, shared_state$sample_info, shared_state$expression_matrix, shared_state$data_source)
      # Assign reactiveValues contents to plain variables for saving
      sample_info <- shared_state$sample_info
      expression_matrix <- shared_state$expression_matrix
      data_source <- shared_state$data_source
      save_path <- file.path(shared_state$workdir, "Step1_project_init.rda")
      tryCatch({
        base::save(sample_info, expression_matrix, data_source, file = save_path)
        shiny::showNotification("Project initialized successfully!", type = "message")
        message("✅ Step1_project_init.rda saved to: ", save_path)
      }, error = function(e) {
        shiny::showNotification(paste("❌ Save failed:", e$message), type = "error")
      })
    })
    # Preview sample info table
    output$tbl_sample_info <- DT::renderDataTable({
      shiny::req(shared_state$sample_info)
      shared_state$sample_info
    })
    # Preview expression matrix table
    output$tbl_expression_matrix <- DT::renderDataTable({
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
