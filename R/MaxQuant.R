#' MaxQuant User Interface Module
#' This function creates the user interface for processing proteomics data
#' from MaxQuant output files. It includes options for filtering, selecting
#' protein identifiers, and previewing processing results.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the MaxQuant processing module.
#' @import shiny
#' @import bslib
#' @importFrom DT dataTableOutput
#' @name MaxQuant_ui
#' @export

MaxQuant_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
      shiny::uiOutput(ns("load_status_panel")),
      bslib::accordion(
        bslib::accordion_panel(
          title = "remove unreliable peptide",
          icon = bslib::bs_icon("Filter"),
          shiny::div(
            style = "font-size: 12px;",
            shiny::checkboxGroupInput(
              inputId = ns("selected_Method"),
              label = "Please Select the Removal Method:",
              choices = c("remove peptide Only identified by site" = "site",
                          "remove potential contaminant peptide" = "conpeptide",
                          "remove reverse peptide" = "peptide"),
              selected = c("site", "peptide", "conpeptide")
            ),
            shiny::actionButton(ns("run_filter_unreliable"), "Remove", class = "btn btn-light fw-bold"),
            shiny::actionButton(ns("report"), "Report", class = "btn btn-light fw-bold")
          )
        )
      )
    ),
    shiny::div(
      bslib::card(
        bslib::card_header("Preview the data processing process"),
        bslib::card_body(
          fill = TRUE,
          bslib::navset_tab(
            id = ns("Expression_Matrix"),
            bslib::nav_panel("Sample info",
                      uiOutput(ns("sample_info_ui"))
            ),
            bslib::nav_panel("Expression Matrix",
                      htmlOutput(ns("matrix_check")),
                      DT::dataTableOutput(ns("tbl_expression_matrix"))
            ),
            bslib::nav_panel("Filtered Unreliable Peptide",
                      DT::dataTableOutput(ns("tbl_unreliable_filtered"))
            ),
            bslib::nav_panel("Reporter",
                      DT::dataTableOutput(ns("result_df"))
            )
          )
        )
      )
    )
  )
}

#' MaxQuant Server Logic Module
#' This function implements the server-side logic for processing proteomics data
#' from MaxQuant output. It supports loading data, filtering proteins based on
#' expression, removing unreliable peptides, selecting protein identifiers, and
#' saving intermediate results.
#' @param id Character. Module ID used for namespacing server inputs/outputs.
#' @param shared_state A reactiveValues object containing shared state variables
#'   across modules, including `workdir`, `sample_info`, and `expression_matrix`.
#' @return A list of reactive values containing processed results, including:
#'   - `protein_id_select`: A reactive expression with selected protein IDs.
#' @import shiny
#' @importFrom dplyr filter mutate select contains
#' @importFrom stringr str_split
#' @importFrom DT renderDataTable datatable renderDT
#' @name MaxQuant_server
#' @export

MaxQuant_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      expression_matrix_filtered = NULL,
      load_success = FALSE,
      filter_summary = NULL
    )
    filter_done <- shiny::reactiveVal(FALSE)
    # Load data
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      rda_path <- base::file.path(shared_state$workdir, "Step1_project_init.rda")
      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        base::load(rda_path, envir = e)
        if (base::exists("sample_info", envir = e)) {
          rv$sample_info <- e$sample_info
        }
        if (base::exists("expression_matrix", envir = e)) {
          rv$expression_matrix <- e$expression_matrix
          rv$expression_matrix_filtered <- e$expression_matrix
          filter_done(FALSE)
        }
        rv$load_success <- TRUE
        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("❌ Step1_project_init.rda not found in working directory.", type = "error")
      }
    })
    # remove unreliable peptide
    shiny::observeEvent(input$run_filter_unreliable, {
      shiny::req(rv$expression_matrix)
      selected_filters <- input$selected_Method
      filtered <- rv$expression_matrix
      if ("site" %in% selected_filters && "Only identified by site" %in% base::colnames(filtered)) {
        filtered <- dplyr::filter(filtered, base::is.na(`Only identified by site`))
      }
      if ("peptide" %in% selected_filters && "Reverse" %in% base::colnames(filtered)) {
        filtered <- dplyr::filter(filtered, base::is.na(`Reverse`))
      }
      if ("conpeptide" %in% selected_filters && "Potential contaminant" %in% base::colnames(filtered)) {
        filtered <- dplyr::filter(filtered, base::is.na(`Potential contaminant`)) %>%
          dplyr::mutate(ID = stringr::str_split(`Protein IDs`,";",2,T)[,1]) %>%
          dplyr::select(ID, dplyr::contains("Reporter"))
      }
      # Update results
      rv$expression_matrix_filtered <- filtered
      rv$unreliable_filtered <- filtered
      filter_done(TRUE)
      shiny::showNotification(paste("Unreliable peptides filtered, remaining rows:", nrow(filtered)), type = "message")
      # save data
      save_path <- base::file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda")
      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix
      expression_matrix_filtered <- rv$expression_matrix_filtered
      base::save(sample_info, expression_matrix, expression_matrix_filtered, file = save_path)
      shiny::showNotification("✅ Saved to Step2_remove_unreliable_peptide.rda", type = "message")
    })
    # Display report results
    shiny::observeEvent(input$report, {
      shiny::req(rv$expression_matrix_filtered)
      shiny::req(rv$expression_matrix)
      filtered <- rv$expression_matrix
      n_oibs <- nrow(dplyr::filter(filtered, !base::is.na(`Only identified by site`)))
      n_r <- nrow(dplyr::filter(filtered, !base::is.na(`Reverse`)))
      n_pc <- nrow(dplyr::filter(filtered, !base::is.na(`Potential contaminant`)))
      rep_id <- (filtered$`Protein IDs` %>% stringr::str_split(";",2,T))[,1]
      removed_protein_group <- base::setdiff(base::sort(rep_id), base::sort(rv$expression_matrix_filtered$ID))
      result_df <- base::data.frame(
        Metric = c("Only identified by site", "Reverse", "Potential contaminant", "Removed protein groups count"),
        Count = c(n_oibs, n_r, n_pc, length(removed_protein_group)),
        stringsAsFactors = FALSE
      )
      rv$filter_summary <- result_df
    })
    output$result_df <- DT::renderDataTable({
      shiny::req(rv$filter_summary)
      DT::datatable(rv$filter_summary,
                    options = list(pageLength = 5, searching = FALSE))
    })
    # UI outputs
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    output$sample_info_ui <- shiny::renderUI({
      shiny::req(rv$load_success)
      DT::DTOutput(ns("tbl_sample_info"))
    })
    output$tbl_sample_info <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(pageLength = 10))
    })
    output$matrix_check <- shiny::renderUI({
      if (!rv$load_success || is.null(rv$expression_matrix)) {
        shiny::HTML("<span style='color: red;'>Expression matrix not loaded.</span>")
      } else {
        shiny::HTML(base::paste("<span style='color: green;'>Matrix dimensions:",
                                base::nrow(rv$expression_matrix), "rows x", base::ncol(rv$expression_matrix),
                                "columns</span>"))
      }
    })
    output$tbl_expression_matrix <- DT::renderDataTable({
      shiny::req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = base::list(pageLength = 10, scrollX = TRUE))
    })
    output$tbl_filtered_expression <- DT::renderDataTable({
      shiny::req(filter_done())
      DT::datatable(rv$expression_matrix_filtered, options = base::list(pageLength = 10, scrollX = TRUE))
    })

    output$tbl_unreliable_filtered <- DT::renderDataTable({
      shiny::req(rv$unreliable_filtered)
      DT::datatable(rv$unreliable_filtered, options = base::list(pageLength = 10, scrollX = TRUE))
    })

  })
}
