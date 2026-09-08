#' MaxQuant User Interface Module
#' This function creates the user interface for processing proteomics data
#' from MaxQuant output files. It includes options for filtering, selecting
#' protein identifiers, and previewing processing results.
#' @param id Character. Module ID used for namespacing the UI elements.
#' @return UI layout for the MaxQuant processing module.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @importFrom DT DTOutput renderDT datatable
#' @name MaxQuant_ui
#' @export

MaxQuant_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    protvis_data_input_style(),
    bslib::layout_sidebar(
      class = "pv-mq-shell",
      sidebar = bslib::sidebar(
        width = 320,
        shiny::div(
          class = "pv-sidebar-card",
          shiny::actionButton(ns("load_data"), "Load data", class = "btn btn-primary fw-bold pv-load-button"),
          shiny::uiOutput(ns("load_status_panel")),
          bslib::accordion(
            open = "Filtering options",
            bslib::accordion_panel(
              title = "Filtering options",
              icon = bsicons::bs_icon("funnel"),
              shiny::p("Remove common MaxQuant flags before downstream processing.", class = "pv-filter-note"),
              shiny::checkboxGroupInput(
                inputId = ns("selected_Method"),
                label = NULL,
                choices = c("Only identified by site" = "site",
                            "Potential contaminant" = "conpeptide",
                            "Reverse peptide" = "peptide"),
                selected = c("site", "peptide", "conpeptide")
              ),
              shiny::div(
                class = "pv-action-row",
                shiny::actionButton(ns("run_filter_unreliable"), "Remove", class = "btn btn-success fw-bold"),
                shiny::actionButton(ns("report"), "Report", class = "btn btn-outline-primary fw-bold")
              )
            )
          )
        )
      ),
      bslib::card(
        class = "pv-preview-card",
        bslib::card_header(
          class = "pv-card-header",
          shiny::div(
            shiny::tags$h4("Processing preview", class = "pv-card-title"),
            shiny::tags$p("Inspect loaded data and generated filtering results.", class = "pv-card-subtitle")
          ),
          shiny::uiOutput(ns("matrix_check"))
        ),
        bslib::card_body(
          fill = TRUE,
          bslib::navset_tab(
            id = ns("Expression_Matrix"),
            bslib::nav_panel("Sample info",
                             uiOutput(ns("sample_info_ui"))
            ),
            bslib::nav_panel("Expression Matrix",
                             DT::DTOutput(ns("tbl_expression_matrix"))
            ),
            bslib::nav_panel("Filtered Peptides",
                             DT::DTOutput(ns("tbl_unreliable_filtered"))
            ),
            bslib::nav_panel("Report",
                             DT::DTOutput(ns("result_df"))
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
#' @importFrom DT renderDT datatable
#' @import magrittr
#' @name MaxQuant_server
#' @export

MaxQuant_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      expression_matrix_filtered = NULL,
      unreliable_filtered = NULL,
      load_success = FALSE,
      filter_summary = NULL
    )
    filter_done <- shiny::reactiveVal(FALSE)

    # Load data
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        rv$sample_info <- shared_state$dataset$sample_info
        rv$expression_matrix <- protvis_expression_matrix(shared_state$dataset)
        rv$expression_matrix_filtered <- rv$expression_matrix
        filter_done(FALSE)
        rv$load_success <- TRUE
        shiny::showNotification("✅ ProtVis_dataset loaded successfully.", type = "message")
        return(invisible(NULL))
      }
      rda_path <- base::file.path(shared_state$workdir, "Step1_project_init.rda")
      if (base::file.exists(rda_path)) {
        dataset <- .protvis_load_stage_dataset(
          rda_path, expression_names = "expression_matrix",
          metadata = list(source = "MaxQuant")
        )
        if (!base::is.null(dataset)) {
          shared_state$dataset <- dataset
          rv$sample_info <- dataset$sample_info
          rv$expression_matrix <- protvis_expression_matrix(dataset)
          rv$expression_matrix_filtered <- rv$expression_matrix
          filter_done(FALSE)
        }
        rv$load_success <- !base::is.null(dataset)
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
      # Uniform filtering of unreliable peptide fragments
      if ("site" %in% selected_filters && "Only identified by site" %in% colnames(filtered)) {
        filtered <- dplyr::filter(filtered, base::is.na(`Only identified by site`))
      }
      if ("peptide" %in% selected_filters && "Reverse" %in% colnames(filtered)) {
        filtered <- dplyr::filter(filtered, base::is.na(`Reverse`))
      }
      if ("conpeptide" %in% selected_filters && "Potential contaminant" %in% colnames(filtered)) {
        filtered <- dplyr::filter(filtered, base::is.na(`Potential contaminant`))
      }
      # Unify ID generation and Reporter column extraction.
      if (base::nrow(filtered) > 0 && "Protein IDs" %in% base::colnames(filtered)) {
        filtered <- filtered %>%
          dplyr::mutate(ID = stringr::str_split(`Protein IDs`, ";", 2, TRUE)[, 1]) %>%
          dplyr::select(
            ID,
            dplyr::contains("Reporter"),
            dplyr::everything(),
            -`Protein IDs`,
            -`Only identified by site`,
            -`Reverse`,
            -`Potential contaminant`
          )
      }
      # Update results
      rv$expression_matrix_filtered <- filtered
      rv$unreliable_filtered <- filtered
      filter_done(TRUE)
      shiny::showNotification(paste("Unreliable peptides filtered, remaining rows:", nrow(filtered)), type = "message")
      # Save results
      save_path <- base::file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda")
      dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        shared_state$dataset
      } else {
        create_protvis_dataset(
          rv$expression_matrix,
          sample_info = rv$sample_info,
          metadata = list(source = "MaxQuant")
        )
      }
      dataset <- .protvis_update_expression(dataset, rv$expression_matrix_filtered)
      dataset <- .protvis_new_analysis_dataset(
        dataset, "filter_unreliable", list(method = selected_filters)
      )
      dataset$analysis_results$filter_unreliable <- list(
        status = "success", filters = selected_filters,
        retained_rows = nrow(dataset$expression_data)
      )
      dataset <- .protvis_append_process(
        dataset, "filter_unreliable", status = "success",
        parameters = list(filters = selected_filters)
      )
      .protvis_ui_sync_state(dataset, shared_state)
      .protvis_save_stage_dataset(dataset, save_path)
      shiny::showNotification("✅ Saved to Step2_remove_unreliable_peptide.rda", type = "message")
    })
    # Display report results
    shiny::observeEvent(input$report, {
      shiny::req(rv$expression_matrix_filtered, rv$expression_matrix)
      filtered <- rv$expression_matrix
      n_oibs <- if("Only identified by site" %in% colnames(filtered)) {
        nrow(dplyr::filter(filtered, !base::is.na(`Only identified by site`)))
      } else {0}
      n_r <- if("Reverse" %in% colnames(filtered)) {
        nrow(dplyr::filter(filtered, !base::is.na(`Reverse`)))
      } else {0}
      n_pc <- if("Potential contaminant" %in% colnames(filtered)) {
        nrow(dplyr::filter(filtered, !base::is.na(`Potential contaminant`)))
      } else {0}
      rep_id <- (filtered$`Protein IDs` %>% stringr::str_split(";", 2, TRUE))[, 1]
      removed_protein_group <- base::setdiff(base::sort(rep_id), base::sort(rv$expression_matrix_filtered$ID))
      result_df <- base::data.frame(
        Metric = c("Only identified by site", "Reverse", "Potential contaminant", "Removed protein groups count"),
        Count = c(n_oibs, n_r, n_pc, length(removed_protein_group)),
        stringsAsFactors = FALSE
      )
      rv$filter_summary <- result_df
    })

    output$result_df <- DT::renderDT({
      shiny::req(rv$filter_summary)
      DT::datatable(rv$filter_summary, options = list(pageLength = 5, searching = FALSE))
    })

    # UI outputs
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::div(class = "pv-status pv-status-ready", "✓ Data loaded")
      } else {
        shiny::div(class = "pv-status pv-status-empty", "× Data not loaded")
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
        shiny::span("Expression matrix not loaded", class = "badge text-bg-secondary")
      } else {
        shiny::span(
          base::paste("Matrix:", base::nrow(rv$expression_matrix), "rows ×", base::ncol(rv$expression_matrix), "columns"),
          class = "badge text-bg-success"
        )
      }
    })

    output$tbl_expression_matrix <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$tbl_unreliable_filtered <- DT::renderDT({
      shiny::req(rv$unreliable_filtered)
      DT::datatable(rv$unreliable_filtered, options = list(pageLength = 10, scrollX = TRUE))
    })

  })
}
