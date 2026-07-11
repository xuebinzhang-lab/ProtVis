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
    shiny::tags$style(shiny::HTML("\n      .pv-data-input-header {\n        display: flex;\n        justify-content: space-between;\n        gap: 1rem;\n        align-items: flex-end;\n        margin: 1.25rem 0 1rem;\n        padding: 1.25rem 1.5rem;\n        border: 1px solid #d7e3ef;\n        border-radius: 18px;\n        background: linear-gradient(135deg, #ffffff 0%, #eef7ff 100%);\n        box-shadow: 0 10px 30px rgba(15, 76, 117, 0.08);\n      }\n      .pv-section-eyebrow {\n        color: #0b84c6;\n        font-size: 0.78rem;\n        font-weight: 700;\n        letter-spacing: 0.08em;\n        text-transform: uppercase;\n      }\n      .pv-page-title {\n        margin: 0.25rem 0;\n        color: #18324a;\n        font-weight: 800;\n      }\n      .pv-page-subtitle {\n        margin: 0;\n        max-width: 760px;\n        color: #607080;\n      }\n      .pv-source-pill {\n        min-width: 210px;\n        padding: 0.85rem 1rem;\n        border-radius: 14px;\n        background: #ffffff;\n        border: 1px solid #cfe2f3;\n        box-shadow: inset 0 0 0 1px rgba(255,255,255,0.6);\n      }\n      .pv-source-label {\n        display: block;\n        color: #6c7a89;\n        font-size: 0.78rem;\n        margin-bottom: 0.2rem;\n      }\n      .pv-mq-shell .sidebar {\n        border-right: 0;\n      }\n      .pv-sidebar-card {\n        padding: 1rem;\n        border: 1px solid #d9e7f2;\n        border-radius: 16px;\n        background: #ffffff;\n        box-shadow: 0 8px 24px rgba(24, 50, 74, 0.08);\n      }\n      .pv-load-button {\n        width: 100%;\n        border-radius: 12px;\n        padding: 0.7rem 1rem;\n        text-transform: uppercase;\n        letter-spacing: 0.03em;\n      }\n      .pv-status {\n        display: flex;\n        gap: 0.5rem;\n        align-items: center;\n        margin: 0.85rem 0 1rem;\n        padding: 0.75rem;\n        border-radius: 12px;\n        font-weight: 700;\n      }\n      .pv-status-ready { background: #eaf7ef; color: #177245; }\n      .pv-status-empty { background: #fff1f1; color: #c73535; }\n      .pv-filter-note {\n        color: #5f6f7f;\n        font-size: 0.82rem;\n        margin-bottom: 0.75rem;\n      }\n      .pv-action-row {\n        display: grid;\n        grid-template-columns: 1fr 1fr;\n        gap: 0.5rem;\n      }\n      .pv-preview-card {\n        border: 1px solid #d7e3ef;\n        border-radius: 18px;\n        overflow: hidden;\n        box-shadow: 0 12px 32px rgba(24, 50, 74, 0.08);\n      }\n      .pv-card-header {\n        display: flex;\n        justify-content: space-between;\n        gap: 1rem;\n        align-items: center;\n        background: #ffffff;\n      }\n      .pv-card-title {\n        margin: 0;\n        font-weight: 800;\n        color: #18324a;\n      }\n      .pv-card-subtitle {\n        margin: 0.15rem 0 0;\n        color: #6c7a89;\n        font-size: 0.9rem;\n      }\n      @media (max-width: 900px) {\n        .pv-data-input-header, .pv-card-header {\n          align-items: stretch;\n          flex-direction: column;\n        }\n        .pv-source-pill { min-width: 0; }\n      }\n    ")),
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
      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix
      expression_matrix_filtered <- rv$expression_matrix_filtered
      base::save(sample_info, expression_matrix, expression_matrix_filtered, file = save_path)
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
