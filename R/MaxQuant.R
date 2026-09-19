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
          shiny::actionButton(ns("load_data"), "Load data",
                              class = "btn btn-primary fw-bold pv-load-button"),
          shiny::uiOutput(ns("load_status_panel")),
          bslib::accordion(
            open = "Filtering options",
            bslib::accordion_panel(
              title = "Filtering options",
              icon = bsicons::bs_icon("funnel"),
              shiny::p(
                "Remove common MaxQuant flags before downstream processing.",
                class = "pv-filter-note"
              ),
              shiny::checkboxGroupInput(
                inputId = ns("selected_Method"),
                label = NULL,
                choices = c(
                  "Only identified by site" = "site",
                  "Potential contaminant" = "contaminant",
                  "Reverse" = "reverse"
                ),
                selected = c("site", "reverse", "contaminant")
              ),
              shiny::div(
                class = "pv-action-row",
                shiny::actionButton(
                  ns("run_filter_unreliable"), "Remove",
                  class = "btn btn-success fw-bold"
                ),
                shiny::actionButton(
                  ns("report"), "Report",
                  class = "btn btn-outline-primary fw-bold"
                )
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
            shiny::tags$p(
              "Inspect loaded data and generated filtering results.",
              class = "pv-card-subtitle"
            )
          ),
          shiny::uiOutput(ns("matrix_check"))
        ),
        bslib::card_body(
          fill = TRUE,
          bslib::navset_tab(
            id = ns("Expression_Matrix"),
            bslib::nav_panel(
              "Sample info",
              uiOutput(ns("sample_info_ui"))
            ),
            bslib::nav_panel(
              "Expression Matrix",
              DT::DTOutput(ns("tbl_expression_matrix"))
            ),
            bslib::nav_panel(
              "Filtered Protein Groups",
              DT::DTOutput(ns("tbl_unreliable_filtered"))
            ),
            bslib::nav_panel(
              "Report",
              DT::DTOutput(ns("result_df"))
            )
          )
        )
      )
    )
  )
}

.protvis_maxquant_filter_labels <- c(
  site = "Only identified by site",
  reverse = "Reverse",
  contaminant = "Potential contaminant"
)

.protvis_maxquant_flag_columns <- function(data) {
  patterns <- c(
    site = "^only identified by site$",
    reverse = "^reverse$",
    contaminant = "^potential contaminant$"
  )
  if (!base::is.data.frame(data)) {
    return(stats::setNames(
      base::rep(NA_character_, base::length(patterns)),
      base::names(patterns)
    ))
  }
  base::vapply(patterns, function(pattern) {
    hit <- .protvis_find_column(base::names(data), pattern)
    if (base::is.null(hit)) NA_character_ else base::as.character(hit)
  }, character(1))
}

.protvis_maxquant_filter_state <- function(
    raw_table,
    filters = c("site", "reverse", "contaminant")) {
  if (!base::is.data.frame(raw_table)) {
    stop("The original MaxQuant table is unavailable.", call. = FALSE)
  }
  columns <- .protvis_maxquant_flag_columns(raw_table)
  flagged <- base::lapply(base::names(.protvis_maxquant_filter_labels),
                          function(key) {
    column <- columns[[key]]
    if (base::is.na(column) || !base::nzchar(column)) {
      return(base::rep(FALSE, base::nrow(raw_table)))
    }
    .protvis_flagged(raw_table[[column]])
  })
  base::names(flagged) <- base::names(.protvis_maxquant_filter_labels)

  selected <- base::intersect(
    base::as.character(filters %||% character()),
    base::names(flagged)
  )
  remove <- base::rep(FALSE, base::nrow(raw_table))
  for (key in selected) remove <- remove | flagged[[key]]

  counts <- base::vapply(flagged, base::sum, numeric(1))
  report <- base::data.frame(
    Metric = c(
      "Before filtering",
      base::unname(.protvis_maxquant_filter_labels),
      "Unique rows removed",
      "After filtering"
    ),
    Count = c(
      base::nrow(raw_table),
      base::unname(counts),
      base::sum(remove),
      base::sum(!remove)
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  list(
    columns = columns,
    flagged = flagged,
    selected = selected,
    remove = remove,
    report = report
  )
}

.protvis_maxquant_filter_report <- function(
    raw_table,
    filters = c("site", "reverse", "contaminant")) {
  .protvis_maxquant_filter_state(raw_table, filters)$report
}

.protvis_attach_maxquant_preparation <- function(dataset, raw_table,
                                                 filename = NULL) {
  dataset <- as_protvis_dataset(dataset)
  if (!base::is.data.frame(raw_table) || base::nrow(raw_table) < 1L) {
    stop("A non-empty original MaxQuant table is required.", call. = FALSE)
  }
  id_column <- .protvis_id_column(raw_table, "MaxQuant")
  if (base::is.null(id_column)) {
    stop("MaxQuant output requires a Protein IDs column.", call. = FALSE)
  }

  state <- .protvis_maxquant_filter_state(raw_table, character())
  other_files <- dataset$other_files
  other_files$maxquant_preparation <- list(
    raw_table = raw_table,
    raw_rows = base::nrow(raw_table),
    id_column = id_column,
    flag_columns = state$columns,
    status = "pending",
    selected_filters = character(),
    report = state$report,
    filename = base::as.character(filename %||% "")
  )
  dataset$other_files <- other_files

  metadata <- dataset$metadata
  metadata$maxquant_filter_pending <- TRUE
  metadata$raw_rows <- base::nrow(raw_table)
  metadata$retained_rows <- base::nrow(dataset$expression_data)
  metadata$removed_rows <- 0L
  metadata$removed_by_flag <- stats::setNames(
    base::rep(0L, base::length(.protvis_maxquant_filter_labels)),
    base::names(.protvis_maxquant_filter_labels)
  )
  dataset$metadata <- metadata

  .protvis_append_process(
    dataset,
    "maxquant_filter_staging",
    status = "success",
    parameters = list(
      raw_rows = base::nrow(raw_table),
      flag_columns = state$columns
    ),
    message = paste0(
      "Preserved original MaxQuant flags for output preparation; ",
      base::nrow(raw_table), " rows remain unfiltered."
    )
  )
}

.protvis_maxquant_raw_from_dataset <- function(dataset, shared_state = NULL) {
  if (inherits(dataset, "ProtVis_dataset")) {
    preparation <- dataset$other_files$maxquant_preparation
    if (base::is.list(preparation) &&
        base::is.data.frame(preparation$raw_table)) {
      return(preparation$raw_table)
    }
  }
  if (!base::is.null(shared_state) &&
      base::is.data.frame(shared_state$maxquant_raw_table)) {
    return(shared_state$maxquant_raw_table)
  }
  NULL
}

#' MaxQuant Server Logic Module
#' This function implements the server-side logic for processing proteomics data
#' from MaxQuant output. It supports loading data, filtering proteins based on
#' original MaxQuant reliability flags, previewing results, and saving the
#' prepared ProtVis_dataset.
#' @param id Character. Module ID used for namespacing server inputs/outputs.
#' @param shared_state A reactiveValues object containing shared state variables
#'   across modules, including `workdir`, `sample_info`, and `dataset`.
#' @return A list of reactive values containing processed results.
#' @import shiny
#' @importFrom DT renderDT datatable
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
      maxquant_raw = NULL,
      load_success = FALSE,
      filter_summary = NULL
    )
    filter_done <- shiny::reactiveVal(FALSE)

    load_dataset <- function(dataset) {
      rv$sample_info <- dataset$sample_info
      rv$expression_matrix <- protvis_expression_matrix(dataset)
      rv$expression_matrix_filtered <- rv$expression_matrix
      rv$unreliable_filtered <- NULL
      rv$maxquant_raw <- .protvis_maxquant_raw_from_dataset(
        dataset, shared_state = shared_state
      )
      if (base::is.data.frame(rv$maxquant_raw)) {
        shared_state$maxquant_raw_table <- rv$maxquant_raw
        rv$filter_summary <- .protvis_maxquant_filter_report(
          rv$maxquant_raw,
          input$selected_Method %||%
            c("site", "reverse", "contaminant")
        )
      } else {
        rv$filter_summary <- NULL
      }
      filter_done(FALSE)
      rv$load_success <- TRUE
      invisible(dataset)
    }

    # Load the Project init dataset. The standard expression matrix is kept
    # separate from the original MaxQuant table stored for flag filtering.
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        load_dataset(shared_state$dataset)
        shiny::showNotification(
          "✅ ProtVis_dataset loaded successfully.", type = "message"
        )
        return(invisible(NULL))
      }

      rda_path <- base::file.path(shared_state$workdir,
                                 "Step1_project_init.rda")
      if (base::file.exists(rda_path)) {
        dataset <- .protvis_load_stage_dataset(
          rda_path,
          expression_names = "expression_matrix",
          metadata = list(source = "MaxQuant")
        )
        if (!base::is.null(dataset)) {
          shared_state$dataset <- dataset
          load_dataset(dataset)
          shiny::showNotification(
            "✅ Data loaded successfully.", type = "message"
          )
        } else {
          rv$load_success <- FALSE
          shiny::showNotification(
            "❌ Step1_project_init.rda could not be loaded.",
            type = "error"
          )
        }
      } else {
        rv$load_success <- FALSE
        shiny::showNotification(
          "❌ Step1_project_init.rda not found in working directory.",
          type = "error"
        )
      }
    })

    # Remove selected MaxQuant flags from the preserved original table, then
    # rebuild the standard protein expression matrix from the retained rows.
    shiny::observeEvent(input$run_filter_unreliable, {
      shiny::req(rv$expression_matrix)

      raw_table <- rv$maxquant_raw
      if (!base::is.data.frame(raw_table)) {
        shiny::showNotification(
          paste0(
            "Original MaxQuant flag columns are unavailable. Re-run Project ",
            "init with the updated ProtVis version so the three MaxQuant flag ",
            "columns are preserved."
          ),
          type = "error",
          duration = NULL
        )
        return(invisible(NULL))
      }

      selected_filters <- base::as.character(
        input$selected_Method %||% character()
      )
      filter_state <- .protvis_maxquant_filter_state(
        raw_table, selected_filters
      )
      parsed <- .protvis_parse_maxquant(
        raw_table, filters = selected_filters
      )
      filtered <- parsed$expression

      rv$expression_matrix_filtered <- filtered
      rv$unreliable_filtered <- filtered
      rv$filter_summary <- filter_state$report
      filter_done(TRUE)

      removed_rows <- base::sum(filter_state$remove)
      after_rows <- base::sum(!filter_state$remove)
      shiny::showNotification(
        base::paste0(
          "MaxQuant filtering completed: ",
          base::nrow(raw_table), " → ", after_rows,
          " rows (", removed_rows, " unique rows removed)."
        ),
        type = "message"
      )

      save_path <- base::file.path(
        shared_state$workdir, "Step1_maxquant_output_preparation.rda"
      )
      dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        shared_state$dataset
      } else {
        create_protvis_dataset(
          rv$expression_matrix,
          sample_info = rv$sample_info,
          metadata = list(source = "MaxQuant")
        )
      }

      dataset <- .protvis_update_expression(dataset, filtered)
      dataset <- .protvis_new_analysis_dataset(
        dataset,
        "filter_unreliable",
        list(method = selected_filters)
      )

      preparation <- dataset$other_files$maxquant_preparation %||% list()
      preparation$raw_table <- raw_table
      preparation$raw_rows <- base::nrow(raw_table)
      preparation$id_column <- .protvis_id_column(raw_table, "MaxQuant")
      preparation$flag_columns <- filter_state$columns
      preparation$status <- "filtered"
      preparation$selected_filters <- selected_filters
      preparation$report <- filter_state$report
      preparation$filtered_raw_rows <- after_rows
      preparation$filtered_expression_rows <- base::nrow(filtered)
      other_files <- dataset$other_files
      other_files$maxquant_preparation <- preparation
      dataset$other_files <- other_files

      metadata <- dataset$metadata
      metadata$maxquant_filter_pending <- FALSE
      metadata$raw_rows <- base::nrow(raw_table)
      metadata$retained_rows <- base::nrow(filtered)
      metadata$removed_rows <- removed_rows
      metadata$removed_by_flag <- stats::setNames(
        c(
          filter_state$report$Count[
            filter_state$report$Metric == "Only identified by site"
          ],
          filter_state$report$Count[
            filter_state$report$Metric == "Reverse"
          ],
          filter_state$report$Count[
            filter_state$report$Metric == "Potential contaminant"
          ]
        ),
        c("site", "reverse", "contaminant")
      )
      dataset$metadata <- metadata

      dataset$analysis_results$filter_unreliable <- list(
        status = "success",
        filters = selected_filters,
        raw_rows = base::nrow(raw_table),
        removed_rows = removed_rows,
        retained_raw_rows = after_rows,
        retained_rows = base::nrow(dataset$expression_data),
        result_table = filter_state$report
      )
      dataset <- .protvis_append_process(
        dataset,
        "filter_unreliable",
        status = "success",
        parameters = list(
          filters = selected_filters,
          raw_rows = base::nrow(raw_table),
          removed_rows = removed_rows,
          retained_raw_rows = after_rows
        )
      )

      .protvis_ui_sync_state(dataset, shared_state)
      shared_state$maxquant_raw_table <- raw_table
      .protvis_save_stage_dataset(dataset, save_path)
      shiny::showNotification(
        "✅ MaxQuant output prepared; continue with Correct Noise.",
        type = "message"
      )
    })

    # Report uses the preserved raw flag columns. Counts for the three flags
    # are independent; Unique rows removed is the de-duplicated union of the
    # currently selected filters.
    shiny::observeEvent(input$report, {
      raw_table <- rv$maxquant_raw
      if (!base::is.data.frame(raw_table)) {
        shiny::showNotification(
          "Original MaxQuant flag columns are unavailable; re-run Project init.",
          type = "error"
        )
        return(invisible(NULL))
      }
      rv$filter_summary <- .protvis_maxquant_filter_report(
        raw_table,
        input$selected_Method %||% character()
      )
    })

    output$result_df <- DT::renderDT({
      shiny::req(rv$filter_summary)
      DT::datatable(
        rv$filter_summary,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        )
      )
    })

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
      DT::datatable(
        rv$sample_info,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$matrix_check <- shiny::renderUI({
      if (!rv$load_success || base::is.null(rv$expression_matrix)) {
        shiny::span(
          "Expression matrix not loaded",
          class = "badge text-bg-secondary"
        )
      } else {
        current_matrix <- if (isTRUE(filter_done()) &&
                              !base::is.null(rv$expression_matrix_filtered)) {
          rv$expression_matrix_filtered
        } else {
          rv$expression_matrix
        }
        shiny::span(
          base::paste(
            "Matrix:",
            base::nrow(current_matrix),
            "rows ×",
            base::ncol(current_matrix),
            "columns"
          ),
          class = "badge text-bg-success"
        )
      }
    })

    output$tbl_expression_matrix <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      current_matrix <- if (isTRUE(filter_done()) &&
                            !base::is.null(rv$expression_matrix_filtered)) {
        rv$expression_matrix_filtered
      } else {
        rv$expression_matrix
      }
      DT::datatable(
        current_matrix,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$tbl_unreliable_filtered <- DT::renderDT({
      shiny::req(rv$unreliable_filtered)
      DT::datatable(
        rv$unreliable_filtered,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  })
}
