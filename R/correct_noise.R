#' Correct zero values in replicate measurements
#' This function processes a raw matrix of protein/peptide intensities and
#' corrects zero values in replicates when at least 2 replicates have non-zero values.
#' The correction replaces zeros with half the sum of the non-zero replicates.
#' @param raw_mat A data frame containing protein/peptide intensities with
#'                samples in columns and IDs in the first column. Sample names
#'                should contain "_1", "_2", "_3" suffixes to indicate replicates.
#' @return A data frame with corrected values where zeros have been replaced
#'         according to the correction rules. Rows with all zeros remain unchanged.
#' @importFrom dplyr select rowwise filter ungroup full_join mutate if_else group_by
#' @importFrom dplyr case_when across na_if contains c_across
#' @importFrom tidyr pivot_longer replace_na pivot_wider
#' @importFrom stringr str_sub
#' @name correct_values
#' @export
#'
utils::globalVariables(c("ID", "sample_id", "value", "value_fix", "sample_group", ".", "tag", "tag_sum"))

correct_values <- function(raw_mat) {
  if (base::is.null(raw_mat) || !base::is.data.frame(raw_mat)) {
    stop("Noise correction requires a data frame with an ID column and sample columns.",
         call. = FALSE)
  }
  if (!"ID" %in% base::names(raw_mat)) {
    base::names(raw_mat)[1] <- "ID"
  }
  clean_mat <- raw_mat

  sample_cols <- base::setdiff(base::names(clean_mat), "ID")
  if (base::length(sample_cols) == 0L) return(clean_mat)

  # Repeated protein IDs would make the replicate joins many-to-many and can
  # expand a modest table into billions of rows. Collapse them before any
  # reshaping, retaining the largest finite intensity per sample.
  if (anyDuplicated(clean_mat$ID)) {
    clean_mat <- clean_mat %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(sample_cols), ~ {
          value <- suppressWarnings(as.numeric(.x))
          value <- value[is.finite(value)]
          if (length(value) == 0L) NA_real_ else max(value)
        }),
        .groups = "drop"
      )
  }

  replicate_cols <- base::lapply(seq_len(3L), function(replicate) {
    pattern <- paste0("(^|_)", replicate, "(_|$)|_", replicate, "$|",
                      "_", replicate, "_")
    sample_cols[base::grepl(pattern, sample_cols, perl = TRUE)]
  })
  replicate_cols <- replicate_cols[base::lengths(replicate_cols) > 0L]
  # Not every imported matrix uses _1/_2/_3 replicate suffixes. In that case
  # there is no replicate-level correction to perform, but the app must remain
  # usable and return the validated matrix instead of calling pivot_longer()
  # with an empty selection.
  if (base::length(replicate_cols) == 0L) return(clean_mat)

  replicate_names <- base::unlist(replicate_cols, use.names = FALSE)
  replicate_names <- intersect(sample_cols, replicate_names)
  if (base::length(replicate_names) == 0L) return(clean_mat)

  # Keep the original rule, but operate directly on a numeric matrix. This
  # avoids creating one joined table and two reshaped copies of the data.
  values <- as.matrix(clean_mat[, sample_cols, drop = FALSE])
  suppressWarnings(storage.mode(values) <- "double")
  values[is.na(values)] <- 0
  replicate_index <- match(replicate_names, sample_cols)
  replicate_groups <- base::sub("^([123])_", "", replicate_names)
  replicate_groups <- base::sub("_[123]$", "", replicate_groups)

  for (group in unique(replicate_groups)) {
    indices <- replicate_index[replicate_groups == group]
    group_values <- values[, indices, drop = FALSE]
    nonzero <- group_values > 0
    tag_sum <- rowSums(nonzero)
    replacement <- rowSums(group_values * nonzero) / 2
    zero_cells <- group_values == 0 & tag_sum >= 2
    if (any(zero_cells)) {
      group_values[zero_cells] <- replacement[row(zero_cells)[zero_cells]]
    }
    # This is equivalent to the original case_when(TRUE ~ 0) branch for
    # groups with fewer than two non-zero replicates.
    group_values[tag_sum < 2, ] <- 0
    values[, indices] <- group_values
  }

  result <- clean_mat
  result[, sample_cols] <- as.data.frame(values, check.names = FALSE)
  result[, sample_cols] <- lapply(result[, sample_cols, drop = FALSE],
                                  function(x) {
                                    x[x == 0] <- NA_real_
                                    x
                                  })
  result
}

#' UI for Noise Correction Module
#' @param id Character string module ID for namespacing
#' @return A Shiny UI layout with sidebar controls and main display area
#' @import shiny
#' @import bslib
#' @importFrom shinyWidgets switchInput progressBar updateProgressBar
#' @importFrom bsicons bs_icon
#' @name correct_noise_ui
#' @export
#'
correct_noise_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    protvis_data_input_style(),
    bslib::layout_sidebar(
      class = "pv-mq-shell",
      sidebar = bslib::sidebar(
        width = 320,
        class = "pv-sidebar-card",

      shiny::actionButton(
        ns("load_data"),
        "Load data",
        class = "btn btn-primary fw-bold pv-load-button"
      ),
      shiny::uiOutput(ns("load_status_panel")),
      shinyWidgets::progressBar(
        id = ns("load_progress"),
        value = 0,
        total = 100,
        display_pct = TRUE,
        striped = TRUE,
        status = "success",
        title = "Load progress"
      ),

      bslib::accordion(
        bslib::accordion_panel(
          title = "Step1 Rename Columns",
          icon = bsicons::bs_icon("tools"),
          shiny::actionButton(
            inputId = ns("rename_columns"),
            label = "Rename Columns",
            class = "btn btn-outline-primary w-100"
          )
        ),
        bslib::accordion_panel(
          title = "Step2 Correct Noise",
          icon = bsicons::bs_icon("tools"),
          shiny::actionButton(
            inputId = ns("correct_noise"),
            label = "Correct Noise",
            class = "btn btn-success w-100"
          ),
          shinyWidgets::progressBar(
            id = ns("noise_progress"),
            value = 0,
            total = 100,
            display_pct = TRUE,
            striped = TRUE,
            status = "warning",
            title = "Noise correction progress"
          )
        )
      ),

      shiny::tags$br(),
      shiny::div(
        class = "pv-status pv-status-ready",
        "Results are saved automatically to ProtVis_dataset."
      )
    ),

    bslib::card(
      class = "pv-preview-card",
      bslib::card_header(
        class = "pv-card-header",
        shiny::div(
          shiny::tags$h4("Processing preview", class = "pv-card-title"),
          shiny::tags$p("Review preprocessing inputs, outputs, and intermediate results.", class = "pv-card-subtitle")
        )
      ),
      bslib::card_body(
        fill = TRUE,
        bslib::navset_tab(
          id = ns("Expression_Matrix"),
          header = NULL,
          bslib::nav_panel(
            "Sample info",
            shiny::uiOutput(ns("sample_info_ui"))
          ),
          bslib::nav_panel(
            "Expression Matrix",
            shiny::htmlOutput(ns("matrix_check")),
            DT::DTOutput(ns("expression_matrix_filtered"))
          ),
          bslib::nav_panel(
            "Rename Columns",
            DT::DTOutput(ns("tbl_rename_columns"))
          ),
          bslib::nav_panel(
            "Correct Noise",
            DT::DTOutput(ns("tbl_correct_noise"))
          )
        )
      )
    )
  )
  )
}


#' Server Logic for Noise Correction Module
#' @param id Character string module ID for namespacing
#' @param shared_state Reactive values list for sharing data between modules
#' @return Server logic for the noise correction module
#' @import shiny
#' @importFrom dplyr left_join pull
#' @importFrom DT renderDT datatable
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom shinyWidgets updateProgressBar
#' @name correct_noise_server
#' @export
#'
correct_noise_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      load_success = FALSE,
      rename_requested = FALSE,
      noise_requested = FALSE
    )

    shiny::observe({
      shinyWidgets::updateProgressBar(
        session = session,
        id = "load_progress",
        value = 0,
        total = 100
      )
      shinyWidgets::updateProgressBar(
        session = session,
        id = "noise_progress",
        value = 0,
        total = 100
      )
    })

    shiny::observeEvent(input$load_data, {
      # The ProtVis_dataset is the primary source. The legacy RDA path below
      # remains only for projects created by older releases.
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        shared_state$sample_info <- shared_state$dataset$sample_info
        shared_state$expression_matrix_filtered <- protvis_expression_matrix(
          shared_state$dataset
        )
        shared_state$rename_result <- NULL
        shared_state$correct_noise_result <- NULL
        rv$load_success <- TRUE
        shinyWidgets::updateProgressBar(session, id = "load_progress", value = 100)
        shiny::showNotification("✅ ProtVis_dataset loaded successfully.", type = "message")
        return(invisible(NULL))
      }
      shiny::req(shared_state$workdir)

      shinyWidgets::updateProgressBar(session, id = "load_progress", value = 10)
      rda_path <- base::file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda")

      if (base::file.exists(rda_path)) {
        shinyWidgets::updateProgressBar(session, id = "load_progress", value = 35)
        dataset <- .protvis_load_stage_dataset(
          rda_path,
          expression_names = c("expression_matrix_filtered", "expression_matrix")
        )
        shinyWidgets::updateProgressBar(session, id = "load_progress", value = 65)
        if (!base::is.null(dataset)) {
          shared_state$dataset <- dataset
          shared_state$sample_info <- dataset$sample_info
          shared_state$expression_matrix_filtered <- protvis_expression_matrix(dataset)
          shared_state$rename_result <- NULL
          shared_state$correct_noise_result <- NULL
        }
        rv$load_success <- !base::is.null(dataset)
        shinyWidgets::updateProgressBar(session, id = "load_progress", value = 100)
        shiny::showNotification("✅ Step2 data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shinyWidgets::updateProgressBar(session, id = "load_progress", value = 0)
        shiny::showNotification(
          "❌ Step2_remove_unreliable_peptide.rda not found in working directory.",
          type = "error"
        )
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::div(class = "pv-status pv-status-ready", "✓ Data loaded")
      } else {
        shiny::div(class = "pv-status pv-status-empty", "× Data not loaded")
      }
    })

    output$sample_info_ui <- shiny::renderUI({
      shiny::req(shared_state$sample_info)
      DT::DTOutput(ns("tbl_sample_info"))
    })

    output$tbl_sample_info <- DT::renderDT({
      shiny::req(shared_state$sample_info)
      DT::datatable(shared_state$sample_info, rownames = FALSE,
                    options = list(pageLength = 10))
    })

    output$expression_matrix_filtered <- DT::renderDT({
      shiny::req(shared_state$expression_matrix_filtered)
      DT::datatable(
        shared_state$expression_matrix_filtered,
        rownames = FALSE,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    correct_noise_step1 <- shiny::reactive({
      shiny::req(shared_state$expression_matrix_filtered, shared_state$sample_info)

      input_data <- base::as.data.frame(
        shared_state$expression_matrix_filtered,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      if (!"ID" %in% base::names(input_data)) {
        input_data <- .protvis_rownames_to_column(input_data, "ID")
      }
      sample_cols <- base::setdiff(base::names(input_data), "ID")
      if (base::length(sample_cols) == 0L) {
        stop("Expression matrix has no sample columns.", call. = FALSE)
      }

      new_name <- dplyr::left_join(
        base::data.frame(maxquant_id = sample_cols, stringsAsFactors = FALSE),
        shared_state$sample_info,
        by = "maxquant_id"
      ) %>%
        dplyr::pull(sample_id)

      # If the matrix already uses sample_id names (common for built-in and
      # imported ProtVis_dataset objects), retain those names when the
      # maxquant_id lookup has no match.  Always make names unique so joins
      # and reshaping cannot expand the data unexpectedly.
      new_name <- as.character(new_name)
      missing_name <- is.na(new_name) | !nzchar(trimws(new_name))
      new_name[missing_name] <- sample_cols[missing_name]
      base::names(input_data) <- c("ID", make.unique(new_name, sep = "_"))
      input_data
    })

    shiny::observeEvent(input$rename_columns, {
      shiny::req(shared_state$expression_matrix_filtered)
      rv$rename_requested <- TRUE
      tryCatch({
        shared_state$rename_result <- correct_noise_step1()
        shiny::showNotification("✅ Columns renamed.", type = "message")
      }, error = function(e) {
        shared_state$rename_result <- NULL
        shiny::showNotification(
          paste0("Column rename failed: ", conditionMessage(e)), type = "error"
        )
      })
    })

    shiny::observeEvent(input$correct_noise, {
      rv$noise_requested <- TRUE
      tryCatch({
          shiny::req(correct_noise_step1())
          shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 15)
          dat <- correct_noise_step1()
          shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 45)
          result <- correct_values(dat)
          shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 100)
          shared_state$correct_noise_result <- result
          dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) {
            shared_state$dataset
          } else {
            create_protvis_dataset(
              shared_state$expression_matrix_filtered,
              sample_info = shared_state$sample_info
            )
          }
          dataset <- .protvis_update_expression(dataset, result)
          dataset <- .protvis_new_analysis_dataset(
            dataset, "noise_correction", list(method = "replicate_correction")
          )
          dataset$analysis_results$noise_correction <- list(
            status = "success", method = "replicate_correction"
          )
          dataset <- .protvis_append_process(
            dataset, "noise_correction", status = "success",
            parameters = list(method = "replicate_correction")
          )
          .protvis_ui_sync_state(dataset, shared_state)
          .protvis_save_stage_dataset(
            dataset,
            base::file.path(shared_state$workdir, "Step3_correct_noise.rda")
          )
          shiny::showNotification("✅ Noise correction completed.", type = "message")
        }, error = function(e) {
          shared_state$correct_noise_result <- NULL
          shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 0)
          shiny::showNotification(
            paste0("Noise correction failed: ", conditionMessage(e)),
            type = "error"
          )
        })
    })

    output$tbl_rename_columns <- DT::renderDT({
      if (isTRUE(rv$rename_requested)) {
        shiny::req(shared_state$rename_result)
        DT::datatable(
          shared_state$rename_result,
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 10)
        )
      }
    })

    output$tbl_correct_noise <- DT::renderDT({
      if (isTRUE(rv$noise_requested)) {
        shiny::req(shared_state$correct_noise_result)
        DT::datatable(
          shared_state$correct_noise_result,
          rownames = FALSE,
          options = list(scrollX = TRUE, pageLength = 10)
        )
      }
    })

  })
}
