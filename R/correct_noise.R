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
  clean_mat <- raw_mat

  rep1 <- dplyr::select(clean_mat, ID, dplyr::contains("_1")) %>%
    dplyr::rowwise() %>%
    dplyr::filter(base::any(dplyr::c_across(-ID) != 0)) %>%
    dplyr::ungroup()

  rep2 <- dplyr::select(clean_mat, ID, dplyr::contains("_2")) %>%
    dplyr::rowwise() %>%
    dplyr::filter(base::any(dplyr::c_across(-ID) != 0)) %>%
    dplyr::ungroup()

  rep3 <- dplyr::select(clean_mat, ID, dplyr::contains("_3")) %>%
    dplyr::rowwise() %>%
    dplyr::filter(base::any(dplyr::c_across(-ID) != 0)) %>%
    dplyr::ungroup()

  mv_mat <- dplyr::full_join(rep1, rep2, by = "ID") %>%
    dplyr::full_join(rep3, by = "ID")

  long_df <- tidyr::pivot_longer(
    mv_mat,
    cols = -ID,
    names_to = "sample",
    values_to = "value"
  ) %>%
    dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
    dplyr::mutate(
      sample_group = stringr::str_sub(sample, 1, -3),
      tag = dplyr::if_else(value > 0, 1L, 0L)
    )

  value_fix_df <- long_df %>%
    dplyr::group_by(ID, sample_group) %>%
    dplyr::mutate(
      tag_sum = base::sum(tag),
      value_fix = dplyr::case_when(
        tag_sum >= 2 & value == 0 ~ base::sum(value[tag == 1]) / 2,
        tag_sum >= 2 & value != 0 ~ value,
        tag_sum == 3 ~ value,
        TRUE ~ 0
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(ID, sample, value_fix)

  final_mat <- tidyr::pivot_wider(
    value_fix_df,
    names_from = sample,
    values_from = value_fix
  ) %>%
    dplyr::mutate(dplyr::across(-ID, ~ dplyr::na_if(., 0)))

  return(final_mat)
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

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,

      shiny::actionButton(
        ns("load_data"),
        "LOAD DATA",
        class = "btn btn-light fw-bold"
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
          shinyWidgets::switchInput(
            inputId = ns("rename_columns"),
            label = "Rename Columns",
            value = FALSE,
            onLabel = "✔",
            offLabel = "✘",
            size = "small",
            labelWidth = "120px",
            handleWidth = 60
          )
        ),
        bslib::accordion_panel(
          title = "Step2 Correct Noise",
          icon = bsicons::bs_icon("tools"),
          shinyWidgets::switchInput(
            inputId = ns("correct_noise"),
            label = "Correct Noise",
            value = FALSE,
            onLabel = "✔",
            offLabel = "✘",
            size = "small",
            labelWidth = "120px",
            handleWidth = 60
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

      shiny::actionButton(
        ns("export_correct_noise"),
        "EXPORT DATA",
        class = "btn btn-light fw-bold"
      ),
      shiny::uiOutput(ns("export_correct_noise_status_panel")),
      shinyWidgets::progressBar(
        id = ns("export_progress"),
        value = 0,
        total = 100,
        display_pct = TRUE,
        striped = TRUE,
        status = "info",
        title = "Export progress"
      )
    ),

    bslib::card(
      bslib::card_header("Preview the data processing process"),
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
      export_success = FALSE
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
      shinyWidgets::updateProgressBar(
        session = session,
        id = "export_progress",
        value = 0,
        total = 100
      )
    })

    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)

      shinyWidgets::updateProgressBar(session, id = "load_progress", value = 10)
      rda_path <- base::file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda")

      if (base::file.exists(rda_path)) {
        shinyWidgets::updateProgressBar(session, id = "load_progress", value = 35)

        e <- base::new.env()
        base::load(rda_path, envir = e)

        shinyWidgets::updateProgressBar(session, id = "load_progress", value = 65)

        if (base::exists("sample_info", envir = e)) {
          shared_state$sample_info <- e$sample_info
        }

        if (base::exists("expression_matrix_filtered", envir = e)) {
          shared_state$expression_matrix_filtered <- e$expression_matrix_filtered
          shared_state$rename_result <- NULL
          shared_state$correct_noise_result <- NULL
        }

        rv$load_success <- TRUE
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
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })

    output$export_correct_noise_status_panel <- shiny::renderUI({
      if (rv$export_success) {
        shiny::span("✅ Data exported", style = "color: green;")
      } else {
        shiny::span("Waiting for export", style = "color: #666;")
      }
    })

    output$sample_info_ui <- shiny::renderUI({
      shiny::req(shared_state$sample_info)
      DT::DTOutput(ns("tbl_sample_info"))
    })

    output$tbl_sample_info <- DT::renderDT({
      shiny::req(shared_state$sample_info)
      DT::datatable(shared_state$sample_info, options = list(pageLength = 10))
    })

    output$expression_matrix_filtered <- DT::renderDT({
      shiny::req(shared_state$expression_matrix_filtered)
      DT::datatable(
        shared_state$expression_matrix_filtered,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    correct_noise_step1 <- shiny::reactive({
      shiny::req(shared_state$expression_matrix_filtered, shared_state$sample_info)

      new_name <- dplyr::left_join(
        base::data.frame(maxquant_id = base::colnames(shared_state$expression_matrix_filtered)[-1]),
        shared_state$sample_info,
        by = "maxquant_id"
      ) %>%
        dplyr::pull(sample_id)

      shared_state$expression_matrix_filtered %>%
        stats::setNames(c("ID", new_name))
    })

    shiny::observe({
      shiny::req(shared_state$expression_matrix_filtered)

      if (isTRUE(input$rename_columns)) {
        shared_state$rename_result <- correct_noise_step1()
      } else {
        shared_state$rename_result <- NULL
      }
    })

    shiny::observeEvent(input$correct_noise, {
      if (isTRUE(input$correct_noise)) {
        shiny::req(correct_noise_step1())

        shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 15)

        dat <- correct_noise_step1()

        shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 45)

        result <- correct_values(dat)

        shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 100)

        shared_state$correct_noise_result <- result
        shiny::showNotification("✅ Noise correction completed.", type = "message")
      } else {
        shared_state$correct_noise_result <- NULL
        shinyWidgets::updateProgressBar(session, id = "noise_progress", value = 0)
      }
    })

    output$tbl_rename_columns <- DT::renderDT({
      if (isTRUE(input$rename_columns)) {
        shiny::req(shared_state$rename_result)
        DT::datatable(
          shared_state$rename_result,
          options = list(scrollX = TRUE, pageLength = 10)
        )
      }
    })

    output$tbl_correct_noise <- DT::renderDT({
      if (isTRUE(input$correct_noise)) {
        shiny::req(shared_state$correct_noise_result)
        DT::datatable(
          shared_state$correct_noise_result %>%
            tibble::column_to_rownames("ID"),
          options = list(scrollX = TRUE, pageLength = 10)
        )
      }
    })

    shiny::observeEvent(input$export_correct_noise, {
      shiny::req(rv$load_success, shared_state$workdir, shared_state$sample_info)

      rv$export_success <- FALSE
      shinyWidgets::updateProgressBar(session, id = "export_progress", value = 10)

      export_data <- NULL

      if (isTRUE(input$correct_noise) && !base::is.null(shared_state$correct_noise_result)) {
        export_data <- shared_state$correct_noise_result
      } else if (isTRUE(input$rename_columns) && !base::is.null(shared_state$rename_result)) {
        export_data <- shared_state$rename_result
      } else {
        export_data <- shared_state$expression_matrix_filtered
      }

      shinyWidgets::updateProgressBar(session, id = "export_progress", value = 45)

      save_path <- base::file.path(shared_state$workdir, "Step3_correct_noise.rda")
      sample_info <- shared_state$sample_info

      correct_noise_result <- export_data %>%
        tibble::column_to_rownames("ID") %>%
        { . * 10000000 }

      shinyWidgets::updateProgressBar(session, id = "export_progress", value = 75)

      base::save(sample_info, correct_noise_result, file = save_path)

      shinyWidgets::updateProgressBar(session, id = "export_progress", value = 100)

      rv$export_success <- TRUE
      shiny::showNotification(paste0("✅ Saved to ", save_path), type = "message")
    })
  })
}
