#' Missing Value Processing UI
#' Creates the user interface for the missing value processing module.
#' This module handles data loading, zero-to-NA conversion, group-based NA filtering,
#' mean imputation, and data transformation.
#' @param id The namespace identifier
#' @return A Shiny UI tagList
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets switchInput
#' @importFrom bsicons bs_icon
#' @name missing_value_ui
#' @export
#'
missing_value_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        shiny::uiOutput(ns("load_status_panel")),
        bslib::accordion(
          bslib::accordion_panel(
            title = "Step1 ",
            icon = bsicons::bs_icon("tools"),
            shiny::tagList(
              shinyWidgets::switchInput(
                inputId = ns("zero_to_na"),
                label = "Zero → NA",
                value = FALSE,
                onLabel = "✔",
                offLabel = "✘",
                size = "small",
                labelWidth = "120px",
                handleWidth = 60
              ),
              tags$small("Replace 0 with NA")
            )
          ),
          bslib::accordion_panel(
            title = "Step2 ",
            icon = bsicons::bs_icon("tools"),
            tagList(
              shinyWidgets::switchInput(
                inputId = ns("filter_half_na"),
                label = "Group NA",
                value = FALSE,
                onLabel = "✔",
                offLabel = "✘",
                size = "small",
                labelWidth = "120px",
                handleWidth = 60
              ),
              tags$small("Set group to NA if >50% missing in group")
            )
          ),
          bslib::accordion_panel(
            title = "Step3 ",
            icon = bsicons::bs_icon("tools"),
            tagList(
              shinyWidgets::switchInput(
                inputId = ns("impute_mean"),
                label = "Impute",
                value = FALSE,
                onLabel = "✔",
                offLabel = "✘",
                size = "small",
                labelWidth = "120px",
                handleWidth = 60
              ),
              tags$small("Impute remaining missing values with group mean")
            )
          ),
          shiny::br(),
          shiny::selectInput(
            inputId = ns("data_transformed"),
            label = "Data transformed",
            choices = c("None", "log10", "log2", "Standardization", "Z-Score",
                        "scale", "center", "scale-center"),
            selected = "None"
          )
        ),
        shiny::actionButton(ns("export_remove_noise_data"), "export data", class = "btn btn-light fw-bold"),
        shiny::uiOutput(ns("export_remove_noise_data_status_panel"))
      ),
      shiny::div(
        bslib::card(
          bslib::card_header("Preview the data processing process"),
          bslib::card_body(
            fill = TRUE,
            bslib::navset_tab(
              id = ns("Sample Info"),
              header = NULL,
              bslib::nav_panel("Sample Info", DT::DTOutput(ns("tbl_sample_info"))),
              bslib::nav_panel("Expression Matrix", DT::DTOutput(ns("tbl_expression_matrix"))),
              bslib::nav_panel("Step 1: 0→NA",DT::DTOutput(ns("table_zero_to_na"))),
              bslib::nav_panel("Step 2: >50% NA in group",DT::DTOutput(ns("table_group_na"))),
              bslib::nav_panel("Step 3: Impute mean",DT::DTOutput(ns("table_imputed"))),
              bslib::nav_panel("Data transformed",DT::DTOutput(ns("table_data_transformed")))
            )
          )
        )
      )
    )
  )
}

#' Missing Value Processing Server
#' Server logic for the missing value processing module.
#' Handles data loading, processing steps, and exporting results.
#' @param id The namespace identifier
#' @param shared_state A reactive values list for sharing state between modules
#' @return A reactiveValues object containing processed data
#' @import shiny
#' @importFrom shinyjs disable
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom dplyr left_join select everything mutate across where na_if
#' @importFrom DT renderDT datatable
#' @name missing_value_server
#' @export
#'
missing_value_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shinyjs::disable("zero_to_na")  # Disable the switch initially
    # Reactive values for storing data and processing states
    rv <- shiny::reactiveValues(
      expression_matrix = NULL,
      expression_matrix_filtered = NULL,
      sample_info = NULL,
      load_success = FALSE,
      merge_data = NULL,
      step1_zero_na = NULL,
      step2_group_na = NULL,
      step3_imputed = NULL,
      step4_transformed = NULL
    )
    # Load data when button is clicked
    shiny::observeEvent(input$load_data, {
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        rv$sample_info <- shared_state$dataset$sample_info
        rv$expression_matrix <- protvis_expression_matrix(shared_state$dataset)
        rv$expression_matrix_filtered <- rv$expression_matrix
        rv$step1_zero_na <- NULL
        rv$step2_group_na <- NULL
        rv$step3_imputed <- NULL
        rv$step4_transformed <- NULL
        rv$load_success <- TRUE
        shiny::showNotification(
          "✅ ProtVis_dataset loaded successfully.", type = "message"
        )
        return(invisible(NULL))
      }
      shiny::req(shared_state$workdir)
      rda_path <- base::file.path(shared_state$workdir, "Step4_select_protein_id.rda")
      if (base::file.exists(rda_path)) {
        dataset <- .protvis_load_stage_dataset(
          rda_path, expression_names = "expression_matrix"
        )
        if (!base::is.null(dataset)) {
          shared_state$dataset <- dataset
          rv$sample_info <- dataset$sample_info
          rv$expression_matrix <- protvis_expression_matrix(dataset)
          rv$expression_matrix_filtered <- rv$expression_matrix
          # Initialize step data
          rv$step1_zero_na <- NULL
          rv$step2_group_na <- NULL
          rv$step3_imputed <- NULL
        }
        rv$load_success <- !base::is.null(dataset)
        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("❌ Step4_select_protein_id.rda not found.", type = "error")
      }
    })
    # Display load status
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    # Render sample info table
    output$tbl_sample_info <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE))
    })
    # Render expression matrix table
    output$tbl_expression_matrix <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = base::list(scrollX = TRUE))
    })
    # Main processing logic
    shiny::observe({
      shiny::req(rv$expression_matrix_filtered, rv$sample_info)
      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix_filtered

      # Step 1: Merge data
      merged <- .protvis_column_to_rownames(expression_matrix, "ID") %>%
        base::t() %>%
        base::as.data.frame() %>%
        .protvis_rownames_to_column("maxquant_id") %>%
        dplyr::left_join(sample_info %>% dplyr::select(sample_id, maxquant_id, group), by = "maxquant_id") %>%
        dplyr::select(sample_id, group, dplyr::everything(), -maxquant_id)
      # Step 1: Replace 0 with NA if checked
      if (isTRUE(input$zero_to_na)) {
        merged <- merged %>%
          dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~na_if(., 0)))
        rv$step1_zero_na <- merged %>%
          dplyr::select(-group) %>%
          .protvis_column_to_rownames("sample_id") %>%
          base::t()
      } else {
        rv$step1_zero_na <- NULL
      }
      # Step 2: Set to NA if >50% missing in group
      if (!is.null(rv$step1_zero_na) && isTRUE(input$filter_half_na)) {
        data_step2 <- rv$step1_zero_na
        groups <- base::unique(sample_info$group)
        for (grp in groups) {
          group_samples <- sample_info$sample_id[sample_info$group == grp]
          cols_in_group <- base::intersect(group_samples, colnames(data_step2))
          mat <- data_step2[, cols_in_group, drop = FALSE]
          to_na <- base::apply(mat, 1, function(x) base::sum(is.na(x)) > (base::length(x) / 2))
          data_step2[to_na, cols_in_group] <- NA
        }
        rv$step2_group_na <- data_step2
      } else {  # 这里是修复点：原代码少了 {，导致 else 报错
        rv$step2_group_na <- NULL
      }
      # Step 3: Impute with group mean
      if (!is.null(rv$step2_group_na) && isTRUE(input$impute_mean)) {
        data_step3 <- rv$step2_group_na
        groups <- base::unique(sample_info$group)
        for (grp in groups) {
          group_samples <- sample_info$sample_id[sample_info$group == grp]
          cols_in_group <- base::intersect(group_samples, base::colnames(data_step3))
          if (base::length(cols_in_group) > 0) {
            mat <- data_step3[, cols_in_group, drop = FALSE]
            row_means <- base::rowMeans(mat, na.rm = TRUE)
            for (i in base::seq_len(base::nrow(mat))) {
              for (j in base::seq_along(cols_in_group)) {
                if (is.na(mat[i, j])) {
                  mat[i, j] <- row_means[i]
                }
              }
            }
            data_step3[, cols_in_group] <- mat
          }
        }
        rv$step3_imputed <- data_step3
      } else {
        rv$step3_imputed <- NULL
      }
      # Step 4: Data transformation
      if (!is.null(rv$step3_imputed) && input$data_transformed == "None" ) {
        rv$step4_transformed <- NULL
      } else if (!is.null(rv$step3_imputed) && input$data_transformed == "log10") {
        rv$step4_transformed <- log10(rv$step3_imputed)
      } else if (!is.null(rv$step3_imputed) && input$data_transformed == "log2") {
        rv$step4_transformed <- log2(rv$step3_imputed)
      } else if (!is.null(rv$step3_imputed) && input$data_transformed == "Standardization") {
        rv$step4_transformed <- log10(rv$step3_imputed)
      } else if (!is.null(rv$step3_imputed) && input$data_transformed == "Z-Score") {
        rv$step4_transformed <- log10(rv$step3_imputed)
      } else if (!is.null(rv$step3_imputed) && input$data_transformed == "scale") {
        rv$step4_transformed <- scale(rv$step3_imputed, scale = TRUE, center = FALSE)
      } else if (!is.null(rv$step3_imputed) && input$data_transformed == "center") {
        rv$step4_transformed <- scale(rv$step3_imputed, scale = FALSE, center = TRUE)
      } else if (!is.null(rv$step3_imputed) && input$data_transformed == "scale-center") {
        rv$step4_transformed <- scale(rv$step3_imputed, scale = TRUE, center = TRUE)
      }
      # Save final merged data for later use
      rv$merge_data <- merged
    })
    # Export processed data
    shiny::observeEvent(input$export_remove_noise_data, {
      shiny::req(shared_state$workdir)
      save_path <- base::file.path(shared_state$workdir, "Step5_missing_value_processed.rda")
      processed <- rv$step4_transformed %||% rv$step3_imputed %||%
        rv$step2_group_na %||% rv$step1_zero_na %||% rv$expression_matrix
      dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        shared_state$dataset
      } else {
        create_protvis_dataset(
          rv$expression_matrix, sample_info = rv$sample_info
        )
      }
      dataset <- .protvis_update_expression(dataset, processed)
      dataset <- .protvis_new_analysis_dataset(
        dataset, "missing_value_processing",
        list(method = input$data_transformed %||% "none")
      )
      dataset$analysis_results$missing_value_processing <- list(
        status = "success",
        zero_to_na = isTRUE(input$zero_to_na),
        filter_half_na = isTRUE(input$filter_half_na),
        impute_mean = isTRUE(input$impute_mean),
        transformation = input$data_transformed %||% "none"
      )
      dataset <- .protvis_append_process(
        dataset, "missing_value_processing", status = "success",
        parameters = dataset$analysis_results$missing_value_processing
      )
      .protvis_ui_sync_state(dataset, shared_state)
      .protvis_save_stage_dataset(dataset, save_path)
      shiny::showNotification(paste0("✅ Exported to: ", save_path), type = "message")
      # Update UI status
      output$export_remove_noise_data_status_panel <- shiny::renderUI({
        shiny::span(base::paste0("✅ Data exported to: ", base::basename(save_path)), style = "color: green;")
      })
    })
    # Render processing step tables
    output$table_filtered <- DT::renderDT({
      shiny::req(rv$expression_matrix_filtered)
      DT::datatable(rv$expression_matrix_filtered, options = base::list(scrollX = TRUE))
    })
    output$table_zero_to_na <- DT::renderDT({
      shiny::req(rv$step1_zero_na)
      DT::datatable(rv$step1_zero_na, options = base::list(scrollX = TRUE))
    })
    output$table_group_na <- DT::renderDT({
      shiny::req(rv$step2_group_na)
      DT::datatable(rv$step2_group_na, options = base::list(scrollX = TRUE))
    })
    output$table_imputed <- DT::renderDT({
      shiny::req(rv$step3_imputed)
      DT::datatable(rv$step3_imputed, options = base::list(scrollX = TRUE))
    })
    output$table_data_transformed <- DT::renderDT({
      shiny::req(rv$step4_transformed)
      DT::datatable(rv$step4_transformed, options = base::list(scrollX = TRUE))
    })
    # Display processing status
    output$noise_status_panel <- shiny::renderUI({
      if (!is.null(rv$step3_imputed)) {
        shiny::span("✅ Noise correction completed", style = "color: green;")
      } else {
        shiny::span("ℹ️ Noise correction not run", style = "color: gray;")
      }
    })
    # Keep "Replace 0 with NA" switch disabled (prevent toggling off)
    shiny::observe({
      shinyjs::disable("zero_to_na")
    })

    return(rv)
  })
}
