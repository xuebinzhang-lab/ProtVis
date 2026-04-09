#' Missing Value Processing UI
#'
#' Creates the user interface for the missing value processing module.
#' This module handles data loading, zero-to-NA conversion, group-based NA filtering,
#' mean imputation, and data transformation.
#'
#' @param id The namespace identifier
#' @return A Shiny UI tagList
#' @noRd
missing_value_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        uiOutput(ns("load_status_panel")),
        accordion(
          accordion_panel(
            title = "Step1 ",
            icon = bs_icon("tools"),
            tagList(
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
          accordion_panel(
            title = "Step2 ",
            icon = bs_icon("tools"),
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
          accordion_panel(
            title = "Step3 ",
            icon = bs_icon("tools"),
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
          br(),
          selectInput(
            inputId = ns("data_transformed"),
            label = "Data transformed",
            choices = c("None", "log10", "log2", "Standardization", "Z-Score",
                        "scale", "center", "scale-center"),
            selected = "None"
          )
        ),
        actionButton(ns("export_remove_noise_data"), "export data", class = "btn btn-light fw-bold"),
        uiOutput(ns("export_remove_noise_data_status_panel"))
      ),
      div(
        card(
          card_header("Preview the data processing process"),
          card_body(
            fill = TRUE,
            navset_tab(
              id = ns("Sample Info"),
              bslib::nav_panel("Sample Info", DT::dataTableOutput(ns("tbl_sample_info"))),
              bslib::nav_panel("Expression Matrix", DT::dataTableOutput(ns("tbl_expression_matrix"))),
              bslib::nav_panel("Step 1: 0→NA",DT::dataTableOutput(ns("table_zero_to_na"))),
              bslib::nav_panel("Step 2: >50% NA in group",DT::dataTableOutput(ns("table_group_na"))),
              bslib::nav_panel("Step 3: Impute mean",DT::dataTableOutput(ns("table_imputed"))),
              bslib::nav_panel("Data transformed",DT::dataTableOutput(ns("table_data_transformed")))
            )
          )
        )
      )
    )
  )
}

#' Missing Value Processing Server
#'
#' Server logic for the missing value processing module.
#' Handles data loading, processing steps, and exporting results.
#'
#' @param id The namespace identifier
#' @param shared_state A reactive values list for sharing state between modules
#' @return A reactiveValues object containing processed data
#' @noRd
#' @name missing_value_server
#' @export
missing_value_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shinyjs::disable("zero_to_na")  # Disable the switch initially

    # Reactive values for storing data and processing states
    rv <- reactiveValues(
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
    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step4_select_protein_id.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        if (exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (exists("expression_matrix", envir = e)) {
          rv$expression_matrix <- e$expression_matrix
          rv$expression_matrix_filtered <- e$expression_matrix
          # Initialize step data
          rv$step1_zero_na <- NULL
          rv$step2_group_na <- NULL
          rv$step3_imputed <- NULL
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        showNotification("❌ Step4_select_protein_id.rda not found.", type = "error")
      }
    })

    # Display load status
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # Render sample info table
    output$tbl_sample_info <- DT::renderDT({
      req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE))
    })

    # Render expression matrix table
    output$tbl_expression_matrix <- DT::renderDT({
      req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = list(scrollX = TRUE))
    })

    # Main processing logic
    observe({
      req(rv$expression_matrix_filtered, rv$sample_info)

      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix_filtered

      # Step 1: Merge data
      merged <- expression_matrix %>%
        tibble::column_to_rownames("ID") %>%
        t() %>%
        as.data.frame() %>%
        tibble::rownames_to_column("maxquant_id") %>%
        left_join(sample_info %>% select(sample_id, maxquant_id, group), by = "maxquant_id") %>%
        select(sample_id, group, everything(), -maxquant_id)

      # Step 1: Replace 0 with NA if checked
      if (isTRUE(input$zero_to_na)) {
        merged <- merged %>%
          mutate(across(where(is.numeric), ~na_if(., 0)))

        rv$step1_zero_na <- merged %>%
          dplyr::select(-group) %>%
          tibble::column_to_rownames("sample_id") %>%
          t()
      } else {
        rv$step1_zero_na <- NULL
      }

      # Step 2: Set to NA if >50% missing in group
      if (!is.null(rv$step1_zero_na) && isTRUE(input$filter_half_na)) {
        data_step2 <- rv$step1_zero_na
        groups <- unique(sample_info$group)

        for (grp in groups) {
          group_samples <- sample_info$sample_id[sample_info$group == grp]
          cols_in_group <- intersect(group_samples, colnames(data_step2))

          if (length(cols_in_group) > 0) {
            mat <- data_step2[, cols_in_group, drop = FALSE]
            to_na <- apply(mat, 1, function(x) sum(is.na(x)) > (length(x) / 2))
            data_step2[to_na, cols_in_group] <- NA
          }
        }

        rv$step2_group_na <- data_step2
      } else {
        rv$step2_group_na <- NULL
      }

      # Step 3: Impute with group mean
      if (!is.null(rv$step2_group_na) && isTRUE(input$impute_mean)) {
        data_step3 <- rv$step2_group_na
        groups <- unique(sample_info$group)

        for (grp in groups) {
          group_samples <- sample_info$sample_id[sample_info$group == grp]
          cols_in_group <- intersect(group_samples, colnames(data_step3))

          if (length(cols_in_group) > 0) {
            mat <- data_step3[, cols_in_group, drop = FALSE]
            row_means <- rowMeans(mat, na.rm = TRUE)
            for (i in seq_len(nrow(mat))) {
              for (j in seq_along(cols_in_group)) {
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
    observeEvent(input$export_remove_noise_data, {
      req(shared_state$workdir)

      save_path <- file.path(shared_state$workdir, "Step5_missing_value_processed.rda")

      # Get data to save
      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix
      step1_zero_na <- rv$step1_zero_na
      step2_group_na <- rv$step2_group_na
      step3_imputed <- rv$step3_imputed

      # Save to RDA file
      save(
        sample_info,
        expression_matrix,
        step1_zero_na,
        step2_group_na,
        step3_imputed,
        file = save_path
      )

      showNotification(paste0("✅ Exported to: ", save_path), type = "message")

      # Update UI status
      output$export_remove_noise_data_status_panel <- renderUI({
        span(paste0("✅ Data exported to: ", basename(save_path)), style = "color: green;")
      })
    })

    # Render processing step tables
    output$table_filtered <- DT::renderDT({
      req(rv$expression_matrix_filtered)
      DT::datatable(rv$expression_matrix_filtered, options = list(scrollX = TRUE))
    })
    output$table_zero_to_na <- DT::renderDT({
      req(rv$step1_zero_na)
      DT::datatable(rv$step1_zero_na, options = list(scrollX = TRUE))
    })
    output$table_group_na <- DT::renderDT({
      req(rv$step2_group_na)
      DT::datatable(rv$step2_group_na, options = list(scrollX = TRUE))
    })
    output$table_imputed <- DT::renderDT({
      req(rv$step3_imputed)
      DT::datatable(rv$step3_imputed, options = list(scrollX = TRUE))
    })
    output$table_data_transformed <- DT::renderDT({
      req(rv$step4_transformed)
      DT::datatable(rv$step4_transformed, options = list(scrollX = TRUE))
    })

    # Display processing status
    output$noise_status_panel <- renderUI({
      if (!is.null(rv$step3_imputed)) {
        span("✅ Noise correction completed", style = "color: green;")
      } else {
        span("ℹ️ Noise correction not run", style = "color: gray;")
      }
    })

    # Keep "Replace 0 with NA" switch disabled (prevent toggling off)
    observe({
      shinyjs::disable("zero_to_na")
    })

    return(rv)
  })
}
