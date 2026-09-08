#' Data Imputation UI Module
#'
#' Creates the user interface for the data imputation module.
#' The module supports:
#' - loading transformed data from the previous step
#' - viewing sample information and expression matrix
#' - visualizing missing-value patterns
#' - selecting an imputation method
#' - running imputation
#' - downloading missing-value visualization plots
#'
#' @param id Character string. Module ID used for namespacing the UI elements.
#'
#' @return A Shiny UI object containing the complete layout for the
#'   data imputation module.
#'
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @name data_imputation_ui
#' @export
data_imputation_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
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

        shiny::actionButton(
          ns("visualize_missing_values"),
          "Visualize missing values",
          class = "btn btn-outline-primary fw-bold pv-load-button"
        ),

        shiny::uiOutput(ns("load_status_panel")),

        bslib::accordion(
          bslib::accordion_panel(
            title = "Imputation Settings",
            icon = bsicons::bs_icon("tools"),
            shiny::selectInput(
              ns("choice_method"),
              "Method",
              choices = c("kNN", "RF", "Mean", "Median", "Zero", "Minimum"),
              selected = "kNN"
            ),
            shiny::numericInput(
              ns("minprob_q"),
              "q for MinProb",
              value = 0.01,
              min = 0,
              max = 0.05,
              step = 0.005
            ),
            shiny::actionButton(
              ns("run_impute"),
              "Run imputation",
              class = "btn btn-success fw-bold pv-load-button"
            )
          ),
          bslib::accordion_panel(
            title = "Download",
            icon = bsicons::bs_icon("download"),
            shiny::numericInput(
              ns("img_height"),
              "Height (inches):",
              value = 5,
              step = 1
            ),
            shiny::numericInput(
              ns("img_width"),
              "Width (inches):",
              value = 5,
              step = 1
            ),
            shiny::downloadButton(
              ns("downloadOriginalPlot"),
              "Download original plot",
              class = "btn btn-outline-primary"
            ),
            shiny::downloadButton(
              ns("downloadImputedPlot"),
              "Download imputed plot",
              class = "btn btn-outline-primary"
            )
          )
        )
      ),

      shiny::tabsetPanel(
        id = ns("tabs"),

        shiny::tabPanel(
          title = "Sample Info",
          DT::DTOutput(ns("sample_info"))
        ),

        shiny::tabPanel(
          title = "Expression Matrix",
          DT::DTOutput(ns("expression_matrix"))
        ),

        shiny::tabPanel(
          title = "Visualize missing values",
          bslib::layout_column_wrap(
            width = 1 / 2,
            height = 600,

            bslib::card(
              class = "pv-preview-card",
              height = "800px",
              bslib::card_header("Original Data"),
              bslib::card_body(
                DT::DTOutput(ns("originalData"))
              )
            ),

            bslib::card(
              class = "pv-preview-card",
              height = "800px",
              bslib::card_header("Original Data Visualization"),
              bslib::card_body(
                shiny::plotOutput(ns("originalPlot"))
              )
            ),

            bslib::card(
              class = "pv-preview-card",
              height = "800px",
              bslib::card_header("Imputed Data"),
              bslib::card_body(
                DT::DTOutput(ns("imputedData"))
              )
            ),

            bslib::card(
              class = "pv-preview-card",
              height = "800px",
              bslib::card_header("Imputed Data Visualization"),
              bslib::card_body(
                shiny::plotOutput(ns("imputedPlot"))
              )
            )
          )
        )
      )
    )
  )
}



#' Data Imputation Server Module
#'
#' Server logic for the data imputation module. This module:
#' - loads transformed expression data from `Step4_data_transformed.rda`
#' - displays sample information and transformed expression matrix
#' - visualizes missing-value patterns in the original data
#' - performs imputation using the selected method
#' - saves imputed results to `Step5_data_imputation.rda`
#' - allows downloading of missing-value visualization plots
#'
#' Before imputation, the function preprocesses the expression matrix by:
#' - separating the `ID` column from numeric expression values
#' - coercing all measurement columns to numeric
#' - converting `NaN`, `Inf`, and `-Inf` to `NA`
#' - removing rows or columns that are entirely missing
#'
#' @param id Character string. Module ID used for namespacing server outputs.
#' @param shared_state A `reactiveValues` object shared across modules. It must
#'   contain at least `workdir`, which points to the current project directory.
#'
#' @return No direct return value. This function creates Shiny server-side
#'   outputs, reactive objects, and saves processed `.rda` files.
#'
#' @import shiny
#' @importFrom DT renderDT datatable
#' @importFrom visdat vis_dat
#' @importFrom ggplot2 scale_fill_manual ggsave
#' @importFrom dplyr mutate across everything
#' @importFrom impute impute.knn
#' @importFrom missForest missForest
#' @name data_imputation_server
#' @export
data_imputation_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE
    )

    clean_missing_sentinels <- function(data) {
      df <- base::as.data.frame(
        data, stringsAsFactors = FALSE, check.names = FALSE
      )
      sample_cols <- base::setdiff(base::colnames(df), "ID")
      for (column in sample_cols) {
        values <- suppressWarnings(base::as.numeric(
          base::as.character(df[[column]])
        ))
        # Raw MaxQuant exports use both 0 and -8 for not-observed values.
        # Keep them as true NA until the user explicitly runs imputation.
        df[[column]][!is.na(values) & values %in% c(0, -8)] <- NA_real_
      }
      base::rownames(df) <- if ("ID" %in% base::colnames(df)) {
        as.character(df$ID)
      } else {
        base::rownames(df)
      }
      df
    }

    shiny::observeEvent(input$load_data, {
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        rv$sample_info <- shared_state$dataset$sample_info
        rv$expression_matrix <- clean_missing_sentinels(
          protvis_expression_matrix(shared_state$dataset)
        )
        rv$load_success <- TRUE
        shiny::showNotification(
          "✅ ProtVis_dataset loaded successfully.", type = "message"
        )
        return(invisible(NULL))
      }
      shiny::req(shared_state$workdir)

      rda_path <- base::file.path(
        shared_state$workdir,
        "Step4_data_transformed.rda"
      )

      if (base::file.exists(rda_path)) {
        dataset <- .protvis_load_stage_dataset(
          rda_path, expression_names = "transformed"
        )
        if (!base::is.null(dataset)) {
          shared_state$dataset <- dataset
          rv$sample_info <- dataset$sample_info
          rv$expression_matrix <- protvis_expression_matrix(dataset)
          rv$expression_matrix <- clean_missing_sentinels(
            rv$expression_matrix
          )
        } else {
          rv$expression_matrix <- NULL
          shiny::showNotification(
            "⚠️ transformed not found in Step4_data_transformed.rda.",
            type = "warning"
          )
        }

        rv$load_success <- !base::is.null(dataset) &&
          !base::is.null(rv$expression_matrix)

        if (isTRUE(rv$load_success)) {
          shiny::showNotification(
            "✅ Data loaded successfully.",
            type = "message"
          )
        }
      } else {
        rv$load_success <- FALSE
        shiny::showNotification(
          "❌ Step4_data_transformed.rda not found.",
          type = "error"
        )
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (isTRUE(rv$load_success)) {
        shiny::div(class = "pv-status pv-status-ready", "✓ Data loaded")
      } else {
        shiny::div(class = "pv-status pv-status-empty", "× Data not loaded")
      }
    })

    output$sample_info <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(
        rv$sample_info,
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = FALSE
      )
    })

    expression_matrix_display <- shiny::reactive({
      shiny::req(rv$expression_matrix)

      df <- base::as.data.frame(
        rv$expression_matrix,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      df <- clean_missing_sentinels(df)

      if ("ID" %in% base::colnames(df)) {
        ids <- df$ID
        df <- df[, base::setdiff(base::colnames(df), "ID"), drop = FALSE]
        base::rownames(df) <- ids
      }

      df
    })

    output$expression_matrix <- DT::renderDT({
      shiny::req(expression_matrix_display())
      DT::datatable(
        expression_matrix_display(),
        options = list(scrollX = TRUE, pageLength = 10),
        rownames = TRUE
      )
    })

    shiny::observeEvent(input$visualize_missing_values, {
      output$originalData <- DT::renderDT({
        shiny::req(expression_matrix_display())
        DT::datatable(
          expression_matrix_display(),
          options = list(pageLength = 10, scrollX = TRUE),
          rownames = TRUE
        )
      })

      output$originalPlot <- shiny::renderPlot({
        shiny::req(expression_matrix_display())

        plot_df <- .protvis_rownames_to_column(expression_matrix_display(), "ID")

        visdat::vis_dat(base::data.frame(plot_df)) +
          ggplot2::scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )
      })
    })

    prepare_imputation_data <- shiny::reactive({
      shiny::req(rv$expression_matrix)

      df <- base::as.data.frame(
        rv$expression_matrix,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if ("ID" %in% base::colnames(df)) {
        id_col <- df$ID
        df_num <- df[, base::setdiff(base::colnames(df), "ID"), drop = FALSE]
      } else {
        id_col <- base::rownames(df)
        df_num <- df
      }

      df_num <- base::as.data.frame(
        base::lapply(df_num, function(x) {
          base::as.numeric(base::as.character(x))
        }),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (!base::is.null(id_col) && length(id_col) == base::nrow(df_num)) {
        base::rownames(df_num) <- id_col
      }

      df_num[] <- base::lapply(df_num, function(x) {
        x[base::is.nan(x) | base::is.infinite(x)] <- NA
        x
      })

      all_na_cols <- vapply(
        df_num,
        function(x) base::all(base::is.na(x)),
        logical(1)
      )
      if (base::any(all_na_cols)) {
        df_num <- df_num[, !all_na_cols, drop = FALSE]
      }

      all_na_rows <- apply(
        df_num,
        1,
        function(x) base::all(base::is.na(x))
      )
      df_num <- df_num[!all_na_rows, , drop = FALSE]

      if (!base::is.null(id_col)) {
        id_col <- id_col[!all_na_rows]
      }

      list(
        id = id_col,
        data = df_num
      )
    })

    imputed_data <- shiny::eventReactive(input$run_impute, {
      prep <- prepare_imputation_data()
      df_num <- prep$data
      id_col <- prep$id
      method <- input$choice_method

      shiny::req(base::nrow(df_num) > 0, base::ncol(df_num) > 0)

      na_ratio <- apply(df_num, 1, function(x) mean(base::is.na(x)))
      high_missing_n <- sum(na_ratio > 0.5, na.rm = TRUE)

      if (high_missing_n > 0) {
        shiny::showNotification(
          paste0(
            "⚠️ ",
            high_missing_n,
            " rows have >50% missing values. kNN may be unstable for these rows."
          ),
          type = "warning",
          duration = 6
        )
      }

      set.seed(12345)

      result <- tryCatch({
        if (method == "kNN") {
          base::as.data.frame(impute::impute.knn(base::as.matrix(df_num))$data)

        } else if (method == "RF") {
          base::as.data.frame(missForest::missForest(df_num)$ximp)

        } else if (method == "Mean") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(base::is.na(.), mean(., na.rm = TRUE), .)
              )
            )

        } else if (method == "Median") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(base::is.na(.), stats::median(., na.rm = TRUE), .)
              )
            )

        } else if (method == "Zero") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(base::is.na(.), 0, .)
              )
            )

        } else if (method == "Minimum") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(base::is.na(.), min(., na.rm = TRUE), .)
              )
            )

        } else {
          df_num
        }
      }, error = function(e) {
        shiny::showNotification(
          paste0("❌ Imputation failed: ", e$message),
          type = "error",
          duration = 8
        )
        return(NULL)
      })

      shiny::req(!base::is.null(result))

      result <- base::as.data.frame(
        result,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      if (!base::is.null(id_col) && length(id_col) == base::nrow(result)) {
        base::rownames(result) <- id_col
      }

      result
    })

    output$imputedData <- DT::renderDT({
      shiny::req(imputed_data())
      DT::datatable(
        imputed_data(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = TRUE
      )
    })

    shiny::observeEvent(input$run_impute, {
      shiny::req(imputed_data(), rv$sample_info, shared_state$workdir)

      sample_info <- rv$sample_info
      imputed_df <- base::as.data.frame(
        imputed_data(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        shared_state$dataset
      } else {
        create_protvis_dataset(
          rv$expression_matrix,
          sample_info = sample_info
        )
      }
      dataset <- .protvis_update_expression(dataset, imputed_df)
      dataset <- .protvis_new_analysis_dataset(
        dataset, "imputation", list(method = input$choice_method)
      )
      dataset$analysis_results$imputation <- list(
        status = "success", method = input$choice_method
      )
      dataset <- .protvis_append_process(
        dataset, "imputation", status = "success",
        parameters = list(method = input$choice_method)
      )
      .protvis_ui_sync_state(dataset, shared_state)
      .protvis_save_stage_dataset(
        dataset,
        base::file.path(
          shared_state$workdir, "Step5_data_imputation.rda"
        )
      )

      shiny::showNotification(
        "✅ Step5_data_imputation.rda saved",
        type = "message"
      )
    })

    output$imputedPlot <- shiny::renderPlot({
      shiny::req(imputed_data())

      plot_df <- imputed_data()
      plot_df <- .protvis_rownames_to_column(plot_df, "ID")

      visdat::vis_dat(base::data.frame(plot_df)) +
        ggplot2::scale_fill_manual(
          values = c(
            "character" = "skyblue",
            "factor" = "lightgreen",
            "numeric" = "#E0F3F8",
            "logical" = "lightyellow",
            "NA" = "#BEBEBE"
          )
        )
    })

    output$downloadOriginalPlot <- shiny::downloadHandler(
      filename = function() {
        base::paste0("original_data_plot_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        plot_df <- expression_matrix_display() %>%
          .protvis_rownames_to_column("ID")

        g <- visdat::vis_dat(base::data.frame(plot_df)) +
          ggplot2::scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )

        ggplot2::ggsave(
          filename = file,
          plot = g,
          width = input$img_width,
          height = input$img_height,
          units = "in"
        )
      }
    )

    output$downloadImputedPlot <- shiny::downloadHandler(
      filename = function() {
        base::paste0("imputed_data_plot_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        shiny::req(imputed_data())

        plot_df <- imputed_data()
        plot_df <- .protvis_rownames_to_column(plot_df, "ID")

        g <- visdat::vis_dat(base::data.frame(plot_df)) +
          ggplot2::scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )

        ggplot2::ggsave(
          filename = file,
          plot = g,
          width = input$img_width,
          height = input$img_height,
          units = "in"
        )
      }
    )
  })
}
