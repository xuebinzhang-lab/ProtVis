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
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,

        shiny::actionButton(
          ns("load_data"),
          "LOAD DATA",
          class = "btn btn-light fw-bold"
        ),

        shiny::actionButton(
          ns("visualize_missing_values"),
          "Visualize missing values",
          class = "btn btn-light fw-bold"
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
              selected = "Mean"
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
              "Run Imputation",
              class = "btn btn-light fw-bold"
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
              "Download Original Plot"
            ),
            shiny::downloadButton(
              ns("downloadImputedPlot"),
              "Download Imputed Plot"
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
              height = "800px",
              bslib::card_header("Original Data"),
              bslib::card_body(
                DT::DTOutput(ns("originalData"))
              )
            ),

            bslib::card(
              height = "800px",
              bslib::card_header("Original Data Visualization"),
              bslib::card_body(
                shiny::plotOutput(ns("originalPlot"))
              )
            ),

            bslib::card(
              height = "800px",
              bslib::card_header("Imputed Data"),
              bslib::card_body(
                DT::DTOutput(ns("imputedData"))
              )
            ),

            bslib::card(
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

    # Load transformed data
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)

      rda_path <- base::file.path(
        shared_state$workdir,
        "Step4_data_transformed.rda"
      )

      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        base::load(rda_path, envir = e)

        if (base::exists("sample_info", envir = e)) {
          rv$sample_info <- e$sample_info
        }

        if (base::exists("transformed", envir = e)) {
          rv$expression_matrix <- e$transformed
        } else {
          rv$expression_matrix <- NULL
          shiny::showNotification(
            "⚠️ transformed not found in Step4_data_transformed.rda.",
            type = "warning"
          )
        }

        rv$load_success <- TRUE
        shiny::showNotification(
          "✅ Data loaded successfully.",
          type = "message"
        )
      } else {
        rv$load_success <- FALSE
        shiny::showNotification(
          "❌ Step4_data_transformed.rda not found.",
          type = "error"
        )
      }
    })

    # Load status
    output$load_status_panel <- shiny::renderUI({
      if (isTRUE(rv$load_success)) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })

    # Sample info table
    output$sample_info <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(
        rv$sample_info,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    # Expression matrix table
    output$expression_matrix <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      DT::datatable(
        rv$expression_matrix,
        options = list(scrollX = TRUE, pageLength = 10)
      )
    })

    # Visualize missing values
    shiny::observeEvent(input$visualize_missing_values, {
      output$originalData <- DT::renderDT({
        shiny::req(rv$expression_matrix)
        DT::datatable(
          rv$expression_matrix,
          options = list(pageLength = 10, scrollX = TRUE)
        )
      })

      output$originalPlot <- shiny::renderPlot({
        shiny::req(rv$expression_matrix)

        visdat::vis_dat(data.frame(rv$expression_matrix)) +
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

    # Prepare numeric data for imputation
    prepare_imputation_data <- shiny::reactive({
      shiny::req(rv$expression_matrix)

      df <- base::as.data.frame(
        rv$expression_matrix,
        stringsAsFactors = FALSE
      )

      # Separate ID column
      if ("ID" %in% base::colnames(df)) {
        id_col <- df$ID
        df_num <- df[, base::setdiff(base::colnames(df), "ID"), drop = FALSE]
      } else {
        id_col <- NULL
        df_num <- df
      }

      # Convert all columns to numeric
      df_num <- base::as.data.frame(
        base::lapply(df_num, function(x) {
          base::as.numeric(as.character(x))
        })
      )

      # Convert NaN / Inf / -Inf to NA
      df_num[] <- base::lapply(df_num, function(x) {
        x[base::is.nan(x) | base::is.infinite(x)] <- NA
        x
      })

      # Remove columns that are completely NA
      all_na_cols <- vapply(
        df_num,
        function(x) base::all(base::is.na(x)),
        logical(1)
      )
      if (base::any(all_na_cols)) {
        df_num <- df_num[, !all_na_cols, drop = FALSE]
      }

      # Remove rows that are completely NA
      all_na_rows <- apply(
        df_num,
        1,
        function(x) base::all(base::is.na(x))
      )
      df_num <- df_num[!all_na_rows, , drop = FALSE]

      if (!is.null(id_col)) {
        id_col <- id_col[!all_na_rows]
      }

      list(
        id = id_col,
        data = df_num
      )
    })

    # Run imputation
    imputed_data <- shiny::eventReactive(input$run_impute, {
      prep <- prepare_imputation_data()
      df_num <- prep$data
      id_col <- prep$id
      method <- input$choice_method

      shiny::req(nrow(df_num) > 0, ncol(df_num) > 0)

      # Warning for high-missing rows
      na_ratio <- apply(df_num, 1, function(x) mean(is.na(x)))
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
          as.data.frame(impute::impute.knn(as.matrix(df_num))$data)

        } else if (method == "RF") {
          as.data.frame(missForest::missForest(df_num)$ximp)

        } else if (method == "Mean") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
              )
            )

        } else if (method == "Median") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
              )
            )

        } else if (method == "Zero") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(is.na(.), 0, .)
              )
            )

        } else if (method == "Minimum") {
          df_num %>%
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ ifelse(is.na(.), min(., na.rm = TRUE), .)
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

      shiny::req(!is.null(result))

      # Add ID column back
      if (!is.null(id_col)) {
        result <- cbind(ID = id_col, result)
      }

      result
    })

    # Save imputed data
    shiny::observeEvent(input$run_impute, {
      shiny::req(imputed_data(), rv$sample_info, shared_state$workdir)

      sample_info <- rv$sample_info
      imputed_df <- base::as.data.frame(imputed_data())

      base::save(
        sample_info,
        imputed_df,
        file = base::file.path(
          shared_state$workdir,
          "Step5_data_imputation.rda"
        )
      )

      shiny::showNotification(
        "✅ Step5_data_imputation.rda saved",
        type = "message"
      )
    })

    # Imputed data table
    output$imputedData <- DT::renderDT({
      shiny::req(imputed_data())
      DT::datatable(
        imputed_data(),
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    # Imputed data visualization
    output$imputedPlot <- shiny::renderPlot({
      shiny::req(imputed_data())

      visdat::vis_dat(data.frame(imputed_data())) +
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

    # Download original plot
    output$downloadOriginalPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("original_data_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        g <- visdat::vis_dat(data.frame(rv$expression_matrix)) +
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
          file,
          plot = g,
          width = input$img_width,
          height = input$img_height,
          units = "in"
        )
      }
    )

    # Download imputed plot
    output$downloadImputedPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("imputed_data_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        shiny::req(imputed_data())

        g <- visdat::vis_dat(data.frame(imputed_data())) +
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
          file,
          plot = g,
          width = input$img_width,
          height = input$img_height,
          units = "in"
        )
      }
    )
  })
}
