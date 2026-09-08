# Shiny interface for the canonical ProtVis_dataset workflow.

.protvis_dataset_style <- function() {
  shiny::tags$style(shiny::HTML("
    .protvis-dataset-shell { padding: 1rem 0 2rem; }
    .protvis-dataset-shell .card { border: 1px solid #dbe8f3; }
    .protvis-dataset-shell .card-header { font-weight: 750; }
    .protvis-dataset-status { min-height: 2.4rem; padding: .65rem .8rem;
      border-radius: .7rem; background: #eef8ff; color: #1f4e6d;
      margin: .7rem 0; }
    .protvis-dataset-error { white-space: pre-wrap; color: #9b2226;
      background: #fff3f3; border: 1px solid #f4b7b7; padding: .8rem;
      border-radius: .65rem; }
    .protvis-dataset-help { color: #607080; font-size: .88rem; }
  "))
}

#' ProtVis_dataset workflow UI.
#' @param id Shiny module ID.
#' @return Shiny UI.
#' @export
protvis_dataset_ui <- function(id) {
  ns <- shiny::NS(id)
  sources <- protvis_supported_sources()
  builtin <- protvis_builtin_datasets()
  stages <- protvis_stage_labels()
  stage_choices <- stats::setNames(names(stages), unname(stages))
  shiny::tagList(
    .protvis_dataset_style(),
    shiny::div(
      class = "protvis-dataset-shell",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 350,
          shiny::h4("ProtVis_dataset"),
          shiny::p(paste0(
            "A standardized, reproducible object for import, QC, analysis, ",
            "checkpoints, and portable export."
          ),
            class = "protvis-dataset-help"
          ),
          shiny::selectInput(
            ns("source"), "Data source",
            choices = stats::setNames(sources$source, sources$source),
            selected = "MaxQuant"
          ),
          shiny::actionButton(
            ns("load_builtin"), "Load selected built-in example",
            class = "btn btn-primary w-100"
          ),
          shiny::selectInput(
            ns("builtin_source"), "Built-in example",
            choices = stats::setNames(
              builtin$file,
              paste0(builtin$source, " — ", builtin$file)
            ),
            selected = "Maxquant_Export.xlsx"
          ),
          shiny::fileInput(
            ns("data_file"), "Upload source table",
            accept = c(".xlsx", ".xls", ".txt", ".tsv", ".csv", ".parquet",
                       ".mzTab", ".mztab")
          ),
          shiny::fileInput(
            ns("sample_file"), "Upload sample_info (optional)",
            accept = c(".xlsx", ".xls", ".txt", ".tsv", ".csv")
          ),
          shiny::actionButton(
            ns("load_file"), "Import selected source",
            class = "btn btn-outline-primary w-100"
          ),
          shiny::hr(),
          shiny::h5("Processing node"),
          shiny::selectInput(ns("stage"), "Stage", choices = stage_choices),
          shiny::selectInput(
            ns("transform_method"), "Transformation",
            choices = c("log2", "log10", "ln", "none"), selected = "log2"
          ),
          shiny::selectInput(
            ns("imputation_method"), "Imputation",
            choices = c("median", "min", "knn", "zero", "none"),
            selected = "median"
          ),
          shiny::selectInput(
            ns("normalization_method"), "Normalization",
            choices = c("median", "mean", "quantile", "zscore", "none"),
            selected = "median"
          ),
          shiny::numericInput(
            ns("max_missing"), "Maximum missing fraction",
            value = 0.5, min = 0, max = 0.99, step = 0.05
          ),
          shiny::numericInput(
            ns("fdr"), "FDR threshold", value = 0.05, min = 0.0001,
            max = 1, step = 0.01
          ),
          shiny::textInput(
            ns("checkpoint_dir"), "Output directory",
            value = "", placeholder = "Optional; defaults to R getwd()"
          ),
          shiny::fileInput(
            ns("checkpoint_upload"), "Restore uploaded checkpoint (.rds)",
            accept = ".rds"
          ),
          shiny::actionButton(ns("restore"), "Restore latest checkpoint",
                              class = "btn btn-outline-secondary w-100"),
          shiny::actionButton(ns("save_checkpoint"), "Save current checkpoint",
                              class = "btn btn-outline-secondary w-100"),
          shiny::hr(),
          shiny::actionButton(ns("run_step"), "Run selected node",
                              class = "btn btn-success w-100"),
          shiny::actionButton(ns("run_all"), "Run full pipeline",
                              class = "btn btn-success w-100"),
          shiny::actionButton(ns("retry"), "Retry failed node",
                              class = "btn btn-outline-warning w-100"),
          shiny::actionButton(ns("rerun"), "Re-run downstream from node",
                              class = "btn btn-outline-warning w-100"),
          shiny::hr(),
          shiny::p(
            "Every loaded or processed dataset is automatically saved to the output directory.",
            class = "protvis-dataset-help"
          )
        ),
        bslib::card(
          bslib::card_header(
            shiny::div(
              shiny::h3("Unified dataset and recoverable workflow"),
              shiny::p(paste0(
                "Each successful node appends provenance and can be resumed or ",
                "rerun downstream with new parameters."
              )
              )
            )
          ),
          bslib::card_body(
            shiny::uiOutput(ns("status")),
            shiny::uiOutput(ns("error_details")),
            bslib::navset_tab(
              bslib::nav_panel("Expression data", DT::DTOutput(ns("expression"))),
              bslib::nav_panel("Sample info", DT::DTOutput(ns("samples"))),
              bslib::nav_panel("Variable info", DT::DTOutput(ns("variables"))),
              bslib::nav_panel("Process history", DT::DTOutput(ns("process"))),
              bslib::nav_panel("Results", DT::DTOutput(ns("results"))),
              bslib::nav_panel("Checkpoints", DT::DTOutput(ns("checkpoints"))),
              bslib::nav_panel("PCA / overview", shiny::plotOutput(ns("pca")))
            )
          )
        )
      )
    )
  )
}

.protvis_ui_dataset_parameters <- function(input, stage) {
  switch(
    stage,
    noise_correction = list(max_missing = as.numeric(input$max_missing %||% 0.5)),
    transformation = list(method = as.character(input$transform_method %||% "log2")),
    imputation = list(method = as.character(input$imputation_method %||% "median"),
                      k = 10L),
    normalization = list(method = as.character(
      input$normalization_method %||% "median"
    )),
    differential_analysis = list(fdr = as.numeric(input$fdr %||% 0.05)),
    list()
  )
}

.protvis_ui_checkpoint_dir <- function(input, shared_state) {
  candidate <- input$checkpoint_dir
  if (is.null(candidate) || !nzchar(trimws(as.character(candidate)))) {
    candidate <- shared_state$workdir %||% getwd()
  }
  protvis_output_directory(candidate)
}

.protvis_ui_sync_state <- function(dataset, shared_state) {
  if (is.null(shared_state)) return(invisible(NULL))
  shared_state$dataset <- dataset
  shared_state$sample_info <- dataset$sample_info
  shared_state$expression_matrix <- protvis_expression_matrix(dataset)
  shared_state$expression_matrix_filtered <- protvis_expression_matrix(dataset)
  shared_state$dataset_name <- protvis_dataset_name(dataset)
  history <- shared_state$dataset_history %||% list()
  history[[length(history) + 1L]] <- dataset
  shared_state$dataset_history <- history
  invisible(NULL)
}

.protvis_ui_save_legacy <- function(dataset, shared_state) {
  if (is.null(shared_state) || is.null(shared_state$workdir) ||
      !dir.exists(shared_state$workdir)) return(invisible(NULL))
  directory <- shared_state$workdir
  sample_info <- dataset$sample_info
  expression_matrix <- protvis_expression_matrix(dataset)
  expression_matrix_filtered <- expression_matrix
  data_source <- dataset$metadata$source %||% "ProtVis_dataset"
  tryCatch({
    save(sample_info, expression_matrix, data_source,
         file = file.path(directory, "Step1_project_init.rda"))
    save(sample_info, expression_matrix, expression_matrix_filtered,
         file = file.path(directory, "Step2_remove_unreliable_peptide.rda"))
    saveRDS(dataset, file = file.path(directory, "ProtVis_dataset.rds"),
            compress = TRUE)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' ProtVis_dataset workflow server.
#' @param id Shiny module ID.
#' @param shared_state Optional reactiveValues object used by legacy modules.
#' @return Reactive values, invisibly.
#' @export
protvis_dataset_server <- function(id, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    builtin <- protvis_builtin_datasets()
    rv <- shiny::reactiveValues(
      dataset = NULL, error = NULL, message = NULL
    )

    notify <- function(text, type = "message") {
      rv$message <- as.character(text)
      tryCatch(shiny::showNotification(text, type = type),
               error = function(e) invisible(NULL))
    }

    set_dataset <- function(dataset, message = NULL, auto_export = TRUE) {
      validate_protvis_dataset(dataset)
      if (isTRUE(auto_export)) {
        dataset <- tryCatch(
          protvis_auto_export_dataset(
            dataset, directory = .protvis_ui_checkpoint_dir(input, shared_state)
          ),
          error = function(e) {
            .protvis_append_process(
              dataset, "auto_export", status = "error",
              error = conditionMessage(e),
              message = "Automatic export failed; the in-memory dataset remains available."
            )
          }
        )
      }
      rv$dataset <- dataset
      rv$error <- NULL
      .protvis_ui_sync_state(dataset, shared_state)
      .protvis_ui_save_legacy(dataset, shared_state)
      if (!is.null(message)) notify(message, "message")
      invisible(dataset)
    }

    safe_call <- function(label, expression) {
      tryCatch(
        force(expression),
        error = function(e) {
          rv$error <- protvis_error_message(e, label)
          notify(rv$error, "error")
          NULL
        }
      )
    }

    shiny::observeEvent(input$load_builtin, {
      selected_file <- as.character(input$builtin_source %||%
                                      "Maxquant_Export.xlsx")
      selected_row <- builtin[builtin$file == selected_file, , drop = FALSE]
      if (nrow(selected_row) != 1L) {
        notify("The selected built-in example is not available.", "error")
        return(invisible(NULL))
      }
      selected_source <- selected_row$source[[1L]]
      dataset <- safe_call(
        paste0("Built-in ", selected_source, " import"),
        load_protvis_builtin_data(source = selected_source,
                                   file = selected_file)
      )
      if (!is.null(dataset)) set_dataset(
        dataset, paste0("Built-in ", selected_source, " example loaded: ",
                        selected_file)
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$load_file, {
      shiny::req(input$data_file)
      dataset <- safe_call(
        paste0(as.character(input$source), " import"),
        import_protvis(
          path = input$data_file$datapath,
          source = input$source,
          filename = input$data_file$name
        )
      )
      if (!is.null(dataset)) set_dataset(
        dataset, paste0(input$source, " imported into ProtVis_dataset.")
      )
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$sample_file, {
      if (is.null(rv$dataset)) {
        notify("Load expression data before uploading sample_info.", "warning")
      } else {
        info <- safe_call(
          "Sample metadata import",
          protvis_read_table(input$sample_file$datapath,
                             filename = input$sample_file$name)
        )
        if (!is.null(info)) {
          current <- rv$dataset
          current <- .protvis_new_analysis_dataset(
            current, "sample_info", list(method = "sample_info")
          )
          current$sample_info <- .protvis_normalise_sample_info(
            info, colnames(current$expression_data)
          )
          current <- .protvis_append_process(
            current, "sample_info", status = "success",
            parameters = list(filename = input$sample_file$name)
          )
          set_dataset(current, "Sample metadata updated.")
        }
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$checkpoint_upload, {
      restored <- safe_call(
        "Checkpoint restore",
        restore_protvis_checkpoint(input$checkpoint_upload$datapath)
      )
      if (!is.null(restored)) set_dataset(restored, "Checkpoint restored.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$restore, {
      directory <- .protvis_ui_checkpoint_dir(input, shared_state)
      if (is.null(directory)) {
        notify("Set a checkpoint directory or upload an RDS checkpoint.", "warning")
      } else {
        restored <- safe_call(
          "Checkpoint restore",
          restore_protvis_checkpoint(directory)
        )
        if (!is.null(restored)) set_dataset(restored, "Latest checkpoint restored.")
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$save_checkpoint, {
      if (is.null(rv$dataset)) {
        notify("Load a dataset before saving a checkpoint.", "warning")
      } else {
        directory <- .protvis_ui_checkpoint_dir(input, shared_state)
        if (is.null(directory)) {
          notify("Set a checkpoint directory before saving.", "warning")
        } else {
          path <- safe_call(
            "Checkpoint save",
            save_protvis_checkpoint(
              rv$dataset, directory, stage = "manual"
            )
          )
          if (!is.null(path)) {
            current <- rv$dataset
            current$checkpoint_info$directory <- directory
            current$checkpoint_info$latest_path <- path
            set_dataset(current, paste0("Checkpoint saved: ", basename(path)))
          }
        }
      }
    }, ignoreInit = TRUE)

    run_step <- function(stage) {
      if (is.null(rv$dataset)) {
        notify("Load a dataset before running a processing node.", "warning")
        return(invisible(NULL))
      }
      directory <- .protvis_ui_checkpoint_dir(input, shared_state)
      parameters <- .protvis_ui_dataset_parameters(input, stage)
      current <- safe_call(
        paste0("Run ", stage),
        run_protvis_step(rv$dataset, stage, params = parameters,
                         checkpoint_dir = directory)
      )
      if (!is.null(current)) {
        set_dataset(current, auto_export = FALSE)
        event <- .protvis_last_event(current)
        if (!is.null(event) && identical(event$status, "error")) {
          rv$error <- protvis_error_message(event$error, stage)
          notify(rv$error, "error")
        } else {
          notify(paste0(stage, " completed."), "message")
        }
      }
      invisible(NULL)
    }

    shiny::observeEvent(input$run_step, {
      run_step(normalise_protvis_stage(input$stage %||%
                                       "noise_correction"))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run_all, {
      if (is.null(rv$dataset)) {
        notify("Load a dataset before running the pipeline.", "warning")
      } else {
        directory <- .protvis_ui_checkpoint_dir(input, shared_state)
        current <- safe_call(
          "Run ProtVis pipeline",
          run_protvis_pipeline(rv$dataset, checkpoint_dir = directory,
                               params = list(
                                 noise_correction = list(
                                   max_missing = as.numeric(
                                     input$max_missing %||% 0.5
                                   )
                                 ),
                                 transformation = list(
                                   method = input$transform_method %||% "log2"
                                 ),
                                 imputation = list(
                                   method = input$imputation_method %||% "median"
                                 ),
                                 normalization = list(
                                   method = input$normalization_method %||% "median"
                                 ),
                                 differential_analysis = list(
                                   fdr = as.numeric(input$fdr %||% 0.05)
                                 )
                               ))
        )
        if (!is.null(current)) {
          set_dataset(current, "Pipeline run finished; inspect history for skipped nodes.",
                      auto_export = FALSE)
          event <- .protvis_last_event(current)
          if (!is.null(event) && identical(event$status, "error")) {
            rv$error <- protvis_error_message(event$error, event$stage)
            notify(rv$error, "error")
          }
        }
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$retry, {
      if (is.null(rv$dataset)) {
        notify("Load a dataset before retrying a node.", "warning")
      } else {
        directory <- .protvis_ui_checkpoint_dir(input, shared_state)
        current <- safe_call(
          "Retry ProtVis node",
          retry_protvis_step(rv$dataset, checkpoint_dir = directory)
        )
        if (!is.null(current)) set_dataset(current, "Failed node retried.",
                                            auto_export = FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$rerun, {
      if (is.null(rv$dataset)) {
        notify("Load a dataset before re-running downstream nodes.", "warning")
      } else {
        directory <- .protvis_ui_checkpoint_dir(input, shared_state)
        stage <- normalise_protvis_stage(input$stage %||% "noise_correction")
        current <- safe_call(
          "Re-run downstream ProtVis nodes",
          rerun_protvis_downstream(
            rv$dataset, stage, checkpoint_dir = directory,
            params = .protvis_ui_dataset_parameters(input, stage)
          )
        )
        if (!is.null(current)) set_dataset(current, "Downstream nodes re-run.",
                                            auto_export = FALSE)
      }
    }, ignoreInit = TRUE)

    output$status <- shiny::renderUI({
      if (is.null(rv$dataset)) {
        return(shiny::div(
          class = "protvis-dataset-status",
          "No dataset loaded. Load the bundled MaxQuant workbook or upload a table."
        ))
      }
      dataset <- rv$dataset
      event <- .protvis_last_event(dataset)
      text <- paste0(
        protvis_dataset_name(dataset), " | ",
        nrow(dataset$expression_data), " proteins × ",
        ncol(dataset$expression_data), " samples | source: ",
        dataset$metadata$source %||% "unknown", " | last node: ",
        event$stage %||% "none", " (", event$status %||% "unknown", ") | output: ",
        dataset$metadata$auto_export_directory %||% getwd()
      )
      shiny::div(class = "protvis-dataset-status", text)
    })

    output$error_details <- shiny::renderUI({
      if (is.null(rv$dataset)) {
        if (is.null(rv$error)) return(NULL)
        return(shiny::div(class = "protvis-dataset-error", rv$error))
      }
      errors <- protvis_error_log(rv$dataset)
      if (!is.null(rv$error)) {
        return(shiny::div(
          class = "protvis-dataset-error",
          paste(rv$error, collapse = "\n")
        ))
      }
      if (nrow(errors) == 0L) return(NULL)
      shiny::div(
        class = "protvis-dataset-error",
        paste(
          paste0(errors$stage, ": ", errors$error),
          collapse = "\n"
        )
      )
    })

    output$expression <- DT::renderDT({
      shiny::req(rv$dataset)
      DT::datatable(utils::head(protvis_expression_matrix(rv$dataset), 100),
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    output$samples <- DT::renderDT({
      shiny::req(rv$dataset)
      DT::datatable(rv$dataset$sample_info,
                    options = list(pageLength = 20, scrollX = TRUE))
    })
    output$variables <- DT::renderDT({
      shiny::req(rv$dataset)
      DT::datatable(utils::head(rv$dataset$variable_info, 100),
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    output$process <- DT::renderDT({
      shiny::req(rv$dataset)
      DT::datatable(protvis_history(rv$dataset),
                    options = list(pageLength = 20, scrollX = TRUE))
    })
    output$results <- DT::renderDT({
      shiny::req(rv$dataset)
      results <- rv$dataset$analysis_results
      if (length(results) == 0L) {
        results <- data.frame(result = "No analysis result yet.",
                              stringsAsFactors = FALSE)
      } else {
        results <- data.frame(
          result = names(results),
          class = vapply(results, function(x) paste(class(x), collapse = ","),
                         character(1)),
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(results, options = list(pageLength = 20))
    })
    output$checkpoints <- DT::renderDT({
      directory <- .protvis_ui_checkpoint_dir(input, shared_state)
      if (is.null(directory)) {
        return(DT::datatable(data.frame(
          status = "No checkpoint directory selected.",
          stringsAsFactors = FALSE
        )))
      }
      DT::datatable(list_protvis_checkpoints(directory),
                    options = list(pageLength = 20, scrollX = TRUE))
    })
    output$pca <- shiny::renderPlot({
      shiny::req(rv$dataset)
      result <- rv$dataset$analysis_results$dimensionality_reduction
      if (!is.list(result) || !is.data.frame(result$scores) ||
          !all(c("PC1", "PC2") %in% names(result$scores))) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Run dimensionality reduction to view PCA.")
        return(invisible(NULL))
      }
      graphics::plot(result$scores$PC1, result$scores$PC2,
                     col = as.factor(result$scores$group),
                     pch = 19, xlab = "PC1", ylab = "PC2",
                     main = "ProtVis_dataset PCA")
      graphics::text(result$scores$PC1, result$scores$PC2,
                     labels = result$scores$sample_id, pos = 3, cex = 0.7)
    })

    output$download_rds <- shiny::downloadHandler(
      filename = function() "ProtVis_dataset.rds",
      content = function(file) {
        if (is.null(rv$dataset)) stop("No dataset is loaded.", call. = FALSE)
        saveRDS(rv$dataset, file, compress = TRUE)
      }
    )
    output$download_report <- shiny::downloadHandler(
      filename = function() "ProtVis_dataset_report.html",
      content = function(file) {
        if (is.null(rv$dataset)) stop("No dataset is loaded.", call. = FALSE)
        write_protvis_report(rv$dataset, file)
      }
    )
    output$download_bundle <- shiny::downloadHandler(
      filename = function() "ProtVis_dataset_export.zip",
      content = function(file) {
        if (is.null(rv$dataset)) stop("No dataset is loaded.", call. = FALSE)
        temporary <- tempfile("protvis_export_")
        dir.create(temporary, recursive = TRUE, showWarnings = FALSE)
        export_dir <- export_protvis_dataset(rv$dataset, temporary)
        files <- list.files(export_dir, recursive = TRUE, full.names = FALSE)
        old <- getwd()
        on.exit(setwd(old), add = TRUE)
        setwd(dirname(export_dir))
        utils::zip(file, files = basename(export_dir), flags = "-r9Xq")
      }
    )
    invisible(rv)
  })
}
