#' Project Initialization UI Module
#' @param id Module ID
#' @return UI elements for project initialization including working directory setup,
#'   sample info and expression matrix uploads, data source selection, and data preview.
#' @import shiny
#' @import bslib
#' @importFrom shinyFiles shinyDirButton
#' @importFrom bsicons bs_icon
#' @name project_init_ui
#' @export
#'
.protvis_raw_sample_template <- function() {
  combinations <- expand.grid(
    genotype = c("B73", "EA2024"),
    replicate = 1:3,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  combinations <- combinations[order(combinations$genotype,
                                      combinations$replicate), , drop = FALSE]
  sample_id <- paste(combinations$genotype, paste0("C", combinations$replicate),
                     sep = "_")
  data.frame(
    sample_id = sample_id,
    mzml_file = paste0(sample_id, ".mzML"),
    genotype = combinations$genotype,
    treatment = "Control",
    group = combinations$genotype,
    condition = "Control",
    replicate = combinations$replicate,
    batch = "Batch1",
    tissue = "leaf",
    organism = "Zea mays",
    accession = "PXD065315",
    source_url = "https://www.ebi.ac.uk/pride/archive/projects/PXD065315",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.protvis_validate_mzml_files <- function(sample_info, directory) {
  if (!is.data.frame(sample_info) || nrow(sample_info) == 0L) {
    stop("Upload sample information before checking mzML files.", call. = FALSE)
  }
  if (length(directory) != 1L || is.na(directory) ||
      !nzchar(directory) || !dir.exists(directory)) {
    stop("Select an accessible input directory before checking mzML files.",
         call. = FALSE)
  }
  names(sample_info) <- trimws(names(sample_info))
  find_col <- function(patterns) {
    hit <- which(tolower(names(sample_info)) %in% tolower(patterns))
    if (length(hit)) names(sample_info)[hit[[1L]]] else NULL
  }
  sample_col <- find_col(c("sample_id", "sample", "sample.name"))
  file_col <- find_col(c("mzml_file", "mzml", "raw_file", "file", "filename"))
  if (is.null(sample_col)) {
    stop("Sample information requires a sample_id column.", call. = FALSE)
  }
  if (is.null(file_col)) {
    stop("Sample information requires an mzML file column named mzml_file.",
         call. = FALSE)
  }
  sample_id <- trimws(as.character(sample_info[[sample_col]]))
  file_name <- trimws(as.character(sample_info[[file_col]]))
  if (anyNA(sample_id) || any(!nzchar(sample_id)) || anyDuplicated(sample_id)) {
    stop("sample_id values must be non-empty and unique.", call. = FALSE)
  }
  if (anyNA(file_name) || any(!nzchar(file_name)) || anyDuplicated(tolower(file_name))) {
    stop("mzML file names must be non-empty and unique.", call. = FALSE)
  }
  extension_ok <- tolower(tools::file_ext(file_name)) == "mzml"
  resolved <- normalizePath(file.path(directory, file_name),
                            winslash = "/", mustWork = FALSE)
  exists <- file.exists(resolved) & !dir.exists(resolved)
  manifest <- data.frame(
    sample_id = sample_id,
    mzml_file = file_name,
    path = resolved,
    extension_ok = extension_ok,
    exists = exists,
    status = ifelse(!extension_ok, "Invalid extension",
                    ifelse(exists, "Found", "Missing")),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  errors <- c(
    if (any(!extension_ok)) paste0("Non-mzML files: ",
                                   paste(file_name[!extension_ok], collapse = ", ")),
    if (any(!exists)) paste0("Missing files: ",
                             paste(file_name[!exists], collapse = ", "))
  )
  list(valid = !length(errors), manifest = manifest,
       message = if (length(errors)) paste(errors, collapse = "; ")
                 else paste(nrow(manifest), "mzML files found and matched."))
}

project_init_ui <- function(id) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      width = 430,
      tags$h4("Setup", class = "text-primary"),
      shinyFiles::shinyDirButton(
        id = ns("prj_wd"),
        label = "Set working directory",
        title = "Select working directory",
        icon = bs_icon("folder")
      ),
      shiny::textOutput(ns("raw_wd_path")),
      shiny::hr(),
      shiny::fileInput(
        inputId = ns("SampleInfo"),
        label = 'Upload Sample Information (.csv, .xlsx, .xls)',
        accept = c(".csv", ".xlsx", ".xls")
      ),
      tags$small(
        "Confirm sample information; Sage requires an mzml_file column.",
        style = "color: #6c757d"
      ),
      bslib::accordion(
        id = ns("raw_input_accordion"),
        open = NULL,
        bslib::accordion_panel(
          "Raw/mzML input (optional)",
          icon = bsicons::bs_icon("file-earmark-binary"),
          tags$p(
            "Use this section when sample_info contains an mzML file column. " ,
            "The selected directory is checked against every sample before search.",
            class = "text-muted small"
          ),
          shiny::actionButton(
            ns("use_builtin_raw_sample_info"),
            "Use built-in PXD065315 sample information",
            icon = bsicons::bs_icon("table"),
            class = "btn btn-outline-primary w-100"
          ),
          shiny::uiOutput(ns("raw_sample_info_status")),
          shiny::fileInput(
            ns("raw_fasta"), "Upload protein FASTA (optional)",
            accept = c(".fa", ".fasta", ".faa", ".gz")
          ),
          shiny::uiOutput(ns("raw_fasta_status")),
          shiny::uiOutput(ns("raw_directory_ui")),
          shiny::actionButton(
            ns("check_raw_files"), "Check mzML files",
            icon = bsicons::bs_icon("check2-circle"),
            class = "btn btn-outline-primary w-100"
          ),
          shiny::uiOutput(ns("raw_check_sidebar")),
          shiny::downloadButton(
            ns("download_raw_template"), "Download PXD065315 sample template",
            class = "btn btn-outline-secondary w-100"
          )
        )
      ),
      shiny::fileInput(
        inputId = ns("expression_matrix"),
        label = 'Upload Expression Matrix (.csv, .xlsx, .xls)',
        accept = c(".csv", ".xlsx", ".xls")
      ),
      tags$small("Confirm expression matrix", style = "color: #6c757d"),
      shiny::selectInput(
        inputId = ns("builtin_dataset"),
        label = "Built-in example",
        choices = stats::setNames(
          protvis_builtin_datasets()$file,
          paste(protvis_builtin_datasets()$source,
                "—", protvis_builtin_datasets()$file)
        ),
        selected = protvis_builtin_datasets()$file[[1L]]
      ),
      shiny::uiOutput(ns("builtin_provenance")),
      shiny::actionButton(
        ns("load_builtin"), "Load selected built-in example",
        class = "btn btn-outline-primary w-100"
      ),
      shiny::selectInput(
        inputId = ns("data_source"),
        label = "Select data source",
        choices = c(
          "Raw", "MaxQuant", "ProteomeDiscoverer", "Proteome Discoverer",
          "DIA-NN", "Spectronaut", "FragPipe", "Skyline", "Mascot", "OpenMS",
          "User-defined matrix"
        ),
        selected = "MaxQuant"
      )
    ),
    shiny::actionButton(ns("run_button"), "Project init"),
    bslib::card(
      bslib::card_header("Preview Sample Info and Expression Matrix"),
      bslib::card_body(
        bslib::navset_tab(
          id = ns("preview_tabs"),
          header = NULL,
          bslib::nav_panel("Sample Info",
                           shiny::htmlOutput(ns("file_check_init")),
                    DT::DTOutput(ns("tbl_sample_info"))
          ),
          bslib::nav_panel("Expression Matrix",
                           shiny::htmlOutput(ns("matrix_check")),
                    DT::DTOutput(ns("tbl_expression_matrix"))
          ),
          bslib::nav_panel("Raw/mzML Files",
                           shiny::htmlOutput(ns("raw_check_summary")),
                           DT::DTOutput(ns("tbl_raw_manifest"))
          )
        )
      )
    )
  )
}

#' Project Initialization Server Module
#' @param id Module ID
#' @param shared_state A reactiveValues object for sharing state (workdir, sample info, etc.)
#' @import shiny
#' @importFrom shinyFiles shinyDirChoose parseDirPath
#' @importFrom fs path_home
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#' @importFrom DT renderDT
#' @name project_init_server
#' @export
#'
project_init_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Pass shinyFiles only accessible, uniquely named roots. In particular,
    # avoid localized WMIC/PowerShell output on Windows, which can leave the
    # chooser modal open with empty directory and content panes.
    volumes <- .protvis_directory_roots()
    shinyFiles::shinyDirChoose(
      input,
      "prj_wd",
      roots = volumes,
      session = session,
      defaultRoot = names(volumes)[[1L]]
    )
    shinyFiles::shinyDirChoose(
      input,
      "raw_directory",
      roots = volumes,
      session = session,
      defaultRoot = names(volumes)[[1L]]
    )
    # Listen to directory selection and update shared_state$workdir
    shiny::observeEvent(input$prj_wd, {
      shiny::req(input$prj_wd)
      tryCatch({
        selected_dir <- shinyFiles::parseDirPath(volumes, input$prj_wd)
        selected_dir <- as.character(selected_dir)[[1L]]
        if (!nzchar(selected_dir) || !base::dir.exists(selected_dir)) {
          stop("The selected directory is not accessible.", call. = FALSE)
        }
        shared_state$workdir <- base::normalizePath(
          selected_dir, winslash = "/", mustWork = TRUE
        )
        shiny::showNotification(
          paste("Working directory set to:", shared_state$workdir),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Unable to select working directory:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
      })
    }, ignoreInit = TRUE)
    # Display selected working directory path
    output$raw_wd_path <- renderText({
      shiny::req(shared_state$workdir)
      base::paste("Working directory:", shared_state$workdir)
    })
    output$raw_directory_ui <- shiny::renderUI({
      tagList(
        shinyFiles::shinyDirButton(
          ns("raw_directory"), "Select mzML input directory",
          title = "Select directory containing mzML files",
          icon = bsicons::bs_icon("folder2-open"),
          class = "btn btn-outline-secondary w-100"
        ),
        tags$small(
          textOutput(ns("raw_directory_path")),
          class = "text-muted"
        )
      )
    })
    shiny::observeEvent(input$raw_directory, {
      shiny::req(input$raw_directory)
      tryCatch({
        selected_dir <- shinyFiles::parseDirPath(volumes, input$raw_directory)
        selected_dir <- as.character(selected_dir)[[1L]]
        if (!nzchar(selected_dir) || !dir.exists(selected_dir)) {
          stop("The selected mzML directory is not accessible.", call. = FALSE)
        }
        shared_state$raw_directory <- normalizePath(selected_dir, winslash = "/",
                                                     mustWork = TRUE)
        shared_state$raw_manifest <- NULL
        shared_state$raw_check <- NULL
      }, error = function(e) {
        shiny::showNotification(paste("Unable to select mzML directory:",
                                      conditionMessage(e)), type = "error")
      })
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$use_builtin_raw_sample_info, {
      builtin_info <- .protvis_raw_sample_template()
      # Keep the built-in manifest visible in the canonical Sample Info tab as
      # well as in the raw/mzML validation state.  Previously only
      # raw_sample_info was updated, while the preview table required
      # shared_state$sample_info and therefore remained blank.
      shared_state$raw_sample_info <- builtin_info
      shared_state$sample_info <- builtin_info
      shared_state$sage_workflow <- TRUE
      shared_state$raw_manifest <- NULL
      shared_state$raw_check <- NULL
      shiny::showNotification(
        "Built-in PXD065315 sample information loaded (6 samples).",
        type = "message"
      )
    }, ignoreInit = TRUE)
    output$raw_sample_info_status <- shiny::renderUI({
      info <- shared_state$raw_sample_info
      if (is.null(info)) return(NULL)
      tags$small(
        paste("Sample information ready:", nrow(info), "samples;", 
              "replace mzml_file names with the downloaded PRIDE filenames."),
        class = "text-success"
      )
    })
    shiny::observeEvent(input$raw_fasta, {
      shiny::req(input$raw_fasta)
      file_name <- as.character(input$raw_fasta$name %||% "")
      valid <- grepl("\\.(fa|fasta|faa)(\\.gz)?$", file_name, ignore.case = TRUE)
      if (!valid) {
        shared_state$raw_fasta <- NULL
        shiny::showNotification("FASTA file must use .fa, .fasta, .faa, or .gz extension.",
                                type = "error")
      } else {
        shared_state$raw_fasta <- list(
          name = file_name,
          path = normalizePath(input$raw_fasta$datapath, winslash = "/", mustWork = TRUE)
        )
        shiny::showNotification("Protein FASTA uploaded.", type = "message")
      }
    })
    output$raw_fasta_status <- shiny::renderUI({
      fasta <- shared_state$raw_fasta
      if (is.null(fasta)) return(NULL)
      tags$small(paste("FASTA ready:", fasta$name), class = "text-success")
    })
    output$raw_directory_path <- shiny::renderText({
      path <- shared_state$raw_directory
      if (is.null(path) || !nzchar(path)) "No mzML directory selected" else path
    })
    # Show the provenance URL immediately for the selected built-in example;
    # this is intentionally rendered in Project init as well as the canonical
    # ProtVis_dataset page because most users choose examples here first.
    output$builtin_provenance <- shiny::renderUI({
      manifest <- protvis_builtin_datasets()
      selected <- as.character(input$builtin_dataset %||% manifest$file[[1L]])
      row <- manifest[manifest$file == selected, , drop = FALSE]
      if (nrow(row) != 1L) return(NULL)
      shiny::tags$div(
        style = paste(
          "margin: -0.15rem 0 0.9rem; padding: 0.65rem 0.75rem;",
          "border-left: 3px solid #1787c9; background: #f3f8fc;",
          "color: #536b7d; font-size: 0.82rem; line-height: 1.45;"
        ),
        shiny::tags$strong("Real data source: "),
        shiny::tags$span(row$description[[1L]]),
        shiny::tags$br(),
        shiny::tags$a(
          href = row$reference[[1L]],
          target = "_blank", rel = "noopener noreferrer",
          row$reference[[1L]]
        )
      )
    })
    # Upload and read sample info, then store it in shared_state
    shiny::observeEvent(input$SampleInfo, {
      shiny::req(input$SampleInfo)
      tryCatch({
        sample_info <- protvis_read_table(
          input$SampleInfo$datapath, filename = input$SampleInfo$name
        )
        shared_state$sample_info <- sample_info
        if (any(tolower(trimws(names(sample_info))) %in%
                c("mzml_file", "mzml", "raw_file", "file", "filename"))) {
          shared_state$raw_sample_info <- sample_info
          shared_state$sage_workflow <- TRUE
          shared_state$raw_manifest <- NULL
          shared_state$raw_check <- NULL
          shiny::showNotification(
            "Sample info uploaded and registered for Sage mzML search.",
            type = "message"
          )
        } else {
          shared_state$sage_workflow <- FALSE
          shiny::showNotification("Sample info uploaded", type = "message")
        }
      }, error = function(e) {
        shiny::showNotification(
          paste0("Sample info upload failed: ", conditionMessage(e)),
          type = "error"
        )
      })
    })
    shiny::observeEvent(input$check_raw_files, {
      tryCatch({
        result <- .protvis_validate_mzml_files(
          shared_state$raw_sample_info %||% shared_state$sample_info,
          shared_state$raw_directory
        )
        shared_state$raw_manifest <- result$manifest
        shared_state$raw_check <- result
        shiny::showNotification(
          result$message, type = if (result$valid) "message" else "error",
          duration = if (result$valid) 5 else NULL
        )
      }, error = function(e) {
        shared_state$raw_manifest <- NULL
        shared_state$raw_check <- list(valid = FALSE, message = conditionMessage(e))
        shiny::showNotification(paste("mzML check failed:", conditionMessage(e)),
                                type = "error", duration = NULL)
      })
    }, ignoreInit = TRUE)
    output$raw_check_sidebar <- shiny::renderUI({
      result <- shared_state$raw_check
      if (is.null(result)) return(NULL)
      cls <- if (isTRUE(result$valid)) "text-success" else "text-danger"
      tags$p(result$message, class = cls, style = "margin-top: .5rem;")
    })
    output$raw_check_summary <- shiny::renderUI({
      result <- shared_state$raw_check
      if (is.null(result)) {
        return(tags$p("Select a directory and check the mzML files.",
                      class = "text-muted"))
      }
      tags$p(result$message,
             class = if (isTRUE(result$valid)) "text-success" else "text-danger")
    })
    output$tbl_raw_manifest <- DT::renderDT({
      shiny::req(shared_state$raw_manifest)
      DT::datatable(shared_state$raw_manifest, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })
    output$download_raw_template <- shiny::downloadHandler(
      filename = function() "PXD065315_sample_info_template.csv",
      content = function(file) utils::write.csv(
        .protvis_raw_sample_template(), file, row.names = FALSE, na = ""
      )
    )
    # Upload and read expression matrix, then store it in shared_state
    shiny::observeEvent(input$expression_matrix, {
      shiny::req(input$expression_matrix)
      tryCatch({
        expression_matrix <- protvis_read_table(
          input$expression_matrix$datapath,
          filename = input$expression_matrix$name
        )
        shared_state$expression_matrix <- expression_matrix
        shiny::showNotification("Expression matrix uploaded", type = "message")
      }, error = function(e) {
        shiny::showNotification(
          paste0("Expression matrix upload failed: ", conditionMessage(e)),
          type = "error"
        )
      })
    })
    # Sync data source selection to shared_state
    shiny::observeEvent(input$data_source, {
      shared_state$data_source <- input$data_source
      if (nzchar(as.character(input$data_source %||% ""))) {
        shared_state$sage_workflow <- FALSE
      }
    })
    # Load a bundled, source-specific example into the shared project state.
    shiny::observeEvent(input$load_builtin, {
      tryCatch({
        selected <- as.character(input$builtin_dataset %||% "")
        manifest <- protvis_builtin_datasets()
        row <- manifest[manifest$file == selected, , drop = FALSE]
        if (nrow(row) != 1L) stop("Please select a valid built-in example.", call. = FALSE)
        dataset <- load_protvis_builtin_data(
          source = row$source[[1L]], file = row$file[[1L]], auto_export = FALSE
        )
        shared_state$sample_info <- dataset$sample_info
        # Keep the protein identifier as an explicit column at every UI
        # boundary.  ProtVis_dataset stores it as row names internally, but
        # the preprocessing modules intentionally consume an ID-first data
        # frame.  Passing expression_data directly makes the first sample
        # look like ID and silently discards the real protein identifiers.
        shared_state$expression_matrix <- protvis_expression_matrix(dataset)
        shared_state$expression_matrix_filtered <- protvis_expression_matrix(dataset)
        shared_state$data_source <- row$source[[1L]]
        shared_state$workdir <- protvis_output_directory(shared_state$workdir %||% getwd())
        shiny::showNotification(
          paste(row$source[[1L]], "built-in example loaded; click Project init to create ProtVis_dataset."),
          type = "message"
        )
      }, error = function(e) {
        error_message <- conditionMessage(e)
        if (grepl("lazy-load database.*is corrupt|internal error.*R_decompress",
                  error_message, ignore.case = TRUE)) {
          error_message <- paste0(
            "The installed ProtVis package is stale or corrupt. Close every R ",
            "session using ProtVis, reinstall ProtVis 0.3.4 or later, and start ",
            "a new R session. Use the clean installer supplied by ProtVis so ",
            "the old lazy-load database is removed and the new installation ",
            "is verified. Original error: ", error_message
          )
        }
        shiny::showNotification(paste("Built-in example failed:", error_message),
                                type = "error", duration = NULL)
      })
    }, ignoreInit = TRUE)

    # On clicking init, validate the current inputs and create one canonical
    # ProtVis_dataset for the project. Later analyses create new versions only.
    shiny::observeEvent(input$run_button, {
      tryCatch({
        directory <- protvis_output_directory(shared_state$workdir %||% getwd())
        expression_missing <- is.null(shared_state$expression_matrix) ||
          !is.data.frame(shared_state$expression_matrix) ||
          nrow(shared_state$expression_matrix) < 1L ||
          ncol(shared_state$expression_matrix) < 2L
        sage_mode <- isTRUE(shared_state$sage_workflow) ||
          (is.list(shared_state$sage_search_bundle) &&
           identical(shared_state$sage_search_bundle$status, "success"))
        if (expression_missing && sage_mode) {
          sage_bundle <- shared_state$sage_search_bundle
          raw_directory <- shared_state$raw_directory %||% ""
          sage_output <- file.path(raw_directory, "Sage_search")
          if (!is.list(sage_bundle) ||
              !identical(sage_bundle$status, "success")) {
            mzml_paths <- if (isTRUE(shared_state$raw_check$valid) &&
                              is.data.frame(shared_state$raw_manifest)) {
              shared_state$raw_manifest$path
            } else if (nzchar(raw_directory) && dir.exists(raw_directory)) {
              list.files(raw_directory, pattern = "[.]mzML$", full.names = TRUE,
                         ignore.case = TRUE)
            } else character()
            fasta <- shared_state$raw_fasta$path %||% ""
            sage_bundle <- .protvis_recover_sage_bundle(
              sage_output, fasta = fasta, mzml_paths = mzml_paths
            )
          }
          if (is.list(sage_bundle) && identical(sage_bundle$status, "success")) {
            fasta <- shared_state$raw_fasta$path %||%
              sage_bundle$config$database$fasta %||% ""
            mzml_paths <- if (isTRUE(shared_state$raw_check$valid) &&
                              is.data.frame(shared_state$raw_manifest)) {
              shared_state$raw_manifest$path
            } else as.character(sage_bundle$config$mzml_paths %||% character())
            dataset <- .protvis_create_sage_dataset(
              sage_bundle,
              shared_state$raw_sample_info %||% shared_state$sample_info,
              mzml_paths,
              shared_state$sage_search_parameters %||% list(),
              fasta, dirname(sage_bundle$config_path)
            )
            dataset$metadata$object_name <- paste0(
              "ProtVis_dataset__project_init__Sage_LFQ__v1"
            )
            dataset$metadata$object_version <- 1L
            dataset <- protvis_auto_export_dataset(
              dataset, directory = directory, include_raw = FALSE
            )
            .protvis_ui_sync_state(dataset, shared_state)
            .protvis_save_stage_dataset(
              dataset, file.path(directory, "Step1_project_init.rda")
            )
            shiny::showNotification(
              "ProtVis_dataset created from the Sage LFQ protein matrix.",
              type = "message"
            )
            return(invisible(NULL))
          }
          mzml_paths <- if (isTRUE(shared_state$raw_check$valid) &&
                            is.data.frame(shared_state$raw_manifest)) {
            shared_state$raw_manifest$path
          } else if (nzchar(raw_directory) && dir.exists(raw_directory)) {
            list.files(raw_directory, pattern = "[.]mzML$", full.names = TRUE,
                       ignore.case = TRUE)
          } else character()
          fasta <- shared_state$raw_fasta$path %||% ""
          if (!length(mzml_paths) || !nzchar(fasta) || !file.exists(fasta)) {
            stop("For Sage staging, register a readable FASTA and at least one mzML file.",
                 call. = FALSE)
          }
          shared_state$workdir <- directory
          dataset <- .protvis_create_sage_staging_dataset(
            shared_state$raw_sample_info %||% shared_state$sample_info,
            fasta, mzml_paths, file.path(directory, "Sage_search")
          )
          dataset <- protvis_auto_export_dataset(
            dataset, directory = directory, include_raw = FALSE
          )
          .protvis_ui_sync_state(dataset, shared_state)
          .protvis_save_stage_dataset(
            dataset, file.path(directory, "Step1_project_init.rda")
          )
          shiny::showNotification(
            "Sage inputs registered. Run Sage Search to add expression data to ProtVis_dataset.",
            type = "message"
          )
          return(invisible(NULL))
        }
        shiny::req(shared_state$sample_info, shared_state$expression_matrix,
                   shared_state$data_source)
        validated <- validate_protvis_data(
          shared_state$expression_matrix, shared_state$sample_info,
          source = shared_state$data_source
        )
        sample_info <- validated$sample_info
        expression_matrix <- validated$expression_matrix
        shared_state$sample_info <- sample_info
        shared_state$expression_matrix <- expression_matrix
        data_source <- shared_state$data_source
        shared_state$workdir <- directory
        dataset <- create_protvis_dataset(
          expression_data = expression_matrix,
          sample_info = sample_info,
          metadata = list(source = data_source, output_directory = directory)
        )
        if (isTRUE(shared_state$raw_check$valid) &&
            is.data.frame(shared_state$raw_manifest)) {
          dataset$metadata$raw_directory <- shared_state$raw_directory
          dataset$metadata$raw_manifest <- shared_state$raw_manifest
          dataset <- .protvis_append_process(
            dataset, "raw_file_registration", status = "success",
            parameters = list(directory = shared_state$raw_directory),
            message = paste0("Validated ", nrow(shared_state$raw_manifest),
                             " mzML files against sample information.")
          )
        }
        if (is.list(shared_state$raw_fasta) &&
            nzchar(shared_state$raw_fasta$path) &&
            file.exists(shared_state$raw_fasta$path)) {
          dataset$metadata$raw_fasta <- shared_state$raw_fasta
          dataset <- .protvis_append_process(
            dataset, "raw_fasta_registration", status = "success",
            parameters = list(filename = shared_state$raw_fasta$name),
            message = "Protein FASTA registered for downstream database search."
          )
        }
        dataset$metadata$object_name <- paste0(
          "ProtVis_dataset__project_init__", .protvis_object_label(data_source), "__v1"
        )
        dataset$metadata$object_version <- 1L
        if (is.list(shared_state$sage_search_bundle) &&
            identical(shared_state$sage_search_bundle$status, "success")) {
          dataset <- .protvis_attach_sage_bundle(
            dataset, shared_state$sage_search_bundle,
            shared_state$sage_search_parameters %||% list(),
            shared_state$sage_search_bundle$config$database$fasta %||% "",
            shared_state$raw_directory %||% "",
            dirname(shared_state$sage_search_bundle$config_path)
          )
          shared_state$sage_search_bundle <- NULL
          shared_state$sage_search_parameters <- list()
        }
        # Preserve a valid background uploaded before Project init.  Attach it
        # after the initial project version is named, creating a traceable v2.
        if (is.list(shared_state$pending_enrichment_background) &&
            all(c("GO_background", "KEGG_background") %in%
                names(shared_state$pending_enrichment_background))) {
          dataset <- .protvis_add_enrichment_background(
            dataset,
            shared_state$pending_enrichment_background,
            shared_state$pending_enrichment_background_name %||%
              "enrichment_background.xlsx"
          )
        }
        dataset <- protvis_auto_export_dataset(dataset, directory = directory)
        shared_state$dataset <- dataset
        shared_state$expression_matrix <- protvis_expression_matrix(dataset)
        shared_state$expression_matrix_filtered <- protvis_expression_matrix(dataset)
        shared_state$dataset_name <- protvis_dataset_name(dataset)
        history <- shared_state$dataset_history %||% list()
        shared_state$dataset_history <- c(history, list(dataset))
        save_path <- file.path(directory, "Step1_project_init.rda")
        .protvis_save_stage_dataset(dataset, save_path)
        shiny::showNotification(
          paste("Project initialized:", protvis_dataset_name(dataset)), type = "message"
        )
        message("✅ Step1_project_init.rda saved to: ", save_path)
      }, error = function(e) {
        shiny::showNotification(paste("❌ Save failed:", e$message), type = "error")
      })
    })
    # Preview sample info table
    output$tbl_sample_info <- DT::renderDT({
      shiny::req(shared_state$sample_info)
      shared_state$sample_info
    })
    # Preview expression matrix table
    output$tbl_expression_matrix <- DT::renderDT({
      shiny::req(shared_state$expression_matrix)
      shared_state$expression_matrix
    })
    # Confirm sample info upload UI
    output$file_check_init <- renderUI({
      if (!is.null(input$SampleInfo)) {
        return(tags$p("✅ Sample info uploaded:", input$SampleInfo$name,
                      class = "text-success"))
      }
      if (is.data.frame(shared_state$sample_info) &&
          identical(shared_state$sample_info, shared_state$raw_sample_info)) {
        return(tags$p("✅ Built-in PXD065315 sample information loaded.",
                      class = "text-success"))
      }
      NULL
    })
    # Confirm expression matrix upload UI
    output$matrix_check <- renderUI({
      shiny::req(input$expression_matrix)
      tags$p("✅ Expression matrix uploaded:", input$expression_matrix$name, class = "text-success")
    })
  })
}
