# Standardized Sage database-search workflow.

.protvis_sage_default_parameters <- function() {
  list(
    precursor_ppm = 20,
    fragment_da = 0.5,
    enzyme = "Trypsin/P",
    missed_cleavages = 2,
    min_peptide_length = 7,
    max_peptide_length = 50,
    fixed_carbamidomethyl = TRUE,
    variable_oxidation = TRUE,
    variable_nterm_acetyl = TRUE,
    fdr = 0.01,
    lfq = TRUE
  )
}

.protvis_attach_sage_bundle <- function(dataset, bundle, parameters,
                                        fasta, mzml_directory, output_directory) {
  dataset <- as_protvis_dataset(dataset)
  validate_protvis_dataset(dataset)
  dataset$analysis_results$Sage_database_search <- bundle
  metadata <- dataset$metadata
  metadata$raw_search <- list(
    fasta = fasta, mzml_directory = mzml_directory,
    output_directory = output_directory, engine = "Sage 0.14.7"
  )
  dataset$metadata <- metadata
  attachments <- dataset$other_files %||% list()
  for (path in unname(unlist(bundle$files))) {
    if (file.exists(path)) {
      attachments[[length(attachments) + 1L]] <- list(
        path = normalizePath(path, winslash = "/", mustWork = FALSE),
        name = basename(path), kind = "Sage database-search output",
        size_bytes = as.numeric(file.info(path)$size),
        md5 = unname(tools::md5sum(path)), attached_at = as.character(Sys.time())
      )
    }
  }
  dataset$other_files <- attachments
  .protvis_append_process(
    dataset, "Sage_database_search", status = "success",
    parameters = parameters,
    message = "Sage search completed and result tables were retained."
  )
}

.protvis_sage_lfq_protein_matrix <- function(lfq, sample_info, mzml_paths) {
  if (!is.data.frame(lfq) || !nrow(lfq)) {
    stop("Sage LFQ output is empty. Enable LFQ to create the protein matrix.",
         call. = FALSE)
  }
  protein_col <- names(lfq)[tolower(names(lfq)) %in% c("proteins", "protein")][1L]
  if (is.na(protein_col) || !nzchar(protein_col)) {
    stop("Sage LFQ output does not contain the required proteins column.",
         call. = FALSE)
  }
  file_names <- basename(mzml_paths)
  intensity_cols <- intersect(file_names, names(lfq))
  if (!length(intensity_cols)) {
    intensity_cols <- names(lfq)[tolower(names(lfq)) %in% tolower(file_names)]
  }
  if (!length(intensity_cols)) {
    stop("Sage LFQ columns could not be matched to the mzML files.", call. = FALSE)
  }
  sample_info <- as.data.frame(sample_info, stringsAsFactors = FALSE,
                               check.names = FALSE)
  sample_ids <- as.character(sample_info$sample_id)
  file_col <- names(sample_info)[tolower(names(sample_info)) %in%
                                  c("mzml_file", "mzml", "raw_file", "file", "filename")][1L]
  mapped_ids <- if (!is.na(file_col) && nzchar(file_col)) {
    sample_ids[match(tolower(intensity_cols), tolower(as.character(sample_info[[file_col]])))]
  } else character()
  mapped_ids[is.na(mapped_ids) | !nzchar(mapped_ids)] <-
    tools::file_path_sans_ext(intensity_cols[is.na(mapped_ids) | !nzchar(mapped_ids)])
  protein_lists <- strsplit(as.character(lfq[[protein_col]]), ";", fixed = TRUE)
  protein_lists <- lapply(protein_lists, function(x) {
    x <- trimws(x)
    x[nzchar(x) & !grepl("^rev_", x, ignore.case = TRUE)]
  })
  proteins <- sort(unique(unlist(protein_lists, use.names = FALSE)))
  proteins <- proteins[nzchar(proteins)]
  if (!length(proteins)) stop("No target proteins were found in Sage LFQ output.",
                              call. = FALSE)
  result <- matrix(NA_real_, nrow = length(proteins), ncol = length(intensity_cols),
                   dimnames = list(proteins, mapped_ids))
  for (j in seq_along(intensity_cols)) {
    values <- .protvis_safe_numeric(lfq[[intensity_cols[[j]]]])
    for (i in which(is.finite(values) & lengths(protein_lists) > 0L)) {
      ids <- protein_lists[[i]]
      result[ids, j] <- rowSums(cbind(result[ids, j], values[[i]]), na.rm = TRUE)
    }
  }
  as.data.frame(result, check.names = FALSE, stringsAsFactors = FALSE)
}

.protvis_create_sage_dataset <- function(bundle, sample_info, mzml_paths,
                                         parameters, fasta, output_directory) {
  sample_info <- as.data.frame(sample_info %||% data.frame(),
                               stringsAsFactors = FALSE, check.names = FALSE)
  file_col <- names(sample_info)[tolower(names(sample_info)) %in%
                                  c("mzml_file", "mzml", "raw_file", "file", "filename")][1L]
  if (nrow(sample_info) > 0L && !is.na(file_col) && nzchar(file_col)) {
    index <- match(tolower(basename(mzml_paths)),
                   tolower(as.character(sample_info[[file_col]])))
    if (all(!is.na(index))) sample_info <- sample_info[index, , drop = FALSE]
  }
  matrix <- .protvis_sage_lfq_protein_matrix(bundle$lfq_table, sample_info, mzml_paths)
  dataset <- create_protvis_dataset(
    matrix, sample_info = sample_info,
    metadata = list(source = "Sage LFQ", raw_fasta = list(
      name = basename(fasta), path = fasta
    ), raw_directory = dirname(mzml_paths[[1L]]), output_directory = output_directory)
  )
  .protvis_attach_sage_bundle(dataset, bundle, parameters, fasta,
                              dirname(mzml_paths[[1L]]), output_directory)
}

.protvis_create_sage_staging_dataset <- function(sample_info, fasta,
                                                 mzml_paths, output_directory) {
  mzml_paths <- normalizePath(as.character(mzml_paths), winslash = "/",
                              mustWork = TRUE)
  sample_names <- basename(mzml_paths)
  placeholder <- matrix(NA_real_, nrow = 1L, ncol = length(sample_names),
                        dimnames = list("__SAGE_PENDING__", sample_names))
  dataset <- create_protvis_dataset(
    placeholder, sample_info = sample_info,
    metadata = list(
      source = "Sage database search",
      workflow_stage = "Sage_staging",
      raw_fasta = list(name = basename(fasta), path = fasta),
      raw_directory = dirname(mzml_paths[[1L]]),
      raw_mzml_paths = mzml_paths,
      output_directory = output_directory
    )
  )
  dataset$expression_data <- data.frame(row.names = character())
  dataset$variable_info <- .protvis_normalise_variable_info(NULL, character())
  dataset$variable_info_note <- .protvis_normalise_note(
    NULL, names(dataset$variable_info), "variable"
  )
  dataset$metadata$object_name <-
    "ProtVis_dataset__project_init__Sage_staging__v1"
  dataset$metadata$object_version <- 1L
  dataset <- .protvis_append_process(
    dataset, "project_init_sage_staging", status = "success",
    parameters = list(fasta = fasta, mzml_paths = mzml_paths),
    message = "Sage inputs registered; expression data will be added after search."
  )
  validate_protvis_dataset(dataset)
  dataset
}

.protvis_finalize_sage_dataset <- function(staged, bundle, parameters, fasta,
                                           mzml_paths, output_directory) {
  dataset <- .protvis_create_sage_dataset(
    bundle, staged$sample_info, mzml_paths, parameters, fasta, output_directory
  )
  dataset$metadata <- utils::modifyList(staged$metadata, dataset$metadata)
  dataset$metadata$workflow_stage <- "Sage_complete"
  dataset$metadata$parent_object_name <- staged$metadata$object_name %||%
    "ProtVis_dataset__project_init__Sage_staging__v1"
  dataset$metadata$object_name <-
    "ProtVis_dataset__Sage_database_search__v2"
  dataset$metadata$object_version <- 2L
  dataset$other_files <- c(staged$other_files %||% list(),
                           dataset$other_files %||% list())
  old_history <- staged$process_info$history %||% list()
  new_history <- dataset$process_info$history %||% list()
  dataset$process_info$history <- c(old_history, new_history)
  validate_protvis_dataset(dataset)
  dataset
}

.protvis_recover_sage_bundle <- function(output_directory, fasta = "",
                                         mzml_paths = character()) {
  output_directory <- path.expand(as.character(output_directory %||% ""))
  files <- list(
    config = file.path(output_directory, "sage_config.json"),
    results = file.path(output_directory, "results.sage.tsv"),
    lfq = file.path(output_directory, "lfq.tsv"),
    report = file.path(output_directory, "results.json")
  )
  if (!file.exists(files$lfq)) return(NULL)
  config <- if (file.exists(files$config)) {
    jsonlite::read_json(files$config, simplifyVector = TRUE)
  } else list()
  if (!length(mzml_paths)) mzml_paths <- as.character(config$mzml_paths %||% character())
  if (!nzchar(fasta)) fasta <- as.character(config$database$fasta %||% "")
  if (!length(mzml_paths) || !nzchar(fasta) || !file.exists(fasta)) return(NULL)
  list(
    status = "success", exit_status = 0L, sage_path = protvis_sage_executable(),
    config = config, config_path = files$config, files = files,
    log = "Recovered from the previous Sage_search output directory.",
    started_at = NA_character_, finished_at = as.character(Sys.time()),
    psms = protvis_read_sage_table(files$results),
    lfq_table = protvis_read_sage_table(files$lfq)
  )
}

.protvis_sage_paths <- function(fasta, mzml_directory, output_directory) {
  fasta <- path.expand(as.character(fasta %||% ""))
  mzml_directory <- path.expand(as.character(mzml_directory %||% ""))
  output_directory <- path.expand(as.character(output_directory %||% ""))
  if (!nzchar(fasta) || !file.exists(fasta)) {
    stop("A readable protein FASTA file is required.", call. = FALSE)
  }
  if (!nzchar(mzml_directory) || !dir.exists(mzml_directory)) {
    stop("A readable mzML input directory is required.", call. = FALSE)
  }
  mzml <- list.files(mzml_directory, pattern = "[.]mzML$", full.names = TRUE,
                     ignore.case = TRUE)
  if (!length(mzml)) stop("No .mzML files were found in the input directory.",
                          call. = FALSE)
  if (!nzchar(output_directory)) stop("A Sage output directory is required.",
                                     call. = FALSE)
  if (!dir.exists(output_directory) &&
      !dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)) {
    stop("Unable to create the Sage output directory.", call. = FALSE)
  }
  list(
    fasta = normalizePath(fasta, winslash = "/", mustWork = TRUE),
    mzml = normalizePath(mzml, winslash = "/", mustWork = TRUE),
    output = normalizePath(output_directory, winslash = "/", mustWork = TRUE)
  )
}

#' Build a validated Sage 0.14 configuration file.
#' @export
protvis_sage_build_config <- function(fasta, mzml_paths, output_directory,
                                       parameters = list()) {
  defaults <- .protvis_sage_default_parameters()
  parameters <- utils::modifyList(defaults, parameters %||% list())
  fasta <- normalizePath(as.character(fasta), winslash = "/", mustWork = TRUE)
  mzml_paths <- normalizePath(as.character(mzml_paths), winslash = "/",
                              mustWork = TRUE)
  output_directory <- normalizePath(output_directory, winslash = "/",
                                    mustWork = FALSE)
  if (!length(mzml_paths)) stop("At least one mzML path is required.", call. = FALSE)
  enzyme <- if (identical(parameters$enzyme, "Trypsin")) {
    list(missed_cleavages = as.integer(parameters$missed_cleavages),
         min_len = as.integer(parameters$min_peptide_length),
         max_len = as.integer(parameters$max_peptide_length),
         cleave_at = "KR", restrict = "P", c_terminal = TRUE)
  } else {
    list(missed_cleavages = as.integer(parameters$missed_cleavages),
         min_len = as.integer(parameters$min_peptide_length),
         max_len = as.integer(parameters$max_peptide_length),
         cleave_at = "KR", restrict = "P", c_terminal = TRUE)
  }
  static_mods <- if (isTRUE(parameters$fixed_carbamidomethyl)) {
    list(C = 57.021464)
  } else list()
  variable_mods <- list()
  if (isTRUE(parameters$variable_oxidation)) variable_mods$M <- 15.994915
  if (isTRUE(parameters$variable_nterm_acetyl)) {
    variable_mods[["["]] <- 42.010565
  }
  list(
    database = list(
      enzyme = enzyme, static_mods = static_mods,
      variable_mods = variable_mods, max_variable_mods = 3,
      generate_decoys = TRUE, decoy_tag = "rev_", fasta = fasta
    ),
    precursor_tol = list(ppm = c(-abs(as.numeric(parameters$precursor_ppm)),
                                  abs(as.numeric(parameters$precursor_ppm)))),
    fragment_tol = list(da = c(-abs(as.numeric(parameters$fragment_da)),
                               abs(as.numeric(parameters$fragment_da)))),
    quant = list(lfq = isTRUE(parameters$lfq)),
    report_psms = 5,
    predict_rt = TRUE,
    mzml_paths = mzml_paths,
    output_directory = output_directory
  )
}

#' Read a Sage TSV result table without losing its original column names.
#' @export
protvis_read_sage_table <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  data.table::fread(path, sep = "\t", data.table = FALSE,
                    check.names = FALSE, showProgress = FALSE)
}

#' Run Sage and return standardized tables, files, configuration, and logs.
#' @export
run_sage_search <- function(fasta, mzml_paths, output_directory,
                            parameters = list(), sage_path = NULL) {
  if (!length(mzml_paths)) stop("At least one mzML file is required.", call. = FALSE)
  if (any(!file.exists(mzml_paths))) stop("One or more mzML files do not exist.",
                                          call. = FALSE)
  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  output_directory <- normalizePath(output_directory, winslash = "/", mustWork = TRUE)
  sage <- protvis_sage_executable(sage_path)
  if (is.null(sage) || !file.exists(sage)) {
    stop("Sage executable was not found. Use the bundled Windows executable or install Sage on PATH.",
         call. = FALSE)
  }
  config <- protvis_sage_build_config(fasta, mzml_paths, output_directory, parameters)
  config_path <- file.path(output_directory, "sage_config.json")
  jsonlite::write_json(config, config_path, auto_unbox = TRUE, pretty = TRUE,
                       na = "null")
  started <- Sys.time()
  log <- tryCatch(
    system2(sage, args = shQuote(config_path), stdout = TRUE, stderr = TRUE),
    error = function(e) structure(conditionMessage(e), status = 1L)
  )
  exit_status <- attr(log, "status", exact = TRUE) %||% 0L
  log <- as.character(log %||% character())
  files <- list(
    config = config_path,
    results = file.path(output_directory, "results.sage.tsv"),
    lfq = file.path(output_directory, "lfq.tsv"),
    report = file.path(output_directory, "results.json")
  )
  bundle <- list(
    status = if (identical(as.integer(exit_status), 0L) &&
                 file.exists(files$results)) "success" else "failed",
    exit_status = as.integer(exit_status), sage_path = sage,
    config = config, config_path = config_path, files = files,
    log = log, started_at = as.character(started),
    finished_at = as.character(Sys.time()),
    psms = protvis_read_sage_table(files$results),
    lfq_table = protvis_read_sage_table(files$lfq)
  )
  bundle
}

sage_search_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::page_sidebar(
    sidebar = bslib::sidebar(
      width = 390,
      tags$h4("Sage database search", class = "text-primary"),
      tags$p("Run a reproducible FASTA + mzML search and retain all outputs in ProtVis_dataset.",
             class = "text-muted small"),
      shiny::uiOutput(ns("path_status")),
      shiny::numericInput(ns("precursor_ppm"), "Precursor tolerance (ppm)", 20, min = 0.1),
      shiny::numericInput(ns("fragment_da"), "Fragment tolerance (Da)", 0.5, min = 0.001),
      shiny::selectInput(ns("enzyme"), "Enzyme", c("Trypsin/P", "Trypsin")),
      shiny::numericInput(ns("missed_cleavages"), "Missed cleavages", 2, min = 0, max = 10, step = 1),
      shiny::numericInput(ns("min_len"), "Minimum peptide length", 7, min = 5, step = 1),
      shiny::numericInput(ns("max_len"), "Maximum peptide length", 50, min = 7, step = 1),
      shiny::checkboxInput(ns("fixed_carbamidomethyl"), "Carbamidomethyl (C), fixed", TRUE),
      shiny::checkboxInput(ns("variable_oxidation"), "Oxidation (M), variable", TRUE),
      shiny::checkboxInput(ns("variable_nterm_acetyl"), "Protein N-term Acetyl, variable", TRUE),
      shiny::numericInput(ns("fdr"), "Spectrum/peptide/protein FDR", 0.01, min = 0.0001, max = 0.2, step = 0.001),
      shiny::checkboxInput(ns("lfq"), "Enable LFQ", TRUE),
      shiny::actionButton(ns("run"), "Run Sage Search", icon = bsicons::bs_icon("play-fill"),
                          class = "btn-primary w-100"),
      shiny::downloadButton(ns("download_config"), "Download Sage config", class = "btn-outline-secondary w-100")
    ),
    bslib::layout_columns(
      bslib::card(bslib::card_header("Search status"), bslib::card_body(shiny::uiOutput(ns("status")))),
      bslib::card(bslib::card_header("Sage log"), bslib::card_body(shiny::verbatimTextOutput(ns("log")))),
      bslib::card(bslib::card_header("Search result tables"), bslib::card_body(
        bslib::navset_tab(
          bslib::nav_panel("Summary", DT::DTOutput(ns("summary"))),
          bslib::nav_panel("PSMs", DT::DTOutput(ns("psms"))),
          bslib::nav_panel("LFQ", DT::DTOutput(ns("lfq")))
        )
      )), col_widths = c(4, 8, 12)
    )
  )
}

sage_search_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(bundle = NULL, config_path = NULL)
    paths <- shiny::reactive({
      fasta <- shared_state$raw_fasta$path %||% ""
      directory <- shared_state$raw_directory %||% ""
      output_dir <- file.path(shared_state$workdir %||% directory, "Sage_search")
      list(fasta = fasta, directory = directory, output = output_dir)
    })
    output$path_status <- shiny::renderUI({
      p <- paths()
      tags$div(class = "small", tags$p(tags$b("FASTA: "), p$fasta),
               tags$p(tags$b("mzML directory: "), p$directory),
               tags$p(tags$b("Output: "), p$output),
               tags$p(tags$b("Sage: "), protvis_sage_executable() %||% "Not found",
                      class = if (is.null(protvis_sage_executable())) "text-danger" else "text-success"))
    })
    output$status <- shiny::renderUI({
      b <- rv$bundle
      if (is.null(b)) return(tags$p("Ready. Confirm the paths above and click Run Sage Search.", class = "text-muted"))
      tags$p(paste0("Status: ", b$status, "; PSM rows: ", nrow(b$psms),
                    "; LFQ rows: ", nrow(b$lfq_table)),
             class = if (identical(b$status, "success")) "text-success" else "text-danger")
    })
    output$log <- shiny::renderText({
      b <- rv$bundle
      if (is.null(b)) "No search has been run." else paste(b$log, collapse = "\n")
    })
    output$summary <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b)) return(data.frame(Message = "Run Sage Search to display results."))
      data.frame(file = names(b$files), path = unname(unlist(b$files)),
                 stringsAsFactors = FALSE)
    }, options = list(pageLength = 10, scrollX = TRUE))
    output$psms <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b)) return(data.frame(Message = "No PSM table available."))
      b$psms
    }, options = list(pageLength = 10, scrollX = TRUE))
    output$lfq <- DT::renderDT({
      b <- rv$bundle
      if (is.null(b)) return(data.frame(Message = "No LFQ table available."))
      b$lfq_table
    }, options = list(pageLength = 10, scrollX = TRUE))
    output$download_config <- shiny::downloadHandler(
      filename = function() "sage_config.json",
      content = function(file) {
        shiny::req(rv$config_path, file.exists(rv$config_path))
        file.copy(rv$config_path, file, overwrite = TRUE)
      }
    )
    shiny::observeEvent(input$run, {
      p <- paths()
      parameters <- list(
        precursor_ppm = input$precursor_ppm, fragment_da = input$fragment_da,
        enzyme = input$enzyme, missed_cleavages = input$missed_cleavages,
        min_peptide_length = input$min_len, max_peptide_length = input$max_len,
        fixed_carbamidomethyl = input$fixed_carbamidomethyl,
        variable_oxidation = input$variable_oxidation,
        variable_nterm_acetyl = input$variable_nterm_acetyl,
        fdr = input$fdr, lfq = input$lfq
      )
      tryCatch({
        validated <- .protvis_sage_paths(p$fasta, p$directory, p$output)
        shiny::withProgress(message = "Running Sage database search", value = 0.1, {
          manifest <- shared_state$raw_manifest
          if (isTRUE(shared_state$raw_check$valid %||% FALSE) &&
              is.data.frame(manifest) && nrow(manifest) > 0L) {
            validated$mzml <- normalizePath(manifest$path, winslash = "/",
                                            mustWork = TRUE)
          }
          bundle <- run_sage_search(validated$fasta, validated$mzml,
                                    validated$output, parameters)
          rv$bundle <- bundle
          rv$config_path <- bundle$config_path
          shiny::incProgress(0.8)
          if (!identical(bundle$status, "success")) {
            stop(paste(c("Sage search failed.", bundle$log), collapse = "\n"),
                 call. = FALSE)
          }
          shared_state$sage_search_bundle <- bundle
          shared_state$sage_search_parameters <- parameters
          dataset <- shared_state$dataset
          if (inherits(dataset, "ProtVis_dataset")) {
            if (identical(dataset$metadata$workflow_stage, "Sage_staging")) {
              dataset <- .protvis_finalize_sage_dataset(
                dataset, bundle, parameters, validated$fasta, validated$mzml,
                validated$output
              )
            } else {
              dataset <- .protvis_attach_sage_bundle(
                dataset, bundle, parameters, validated$fasta, p$directory,
                validated$output
              )
            }
            dataset <- protvis_auto_export_dataset(
              dataset, directory = shared_state$workdir, include_raw = FALSE
            )
            .protvis_ui_sync_state(dataset, shared_state)
            .protvis_save_stage_dataset(
              dataset, file.path(shared_state$workdir,
                                 "Step2_sage_database_search.rda")
            )
            shared_state$sage_search_bundle <- NULL
            shared_state$sage_search_parameters <- list()
          } else {
            dataset <- .protvis_create_sage_dataset(
              bundle, shared_state$raw_sample_info %||% shared_state$sample_info,
              validated$mzml, parameters, validated$fasta, validated$output
            )
            dataset <- protvis_auto_export_dataset(
              dataset, directory = validated$output, include_raw = FALSE
            )
            .protvis_ui_sync_state(dataset, shared_state)
            .protvis_save_stage_dataset(
              dataset, file.path(validated$output, "Step2_sage_database_search.rda")
            )
            shared_state$sage_search_bundle <- NULL
            shared_state$sage_search_parameters <- list()
          }
        })
        shiny::showNotification(
          "Sage search completed and the protein LFQ matrix and result tables were saved to ProtVis_dataset.",
          type = "message", duration = 5
        )
      }, error = function(e) {
        shiny::showNotification(paste("Sage search failed:", conditionMessage(e)),
                                type = "error", duration = NULL)
      })
    }, ignoreInit = TRUE)
  })
}
