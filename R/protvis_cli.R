# Headless command-line runner using the same ProtVis backend as Shiny.

.protvis_cli_parse <- function(args) {
  result <- list()
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (!startsWith(token, "--")) {
      i <- i + 1L
      next
    }
    token <- sub("^--", "", token)
    if (grepl("=", token, fixed = TRUE)) {
      pieces <- strsplit(token, "=", fixed = TRUE)[[1L]]
      key <- pieces[[1L]]
      value <- paste(pieces[-1L], collapse = "=")
    } else {
      key <- token
      if (i < length(args) && !startsWith(args[[i + 1L]], "--")) {
        value <- args[[i + 1L]]
        i <- i + 1L
      } else {
        value <- TRUE
      }
    }
    key <- gsub("-", "_", key, fixed = TRUE)
    result[[key]] <- value
    i <- i + 1L
  }
  result
}

#' Print ProtVis command-line usage.
#' @export
protvis_cli_help <- function() {
  text <- c(
    "ProtVis headless workflow",
    "",
    "Table input:",
    "  Rscript -e \"ProtVis::run_protvis_cli()\" --args --input proteins.tsv --source DIA-NN --output results",
    "",
    "Sage raw-data route:",
    "  Rscript -e \"ProtVis::run_protvis_cli()\" --args --fasta proteins.fasta --mzml-dir ./mzML --sample-info samples.csv --output results",
    "",
    "Resume:",
    "  Rscript -e \"ProtVis::run_protvis_cli()\" --args --resume results --output results",
    "",
    "Options:",
    "  --stages noise_correction,transformation,imputation,normalization,...",
    "  --config analysis.json",
    "  --no-downstream",
    "  --help"
  )
  cat(paste(text, collapse = "\n"), "\n")
  invisible(text)
}

.protvis_cli_read_sample_info <- function(path) {
  if (is.null(path) || !nzchar(as.character(path))) return(NULL)
  protvis_read_table(path, filename = basename(path))
}

#' Run ProtVis without the Shiny interface.
#'
#' The CLI uses the same import, Sage, checkpoint, processing, provenance, and
#' export functions as the GUI.
#' @export
run_protvis_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  options <- .protvis_cli_parse(args)
  if (isTRUE(options$help) || identical(options$help, "TRUE")) {
    protvis_cli_help()
    return(invisible(NULL))
  }

  config <- list()
  if (!is.null(options$config)) {
    config_path <- path.expand(as.character(options$config))
    if (!file.exists(config_path)) {
      stop("Config file does not exist: ", config_path, call. = FALSE)
    }
    config <- jsonlite::read_json(config_path, simplifyVector = TRUE)
  }

  output <- path.expand(as.character(
    options$output %||% config$output %||% getwd()
  ))
  if (!dir.exists(output)) dir.create(output, recursive = TRUE, showWarnings = FALSE)

  object <- NULL
  if (!is.null(options$resume)) {
    object <- restore_protvis_checkpoint(
      path.expand(as.character(options$resume))
    )
  } else if (!is.null(options$input)) {
    input <- path.expand(as.character(options$input))
    source <- as.character(
      options$source %||% config$source %||% "User-defined matrix"
    )
    object <- import_protvis(
      path = input, source = source,
      sample_info = .protvis_cli_read_sample_info(options$sample_info %||% NULL),
      filename = basename(input), auto_export = FALSE
    )
    object <- .protvis_record_file(object, input, kind = "quantification input")
    if (!is.null(options$sample_info)) {
      object <- .protvis_record_file(
        object, path.expand(as.character(options$sample_info)),
        kind = "sample metadata"
      )
    }
  } else if (!is.null(options$fasta) && !is.null(options$mzml_dir)) {
    fasta <- path.expand(as.character(options$fasta))
    mzml_dir <- path.expand(as.character(options$mzml_dir))
    sample_info_path <- options$sample_info %||% config$sample_info
    if (is.null(sample_info_path)) {
      stop("--sample-info is required for the Sage headless route.",
           call. = FALSE)
    }
    sample_info <- .protvis_cli_read_sample_info(
      path.expand(as.character(sample_info_path))
    )
    validated <- .protvis_sage_paths(
      fasta, mzml_dir, file.path(output, "Sage_search")
    )
    parameters <- utils::modifyList(
      .protvis_sage_default_parameters(),
      config$sage %||% list()
    )
    bundle <- run_sage_search(
      validated$fasta, validated$mzml, validated$output, parameters
    )
    if (!identical(bundle$status, "success")) {
      stop("Sage search failed. See Sage_search outputs.", call. = FALSE)
    }
    object <- .protvis_create_sage_dataset(
      bundle, sample_info, validated$mzml, parameters,
      validated$fasta, validated$output
    )
  } else {
    protvis_cli_help()
    stop(
      "Provide --input, --resume, or the Sage route (--fasta + --mzml-dir + --sample-info).",
      call. = FALSE
    )
  }

  object <- protvis_standardize_dataset(object)
  object$metadata$checkpoint_dir <- output
  object$checkpoint_info$directory <- output

  no_downstream <- isTRUE(options$no_downstream) ||
    identical(tolower(as.character(options$no_downstream %||% "")), "true")
  if (!no_downstream) {
    stages <- options$stages %||% config$stages %||% protvis_stage_order()
    if (length(stages) == 1L && is.character(stages)) {
      stages <- trimws(strsplit(stages, ",", fixed = TRUE)[[1L]])
      stages <- stages[nzchar(stages)]
    }
    params <- config$params %||% list()
    object <- run_protvis_pipeline(
      object, stages = stages, params = params,
      checkpoint_dir = output, continue_on_error = FALSE,
      stop_on_error = TRUE
    )
  }

  object <- protvis_standardize_dataset(object)
  final_dir <- file.path(output, "ProtVis_final")
  exported <- export_protvis_dataset(object, final_dir, include_raw = FALSE)
  report <- write_protvis_report(
    object, file.path(output, "ProtVis_report.html")
  )
  message("ProtVis headless workflow complete.")
  message("Export: ", exported)
  message("Report: ", report)
  invisible(object)
}
