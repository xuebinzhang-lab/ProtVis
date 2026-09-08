# Import and harmonise common proteomics tabular outputs.

.protvis_source_aliases <- c(
  maxquant = "MaxQuant",
  mq = "MaxQuant",
  proteomediscoverer = "Proteome Discoverer",
  proteome_discoverer = "Proteome Discoverer",
  pd = "Proteome Discoverer",
  diann = "DIA-NN",
  dia_nn = "DIA-NN",
  spectronaut = "Spectronaut",
  fragpipe = "FragPipe",
  skyline = "Skyline",
  openms = "OpenMS",
  userdefined = "User-defined matrix",
  user_defined_matrix = "User-defined matrix",
  raw = "User-defined matrix",
  mascot = "User-defined matrix"
)

.protvis_normalise_source <- function(source) {
  if (length(source) != 1L || is.na(source) || !nzchar(as.character(source))) {
    stop("A data source is required.", call. = FALSE)
  }
  key <- tolower(gsub("[^a-z0-9]+", "_", as.character(source)))
  key <- gsub("^_|_$", "", key)
  result <- if (key %in% names(.protvis_source_aliases)) {
    unname(.protvis_source_aliases[key])
  } else {
    NULL
  }
  if (is.null(result)) {
    choices <- unique(unname(.protvis_source_aliases))
    result <- choices[match(tolower(as.character(source)), tolower(choices))]
  }
  if (is.null(result) || is.na(result)) {
    stop("Unsupported data source: ", source, call. = FALSE)
  }
  result
}

#' Return the supported import sources and formats.
#' @export
protvis_supported_sources <- function() {
  data.frame(
    source = c("MaxQuant", "Proteome Discoverer", "DIA-NN", "Spectronaut",
               "FragPipe", "Skyline", "OpenMS", "User-defined matrix"),
    formats = c(
      ".xlsx, .xls, .txt, .tsv, .csv",
      ".xlsx, .xls, .csv, .txt, .tsv",
      ".parquet, .tsv, .txt, .csv",
      ".csv, .tsv, .xlsx, .xls",
      ".tsv, .txt, .csv",
      ".csv, .tsv, .xlsx, .xls",
      ".csv, .tsv, .xlsx, .xls, .mzTab",
      ".csv, .tsv, .xlsx, .xls, .parquet"
    ),
    stringsAsFactors = FALSE
  )
}

#' List the small, bundled demonstration files for each supported source.
#'
#' These fixtures intentionally contain only a few proteins and samples. They
#' are real-world-shaped exports, not synthetic wide matrices, so every file
#' can be used to exercise the source adapter and the downstream pipeline.
#' @return A data.frame with source, file, format, and provenance fields.
#' @export
protvis_builtin_datasets <- function() {
  data.frame(
    source = c("MaxQuant", "Proteome Discoverer", "DIA-NN", "Spectronaut",
               "FragPipe", "Skyline", "OpenMS", "OpenMS"),
    file = c(
      "Maxquant_Export.xlsx", "ProteomeDiscoverer_proteins.txt",
      "DIA-NN_report.tsv", "Spectronaut_report.tsv",
      "FragPipe_combined_protein.tsv", "Skyline_report.csv",
      "OpenMS_protein_quantification.tsv", "OpenMS_proteins.mzTab"
    ),
    format = c("xlsx", "txt", "tsv", "tsv", "tsv", "csv", "tsv", "mzTab"),
    description = c(
      "MaxQuant reporter-intensity protein export",
      "Proteome Discoverer protein result export",
      "DIA-NN long-format protein-group report",
      "Spectronaut long-format protein-group report",
      "FragPipe combined protein report",
      "Skyline protein report export",
      "OpenMS protein quantification table",
      "HUPO-PSI mzTab protein quantification export"
    ),
    reference = c(
      "https://www.maxquant.org/",
      "https://docs.thermofisher.com/r/Proteome-Discoverer-3.1-User-Guide/en-US1325293963v1",
      "https://github.com/vdemichev/DiaNN",
      "https://biognosys.com/software/spectronaut/",
      "https://fragpipe.nesvilab.org/docs/tutorial_fragpipe_outputs.html",
      "https://skyline.ms/",
      "https://openms.de/documentation/TOPP_ProteinQuantifier.html",
      "https://www.psidev.info/mztab-specifications"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.protvis_builtin_fixture_path <- function(source, format = NULL, file = NULL) {
  source <- .protvis_normalise_source(source)
  manifest <- protvis_builtin_datasets()
  row <- manifest[manifest$source == source, , drop = FALSE]
  if (!is.null(file)) row <- row[row$file == as.character(file), , drop = FALSE]
  if (!is.null(format)) row <- row[tolower(row$format) ==
                                     tolower(as.character(format)), , drop = FALSE]
  if (nrow(row) == 0L) stop("No built-in fixture is registered for ", source,
                            ".", call. = FALSE)
  installed <- system.file("extdata", row$file[[1L]], package = "ProtVis")
  candidates <- c(
    installed,
    file.path(getwd(), "inst", "extdata", row$file[[1L]]),
    file.path(getwd(), "..", "inst", "extdata", row$file[[1L]]),
    file.path(getwd(), "..", "..", "inst", "extdata", row$file[[1L]])
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0L) {
    stop("Built-in fixture not found for ", source, ": ", row$file[[1L]],
         call. = FALSE)
  }
  normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
}

.protvis_mztab_table <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines) == 0L) stop("The mzTab file is empty.", call. = FALSE)
  section <- grep("^(PRT|PSM|PEP)\\t", lines, value = TRUE)
  if (length(section) < 2L) {
    stop("No PRT, PSM, or PEP table was found in the mzTab file.",
         call. = FALSE)
  }
  section_name <- sub("\\t.*$", "", section)
  selected <- if (any(section_name == "PRT")) "PRT" else section_name[[1L]]
  section <- section[section_name == selected]
  rows <- strsplit(section, "\t", fixed = TRUE)
  header <- rows[[1L]][-1L]
  body <- rows[-1L]
  width <- length(header)
  values <- lapply(body, function(row) {
    row <- row[-1L]
    length(row) <- width
    row
  })
  out <- as.data.frame(do.call(rbind, values), stringsAsFactors = FALSE,
                        check.names = FALSE)
  names(out) <- header
  out
}

#' Read a supported tabular file using its original filename for extension
#' detection (important for Shiny temporary upload paths).
#' @export
protvis_read_table <- function(path, filename = NULL, sheet = 1L) {
  if (is.data.frame(path) || is.matrix(path)) return(.protvis_as_data_frame(path))
  if (length(path) != 1L || is.na(path) || !file.exists(path)) {
    stop("Input file does not exist.", call. = FALSE)
  }
  filename <- filename %||% basename(path)
  ext <- tolower(tools::file_ext(as.character(filename)))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package readxl is required for Excel files.", call. = FALSE)
    }
    return(as.data.frame(
      readxl::read_excel(path, sheet = sheet, .name_repair = "minimal",
                         guess_max = 100000),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  if (ext %in% c("parquet", "pq")) {
    if (!requireNamespace("arrow", quietly = TRUE)) {
      stop("Package arrow is required for parquet files.", call. = FALSE)
    }
    return(as.data.frame(arrow::read_parquet(path), stringsAsFactors = FALSE,
                         check.names = FALSE))
  }
  if (ext %in% c("mztab", "mztext")) return(.protvis_mztab_table(path))
  if (ext %in% c("tsv", "txt", "tab")) {
    return(as.data.frame(
      data.table::fread(path, sep = "\t", data.table = FALSE,
                        check.names = FALSE, showProgress = FALSE),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  if (ext %in% c("csv", "")) {
    return(as.data.frame(
      data.table::fread(path, sep = ",", data.table = FALSE,
                        check.names = FALSE, showProgress = FALSE),
      stringsAsFactors = FALSE, check.names = FALSE
    ))
  }
  stop("Unsupported file extension: .", ext, call. = FALSE)
}

.protvis_numeric_columns <- function(data, exclude = character(),
                                     preferred = character()) {
  candidates <- setdiff(names(data), exclude)
  if (length(candidates) == 0L) return(character())
  score <- vapply(data[candidates], function(column) {
    values <- .protvis_safe_numeric(column)
    sum(!is.na(values))
  }, numeric(1))
  candidates <- candidates[score > 0]
  if (length(preferred) > 0L) {
    preferred_hits <- candidates[grepl(
      paste(preferred, collapse = "|"), candidates, ignore.case = TRUE,
      perl = TRUE
    )]
    if (length(preferred_hits) > 0L) return(preferred_hits)
  }
  candidates
}

.protvis_flagged <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  text <- trimws(tolower(as.character(x)))
  !(is.na(text) | text == "" | text %in% c("0", "false", "no", "none", "na"))
}

.protvis_id_column <- function(data, source = "") {
  source <- tolower(source)
  patterns <- switch(
    source,
    "maxquant" = c("^protein ids?$", "^protein group$", "^majority protein ids?$",
                   "^accession$"),
    "proteome discoverer" = c("^master protein accessions?$",
                               "^protein accessions?$", "^accession$",
                               "^protein group id$", "^protein id$"),
    "dia-nn" = c("^protein[.]group$", "^protein group$", "^protein ids?$",
                 "^pg[.]protein"),
    "spectronaut" = c("^pg[.]proteingroups?$", "^protein groups?$",
                      "^protein ids?$", "^accession$"),
    "fragpipe" = c("^protein$", "^protein id$", "^protein group$",
                   "^accession$"),
    "skyline" = c("^proteinname$", "^protein[.]name$", "^protein$",
                  "^accession$"),
    "openms" = c("^proteinname$", "^protein_accessions?$",
                 "^protein accession$", "^accession$", "^protein$", "^id$"),
    c("^protein ids?$", "^protein[.]group$", "^protein$", "^accession$",
      "^id$", "^feature$")
  )
  .protvis_find_column(names(data), patterns)
}

.protvis_long_columns <- function(data, source = "") {
  source <- tolower(source)
  sample_patterns <- switch(
    source,
    "dia-nn" = c("^run$", "^file.name$", "^file$", "run", "sample"),
    "spectronaut" = c("^r[.]filename$", "^filename$", "^file$", "run", "sample"),
    "skyline" = c("^filename$", "^replicate$", "^run$", "sample", "file"),
    c("^sample_id$", "^sample$", "^run$", "^file$", "replicate",
      "filename", "sample")
  )
  intensity_patterns <- switch(
    source,
    "dia-nn" = c("pg[.]maxlfq", "precursor[.]normal", "protein[.]quantity",
                 "quantity", "intensity"),
    "spectronaut" = c("pg[.]quantity", "pg[.]normalized", "quantity",
                      "intensity", "area"),
    "skyline" = c("^area$", "totalarea", "peakarea", "intensity"),
    c("intensity", "abundance", "quantity", "area", "amount", "lfq",
      "normal")
  )
  list(
    sample = .protvis_find_column(names(data), sample_patterns),
    intensity = .protvis_find_column(names(data), intensity_patterns)
  )
}

.protvis_mean <- function(x) {
  x <- .protvis_safe_numeric(x)
  if (length(x) == 0L || all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
}

.protvis_long_to_wide <- function(data, id_col, sample_col, intensity_col) {
  ids <- trimws(as.character(data[[id_col]]))
  samples <- .protvis_clean_sample_name(data[[sample_col]])
  values <- .protvis_safe_numeric(data[[intensity_col]])
  keep <- !is.na(ids) & nzchar(ids) & !is.na(samples) & !is.na(values)
  if (!any(keep)) stop("The long-format table contains no numeric measurements.",
                       call. = FALSE)
  long <- data.frame(ID = sub(";.*$", "", ids[keep]),
                     Sample = samples[keep], Value = values[keep],
                     stringsAsFactors = FALSE)
  aggregate_value <- stats::aggregate(
    Value ~ ID + Sample, data = long, FUN = .protvis_mean
  )
  ids <- unique(aggregate_value$ID)
  samples <- unique(aggregate_value$Sample)
  result <- data.frame(ID = ids, stringsAsFactors = FALSE,
                       check.names = FALSE)
  for (sample in samples) {
    result[[sample]] <- NA_real_
    hit <- match(aggregate_value$ID[aggregate_value$Sample == sample], ids)
    result[[sample]][hit] <- aggregate_value$Value[
      aggregate_value$Sample == sample
    ]
  }
  result
}

.protvis_wide_expression <- function(data, id_col, abundance_cols) {
  if (is.null(id_col) || length(abundance_cols) == 0L) {
    stop(paste0("A protein identifier and at least one numeric sample column ",
                "are required."), call. = FALSE)
  }
  ids <- sub(";.*$", "", trimws(as.character(data[[id_col]])))
  result <- data.frame(ID = ids, stringsAsFactors = FALSE,
                       check.names = FALSE)
  names_out <- .protvis_clean_sample_name(abundance_cols)
  for (i in seq_along(abundance_cols)) {
    result[[names_out[[i]]]] <- .protvis_safe_numeric(
      data[[abundance_cols[[i]]]]
    )
  }
  result <- result[!is.na(result$ID) & nzchar(result$ID), , drop = FALSE]
  if (nrow(result) == 0L) stop("No valid protein identifiers were found.",
                               call. = FALSE)
  if (!anyDuplicated(result$ID)) return(result)
  # Aggregate duplicate proteins using the same semantics as the object.
  groups <- split(seq_len(nrow(result)), result$ID)
  collapsed <- lapply(groups, function(index) {
    row <- lapply(result[-1], function(column) .protvis_mean(column[index]))
    as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE)
  })
  collapsed <- do.call(rbind, collapsed)
  collapsed <- cbind(ID = names(groups), collapsed, stringsAsFactors = FALSE)
  names(collapsed) <- c("ID", names_out)
  collapsed
}

.protvis_variable_info_from_table <- function(data, id_col, protein_ids) {
  result <- data.frame(
    protein_id = protein_ids,
    accession = protein_ids,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (is.null(id_col)) return(result)
  gene_col <- .protvis_find_column(names(data),
                                   c("^gene$", "gene[ ._-]*name", "symbol"))
  description_col <- .protvis_find_column(
    names(data), c("^description$", "protein[ ._-]*name", "fasta[ ._-]*headers?")
  )
  raw_ids <- sub(";.*$", "", trimws(as.character(data[[id_col]])))
  index <- match(protein_ids, raw_ids)
  if (!is.null(gene_col)) result$gene <- as.character(data[[gene_col]])[index]
  if (!is.null(description_col)) {
    result$description <- as.character(data[[description_col]])[index]
  }
  result
}

.protvis_parse_table <- function(data, source) {
  id_col <- .protvis_id_column(data, source)
  if (is.null(id_col)) {
    # A user-defined matrix may use row names instead of an ID column.
    if (!is.null(rownames(data)) && nrow(data) > 0L) {
      data <- cbind(ID = rownames(data), data, stringsAsFactors = FALSE)
      id_col <- "ID"
    } else {
      stop("Could not find a protein identifier column for ", source, ".",
           call. = FALSE)
    }
  }
  long <- .protvis_long_columns(data, source)
  is_long <- !is.null(long$sample) && !is.null(long$intensity) &&
    !identical(long$sample, long$intensity) && long$sample != id_col
  if (is_long) {
    expression <- .protvis_long_to_wide(data, id_col, long$sample,
                                         long$intensity)
  } else {
    preferred <- switch(
      tolower(source),
      "maxquant" = c("^reporter intensity corrected", "^lfq intensity",
                     "^intensity", "^ibaq"),
      "proteome discoverer" = c("abundance", "area"),
      "dia-nn" = c("quantity", "intensity", "area"),
      "spectronaut" = c("quantity", "intensity", "area"),
      "fragpipe" = c("intensity", "lfq", "area"),
      "skyline" = c("area", "intensity"),
      "openms" = c("intensity", "abundance", "area", "^map_"),
      character()
    )
    abundance <- .protvis_numeric_columns(
      data, exclude = c(id_col, .protvis_find_column(
        names(data), c("reverse", "potential contaminant",
                       "only identified by site")
      )), preferred = preferred
    )
    # For MaxQuant, exclude flags even when they are numeric/logical and
    # explicitly prefer reporter/quantitative columns.
    if (identical(source, "MaxQuant")) {
      abundance <- setdiff(abundance, .protvis_find_column(
        names(data), c("^reverse$", "^potential contaminant$",
                       "^only identified by site$")
      ))
    }
    expression <- .protvis_wide_expression(data, id_col, abundance)
  }
  protein_ids <- expression$ID
  list(
    expression = expression,
    variable_info = .protvis_variable_info_from_table(data, id_col, protein_ids),
    raw_rows = nrow(data),
    retained_rows = nrow(expression)
  )
}

.protvis_parse_maxquant <- function(data, filters) {
  id_col <- .protvis_id_column(data, "MaxQuant")
  if (is.null(id_col)) stop("MaxQuant output requires a Protein IDs column.",
                            call. = FALSE)
  flag_patterns <- c(
    site = "^only identified by site$",
    reverse = "^reverse$",
    contaminant = "^potential contaminant$"
  )
  keep <- rep(TRUE, nrow(data))
  removed_by_flag <- stats::setNames(integer(length(flag_patterns)),
                                     names(flag_patterns))
  for (flag in intersect(as.character(filters), names(flag_patterns))) {
    column <- .protvis_find_column(names(data), flag_patterns[[flag]])
    if (!is.null(column)) {
      flagged <- .protvis_flagged(data[[column]])
      removed_by_flag[[flag]] <- sum(keep & flagged)
      keep <- keep & !flagged
    }
  }
  filtered <- data[keep, , drop = FALSE]
  if (nrow(filtered) == 0L) {
    stop("MaxQuant filtering removed every row. Relax the selected flags.",
         call. = FALSE)
  }
  parsed <- .protvis_parse_table(filtered, "MaxQuant")
  parsed$raw_rows <- nrow(data)
  parsed$removed_rows <- sum(!keep)
  parsed$removed_by_flag <- removed_by_flag
  parsed
}

#' Import a supported table into a ProtVis_dataset.
#'
#' @param path File path or an in-memory data.frame/matrix.
#' @param source One of the sources returned by protvis_supported_sources().
#' @param sample_info Optional sample metadata table.
#' @param filename Original filename when path is a Shiny temporary upload.
#' @param maxquant_filters MaxQuant flags to remove: site, reverse, contaminant.
#' @param sheet Excel sheet number or name.
#' @return A ProtVis_dataset.
#' @export
import_protvis <- function(path = NULL, source = "MaxQuant",
                            sample_info = NULL, filename = NULL,
                            maxquant_filters = c("site", "reverse", "contaminant"),
                            sheet = 1L) {
  source <- .protvis_normalise_source(source)
  if (is.null(path)) {
    if (identical(source, "MaxQuant")) path <- protvis_builtin_data_path()
    else stop("An input path is required for ", source, ".", call. = FALSE)
  }
  if (is.data.frame(path) || is.matrix(path)) {
    data <- .protvis_as_data_frame(path)
    filename <- filename %||% "in_memory.csv"
  } else {
    data <- protvis_read_table(path, filename = filename, sheet = sheet)
    filename <- filename %||% basename(path)
  }
  parsed <- if (identical(source, "MaxQuant")) {
    .protvis_parse_maxquant(data, maxquant_filters)
  } else {
    .protvis_parse_table(data, source)
  }
  metadata <- list(
    source = source,
    filename = as.character(filename),
    imported_at = as.character(Sys.time()),
    raw_rows = parsed$raw_rows,
    retained_rows = parsed$retained_rows,
    removed_rows = parsed$removed_rows %||% 0L,
    removed_by_flag = parsed$removed_by_flag %||% integer()
  )
  object <- create_protvis_dataset(
    parsed$expression,
    sample_info = sample_info,
    variable_info = parsed$variable_info,
    metadata = metadata
  )
  object <- .protvis_append_process(
    object, "import", status = "success",
    parameters = list(source = source, filename = filename,
                      maxquant_filters = maxquant_filters),
    message = paste0("Imported ", parsed$retained_rows,
                     " protein rows from ", source, ".")
  )
  if (!is.data.frame(path) && !is.matrix(path) && file.exists(path)) {
    object <- attach_protvis_file(object, path, name = filename,
                                  kind = "imported")
  }
  object
}

#' Resolve the bundled MaxQuant workbook.
#' @export
protvis_builtin_data_path <- function(source = "MaxQuant", format = NULL) {
  if (!identical(.protvis_normalise_source(source), "MaxQuant")) {
    return(.protvis_builtin_fixture_path(source, format = format))
  }
  installed <- system.file("extdata", "Maxquant_Export.xlsx", package = "ProtVis")
  if (nzchar(installed) && file.exists(installed)) {
    return(normalizePath(installed, winslash = "/", mustWork = TRUE))
  }
  candidates <- c(
    file.path(getwd(), "inst", "extdata", "Maxquant_Export.xlsx"),
    file.path(getwd(), "Maxquant_Export.xlsx"),
    file.path(getwd(), "..", "inst", "extdata", "Maxquant_Export.xlsx"),
    file.path(getwd(), "..", "..", "inst", "extdata", "Maxquant_Export.xlsx")
  )
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) > 0L) {
    return(normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE))
  }
  stop(paste0("Bundled Maxquant_Export.xlsx was not found. Install the ",
              "package or place it in inst/extdata."), call. = FALSE)
}

#' Load a bundled demonstration dataset.
#'
#' @param source Source name returned by protvis_builtin_datasets().
#' @export
load_protvis_builtin_data <- function(sample_info = NULL, source = "MaxQuant",
                                      format = NULL, file = NULL) {
  source <- .protvis_normalise_source(source)
  path <- if (!is.null(file)) {
    .protvis_builtin_fixture_path(source, file = file)
  } else {
    protvis_builtin_data_path(source, format = format)
  }
  manifest <- protvis_builtin_datasets()
  manifest_rows <- manifest[manifest$source == source, , drop = FALSE]
  file_name <- if (!is.null(file)) {
    as.character(file)
  } else if (!is.null(format)) {
    manifest_rows$file[match(tolower(as.character(format)),
                             tolower(manifest_rows$format))]
  } else {
    manifest_rows$file[[1L]]
  }
  object <- import_protvis(path, source = source,
                 sample_info = sample_info,
                 filename = file_name)
  object$metadata$builtin_fixture <- TRUE
  object$metadata$builtin_reference <- manifest$reference[match(
    source, manifest$source
  )]
  object
}

#' Compatibility alias for import_protvis.
#' @export
import_data <- function(...) import_protvis(...)
