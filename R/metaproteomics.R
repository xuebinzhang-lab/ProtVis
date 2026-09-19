utils::globalVariables(c(
  ".data", "Sample", "Group", "ProteinID", "Peptide", "Intensity",
  "Category", "Taxon", "Function", "value", "Weight", "WeightedIntensity"
))

.mp_first_existing <- function(x, candidates) {
  hit <- candidates[candidates %in% names(x)]
  if (length(hit)) hit[[1L]] else NULL
}

.mp_nonempty_character <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x
}

.mp_collapse_values <- function(x) {
  x <- unique(.mp_nonempty_character(x))
  x <- x[!is.na(x)]
  if (!length(x)) NA_character_ else paste(x, collapse = ";")
}

.mp_normalise_id_table <- function(x, id_candidates = c(
    "ProteinID", "protein_id", "Protein", "protein", "Accession",
    "accession", "ID", "id", "variable_id"
  )) {
  if (is.null(x)) return(data.frame())
  x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  if (!nrow(x)) return(x)
  id_col <- .mp_first_existing(x, id_candidates)
  if (is.null(id_col)) {
    stop(
      "Annotation table needs a protein identifier column such as ProteinID, protein_id, Accession, or ID.",
      call. = FALSE
    )
  }
  names(x)[names(x) == id_col] <- "ProteinID"
  x$ProteinID <- .mp_nonempty_character(x$ProteinID)
  x <- x[!is.na(x$ProteinID), , drop = FALSE]
  if (!nrow(x)) return(x)

  if (anyDuplicated(x$ProteinID)) {
    value_cols <- setdiff(names(x), "ProteinID")
    rows <- split(seq_len(nrow(x)), x$ProteinID)
    collapsed <- lapply(names(rows), function(id) {
      idx <- rows[[id]]
      out <- data.frame(ProteinID = id, stringsAsFactors = FALSE)
      for (nm in value_cols) out[[nm]] <- .mp_collapse_values(x[[nm]][idx])
      out
    })
    x <- do.call(rbind, collapsed)
    rownames(x) <- NULL
  }
  x
}

.mp_sample_info <- function(sample_ids, sample_info = NULL) {
  sample_ids <- as.character(sample_ids)
  if (is.null(sample_info) || !NROW(sample_info)) {
    inferred <- sub(
      "([._-](rep|r|c)?[0-9]+)$", "", sample_ids,
      ignore.case = TRUE, perl = TRUE
    )
    inferred[!nzchar(inferred) | inferred == sample_ids] <- "Group1"
    return(data.frame(
      sample_id = sample_ids,
      class = inferred,
      group = inferred,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  sample_info <- as.data.frame(
    sample_info, stringsAsFactors = FALSE, check.names = FALSE
  )
  id_col <- .mp_first_existing(
    sample_info, c("sample_id", "Sample", "sample", "SampleID", "sample_name")
  )
  if (is.null(id_col)) {
    stop("Sample metadata must contain sample_id or Sample.", call. = FALSE)
  }
  group_col <- .mp_first_existing(
    sample_info, c("group", "Group", "class", "Class", "condition", "Condition")
  )
  sample_info$sample_id <- as.character(sample_info[[id_col]])
  if (is.null(group_col)) {
    sample_info$group <- "Group1"
  } else {
    sample_info$group <- as.character(sample_info[[group_col]])
  }
  sample_info$group[is.na(sample_info$group) | !nzchar(sample_info$group)] <- "Group1"
  sample_info$class <- sample_info$group
  sample_info <- sample_info[match(sample_ids, sample_info$sample_id), , drop = FALSE]
  if (anyNA(sample_info$sample_id)) {
    missing <- sample_ids[is.na(sample_info$sample_id)]
    stop(
      "Sample metadata does not cover: ", paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  sample_info$sample_id <- sample_ids
  sample_info
}

.mp_abundance_table_from_dataset <- function(dataset) {
  dataset <- as_protvis_dataset(dataset)
  expression <- as.data.frame(
    dataset$expression_data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  data.frame(
    ProteinID = rownames(expression),
    expression,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    row.names = NULL
  )
}

.mp_taxonomy_from_dataset <- function(dataset) {
  dataset <- as_protvis_dataset(dataset)
  info <- as.data.frame(
    dataset$variable_info,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  id_col <- .mp_first_existing(info, c("protein_id", "variable_id"))
  tax_cols <- intersect(
    c(
      "Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
      "Species", "Strain", "Taxon", "Organism", "organism"
    ),
    names(info)
  )
  if (is.null(id_col) || !length(tax_cols)) return(data.frame())
  out <- info[, c(id_col, tax_cols), drop = FALSE]
  names(out)[1L] <- "ProteinID"
  .mp_normalise_id_table(out)
}

.mp_function_from_dataset <- function(dataset) {
  dataset <- as_protvis_dataset(dataset)
  info <- as.data.frame(
    dataset$variable_info,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  id_col <- .mp_first_existing(info, c("protein_id", "variable_id"))
  pattern <- paste(
    c("KO", "KEGG", "Pathway", "COG", "GO", "eggNOG", "CAZy", "EC",
      "Function", "Description"),
    collapse = "|"
  )
  value_cols <- grep(pattern, names(info), ignore.case = TRUE, value = TRUE)
  tables <- list()
  if (!is.null(id_col) && length(value_cols)) {
    tmp <- info[, unique(c(id_col, value_cols)), drop = FALSE]
    names(tmp)[1L] <- "ProteinID"
    tables[[length(tables) + 1L]] <- .mp_normalise_id_table(tmp)
  }

  ann <- dataset$annotation
  if (is.list(ann)) {
    for (nm in names(ann)) {
      tab <- ann[[nm]]
      if (!is.data.frame(tab) || !nrow(tab)) next
      ann_id <- .mp_first_existing(
        tab,
        c("ProteinID", "protein_id", "Protein", "Accession", "ID", "variable_id")
      )
      if (is.null(ann_id)) next
      keep <- unique(c(
        ann_id,
        grep(pattern, names(tab), ignore.case = TRUE, value = TRUE)
      ))
      if (length(keep) < 2L) {
        keep <- names(tab)
      }
      tmp <- tab[, keep, drop = FALSE]
      names(tmp)[names(tmp) == ann_id] <- "ProteinID"
      tmp <- .mp_normalise_id_table(tmp)
      extra <- setdiff(names(tmp), "ProteinID")
      duplicate_names <- extra[extra %in% unlist(lapply(tables, names), use.names = FALSE)]
      if (length(duplicate_names)) {
        names(tmp)[match(duplicate_names, names(tmp))] <- paste0(nm, "_", duplicate_names)
      }
      tables[[length(tables) + 1L]] <- tmp
    }
  }

  if (!length(tables)) return(data.frame())
  Reduce(function(x, y) merge(x, y, by = "ProteinID", all = TRUE), tables)
}

.mp_peptide_from_dataset <- function(dataset) {
  dataset <- as_protvis_dataset(dataset)
  peptide <- tryCatch(protvis_assay(dataset, "peptide"), error = function(e) NULL)
  if (is.null(peptide)) return(data.frame())
  as.data.frame(peptide, stringsAsFactors = FALSE, check.names = FALSE)
}

.mp_data_from_dataset <- function(dataset) {
  dataset <- as_protvis_dataset(dataset)
  list(
    abundance = .mp_abundance_table_from_dataset(dataset),
    sample_info = as.data.frame(
      dataset$sample_info,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    taxonomy = .mp_taxonomy_from_dataset(dataset),
    function = .mp_function_from_dataset(dataset),
    peptide = .mp_peptide_from_dataset(dataset)
  )
}

.mp_tax_levels <- function(taxonomy) {
  preferred <- c(
    "Domain", "Kingdom", "Phylum", "Class", "Order", "Family", "Genus",
    "Species", "Strain", "Taxon", "Organism"
  )
  current <- names(taxonomy)
  idx <- match(tolower(preferred), tolower(current))
  current[idx[!is.na(idx)]]
}

.mp_function_levels <- function(function_table, peptide = NULL) {
  ignore <- c(
    "ProteinID", "protein_id", "Protein", "Proteins", "Peptide", "Sequence",
    "peptide", "sequence", "Sample", "sample_id", "Group", "Intensity"
  )
  cols <- setdiff(names(function_table), ignore)
  if (!is.null(peptide) && NROW(peptide)) {
    peptide <- as.data.frame(peptide, stringsAsFactors = FALSE, check.names = FALSE)
    extra <- setdiff(
      names(peptide),
      c(
        ignore,
        grep(
          "intensity|abundance|quantity|area|lfq",
          names(peptide), ignore.case = TRUE, value = TRUE
        )
      )
    )
    numeric_like <- vapply(
      peptide[extra],
      function(x) {
        value <- suppressWarnings(as.numeric(as.character(x)))
        mean(is.finite(value)) >= 0.8
      },
      logical(1)
    )
    extra <- extra[!numeric_like]
    cols <- unique(c(cols, extra))
  }
  cols[nzchar(cols)]
}

.mp_abundance_long <- function(abundance, sample_info = NULL) {
  abundance <- as.data.frame(
    abundance, stringsAsFactors = FALSE, check.names = FALSE
  )
  id_col <- .mp_first_existing(
    abundance,
    c("ProteinID", "protein_id", "Protein", "Accession", "ID", "variable_id")
  )
  if (is.null(id_col)) {
    stop("Abundance table must contain ProteinID or an equivalent protein ID column.", call. = FALSE)
  }
  names(abundance)[names(abundance) == id_col] <- "ProteinID"
  sample_cols <- setdiff(names(abundance), "ProteinID")
  if (!length(sample_cols)) {
    stop("Abundance table must contain at least one sample column.", call. = FALSE)
  }

  numeric_values <- lapply(abundance[sample_cols], function(x) {
    suppressWarnings(as.numeric(gsub(",", "", as.character(x), fixed = TRUE)))
  })
  keep <- vapply(numeric_values, function(x) any(is.finite(x)), logical(1))
  sample_cols <- sample_cols[keep]
  if (!length(sample_cols)) {
    stop("No numeric abundance sample columns were detected.", call. = FALSE)
  }
  abundance[sample_cols] <- numeric_values[sample_cols]

  info <- .mp_sample_info(sample_cols, sample_info)
  long <- tidyr::pivot_longer(
    abundance[, c("ProteinID", sample_cols), drop = FALSE],
    cols = tidyselect::all_of(sample_cols),
    names_to = "Sample",
    values_to = "Intensity"
  )
  long$ProteinID <- as.character(long$ProteinID)
  long$Intensity <- suppressWarnings(as.numeric(long$Intensity))
  long <- long[is.finite(long$Intensity), , drop = FALSE]
  long$Group <- info$group[match(long$Sample, info$sample_id)]
  list(long = long, sample_info = info, abundance = abundance)
}

.mp_expand_weighted <- function(df, category, output = "Category") {
  if (!category %in% names(df)) return(data.frame())
  x <- df
  x[[output]] <- .mp_nonempty_character(x[[category]])
  x <- x[!is.na(x[[output]]), , drop = FALSE]
  if (!nrow(x)) return(x)
  x$.mp_original_row <- seq_len(nrow(x))
  x <- tidyr::separate_rows(
    x,
    tidyselect::all_of(output),
    sep = "\\s*[;|]\\s*"
  )
  x[[output]] <- trimws(as.character(x[[output]]))
  x <- x[nzchar(x[[output]]), , drop = FALSE]
  counts <- table(x$.mp_original_row)
  x$Weight <- 1 / as.numeric(counts[as.character(x$.mp_original_row)])
  x$WeightedIntensity <- x$Intensity * x$Weight
  x
}

.mp_top_categories <- function(summary, top_n = 10L, relative = TRUE) {
  if (!nrow(summary)) return(summary)
  top_n <- max(1L, as.integer(top_n[[1L]]))
  totals <- summary |>
    dplyr::group_by(.data$Category) |>
    dplyr::summarise(Total = sum(.data$Intensity, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$Total)) |>
    dplyr::slice_head(n = top_n)
  keep <- as.character(totals$Category)
  summary$Category <- ifelse(summary$Category %in% keep, summary$Category, "Other")
  summary <- summary |>
    dplyr::group_by(.data$Sample, .data$Group, .data$Category) |>
    dplyr::summarise(Intensity = sum(.data$Intensity, na.rm = TRUE), .groups = "drop")
  if (isTRUE(relative)) {
    summary <- summary |>
      dplyr::group_by(.data$Sample) |>
      dplyr::mutate(
        Intensity = ifelse(
          sum(.data$Intensity, na.rm = TRUE) > 0,
          .data$Intensity / sum(.data$Intensity, na.rm = TRUE),
          0
        )
      ) |>
      dplyr::ungroup()
  }
  summary
}

.mp_category_summary <- function(merged, category, top_n = 10L, relative = TRUE) {
  expanded <- .mp_expand_weighted(merged, category, "Category")
  if (!nrow(expanded)) {
    return(data.frame(
      Sample = character(), Group = character(), Category = character(),
      Intensity = numeric(), stringsAsFactors = FALSE
    ))
  }
  summary <- expanded |>
    dplyr::group_by(.data$Sample, .data$Group, .data$Category) |>
    dplyr::summarise(
      Intensity = sum(.data$WeightedIntensity, na.rm = TRUE),
      .groups = "drop"
    )
  .mp_top_categories(summary, top_n = top_n, relative = relative)
}

.mp_taxon_function_summary <- function(merged, tax_level, function_level) {
  if (!all(c(tax_level, function_level) %in% names(merged))) {
    return(data.frame())
  }
  tax <- .mp_expand_weighted(merged, tax_level, "Taxon")
  if (!nrow(tax)) return(data.frame())
  tax$Intensity <- tax$WeightedIntensity
  fun <- .mp_expand_weighted(tax, function_level, "Function")
  if (!nrow(fun)) return(data.frame())
  fun |>
    dplyr::group_by(.data$Sample, .data$Group, .data$Taxon, .data$Function) |>
    dplyr::summarise(
      Intensity = sum(.data$WeightedIntensity, na.rm = TRUE),
      .groups = "drop"
    )
}

.mp_prepare_peptide_long <- function(peptide, sample_info, function_table, function_level) {
  if (is.null(peptide) || !NROW(peptide)) return(data.frame())
  peptide <- as.data.frame(peptide, stringsAsFactors = FALSE, check.names = FALSE)
  peptide_col <- .mp_first_existing(
    peptide, c("Peptide", "Sequence", "peptide", "sequence", "Stripped.Sequence")
  )
  if (is.null(peptide_col)) return(data.frame())
  names(peptide)[names(peptide) == peptide_col] <- "Peptide"

  if (!function_level %in% names(peptide)) {
    protein_col <- .mp_first_existing(
      peptide,
      c("ProteinID", "protein_id", "Protein", "Proteins", "Protein.Group")
    )
    if (!is.null(protein_col) && NROW(function_table)) {
      names(peptide)[names(peptide) == protein_col] <- "ProteinID"
      fun <- .mp_normalise_id_table(function_table)
      if (function_level %in% names(fun)) {
        peptide <- dplyr::left_join(
          peptide,
          fun[, c("ProteinID", function_level), drop = FALSE],
          by = "ProteinID"
        )
      }
    }
  }
  if (!function_level %in% names(peptide)) return(data.frame())

  sample_col <- .mp_first_existing(peptide, c("Sample", "sample_id", "Run", "run"))
  intensity_col <- .mp_first_existing(
    peptide,
    c("Intensity", "intensity", "Abundance", "Quantity", "Area", "LFQ")
  )

  if (!is.null(sample_col) && !is.null(intensity_col)) {
    names(peptide)[names(peptide) == sample_col] <- "Sample"
    names(peptide)[names(peptide) == intensity_col] <- "Intensity"
    long <- peptide
  } else {
    sample_ids <- as.character(sample_info$sample_id)
    wide_samples <- intersect(sample_ids, names(peptide))
    if (!length(wide_samples)) {
      numeric_candidates <- setdiff(
        names(peptide),
        c("Peptide", "ProteinID", function_level)
      )
      numeric_candidates <- numeric_candidates[vapply(
        peptide[numeric_candidates],
        function(x) any(is.finite(suppressWarnings(as.numeric(as.character(x))))),
        logical(1)
      )]
      wide_samples <- numeric_candidates
    }
    if (!length(wide_samples)) return(data.frame())
    long <- tidyr::pivot_longer(
      peptide,
      cols = tidyselect::all_of(wide_samples),
      names_to = "Sample",
      values_to = "Intensity"
    )
  }

  long$Intensity <- suppressWarnings(as.numeric(long$Intensity))
  long <- long[is.finite(long$Intensity), , drop = FALSE]
  long$Group <- sample_info$group[match(long$Sample, sample_info$sample_id)]
  long$Group[is.na(long$Group)] <- "Group1"
  long
}

.mp_peptide_function <- function(peptide, sample_info, function_table, function_level) {
  long <- .mp_prepare_peptide_long(
    peptide, sample_info, function_table, function_level
  )
  if (!nrow(long)) {
    return(list(scores = data.frame(), differential = data.frame()))
  }
  expanded <- .mp_expand_weighted(long, function_level, "Function")
  if (!nrow(expanded)) {
    return(list(scores = data.frame(), differential = data.frame()))
  }
  scores <- expanded |>
    dplyr::group_by(.data$Sample, .data$Group, .data$Function) |>
    dplyr::summarise(
      Intensity = sum(.data$WeightedIntensity, na.rm = TRUE),
      Peptides = dplyr::n_distinct(.data$Peptide),
      .groups = "drop"
    )

  groups <- unique(as.character(scores$Group))
  differential <- data.frame()
  if (length(groups) >= 2L) {
    g1 <- groups[[1L]]
    g2 <- groups[[2L]]
    group_summary <- scores |>
      dplyr::group_by(.data$Function, .data$Group) |>
      dplyr::summarise(
        MeanIntensity = mean(.data$Intensity, na.rm = TRUE),
        .groups = "drop"
      )
    left <- group_summary[group_summary$Group == g1, c("Function", "MeanIntensity")]
    right <- group_summary[group_summary$Group == g2, c("Function", "MeanIntensity")]
    names(left)[2L] <- "Mean1"
    names(right)[2L] <- "Mean2"
    differential <- merge(left, right, by = "Function", all = TRUE)
    differential$Mean1[is.na(differential$Mean1)] <- 0
    differential$Mean2[is.na(differential$Mean2)] <- 0
    differential$log2FC <- log2((differential$Mean2 + 1) / (differential$Mean1 + 1))
    differential$comparison <- paste(g2, "vs", g1)
  }
  list(scores = scores, differential = differential)
}

.mp_prepare_analysis <- function(dat, tax_level, function_level,
                                 relative = TRUE, top_n = 10L) {
  abundance <- .mp_abundance_long(dat$abundance, dat$sample_info)
  taxonomy <- .mp_normalise_id_table(dat$taxonomy)
  function_table <- .mp_normalise_id_table(dat$function)

  if (!tax_level %in% names(taxonomy)) {
    stop("Selected taxonomic level is not available: ", tax_level, call. = FALSE)
  }
  if (!function_level %in% names(function_table) &&
      !(NROW(dat$peptide) && function_level %in% names(dat$peptide))) {
    stop("Selected function level is not available: ", function_level, call. = FALSE)
  }

  merged <- abundance$long
  if (nrow(taxonomy)) merged <- dplyr::left_join(merged, taxonomy, by = "ProteinID")
  if (nrow(function_table)) {
    duplicated_cols <- intersect(
      setdiff(names(function_table), "ProteinID"),
      setdiff(names(merged), c("ProteinID", "Sample", "Group", "Intensity"))
    )
    if (length(duplicated_cols)) {
      names(function_table)[match(duplicated_cols, names(function_table))] <-
        paste0("Function_", duplicated_cols)
      if (function_level %in% duplicated_cols) {
        function_level <- paste0("Function_", function_level)
      }
    }
    merged <- dplyr::left_join(merged, function_table, by = "ProteinID")
  }

  taxonomy_summary <- .mp_category_summary(
    merged, tax_level, top_n = top_n, relative = relative
  )
  function_summary <- .mp_category_summary(
    merged, function_level, top_n = top_n, relative = relative
  )
  taxon_function <- .mp_taxon_function_summary(
    merged, tax_level, function_level
  )
  peptide_function <- .mp_peptide_function(
    dat$peptide,
    abundance$sample_info,
    function_table,
    function_level
  )

  list(
    parameters = list(
      tax_level = tax_level,
      function_level = function_level,
      relative_abundance = isTRUE(relative),
      top_n = as.integer(top_n)
    ),
    inputs = list(
      abundance = abundance$abundance,
      sample_info = abundance$sample_info,
      taxonomy = taxonomy,
      function = function_table,
      peptide = as.data.frame(
        dat$peptide %||% data.frame(),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    ),
    tables = list(
      protein_long = abundance$long,
      merged = merged,
      taxonomy_composition = taxonomy_summary,
      function_composition = function_summary,
      taxon_function = taxon_function,
      peptide_function_scores = peptide_function$scores,
      peptide_function_differential = peptide_function$differential
    )
  )
}

.mp_result_run <- function(result, source = "Metaproteomics") {
  run_id <- paste0(
    "metaproteomics_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    "_",
    sprintf("%04d", sample.int(9999L, 1L))
  )
  c(
    list(
      run_id = run_id,
      created_at = as.character(Sys.time()),
      source = source,
      design = list(
        inspiration = c("conduitR", "QFeatures", "pepFunk", "metaprotr"),
        model = "protein-taxonomy-function plus peptide-centric function",
        storage = "append-only"
      )
    ),
    result
  )
}

.mp_append_run <- function(dataset, run) {
  dataset <- protvis_standardize_dataset(dataset)
  if (exists(".protvis_append_analysis_run", mode = "function")) {
    return(.protvis_append_analysis_run(
      dataset,
      module = "metaproteomics",
      run = run,
      run_id = run$run_id,
      parameters = run$parameters
    ))
  }

  root <- dataset$analysis_results$metaproteomics %||% list()
  runs <- root$runs %||% list()
  id <- as.character(run$run_id)
  if (id %in% names(runs)) {
    id <- paste0(id, "_", length(runs) + 1L)
    run$run_id <- id
  }
  runs[[id]] <- run
  root$runs <- runs
  root$latest_run_id <- id
  root$n_runs <- length(runs)
  dataset$analysis_results$metaproteomics <- root
  dataset <- .protvis_append_process(
    dataset,
    "metaproteomics",
    status = "success",
    parameters = run$parameters
  )
  dataset
}

.mp_dataset_from_run <- function(run) {
  abundance <- run$inputs$abundance
  info <- run$inputs$sample_info
  samples <- intersect(as.character(info$sample_id), names(abundance))
  matrix_data <- as.data.frame(
    abundance[, samples, drop = FALSE],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  matrix_data[] <- lapply(matrix_data, as.numeric)
  rownames(matrix_data) <- as.character(abundance$ProteinID)
  create_protvis_dataset(
    expression_data = matrix_data,
    sample_info = info,
    metadata = list(
      source = "Metaproteomics",
      object_name = "ProtVis_dataset__metaproteomics__v1",
      object_version = 1L
    )
  )
}

#' Built-in demo data for the metaproteomics module
#'
#' The demo includes protein abundance, sample metadata, taxonomy, protein
#' function annotations, and a peptide-centric table.
#'
#' @return A named list containing abundance, sample_info, taxonomy, function,
#'   and peptide tables.
#' @export
metaproteomics_demo_data <- function() {
  abundance <- data.frame(
    ProteinID = paste0("MP", sprintf("%03d", 1:12)),
    Control_1 = c(18, 30, 11, 24, 15, 19, 8, 12, 32, 21, 15, 10),
    Control_2 = c(20, 27, 12, 22, 17, 18, 9, 11, 30, 19, 16, 9),
    Control_3 = c(17, 29, 10, 25, 14, 20, 7, 13, 31, 20, 15, 11),
    Treatment_1 = c(29, 14, 22, 13, 26, 28, 19, 9, 16, 30, 25, 21),
    Treatment_2 = c(31, 16, 24, 12, 28, 26, 20, 10, 15, 32, 24, 19),
    Treatment_3 = c(30, 15, 23, 14, 27, 27, 18, 8, 17, 31, 26, 20),
    check.names = FALSE
  )

  sample_info <- data.frame(
    sample_id = names(abundance)[-1L],
    class = rep(c("Control", "Treatment"), each = 3L),
    group = rep(c("Control", "Treatment"), each = 3L),
    stringsAsFactors = FALSE
  )

  taxonomy <- data.frame(
    ProteinID = abundance$ProteinID,
    Phylum = c(
      "Firmicutes", "Bacteroidota", "Proteobacteria", "Actinobacteriota",
      "Firmicutes", "Firmicutes", "Bacteroidota", "Proteobacteria",
      "Actinobacteriota", "Firmicutes", "Bacteroidota", "Proteobacteria"
    ),
    Genus = c(
      "Faecalibacterium", "Bacteroides", "Escherichia", "Bifidobacterium",
      "Roseburia", "Lactobacillus", "Prevotella", "Pseudomonas",
      "Collinsella", "Ruminococcus", "Alistipes", "Klebsiella"
    ),
    Species = c(
      "F. prausnitzii", "B. vulgatus", "E. coli", "B. longum",
      "R. intestinalis", "L. plantarum", "P. copri", "P. aeruginosa",
      "C. aerofaciens", "R. bromii", "A. putredinis", "K. pneumoniae"
    ),
    check.names = FALSE
  )

  function_table <- data.frame(
    ProteinID = abundance$ProteinID,
    KO = c(
      "K01689", "K01810", "K00844", "K01190", "K01803", "K00174",
      "K00626", "K02003", "K00927", "K01834", "K01647", "K01915"
    ),
    Pathway = c(
      "Butanoate metabolism", "Glycolysis / Gluconeogenesis", "Carbon metabolism",
      "Starch and sucrose metabolism", "Pyruvate metabolism", "Methane metabolism",
      "Amino sugar metabolism", "ABC transporters", "Purine metabolism",
      "Propanoate metabolism", "TCA cycle", "Fatty acid biosynthesis"
    ),
    COG = c(
      "Energy production", "Carbohydrate transport", "Carbohydrate transport",
      "Carbohydrate transport", "Energy production", "Energy production",
      "Cell wall biogenesis", "Transport", "Nucleotide metabolism",
      "Energy production", "Energy production", "Lipid metabolism"
    ),
    CAZy = c(
      "GH13", "GH2", "GT4", "GH3", "CE1", "GT2",
      "GH18", "AA3", "GT5", "CE4", "GH5", "GT28"
    ),
    check.names = FALSE
  )

  peptide <- data.frame(
    Peptide = paste0("PEPTIDE", sprintf("%02d", 1:18)),
    ProteinID = abundance$ProteinID[c(1:12, 1:6)],
    Pathway = function_table$Pathway[c(1:12, 1:6)],
    KO = function_table$KO[c(1:12, 1:6)],
    Control_1 = c(8:19, 6:11),
    Control_2 = c(9:20, 5:10),
    Control_3 = c(7:18, 7:12),
    Treatment_1 = c(14:25, 8:13),
    Treatment_2 = c(15:26, 9:14),
    Treatment_3 = c(13:24, 10:15),
    check.names = FALSE
  )

  list(
    abundance = abundance,
    sample_info = sample_info,
    taxonomy = taxonomy,
    function = function_table,
    peptide = peptide
  )
}

#' Metaproteomics UI Module
#'
#' @param id A unique module id.
#' @return A Shiny UI definition.
#' @import shiny
#' @import bslib
#' @export
metaproteomics_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 360,
        bslib::accordion(
          open = c("Data Input", "Analysis Settings"),
          bslib::accordion_panel(
            "Data Input",
            shiny::p(
              "Use the active ProtVis_dataset or upload metaproteomics tables. Completed runs are appended to the active dataset when available."
            ),
            shiny::actionButton(
              ns("load_active_dataset"),
              "Use Active ProtVis_dataset",
              class = "btn btn-primary w-100"
            ),
            shiny::br(), shiny::br(),
            shiny::fileInput(ns("abundance_file"), "Protein abundance CSV", accept = ".csv"),
            shiny::fileInput(ns("sample_info_file"), "Sample metadata CSV (optional)", accept = ".csv"),
            shiny::fileInput(ns("taxonomy_file"), "Taxonomy annotation CSV", accept = ".csv"),
            shiny::fileInput(ns("function_file"), "Function annotation CSV", accept = ".csv"),
            shiny::fileInput(
              ns("peptide_file"),
              "Peptide-centric table CSV (optional)",
              accept = ".csv"
            ),
            shiny::actionButton(
              ns("use_demo"), "Load Demo Data",
              class = "btn btn-outline-primary w-100"
            ),
            shiny::br(), shiny::br(),
            shiny::uiOutput(ns("data_status"))
          ),
          bslib::accordion_panel(
            "Analysis Settings",
            shiny::selectInput(ns("tax_level"), "Taxonomic level", choices = character()),
            shiny::selectInput(ns("function_level"), "Function level", choices = character()),
            shiny::checkboxInput(ns("relative_abundance"), "Use relative abundance", value = TRUE),
            shiny::numericInput(ns("top_n"), "Top categories", value = 10, min = 3, max = 50, step = 1),
            shiny::actionButton(
              ns("run_analysis"),
              "Run Metaproteomics Analysis",
              class = "btn btn-success w-100 pv-run-button"
            )
          ),
          bslib::accordion_panel(
            "Download",
            shiny::downloadButton(ns("download_merged"), "Merged Protein Table"),
            shiny::br(), shiny::br(),
            shiny::downloadButton(ns("download_run"), "Current Run RDS")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Metaproteomics"),
        bslib::layout_column_wrap(
          width = 1 / 4,
          bslib::value_box("Proteins", shiny::textOutput(ns("n_proteins")), theme = "primary"),
          bslib::value_box("Taxa", shiny::textOutput(ns("n_taxa")), theme = "success"),
          bslib::value_box("Functions", shiny::textOutput(ns("n_functions")), theme = "warning"),
          bslib::value_box("Peptides", shiny::textOutput(ns("n_peptides")), theme = "info")
        ),
        shiny::hr(),
        shiny::tabsetPanel(
          shiny::tabPanel("Merged table", DT::DTOutput(ns("merged_table"))),
          shiny::tabPanel(
            "Taxonomy composition",
            shiny::plotOutput(ns("taxonomy_plot"), height = "520px")
          ),
          shiny::tabPanel(
            "Taxonomy rank",
            shiny::plotOutput(ns("taxonomy_rank_plot"), height = "520px")
          ),
          shiny::tabPanel(
            "Function composition",
            shiny::plotOutput(ns("function_plot"), height = "520px")
          ),
          shiny::tabPanel(
            "Taxon × Function",
            plotly::plotlyOutput(ns("sankey_plot"), height = "540px"),
            shiny::plotOutput(ns("heatmap_plot"), height = "540px")
          ),
          shiny::tabPanel(
            "Peptide-centric",
            shiny::p(
              "Shared peptide intensity is split equally across multiple functional assignments before sample-level aggregation."
            ),
            shiny::plotOutput(ns("peptide_heatmap"), height = "500px"),
            DT::DTOutput(ns("peptide_table"))
          ),
          shiny::tabPanel(
            "Result tables",
            shiny::selectInput(
              ns("result_table_name"),
              "Stored table",
              choices = character()
            ),
            DT::DTOutput(ns("result_table"))
          )
        )
      )
    )
  )
}

#' Metaproteomics Server Module
#'
#' @param id A unique module id.
#' @param shared_state Optional ProtVis application shared reactive state.
#' @return No return value. Called for side effects.
#' @import shiny
#' @export
metaproteomics_server <- function(id, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    demo <- metaproteomics_demo_data()
    rv <- shiny::reactiveValues(
      data = demo,
      result = NULL,
      run = NULL,
      source = "Built-in demo"
    )

    read_csv_file <- function(file) {
      shiny::req(file)
      utils::read.csv(
        file$datapath,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    available_active_dataset <- function() {
      !is.null(shared_state) &&
        inherits(shared_state$dataset, "ProtVis_dataset")
    }

    refresh_choices <- function() {
      tax_levels <- .mp_tax_levels(rv$data$taxonomy)
      fun_levels <- .mp_function_levels(rv$data$function, rv$data$peptide)
      tax_selected <- if ("Genus" %in% tax_levels) {
        "Genus"
      } else if (length(tax_levels)) {
        tax_levels[[1L]]
      } else {
        character()
      }
      fun_selected <- if ("Pathway" %in% fun_levels) {
        "Pathway"
      } else if (length(fun_levels)) {
        fun_levels[[1L]]
      } else {
        character()
      }
      shiny::updateSelectInput(
        session, "tax_level",
        choices = tax_levels,
        selected = tax_selected
      )
      shiny::updateSelectInput(
        session, "function_level",
        choices = fun_levels,
        selected = fun_selected
      )
    }

    shiny::observe({
      rv$data
      refresh_choices()
    })

    shiny::observeEvent(input$load_active_dataset, {
      if (!available_active_dataset()) {
        shiny::showNotification(
          "No active ProtVis_dataset is available.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      rv$data <- .mp_data_from_dataset(shared_state$dataset)
      rv$result <- NULL
      rv$run <- NULL
      rv$source <- "Active ProtVis_dataset"
      shiny::showNotification(
        "Active ProtVis_dataset loaded for metaproteomics analysis.",
        type = "message"
      )
    })

    shiny::observeEvent(input$use_demo, {
      rv$data <- metaproteomics_demo_data()
      rv$result <- NULL
      rv$run <- NULL
      rv$source <- "Built-in demo"
      shiny::showNotification(
        "Built-in metaproteomics demo data loaded.",
        type = "message"
      )
    })

    shiny::observeEvent(
      list(
        input$abundance_file, input$sample_info_file, input$taxonomy_file,
        input$function_file, input$peptide_file
      ),
      {
        dat <- rv$data
        if (!is.null(input$abundance_file)) {
          dat$abundance <- read_csv_file(input$abundance_file)
          rv$source <- "Uploaded tables"
        }
        if (!is.null(input$sample_info_file)) {
          dat$sample_info <- read_csv_file(input$sample_info_file)
        }
        if (!is.null(input$taxonomy_file)) {
          dat$taxonomy <- read_csv_file(input$taxonomy_file)
        }
        if (!is.null(input$function_file)) {
          dat$function <- read_csv_file(input$function_file)
        }
        if (!is.null(input$peptide_file)) {
          dat$peptide <- read_csv_file(input$peptide_file)
        }
        rv$data <- dat
        rv$result <- NULL
        rv$run <- NULL
      },
      ignoreInit = TRUE
    )

    persist_run <- function(run) {
      if (is.null(shared_state)) return(invisible(NULL))
      dataset <- if (available_active_dataset()) {
        shared_state$dataset
      } else {
        tryCatch(.mp_dataset_from_run(run), error = function(e) NULL)
      }
      if (is.null(dataset)) return(invisible(NULL))

      dataset <- .mp_append_run(dataset, run)
      if (!is.null(shared_state$dataset_history)) {
        history <- shared_state$dataset_history %||% list()
        history[[length(history) + 1L]] <- dataset
        shared_state$dataset_history <- history
      }
      shared_state$dataset <- dataset
      shared_state$dataset_name <- tryCatch(
        protvis_dataset_name(dataset),
        error = function(e) "ProtVis_dataset"
      )

      workdir <- shared_state$workdir %||% NULL
      if (!is.null(workdir) && nzchar(as.character(workdir))) {
        exported <- tryCatch(
          protvis_auto_export_dataset(dataset, directory = workdir),
          error = function(e) NULL
        )
        if (inherits(exported, "ProtVis_dataset")) {
          dataset <- exported
          shared_state$dataset <- dataset
        }
        try(
          save_protvis_checkpoint(
            dataset,
            directory = workdir,
            stage = "metaproteomics"
          ),
          silent = TRUE
        )
      }
      invisible(dataset)
    }

    shiny::observeEvent(input$run_analysis, {
      tryCatch({
        shiny::req(input$tax_level, input$function_level)
        result <- .mp_prepare_analysis(
          rv$data,
          tax_level = input$tax_level,
          function_level = input$function_level,
          relative = isTRUE(input$relative_abundance),
          top_n = input$top_n
        )
        run <- .mp_result_run(result, source = rv$source)
        rv$result <- result
        rv$run <- run
        persist_run(run)
        shiny::updateSelectInput(
          session,
          "result_table_name",
          choices = names(result$tables),
          selected = "merged"
        )
        shiny::showNotification(
          paste0(
            "Metaproteomics run completed and stored as ", run$run_id, "."
          ),
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          conditionMessage(e),
          type = "error",
          duration = NULL
        )
      })
      session$sendCustomMessage(
        "protvis-unlock-run-button",
        list(id = session$ns("run_analysis"))
      )
    })

    result_data <- shiny::reactive({
      if (is.null(rv$result)) {
        tax <- .mp_tax_levels(rv$data$taxonomy)
        fun <- .mp_function_levels(rv$data$function, rv$data$peptide)
        shiny::validate(
          shiny::need(length(tax), "Load taxonomy annotation to start."),
          shiny::need(length(fun), "Load function annotation to start.")
        )
        return(.mp_prepare_analysis(
          rv$data,
          tax_level = if ("Genus" %in% tax) "Genus" else tax[[1L]],
          function_level = if ("Pathway" %in% fun) "Pathway" else fun[[1L]],
          relative = isTRUE(input$relative_abundance %||% TRUE),
          top_n = input$top_n %||% 10L
        ))
      }
      rv$result
    })

    output$data_status <- shiny::renderUI({
      dat <- rv$data
      peptide_n <- if (is.null(dat$peptide)) 0L else nrow(dat$peptide)
      active_note <- if (available_active_dataset()) {
        " Active ProtVis_dataset available; runs will be appended."
      } else {
        " No active dataset; ProtVis can create one from the protein abundance table after analysis."
      }
      shiny::tags$small(
        paste0(
          "Source: ", rv$source,
          ". Proteins: ", nrow(dat$abundance %||% data.frame()),
          "; taxonomy rows: ", nrow(dat$taxonomy %||% data.frame()),
          "; function rows: ", nrow(dat$function %||% data.frame()),
          "; peptide rows: ", peptide_n, ".",
          active_note
        )
      )
    })

    output$n_proteins <- shiny::renderText({
      length(unique(result_data()$tables$merged$ProteinID))
    })
    output$n_taxa <- shiny::renderText({
      table <- result_data()$tables$taxonomy_composition
      length(unique(table$Category))
    })
    output$n_functions <- shiny::renderText({
      table <- result_data()$tables$function_composition
      length(unique(table$Category))
    })
    output$n_peptides <- shiny::renderText({
      peptide <- result_data()$inputs$peptide
      col <- .mp_first_existing(peptide, c("Peptide", "Sequence", "peptide", "sequence"))
      if (is.null(col)) 0L else length(unique(peptide[[col]]))
    })

    output$merged_table <- DT::renderDT({
      DT::datatable(
        result_data()$tables$merged,
        options = list(pageLength = 12, scrollX = TRUE),
        rownames = FALSE
      )
    })

    make_bar_plot <- function(df, title, y_label) {
      ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$Sample, y = .data$Intensity, fill = .data$Category)
      ) +
        ggplot2::geom_col(width = 0.78, color = "white", linewidth = 0.15) +
        ggplot2::facet_grid(~Group, scales = "free_x", space = "free_x") +
        ggplot2::labs(title = title, x = NULL, y = y_label, fill = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.major.x = ggplot2::element_blank(),
          legend.position = "right"
        )
    }

    output$taxonomy_plot <- shiny::renderPlot({
      df <- result_data()$tables$taxonomy_composition
      shiny::validate(shiny::need(nrow(df), "No taxonomy composition available."))
      make_bar_plot(
        df,
        paste("Taxonomy composition by", result_data()$parameters$tax_level),
        if (isTRUE(result_data()$parameters$relative_abundance)) {
          "Relative abundance"
        } else {
          "Summed intensity"
        }
      )
    })

    output$taxonomy_rank_plot <- shiny::renderPlot({
      df <- result_data()$tables$taxonomy_composition |>
        dplyr::group_by(.data$Category) |>
        dplyr::summarise(Abundance = mean(.data$Intensity, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(.data$Abundance)
      shiny::validate(shiny::need(nrow(df), "No taxonomy ranking available."))
      ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = stats::reorder(.data$Category, .data$Abundance),
          y = .data$Abundance
        )
      ) +
        ggplot2::geom_col() +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Mean abundance by", result_data()$parameters$tax_level),
          x = NULL,
          y = if (isTRUE(result_data()$parameters$relative_abundance)) {
            "Mean relative abundance"
          } else {
            "Mean intensity"
          }
        ) +
        ggplot2::theme_minimal(base_size = 13)
    })

    output$function_plot <- shiny::renderPlot({
      df <- result_data()$tables$function_composition
      shiny::validate(shiny::need(nrow(df), "No function composition available."))
      make_bar_plot(
        df,
        paste("Function composition by", result_data()$parameters$function_level),
        if (isTRUE(result_data()$parameters$relative_abundance)) {
          "Relative abundance"
        } else {
          "Summed intensity"
        }
      )
    })

    taxon_function_aggregate <- shiny::reactive({
      df <- result_data()$tables$taxon_function
      if (!nrow(df)) return(df)
      df |>
        dplyr::group_by(.data$Taxon, .data$Function) |>
        dplyr::summarise(
          Intensity = sum(.data$Intensity, na.rm = TRUE),
          .groups = "drop"
        )
    })

    output$sankey_plot <- plotly::renderPlotly({
      df <- taxon_function_aggregate()
      shiny::validate(shiny::need(nrow(df), "No taxon-function links available."))
      nodes <- data.frame(
        name = unique(c(as.character(df$Taxon), as.character(df$Function))),
        stringsAsFactors = FALSE
      )
      df$source <- match(df$Taxon, nodes$name) - 1L
      df$target <- match(df$Function, nodes$name) - 1L
      plotly::plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(label = nodes$name, pad = 14, thickness = 16),
        link = list(
          source = df$source,
          target = df$target,
          value = df$Intensity
        )
      )
    })

    output$heatmap_plot <- shiny::renderPlot({
      df <- taxon_function_aggregate()
      shiny::validate(shiny::need(nrow(df), "No taxon-function links available."))
      ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$Function, y = .data$Taxon, fill = log1p(.data$Intensity))
      ) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient() +
        ggplot2::labs(
          title = "Taxon-function abundance heatmap",
          x = result_data()$parameters$function_level,
          y = result_data()$parameters$tax_level,
          fill = "log1p intensity"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    })

    output$peptide_heatmap <- shiny::renderPlot({
      df <- result_data()$tables$peptide_function_scores
      shiny::validate(shiny::need(
        nrow(df),
        "No peptide-centric function scores are available. Upload a peptide table with peptide sequence, abundance, and functional mapping."
      ))
      top <- df |>
        dplyr::group_by(.data$Function) |>
        dplyr::summarise(Total = sum(.data$Intensity, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(.data$Total)) |>
        dplyr::slice_head(n = input$top_n %||% 10L) |>
        dplyr::pull(.data$Function)
      df <- df[df$Function %in% top, , drop = FALSE]
      ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data$Sample, y = .data$Function, fill = log1p(.data$Intensity))
      ) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient() +
        ggplot2::labs(
          title = "Peptide-centric functional abundance",
          x = NULL,
          y = result_data()$parameters$function_level,
          fill = "log1p intensity"
        ) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    })

    output$peptide_table <- DT::renderDT({
      table <- result_data()$tables$peptide_function_differential
      if (!nrow(table)) table <- result_data()$tables$peptide_function_scores
      DT::datatable(
        table,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })

    shiny::observe({
      tables <- names(result_data()$tables)
      shiny::updateSelectInput(
        session,
        "result_table_name",
        choices = tables,
        selected = if (input$result_table_name %in% tables) {
          input$result_table_name
        } else {
          "merged"
        }
      )
    })

    output$result_table <- DT::renderDT({
      nm <- input$result_table_name %||% "merged"
      table <- result_data()$tables[[nm]] %||% data.frame()
      DT::datatable(
        table,
        rownames = FALSE,
        options = list(pageLength = 12, scrollX = TRUE)
      )
    })

    output$download_merged <- shiny::downloadHandler(
      filename = function() {
        paste0("metaproteomics_merged_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(
          result_data()$tables$merged,
          file,
          row.names = FALSE
        )
      }
    )

    output$download_run <- shiny::downloadHandler(
      filename = function() {
        id <- rv$run$run_id %||% paste0("metaproteomics_", Sys.Date())
        paste0(id, ".rds")
      },
      content = function(file) {
        run <- rv$run
        if (is.null(run)) {
          run <- .mp_result_run(result_data(), source = rv$source)
        }
        saveRDS(run, file, compress = TRUE)
      }
    )
  })
}
