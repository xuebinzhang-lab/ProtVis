# Advanced differential-proteomics engines --------------------------------
# This additive layer preserves the existing limma/DEP workflow and adds an
# independent statistical-engine workspace. It is collated after DEP_analysis.R.

.protvis_dep_base_ui <- DEP_analysis_ui
.protvis_dep_base_server <- DEP_analysis_server

.protvis_dep_engine_names <- c(
  "limma" = "limma",
  "DEqMS" = "deqms",
  "proDA" = "proda",
  "MSstats" = "msstats"
)

.protvis_dep_matrix <- function(dataset) {
  if (!inherits(dataset, "ProtVis_dataset")) {
    base::stop("A ProtVis_dataset is required.", call. = FALSE)
  }
  mat <- base::as.matrix(dataset$expression_data)
  storage.mode(mat) <- "numeric"
  mat
}

.protvis_dep_group_columns <- function(sample_info) {
  if (base::is.null(sample_info) || !base::nrow(sample_info)) return(base::character())
  preferred <- c("group", "condition", "tissue", "tissue2", "class", "batch")
  columns <- base::intersect(preferred, base::names(sample_info))
  columns[base::vapply(columns, function(column) {
    values <- base::as.character(sample_info[[column]])
    values <- values[!base::is.na(values) & base::nzchar(base::trimws(values)) & values != "Unassigned"]
    base::length(base::unique(values)) >= 2L
  }, logical(1))]
}

.protvis_dep_group_values <- function(sample_info, column) {
  if (base::is.null(sample_info) || !column %in% base::names(sample_info)) return(base::character())
  values <- base::unique(base::as.character(sample_info[[column]]))
  values[!base::is.na(values) & base::nzchar(base::trimws(values)) & values != "Unassigned"]
}

.protvis_dep_samples <- function(dataset, grouping_column, group1, group2) {
  info <- dataset$sample_info
  if (!"sample_id" %in% base::names(info)) base::stop("sample_info requires sample_id.", call. = FALSE)
  if (!grouping_column %in% base::names(info)) base::stop("Grouping column was not found.", call. = FALSE)
  values <- base::as.character(info[[grouping_column]])
  s1 <- base::as.character(info$sample_id[values == group1])
  s2 <- base::as.character(info$sample_id[values == group2])
  available <- base::colnames(dataset$expression_data)
  s1 <- s1[s1 %in% available]
  s2 <- s2[s2 %in% available]
  if (!base::length(s1) || !base::length(s2)) {
    base::stop("Both comparison groups must contain expression samples.", call. = FALSE)
  }
  list(group1 = s1, group2 = s2, samples = c(s1, s2))
}

.protvis_dep_design <- function(samples, group1_samples, group2_samples, group1, group2) {
  groups <- base::factor(
    ifelse(samples %in% group1_samples, group1, group2),
    levels = c(group1, group2)
  )
  design <- stats::model.matrix(~ 0 + groups)
  safe <- base::make.names(base::levels(groups), unique = TRUE)
  base::colnames(design) <- safe
  contrast <- limma::makeContrasts(
    contrasts = base::paste(safe[[1L]], safe[[2L]], sep = " - "),
    levels = design
  )
  list(groups = groups, design = design, contrast = contrast)
}

.protvis_dep_standardize <- function(table, method, id, logfc, pval, adjp, extra = NULL) {
  out <- base::as.data.frame(table, stringsAsFactors = FALSE, check.names = FALSE)
  result <- base::data.frame(
    ID = base::as.character(id),
    logFC = base::as.numeric(logfc),
    P.Value = base::as.numeric(pval),
    adj.P.Val = base::as.numeric(adjp),
    method = method,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!base::is.null(extra)) {
    extra <- base::as.data.frame(extra, stringsAsFactors = FALSE, check.names = FALSE)
    extra <- extra[, base::setdiff(base::names(extra), base::names(result)), drop = FALSE]
    result <- base::cbind(result, extra)
  }
  result
}

.protvis_dep_run_limma <- function(mat, group1_samples, group2_samples, group1, group2) {
  samples <- c(group1_samples, group2_samples)
  mat <- mat[, samples, drop = FALSE]
  setup <- .protvis_dep_design(samples, group1_samples, group2_samples, group1, group2)
  fit <- limma::lmFit(mat, setup$design)
  fit <- limma::contrasts.fit(fit, setup$contrast)
  fit <- limma::eBayes(fit)
  result <- limma::topTable(fit, coef = 1, n = Inf, sort.by = "none", adjust.method = "BH")
  ids <- base::rownames(result)
  .protvis_dep_standardize(result, "limma", ids, result$logFC, result$P.Value, result$adj.P.Val, result)
}

.protvis_dep_count_columns <- function(table) {
  if (base::is.null(table) || !base::ncol(table)) return(base::character())
  pattern <- paste(
    c(
      "(^|[._ -])psms?($|[._ -])", "peptide[._ -]*count", "^peptides?$",
      "unique[._ -]*peptides?", "razor.*unique", "ms[./_-]*ms[._ -]*count",
      "spectra[._ -]*count", "spectral[._ -]*count", "sequence[._ -]*count"
    ), collapse = "|"
  )
  base::names(table)[base::grepl(pattern, base::names(table), ignore.case = TRUE, perl = TRUE)]
}

.protvis_dep_count_table <- function(table, protein_ids) {
  if (base::is.null(table) || !base::nrow(table)) return(NULL)
  table <- base::as.data.frame(table, stringsAsFactors = FALSE, check.names = FALSE)
  id_candidates <- base::intersect(
    c("variable_id", "protein_id", "accession", "ID", "Protein IDs", "Protein.IDs"),
    base::names(table)
  )
  if (!base::length(id_candidates)) return(NULL)
  id_col <- id_candidates[[1L]]
  count_cols <- .protvis_dep_count_columns(table)
  if (!base::length(count_cols)) return(NULL)
  ids <- sub(";.*$", "", base::trimws(base::as.character(table[[id_col]])))
  index <- base::match(protein_ids, ids)
  values <- base::lapply(count_cols, function(column) {
    suppressWarnings(base::as.numeric(base::as.character(table[[column]][index])))
  })
  count_matrix <- base::do.call(base::cbind, values)
  if (base::is.null(base::dim(count_matrix))) count_matrix <- base::matrix(count_matrix, ncol = 1L)
  # Conservative DEqMS count: use the minimum available evidence count when
  # multiple peptide/PSM count fields are present.
  counts <- base::apply(count_matrix, 1, function(x) {
    x <- x[base::is.finite(x) & x > 0]
    if (!base::length(x)) NA_real_ else base::min(x)
  })
  base::data.frame(
    ID = protein_ids,
    count = counts,
    count_source = base::paste(count_cols, collapse = "; "),
    stringsAsFactors = FALSE
  )
}

.protvis_dep_run_deqms <- function(mat, group1_samples, group2_samples, group1, group2, count_table) {
  if (!base::requireNamespace("DEqMS", quietly = TRUE)) {
    base::stop("Package 'DEqMS' is not installed.", call. = FALSE)
  }
  counts <- .protvis_dep_count_table(count_table, base::rownames(mat))
  if (base::is.null(counts)) {
    base::stop(
      "DEqMS requires peptide/PSM counts. No count-like column was found in variable_info or the uploaded count table.",
      call. = FALSE
    )
  }
  keep <- base::is.finite(counts$count) & counts$count > 0
  if (base::sum(keep) < 10L) base::stop("Too few proteins have usable peptide/PSM counts for DEqMS.", call. = FALSE)
  mat <- mat[keep, , drop = FALSE]
  counts <- counts[keep, , drop = FALSE]
  samples <- c(group1_samples, group2_samples)
  mat <- mat[, samples, drop = FALSE]
  setup <- .protvis_dep_design(samples, group1_samples, group2_samples, group1, group2)
  fit <- limma::lmFit(mat, setup$design)
  fit <- limma::contrasts.fit(fit, setup$contrast)
  fit <- limma::eBayes(fit)
  fit$count <- counts$count[base::match(base::rownames(fit$coefficients), counts$ID)]
  fit2 <- DEqMS::spectraCounteBayes(fit, coef_col = 1)
  result <- DEqMS::outputResult(fit2, coef_col = 1)
  result <- base::as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE)
  ids <- if ("gene" %in% base::names(result)) base::as.character(result$gene) else base::rownames(result)
  p <- if ("sca.P.Value" %in% base::names(result)) result$sca.P.Value else result$P.Value
  adj <- if ("sca.adj.pval" %in% base::names(result)) result$sca.adj.pval else stats::p.adjust(p, method = "BH")
  .protvis_dep_standardize(result, "DEqMS", ids, result$logFC, p, adj, result)
}

.protvis_dep_run_proda <- function(mat, group1_samples, group2_samples, group1, group2) {
  if (!base::requireNamespace("proDA", quietly = TRUE)) {
    base::stop("Package 'proDA' is not installed.", call. = FALSE)
  }
  samples <- c(group1_samples, group2_samples)
  mat <- mat[, samples, drop = FALSE]
  keep <- base::rowSums(base::is.finite(mat)) >= 2L
  mat <- mat[keep, , drop = FALSE]
  if (base::nrow(mat) < 10L) base::stop("Too few proteins are available for proDA.", call. = FALSE)
  groups <- base::factor(
    ifelse(samples %in% group1_samples, group1, group2),
    levels = c(group1, group2)
  )
  fit <- proDA::proDA(mat, design = groups)
  rn <- proDA::result_names(fit)
  if (base::length(rn) < 2L) base::stop("proDA could not resolve the two comparison groups.", call. = FALSE)
  contrast <- base::paste0("`", rn[[1L]], "` - `", rn[[2L]], "`")
  result <- proDA::test_diff(fit, contrast = contrast)
  result <- base::as.data.frame(result, stringsAsFactors = FALSE, check.names = FALSE)
  .protvis_dep_standardize(
    result, "proDA", result$name, result$diff, result$pval, result$adj_pval, result
  )
}

.protvis_dep_msstats_required <- c(
  "ProteinName", "PeptideSequence", "PrecursorCharge", "FragmentIon",
  "ProductCharge", "IsotopeLabelType", "Condition", "BioReplicate", "Run", "Intensity"
)

.protvis_dep_msstats_canonical <- function(table) {
  table <- base::as.data.frame(table, stringsAsFactors = FALSE, check.names = FALSE)
  lower <- base::tolower(base::names(table))
  for (required in .protvis_dep_msstats_required) {
    hit <- base::which(lower == base::tolower(required))
    if (!base::length(hit)) {
      base::stop(
        "MSstats input is missing required column: ", required,
        ". Supply feature/peptide-level MSstats long-format data rather than a protein matrix.",
        call. = FALSE
      )
    }
    base::names(table)[hit[[1L]]] <- required
  }
  table$Intensity <- suppressWarnings(base::as.numeric(base::as.character(table$Intensity)))
  table
}

.protvis_dep_run_msstats <- function(table, group1, group2) {
  if (!base::requireNamespace("MSstats", quietly = TRUE)) {
    base::stop("Package 'MSstats' is not installed.", call. = FALSE)
  }
  table <- .protvis_dep_msstats_canonical(table)
  table <- table[base::as.character(table$Condition) %in% c(group1, group2), , drop = FALSE]
  if (!base::nrow(table)) base::stop("The MSstats table contains no rows for the selected groups.", call. = FALSE)
  if (!base::all(c(group1, group2) %in% base::unique(base::as.character(table$Condition)))) {
    base::stop("Both selected comparison groups must be present in the MSstats Condition column.", call. = FALSE)
  }
  processed <- MSstats::dataProcess(
    raw = table,
    normalization = FALSE,
    summaryMethod = "TMP",
    censoredInt = "NA",
    MBimpute = TRUE,
    use_log_file = FALSE,
    verbose = FALSE
  )
  protein_data <- processed$ProteinLevelData
  group_col <- if ("GROUP" %in% base::names(protein_data)) "GROUP" else if ("GROUP_ORIGINAL" %in% base::names(protein_data)) "GROUP_ORIGINAL" else NULL
  if (base::is.null(group_col)) base::stop("MSstats processed data did not expose group labels.", call. = FALSE)
  groups <- base::unique(base::as.character(protein_data[[group_col]]))
  if (!base::all(c(group1, group2) %in% groups)) {
    base::stop("MSstats group labels do not match the selected comparison.", call. = FALSE)
  }
  contrast <- base::matrix(0, nrow = 1, ncol = base::length(groups), dimnames = list(base::paste0(group1, "_vs_", group2), groups))
  contrast[1, group1] <- 1
  contrast[1, group2] <- -1
  comparison <- MSstats::groupComparison(
    contrast.matrix = contrast,
    data = processed,
    use_log_file = FALSE,
    verbose = FALSE
  )
  result <- base::as.data.frame(comparison$ComparisonResult, stringsAsFactors = FALSE, check.names = FALSE)
  .protvis_dep_standardize(
    result, "MSstats", result$Protein, result$log2FC, result$pvalue, result$adj.pvalue, result
  )
}


# Visualization helpers for the statistical-engine workspace. These are
# intentionally additive: the preserved DEP workflow keeps its original
# volcano/heatmap/bar implementation, while every engine result receives the
# same publication-oriented visual summaries using that engine's own statistics.

.protvis_dep_engine_empty_plot <- function(message) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message, size = 5) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
}

.protvis_dep_engine_classify <- function(table, fdr = 0.05, logfc = 0.27) {
  table <- base::as.data.frame(
    table, stringsAsFactors = FALSE, check.names = FALSE
  )
  if (!base::all(c("ID", "logFC") %in% base::names(table))) {
    base::stop("Engine result requires ID and logFC columns.", call. = FALSE)
  }

  logfc_values <- suppressWarnings(base::as.numeric(table$logFC))
  adjusted <- if ("adj.P.Val" %in% base::names(table)) {
    suppressWarnings(base::as.numeric(table$adj.P.Val))
  } else {
    base::rep(NA_real_, base::nrow(table))
  }
  raw_p <- if ("P.Value" %in% base::names(table)) {
    suppressWarnings(base::as.numeric(table$P.Value))
  } else {
    base::rep(NA_real_, base::nrow(table))
  }

  use_fdr <- base::any(base::is.finite(adjusted))
  significance <- if (use_fdr) adjusted else raw_p
  metric <- if (use_fdr) "FDR" else "P-value"
  threshold <- suppressWarnings(base::as.numeric(fdr[[1L]]))
  if (!base::is.finite(threshold) || threshold <= 0) threshold <- 0.05
  logfc_threshold <- suppressWarnings(base::as.numeric(logfc[[1L]]))
  if (!base::is.finite(logfc_threshold) || logfc_threshold < 0) {
    logfc_threshold <- 0.27
  }

  regulation <- base::rep("Not significant", base::nrow(table))
  regulation[
    base::is.finite(significance) &
      significance <= threshold &
      base::is.finite(logfc_values) &
      logfc_values >= logfc_threshold
  ] <- "Upregulated"
  regulation[
    base::is.finite(significance) &
      significance <= threshold &
      base::is.finite(logfc_values) &
      logfc_values <= -logfc_threshold
  ] <- "Downregulated"

  table$.pv_logFC <- logfc_values
  table$.pv_significance <- significance
  table$.pv_regulation <- base::factor(
    regulation,
    levels = c("Downregulated", "Not significant", "Upregulated")
  )
  list(
    data = table,
    metric = metric,
    threshold = threshold,
    logfc_threshold = logfc_threshold
  )
}

.protvis_dep_engine_volcano_plot <- function(
    table, method, comparison, fdr = 0.05, logfc = 0.27,
    up = "#d62728", down = "#1f77b4", ns = "#9aa6b2") {
  classified <- .protvis_dep_engine_classify(table, fdr, logfc)
  data <- classified$data
  keep <- base::is.finite(data$.pv_logFC) &
    base::is.finite(data$.pv_significance) &
    data$.pv_significance >= 0
  data <- data[keep, , drop = FALSE]
  if (!base::nrow(data)) {
    return(.protvis_dep_engine_empty_plot("No finite statistics for volcano plot"))
  }

  data$.pv_minus_log10 <- -base::log10(
    base::pmax(data$.pv_significance, .Machine$double.xmin)
  )
  ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = .pv_logFC, y = .pv_minus_log10, colour = .pv_regulation
    )
  ) +
    ggplot2::geom_point(alpha = 0.72, size = 1.8) +
    ggplot2::geom_vline(
      xintercept = c(-classified$logfc_threshold, classified$logfc_threshold),
      linetype = 2, linewidth = 0.45
    ) +
    ggplot2::geom_hline(
      yintercept = -base::log10(classified$threshold),
      linetype = 2, linewidth = 0.45
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Downregulated" = down,
        "Not significant" = ns,
        "Upregulated" = up
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = base::paste(method, "·", comparison),
      subtitle = base::paste0(
        classified$metric, " ≤ ", signif(classified$threshold, 3),
        "  |  |log2FC| ≥ ", signif(classified$logfc_threshold, 3)
      ),
      x = "log2 fold change",
      y = base::paste0("-log10(", classified$metric, ")"),
      colour = NULL
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "top",
      plot.title = ggplot2::element_text(face = "bold")
    )
}

.protvis_dep_engine_bar_plot <- function(
    table, method, comparison, fdr = 0.05, logfc = 0.27,
    up = "#d62728", down = "#1f77b4") {
  classified <- .protvis_dep_engine_classify(table, fdr, logfc)
  values <- base::as.character(classified$data$.pv_regulation)
  counts <- base::data.frame(
    regulation = base::factor(
      c("Downregulated", "Upregulated"),
      levels = c("Downregulated", "Upregulated")
    ),
    n = c(
      base::sum(values == "Downregulated", na.rm = TRUE),
      base::sum(values == "Upregulated", na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )

  ggplot2::ggplot(
    counts, ggplot2::aes(x = regulation, y = n, fill = regulation)
  ) +
    ggplot2::geom_col(width = 0.66) +
    ggplot2::geom_text(
      ggplot2::aes(label = n), vjust = -0.35, size = 4
    ) +
    ggplot2::scale_fill_manual(
      values = c("Downregulated" = down, "Upregulated" = up),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = base::paste("Differential proteins ·", method),
      subtitle = comparison,
      x = NULL, y = "Protein count", fill = NULL
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::expand_limits(y = base::max(counts$n, 1L) * 1.12)
}

.protvis_dep_engine_heatmap_plot <- function(
    table, dataset, sample_sets, method, comparison,
    fdr = 0.05, logfc = 0.27, top_n = 50L,
    show_colnames = TRUE, show_rownames = FALSE) {
  if (!inherits(dataset, "ProtVis_dataset")) {
    return(.protvis_dep_engine_empty_plot("No ProtVis_dataset available"))
  }
  classified <- .protvis_dep_engine_classify(table, fdr, logfc)
  sig <- classified$data[
    base::as.character(classified$data$.pv_regulation) != "Not significant",
    , drop = FALSE
  ]
  if (!base::nrow(sig)) {
    return(.protvis_dep_engine_empty_plot("No significant proteins"))
  }

  sig <- sig[base::order(
    sig$.pv_significance,
    -base::abs(sig$.pv_logFC),
    na.last = TRUE
  ), , drop = FALSE]
  top_n <- suppressWarnings(base::as.integer(top_n[[1L]]))
  if (!base::is.finite(top_n) || top_n < 2L) top_n <- 50L
  sig <- sig[base::seq_len(base::min(base::nrow(sig), top_n)), , drop = FALSE]

  mat <- .protvis_dep_matrix(dataset)
  samples <- base::as.character(sample_sets$samples %||% base::character())
  samples <- samples[samples %in% base::colnames(mat)]
  if (!base::length(samples)) {
    return(.protvis_dep_engine_empty_plot("Comparison samples are unavailable"))
  }
  mat <- mat[, samples, drop = FALSE]

  matrix_ids <- base::rownames(mat)
  if (base::is.null(matrix_ids)) {
    return(.protvis_dep_engine_empty_plot("Protein identifiers are unavailable"))
  }
  clean_matrix_ids <- sub(";.*$", "", base::trimws(matrix_ids))
  clean_result_ids <- sub(";.*$", "", base::trimws(base::as.character(sig$ID)))
  row_index <- base::match(clean_result_ids, clean_matrix_ids)
  row_index <- base::unique(row_index[!base::is.na(row_index)])
  if (!base::length(row_index)) {
    return(.protvis_dep_engine_empty_plot(
      "Significant proteins could not be matched to the expression matrix"
    ))
  }

  mat <- mat[row_index, , drop = FALSE]
  keep <- base::apply(mat, 1, function(x) base::any(base::is.finite(x)))
  mat <- mat[keep, , drop = FALSE]
  if (!base::nrow(mat)) {
    return(.protvis_dep_engine_empty_plot("No finite values for heatmap"))
  }

  for (i in base::seq_len(base::nrow(mat))) {
    missing <- !base::is.finite(mat[i, ])
    if (base::any(missing)) {
      observed <- mat[i, !missing]
      mat[i, missing] <- if (base::length(observed)) {
        stats::median(observed)
      } else {
        0
      }
    }
  }

  zmat <- base::t(base::apply(mat, 1, function(x) {
    center <- base::mean(x)
    spread <- stats::sd(x)
    if (!base::is.finite(spread) || spread == 0) {
      base::rep(0, base::length(x))
    } else {
      (x - center) / spread
    }
  }))
  base::rownames(zmat) <- base::rownames(mat)
  base::colnames(zmat) <- base::colnames(mat)

  row_order <- base::seq_len(base::nrow(zmat))
  col_order <- base::seq_len(base::ncol(zmat))
  if (base::nrow(zmat) >= 2L) {
    row_order <- stats::hclust(stats::dist(zmat))$order
  }
  if (base::ncol(zmat) >= 2L) {
    col_order <- stats::hclust(stats::dist(base::t(zmat)))$order
  }
  zmat <- zmat[row_order, col_order, drop = FALSE]

  long <- base::data.frame(
    protein = base::rep(base::rownames(zmat), times = base::ncol(zmat)),
    sample = base::rep(base::colnames(zmat), each = base::nrow(zmat)),
    z = base::as.vector(zmat),
    stringsAsFactors = FALSE
  )
  long$protein <- base::factor(
    long$protein, levels = base::rev(base::rownames(zmat))
  )
  long$sample <- base::factor(long$sample, levels = base::colnames(zmat))

  ggplot2::ggplot(long, ggplot2::aes(x = sample, y = protein, fill = z)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2() +
    ggplot2::labs(
      title = base::paste("Significant-protein heatmap ·", method),
      subtitle = base::paste0(
        comparison, "  |  top ", base::nrow(zmat), " matched proteins"
      ),
      x = NULL, y = NULL, fill = "Row z-score"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = if (isTRUE(show_colnames)) {
        ggplot2::element_text(angle = 45, hjust = 1)
      } else {
        ggplot2::element_blank()
      },
      axis.text.y = if (isTRUE(show_rownames)) {
        ggplot2::element_text(size = 7)
      } else {
        ggplot2::element_blank()
      },
      plot.title = ggplot2::element_text(face = "bold")
    )
}

.protvis_dep_engine_safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", base::as.character(x))
  gsub("^_+|_+$", "", x)
}

.protvis_dep_engine_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("\n      .pv-dep-engine-note{font-size:12px;color:#657789;line-height:1.55;}\n      .pv-dep-engine-wrap{min-height:760px;}\n    ")),
    bslib::layout_sidebar(
      class = "pv-dep-engine-wrap",
      sidebar = bslib::sidebar(
        width = 350,
        shiny::actionButton(ns("engine_load"), "LOAD CURRENT DATASET", class = "btn btn-primary fw-bold pv-run-button"),
        shiny::uiOutput(ns("engine_load_status")),
        shiny::hr(),
        shiny::selectInput(ns("engine_group_column"), "Grouping column", choices = character()),
        shiny::uiOutput(ns("engine_group1_ui")),
        shiny::uiOutput(ns("engine_group2_ui")),
        shiny::checkboxGroupInput(
          ns("engine_methods"), "Statistical engines",
          choices = .protvis_dep_engine_names,
          selected = c("limma", "deqms", "proda")
        ),
        shiny::numericInput(ns("engine_fdr"), "FDR threshold", value = 0.05, min = 0, max = 1, step = 0.01),
        shiny::numericInput(ns("engine_logfc"), "|log2FC| threshold", value = 0.27, min = 0, max = 10, step = 0.05),
        shiny::hr(),
        shiny::fileInput(
          ns("deqms_counts"), "DEqMS peptide/PSM count table (optional)",
          accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
        ),
        shiny::p("If omitted, ProtVis searches variable_info for peptide/PSM count columns.", class = "pv-dep-engine-note"),
        shiny::fileInput(
          ns("msstats_file"), "MSstats feature-level long table",
          accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
        ),
        shiny::p(
          "MSstats is run only from genuine feature/peptide-level long-format input; ProtVis does not fabricate MSstats input from a protein matrix.",
          class = "pv-dep-engine-note"
        ),
        shiny::actionButton(ns("run_engines"), "RUN SELECTED ENGINES", class = "btn btn-success fw-bold pv-run-button")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Differential-proteomics statistical engines"),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "Availability",
            shiny::p(
              "limma uses the current normalized protein matrix. DEqMS additionally requires peptide/PSM counts. proDA preferentially uses the pre-imputation transformed matrix. MSstats requires its native feature-level long table.",
              class = "pv-dep-engine-note"
            ),
            DT::DTOutput(ns("engine_availability"))
          ),
          bslib::nav_panel("Run summary", DT::DTOutput(ns("engine_summary"))),
          bslib::nav_panel("Results", shiny::uiOutput(ns("engine_result_tabs")))
        )
      )
    )
  )
}

#' DEP Analysis UI Module with additional statistical engines
#' @inheritParams DEP_analysis_ui
#' @export
DEP_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML(".pv-dep-legacy-wrap{min-height:900px;}")),
    bslib::navset_card_tab(
      id = ns("dep_workspace"),
      bslib::nav_panel(
        "DEP workflow",
        shiny::div(class = "pv-dep-legacy-wrap", .protvis_dep_base_ui(id))
      ),
      bslib::nav_panel(
        "Statistical engines",
        .protvis_dep_engine_ui(id)
      )
    )
  )
}

#' DEP Analysis Server with additional statistical engines
#' @inheritParams DEP_analysis_server
#' @export
DEP_analysis_server <- function(id, shared_state) {
  .protvis_dep_base_server(id, shared_state)

  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      dataset = NULL,
      pre_imputation_dataset = NULL,
      count_table = NULL,
      msstats_table = NULL,
      results = base::list(),
      summary = base::data.frame(),
      comparison = NULL,
      loaded = FALSE
    )

    .read_upload <- function(file) {
      if (base::is.null(file) || !base::nrow(file)) return(NULL)
      protvis_read_table(file$datapath, filename = file$name)
    }

    .load_pre_imputation <- function() {
      workdir <- shared_state$workdir %||% ""
      if (!base::nzchar(base::as.character(workdir))) return(NULL)
      path <- base::file.path(workdir, "Step4_data_transformed.rda")
      if (!base::file.exists(path)) return(NULL)
      base::tryCatch(.protvis_load_stage_dataset(path, expression_names = "transformed"), error = function(e) NULL)
    }

    shiny::observeEvent(input$engine_load, {
      dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) shared_state$dataset else NULL
      if (base::is.null(dataset) && base::nzchar(base::as.character(shared_state$workdir %||% ""))) {
        path <- base::file.path(shared_state$workdir, "Step6_data_normalization.rda")
        if (base::file.exists(path)) dataset <- .protvis_load_stage_dataset(path, expression_names = "normalized_data")
      }
      if (base::is.null(dataset)) {
        rv$loaded <- FALSE
        shiny::showNotification("No current/Step6 ProtVis_dataset was found.", type = "error")
        return(invisible(NULL))
      }
      rv$dataset <- dataset
      rv$pre_imputation_dataset <- .load_pre_imputation()
      rv$count_table <- dataset$variable_info
      rv$results <- base::list()
      rv$summary <- base::data.frame()
      rv$comparison <- NULL
      rv$loaded <- TRUE
      columns <- .protvis_dep_group_columns(dataset$sample_info)
      shiny::updateSelectInput(session, "engine_group_column", choices = columns, selected = if (base::length(columns)) columns[[1L]] else character())
      shiny::showNotification("Differential-proteomics dataset loaded.", type = "message")
    })

    shiny::observeEvent(input$deqms_counts, {
      table <- base::tryCatch(.read_upload(input$deqms_counts), error = function(e) e)
      if (inherits(table, "error")) {
        shiny::showNotification(base::conditionMessage(table), type = "error")
      } else if (!base::is.null(table)) {
        rv$count_table <- table
        shiny::showNotification("DEqMS count table loaded.", type = "message")
      }
    })

    shiny::observeEvent(input$msstats_file, {
      table <- base::tryCatch(.read_upload(input$msstats_file), error = function(e) e)
      if (inherits(table, "error")) {
        shiny::showNotification(base::conditionMessage(table), type = "error")
      } else if (!base::is.null(table)) {
        rv$msstats_table <- table
        shiny::showNotification("MSstats feature table loaded.", type = "message")
      }
    })

    output$engine_load_status <- shiny::renderUI({
      if (isTRUE(rv$loaded)) {
        shiny::div(class = "pv-status pv-status-ready", base::paste0("✓ ", base::nrow(rv$dataset$expression_data), " proteins · ", base::ncol(rv$dataset$expression_data), " samples"))
      } else shiny::div(class = "pv-status pv-status-empty", "Data not loaded")
    })

    output$engine_group1_ui <- shiny::renderUI({
      values <- if (isTRUE(rv$loaded)) .protvis_dep_group_values(rv$dataset$sample_info, input$engine_group_column %||% "") else character()
      shiny::selectInput(session$ns("engine_group1"), "Group 1 (numerator)", choices = values, selected = if (base::length(values)) values[[1L]] else character())
    })
    output$engine_group2_ui <- shiny::renderUI({
      values <- if (isTRUE(rv$loaded)) .protvis_dep_group_values(rv$dataset$sample_info, input$engine_group_column %||% "") else character()
      shiny::selectInput(session$ns("engine_group2"), "Group 2 (denominator)", choices = values, selected = if (base::length(values) > 1L) values[[2L]] else character())
    })

    output$engine_availability <- DT::renderDT({
      count_cols <- if (isTRUE(rv$loaded)) .protvis_dep_count_columns(rv$count_table) else character()
      table <- base::data.frame(
        engine = c("limma", "DEqMS", "proDA", "MSstats"),
        package = c("limma", "DEqMS", "proDA", "MSstats"),
        package_available = c(
          base::requireNamespace("limma", quietly = TRUE),
          base::requireNamespace("DEqMS", quietly = TRUE),
          base::requireNamespace("proDA", quietly = TRUE),
          base::requireNamespace("MSstats", quietly = TRUE)
        ),
        data_requirement = c(
          "normalized protein matrix",
          if (base::length(count_cols)) base::paste0("counts: ", base::paste(count_cols, collapse = ", ")) else "peptide/PSM counts not detected",
          if (!base::is.null(rv$pre_imputation_dataset)) "Step4 pre-imputation matrix available" else "Step4 unavailable; current matrix would be used",
          if (!base::is.null(rv$msstats_table)) "feature-level table loaded" else "feature-level table not loaded"
        ),
        stringsAsFactors = FALSE
      )
      DT::datatable(table, rownames = FALSE, options = base::list(dom = "t", paging = FALSE, scrollX = TRUE))
    })

    shiny::observeEvent(input$run_engines, {
      shiny::req(rv$loaded)
      methods <- input$engine_methods %||% character()
      if (!base::length(methods)) {
        shiny::showNotification("Select at least one statistical engine.", type = "warning")
        return(invisible(NULL))
      }
      gcol <- input$engine_group_column %||% ""
      g1 <- input$engine_group1 %||% ""
      g2 <- input$engine_group2 %||% ""
      if (!base::nzchar(gcol) || !base::nzchar(g1) || !base::nzchar(g2) || identical(g1, g2)) {
        shiny::showNotification("Choose two different comparison groups.", type = "error")
        return(invisible(NULL))
      }
      sample_sets <- base::tryCatch(.protvis_dep_samples(rv$dataset, gcol, g1, g2), error = function(e) e)
      if (inherits(sample_sets, "error")) {
        shiny::showNotification(base::conditionMessage(sample_sets), type = "error")
        return(invisible(NULL))
      }
      current_mat <- .protvis_dep_matrix(rv$dataset)
      rv$results <- base::list()
      summaries <- base::list()
      fdr <- base::as.numeric(input$engine_fdr %||% 0.05)
      lfc <- base::as.numeric(input$engine_logfc %||% 0.27)

      run_one <- function(method) {
        base::tryCatch({
          result <- switch(
            method,
            limma = .protvis_dep_run_limma(current_mat, sample_sets$group1, sample_sets$group2, g1, g2),
            deqms = .protvis_dep_run_deqms(current_mat, sample_sets$group1, sample_sets$group2, g1, g2, rv$count_table),
            proda = {
              source_dataset <- rv$pre_imputation_dataset %||% rv$dataset
              proda_mat <- .protvis_dep_matrix(source_dataset)
              if (!base::all(sample_sets$samples %in% base::colnames(proda_mat))) {
                base::stop("The pre-imputation matrix does not contain all selected samples.")
              }
              .protvis_dep_run_proda(proda_mat, sample_sets$group1, sample_sets$group2, g1, g2)
            },
            msstats = {
              if (base::is.null(rv$msstats_table)) base::stop("Load an MSstats feature-level long table first.")
              .protvis_dep_run_msstats(rv$msstats_table, g1, g2)
            },
            base::stop("Unknown engine: ", method)
          )
          result$significant <- base::is.finite(result$adj.P.Val) & result$adj.P.Val <= fdr & base::is.finite(result$logFC) & base::abs(result$logFC) >= lfc
          list(result = result, summary = base::data.frame(
            method = base::unique(result$method)[[1L]], status = "Completed",
            proteins = base::nrow(result), significant = base::sum(result$significant, na.rm = TRUE),
            message = "", stringsAsFactors = FALSE
          ))
        }, error = function(e) {
          label <- base::names(.protvis_dep_engine_names)[base::match(method, .protvis_dep_engine_names)]
          list(result = NULL, summary = base::data.frame(
            method = label, status = "Unavailable", proteins = NA_integer_, significant = NA_integer_,
            message = base::conditionMessage(e), stringsAsFactors = FALSE
          ))
        })
      }

      shiny::withProgress(message = "Running statistical engines", value = 0, {
        for (i in base::seq_along(methods)) {
          shiny::setProgress((i - 1) / base::length(methods), detail = methods[[i]])
          one <- run_one(methods[[i]])
          summaries[[methods[[i]]]] <- one$summary
          if (!base::is.null(one$result)) rv$results[[methods[[i]]]] <- one$result
        }
        shiny::setProgress(1)
      })
      rv$summary <- base::do.call(base::rbind, summaries)
      rv$comparison <- base::list(
        grouping_column = gcol,
        group1 = g1,
        group2 = g2,
        sample_sets = sample_sets
      )

      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        dataset <- shared_state$dataset
        comparison_name <- base::paste0(g1, "_vs_", g2)
        existing <- dataset$analysis_results$dep_engines %||% base::list()
        existing[[comparison_name]] <- list(results = rv$results, summary = rv$summary, grouping_column = gcol, group1 = g1, group2 = g2, fdr = fdr, logfc = lfc)
        dataset$analysis_results$dep_engines <- existing
        dataset <- .protvis_append_process(
          dataset, "dep_engines", status = "success",
          parameters = list(methods = methods, grouping_column = gcol, group1 = g1, group2 = g2, fdr = fdr, logfc = lfc)
        )
        .protvis_ui_sync_state(dataset, shared_state)
        workdir <- shared_state$workdir %||% ""
        if (base::nzchar(base::as.character(workdir)) && base::dir.exists(workdir)) {
          .protvis_save_stage_dataset(dataset, base::file.path(workdir, "Step7_DEP_engines.rda"))
        }
      }
      shiny::showNotification("Selected differential-proteomics engines finished; see Run summary for availability/errors.", type = "message", duration = 5)
    })

    output$engine_summary <- DT::renderDT({
      table <- if (base::nrow(rv$summary)) rv$summary else base::data.frame(Message = "Run one or more statistical engines to compare results.")
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 10, scrollX = TRUE))
    })

    output$engine_result_tabs <- shiny::renderUI({
      if (!base::length(rv$results)) {
        return(shiny::div(
          "No completed engine results yet.",
          class = "pv-dep-engine-note"
        ))
      }

      comparison <- rv$comparison
      if (base::is.null(comparison)) {
        return(shiny::div(
          "Run the statistical engines to create result visualizations.",
          class = "pv-dep-engine-note"
        ))
      }
      comparison_label <- base::paste(
        comparison$group1, "vs", comparison$group2
      )

      tabs <- base::lapply(base::names(rv$results), function(method) {
        label <- base::names(.protvis_dep_engine_names)[
          base::match(method, .protvis_dep_engine_names)
        ]
        if (!base::length(label) || base::is.na(label)) label <- method

        table_id <- base::paste0("engine_table_", method)
        volcano_id <- base::paste0("engine_volcano_", method)
        heatmap_id <- base::paste0("engine_heatmap_", method)
        bar_id <- base::paste0("engine_bar_", method)
        volcano_download_id <- base::paste0(
          "download_engine_volcano_", method
        )
        heatmap_download_id <- base::paste0(
          "download_engine_heatmap_", method
        )
        bar_download_id <- base::paste0("download_engine_bar_", method)

        output[[table_id]] <- DT::renderDT({
          table <- rv$results[[method]]
          DT::datatable(
            table,
            rownames = FALSE,
            filter = "top",
            extensions = "Buttons",
            options = base::list(
              scrollX = TRUE,
              pageLength = 15,
              dom = "Bfrtip",
              buttons = c("copy", "csv", "excel")
            )
          )
        })

        output[[volcano_id]] <- shiny::renderPlot({
          shiny::req(rv$results[[method]], rv$comparison)
          .protvis_dep_engine_volcano_plot(
            rv$results[[method]],
            method = label,
            comparison = comparison_label,
            fdr = input$engine_fdr %||% 0.05,
            logfc = input$engine_logfc %||% 0.27,
            up = input[[base::paste0("engine_up_", method)]] %||% "#d62728",
            down = input[[base::paste0("engine_down_", method)]] %||% "#1f77b4",
            ns = input[[base::paste0("engine_ns_", method)]] %||% "#9aa6b2"
          )
        })

        output[[heatmap_id]] <- shiny::renderPlot({
          shiny::req(rv$results[[method]], rv$comparison, rv$dataset)
          .protvis_dep_engine_heatmap_plot(
            rv$results[[method]],
            dataset = rv$dataset,
            sample_sets = rv$comparison$sample_sets,
            method = label,
            comparison = comparison_label,
            fdr = input$engine_fdr %||% 0.05,
            logfc = input$engine_logfc %||% 0.27,
            top_n = input[[base::paste0("engine_heatmap_top_", method)]] %||% 50L,
            show_colnames = isTRUE(
              input[[base::paste0("engine_heatmap_colnames_", method)]]
            ),
            show_rownames = isTRUE(
              input[[base::paste0("engine_heatmap_rownames_", method)]]
            )
          )
        })

        output[[bar_id]] <- shiny::renderPlot({
          shiny::req(rv$results[[method]], rv$comparison)
          .protvis_dep_engine_bar_plot(
            rv$results[[method]],
            method = label,
            comparison = comparison_label,
            fdr = input$engine_fdr %||% 0.05,
            logfc = input$engine_logfc %||% 0.27,
            up = input[[base::paste0("engine_bar_up_", method)]] %||% "#d62728",
            down = input[[base::paste0("engine_bar_down_", method)]] %||% "#1f77b4"
          )
        })

        output[[volcano_download_id]] <- shiny::downloadHandler(
          filename = function() {
            base::paste0(
              .protvis_dep_engine_safe_name(label), "_",
              .protvis_dep_engine_safe_name(comparison_label),
              "_volcano.pdf"
            )
          },
          content = function(file) {
            plot <- .protvis_dep_engine_volcano_plot(
              rv$results[[method]],
              method = label,
              comparison = comparison_label,
              fdr = input$engine_fdr %||% 0.05,
              logfc = input$engine_logfc %||% 0.27,
              up = input[[base::paste0("engine_up_", method)]] %||% "#d62728",
              down = input[[base::paste0("engine_down_", method)]] %||% "#1f77b4",
              ns = input[[base::paste0("engine_ns_", method)]] %||% "#9aa6b2"
            )
            ggplot2::ggsave(
              file, plot = plot, device = "pdf",
              width = input[[base::paste0("engine_volcano_width_", method)]] %||% 8,
              height = input[[base::paste0("engine_volcano_height_", method)]] %||% 6,
              units = "in"
            )
          }
        )

        output[[heatmap_download_id]] <- shiny::downloadHandler(
          filename = function() {
            base::paste0(
              .protvis_dep_engine_safe_name(label), "_",
              .protvis_dep_engine_safe_name(comparison_label),
              "_heatmap.pdf"
            )
          },
          content = function(file) {
            plot <- .protvis_dep_engine_heatmap_plot(
              rv$results[[method]],
              dataset = rv$dataset,
              sample_sets = rv$comparison$sample_sets,
              method = label,
              comparison = comparison_label,
              fdr = input$engine_fdr %||% 0.05,
              logfc = input$engine_logfc %||% 0.27,
              top_n = input[[base::paste0("engine_heatmap_top_", method)]] %||% 50L,
              show_colnames = isTRUE(
                input[[base::paste0("engine_heatmap_colnames_", method)]]
              ),
              show_rownames = isTRUE(
                input[[base::paste0("engine_heatmap_rownames_", method)]]
              )
            )
            ggplot2::ggsave(
              file, plot = plot, device = "pdf",
              width = input[[base::paste0("engine_heatmap_width_", method)]] %||% 9,
              height = input[[base::paste0("engine_heatmap_height_", method)]] %||% 7,
              units = "in"
            )
          }
        )

        output[[bar_download_id]] <- shiny::downloadHandler(
          filename = function() {
            base::paste0(
              .protvis_dep_engine_safe_name(label), "_",
              .protvis_dep_engine_safe_name(comparison_label),
              "_DEP_count.pdf"
            )
          },
          content = function(file) {
            plot <- .protvis_dep_engine_bar_plot(
              rv$results[[method]],
              method = label,
              comparison = comparison_label,
              fdr = input$engine_fdr %||% 0.05,
              logfc = input$engine_logfc %||% 0.27,
              up = input[[base::paste0("engine_bar_up_", method)]] %||% "#d62728",
              down = input[[base::paste0("engine_bar_down_", method)]] %||% "#1f77b4"
            )
            ggplot2::ggsave(
              file, plot = plot, device = "pdf",
              width = input[[base::paste0("engine_bar_width_", method)]] %||% 7,
              height = input[[base::paste0("engine_bar_height_", method)]] %||% 5.5,
              units = "in"
            )
          }
        )

        bslib::nav_panel(
          label,
          bslib::navset_card_tab(
            full_screen = TRUE,
            bslib::nav_panel(
              "Table",
              DT::DTOutput(session$ns(table_id))
            ),
            bslib::nav_panel(
              "Volcano",
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  width = 245,
                  shiny::p(
                    "Uses the FDR and |log2FC| thresholds in the main Statistical engines sidebar.",
                    class = "pv-dep-engine-note"
                  ),
                  colourpicker::colourInput(
                    session$ns(base::paste0("engine_up_", method)),
                    "Upregulated", value = "#d62728"
                  ),
                  colourpicker::colourInput(
                    session$ns(base::paste0("engine_down_", method)),
                    "Downregulated", value = "#1f77b4"
                  ),
                  colourpicker::colourInput(
                    session$ns(base::paste0("engine_ns_", method)),
                    "Not significant", value = "#9aa6b2"
                  ),
                  shiny::numericInput(
                    session$ns(base::paste0("engine_volcano_width_", method)),
                    "PDF width (inch)", value = 8, min = 4, max = 20
                  ),
                  shiny::numericInput(
                    session$ns(base::paste0("engine_volcano_height_", method)),
                    "PDF height (inch)", value = 6, min = 4, max = 20
                  ),
                  shiny::downloadButton(
                    session$ns(volcano_download_id), "Download Volcano"
                  )
                ),
                shiny::plotOutput(
                  session$ns(volcano_id), height = "520px"
                )
              )
            ),
            bslib::nav_panel(
              "Heatmap",
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  width = 245,
                  shiny::numericInput(
                    session$ns(base::paste0("engine_heatmap_top_", method)),
                    "Top significant proteins",
                    value = 50, min = 2, max = 500, step = 1
                  ),
                  shiny::checkboxInput(
                    session$ns(base::paste0(
                      "engine_heatmap_colnames_", method
                    )),
                    "Show sample names", value = TRUE
                  ),
                  shiny::checkboxInput(
                    session$ns(base::paste0(
                      "engine_heatmap_rownames_", method
                    )),
                    "Show protein names", value = FALSE
                  ),
                  shiny::numericInput(
                    session$ns(base::paste0("engine_heatmap_width_", method)),
                    "PDF width (inch)", value = 9, min = 4, max = 20
                  ),
                  shiny::numericInput(
                    session$ns(base::paste0("engine_heatmap_height_", method)),
                    "PDF height (inch)", value = 7, min = 4, max = 20
                  ),
                  shiny::downloadButton(
                    session$ns(heatmap_download_id), "Download Heatmap"
                  )
                ),
                shiny::plotOutput(
                  session$ns(heatmap_id), height = "560px"
                )
              )
            ),
            bslib::nav_panel(
              "DEP count",
              bslib::layout_sidebar(
                sidebar = bslib::sidebar(
                  width = 245,
                  colourpicker::colourInput(
                    session$ns(base::paste0("engine_bar_up_", method)),
                    "Upregulated", value = "#d62728"
                  ),
                  colourpicker::colourInput(
                    session$ns(base::paste0("engine_bar_down_", method)),
                    "Downregulated", value = "#1f77b4"
                  ),
                  shiny::numericInput(
                    session$ns(base::paste0("engine_bar_width_", method)),
                    "PDF width (inch)", value = 7, min = 4, max = 20
                  ),
                  shiny::numericInput(
                    session$ns(base::paste0("engine_bar_height_", method)),
                    "PDF height (inch)", value = 5.5, min = 4, max = 20
                  ),
                  shiny::downloadButton(
                    session$ns(bar_download_id), "Download DEP Count"
                  )
                ),
                shiny::plotOutput(
                  session$ns(bar_id), height = "500px"
                )
              )
            )
          )
        )
      })
      base::do.call(
        bslib::navset_card_tab,
        c(base::list(full_screen = TRUE), tabs)
      )
    })

  })
}
