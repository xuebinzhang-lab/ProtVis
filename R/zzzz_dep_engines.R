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
      if (!base::length(rv$results)) return(shiny::div("No completed engine results yet.", class = "pv-dep-engine-note"))
      tabs <- base::lapply(base::names(rv$results), function(method) {
        label <- base::names(.protvis_dep_engine_names)[base::match(method, .protvis_dep_engine_names)]
        output_id <- base::paste0("engine_table_", method)
        output[[output_id]] <- DT::renderDT({
          table <- rv$results[[method]]
          DT::datatable(table, rownames = FALSE, filter = "top", extensions = "Buttons", options = base::list(scrollX = TRUE, pageLength = 15, dom = "Bfrtip", buttons = c("copy", "csv", "excel")))
        })
        bslib::nav_panel(label, DT::DTOutput(session$ns(output_id)))
      })
      do.call(bslib::navset_card_tab, c(list(full_screen = TRUE), tabs))
    })
  })
}
