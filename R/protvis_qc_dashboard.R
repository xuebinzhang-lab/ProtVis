# Project QC summaries and dashboard.

.protvis_cv <- function(values) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) < 2L) return(NA_real_)
  centre <- mean(values)
  if (!is.finite(centre) || centre == 0) return(NA_real_)
  stats::sd(values) / abs(centre)
}

#' Calculate project-level quality-control metrics.
#' @export
protvis_qc_summary <- function(object) {
  object <- protvis_standardize_dataset(object)
  x <- as.matrix(object$expression_data)
  storage.mode(x) <- "numeric"
  missing <- is.na(x) | !is.finite(x)
  completeness <- if (length(x)) 1 - mean(missing) else NA_real_
  cvs <- if (nrow(x)) apply(x, 1L, .protvis_cv) else numeric()
  groups <- if ("group" %in% names(object$sample_info)) {
    unique(as.character(object$sample_info$group))
  } else character()
  workflow <- protvis_workflow_status(object)
  sample_count <- if (ncol(x) > 0L) ncol(x) else nrow(object$sample_info)
  data.frame(
    proteins = nrow(x),
    samples = sample_count,
    completeness = completeness,
    missing_fraction = if (length(x)) mean(missing) else NA_real_,
    median_protein_cv = if (any(is.finite(cvs))) {
      stats::median(cvs[is.finite(cvs)], na.rm = TRUE)
    } else NA_real_,
    groups = length(groups[nzchar(groups) & !is.na(groups)]),
    completed_stages = sum(workflow$status == "complete"),
    total_stages = nrow(workflow),
    stringsAsFactors = FALSE
  )
}

#' Calculate per-sample quality-control metrics.
#' @export
protvis_sample_qc <- function(object) {
  object <- protvis_standardize_dataset(object)
  x <- as.matrix(object$expression_data)
  storage.mode(x) <- "numeric"
  samples <- colnames(x)
  info <- object$sample_info
  index <- match(samples, as.character(info$sample_id))
  group <- if ("group" %in% names(info)) {
    as.character(info$group[index])
  } else rep(NA_character_, length(samples))
  if (!length(samples) && nrow(info)) {
    return(data.frame(
      sample_id = as.character(info$sample_id),
      group = if ("group" %in% names(info)) as.character(info$group) else
        rep(NA_character_, nrow(info)),
      identified_proteins = rep(NA_integer_, nrow(info)),
      missing_fraction = rep(NA_real_, nrow(info)),
      median_intensity = rep(NA_real_, nrow(info)),
      total_intensity = rep(NA_real_, nrow(info)),
      stringsAsFactors = FALSE
    ))
  }
  values <- lapply(seq_along(samples), function(j) {
    v <- x[, j]
    finite <- is.finite(v) & !is.na(v)
    data.frame(
      sample_id = samples[[j]],
      group = group[[j]],
      identified_proteins = sum(finite),
      missing_fraction = mean(!finite),
      median_intensity = if (any(finite)) stats::median(v[finite]) else NA_real_,
      total_intensity = if (any(finite)) sum(v[finite]) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  if (!length(values)) {
    return(data.frame(
      sample_id = character(), group = character(),
      identified_proteins = integer(), missing_fraction = numeric(),
      median_intensity = numeric(), total_intensity = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, values)
}

.protvis_sage_find_column <- function(data, candidates = character(),
                                      patterns = character()) {
  if (!is.data.frame(data) || !length(names(data))) return(NA_character_)
  lower <- tolower(names(data))
  candidate_lower <- tolower(candidates)
  hit <- match(candidate_lower, lower, nomatch = 0L)
  hit <- hit[hit > 0L]
  if (length(hit)) return(names(data)[hit[[1L]]])
  for (pattern in patterns) {
    idx <- grep(pattern, lower, perl = TRUE)
    if (length(idx)) return(names(data)[idx[[1L]]])
  }
  NA_character_
}

.protvis_sage_numeric <- function(data, column) {
  if (is.na(column) || !column %in% names(data)) return(rep(NA_real_, nrow(data)))
  suppressWarnings(as.numeric(data[[column]]))
}

#' Summarize Sage PSM quality metrics.
#' @export
protvis_sage_qc <- function(psms) {
  if (is.null(psms) || !is.data.frame(psms) || !nrow(psms)) {
    empty <- data.frame(stringsAsFactors = FALSE)
    return(list(
      summary = data.frame(
        Metric = "PSMs", Value = 0, stringsAsFactors = FALSE
      ),
      per_run = empty,
      charge = data.frame(charge = numeric(), psms = integer()),
      mass_error = data.frame(run = character(), ppm = numeric()),
      q_value = data.frame(run = character(), q_value = numeric()),
      peptide_length = data.frame(run = character(), peptide_length = integer()),
      missed_cleavages = data.frame(
        missed_cleavages = numeric(), psms = integer()
      ),
      retention_time = data.frame(
        run = character(), retention_time = numeric()
      )
    ))
  }
  run_col <- .protvis_sage_find_column(
    psms, c("filename", "file", "run", "spectrum_file"),
    c("file", "run")
  )
  peptide_col <- .protvis_sage_find_column(
    psms, c("peptide", "sequence", "stripped_peptide"),
    c("peptide", "sequence")
  )
  protein_col <- .protvis_sage_find_column(
    psms, c("proteins", "protein"), c("^protein")
  )
  charge_col <- .protvis_sage_find_column(psms, c("charge"), c("charge"))
  q_col <- .protvis_sage_find_column(
    psms,
    c("spectrum_q", "peptide_q", "q_value", "qvalue", "q-value"),
    c("(^|_)q($|_)|qvalue|q.value")
  )
  ppm_col <- .protvis_sage_find_column(
    psms,
    c("precursor_ppm", "mass_error_ppm", "delta_mass_ppm", "ppm"),
    c("precursor.*ppm|mass.*ppm|ppm.*error")
  )
  missed_col <- .protvis_sage_find_column(
    psms,
    c("missed_cleavages", "num_missed_cleavages"),
    c("missed.*cleav")
  )
  rt_col <- .protvis_sage_find_column(
    psms,
    c("retention_time", "retentiontime", "rt", "rtime"),
    c("retention.*time|(^|_)rt($|_)|rtime")
  )

  q <- .protvis_sage_numeric(psms, q_col)
  ppm <- .protvis_sage_numeric(psms, ppm_col)
  charge <- .protvis_sage_numeric(psms, charge_col)
  missed <- .protvis_sage_numeric(psms, missed_col)
  retention_time <- .protvis_sage_numeric(psms, rt_col)

  peptide_values <- if (!is.na(peptide_col)) {
    as.character(psms[[peptide_col]])
  } else rep(NA_character_, nrow(psms))
  peptides <- unique(peptide_values)
  clean_peptides <- gsub("\\[[^]]*\\]|\\([^)]*\\)", "", peptide_values)
  clean_peptides <- gsub("[^A-Za-z]", "", clean_peptides)
  peptide_length <- nchar(clean_peptides)
  peptide_length[!nzchar(clean_peptides) | is.na(peptide_values)] <- NA_integer_
  proteins <- if (!is.na(protein_col)) {
    trimws(unlist(strsplit(
      paste(as.character(psms[[protein_col]]), collapse = ";"),
      ";", fixed = TRUE
    )))
  } else character()
  proteins <- unique(proteins[nzchar(proteins) & !grepl("^rev_", proteins, ignore.case = TRUE)])

  summary <- data.frame(
    Metric = c(
      "PSMs", "Unique peptides", "Target proteins", "PSMs at q <= 0.01",
      "Median precursor error (ppm)", "Median charge", "Median missed cleavages"
    ),
    Value = c(
      nrow(psms),
      length(peptides[nzchar(peptides) & !is.na(peptides)]),
      length(proteins),
      if (any(is.finite(q))) sum(q <= 0.01, na.rm = TRUE) else NA_real_,
      if (any(is.finite(ppm))) stats::median(ppm[is.finite(ppm)]) else NA_real_,
      if (any(is.finite(charge))) stats::median(charge[is.finite(charge)]) else NA_real_,
      if (any(is.finite(missed))) stats::median(missed[is.finite(missed)]) else NA_real_
    ),
    stringsAsFactors = FALSE
  )

  runs <- if (!is.na(run_col)) as.character(psms[[run_col]]) else rep("All", nrow(psms))
  runs[is.na(runs) | !nzchar(runs)] <- "Unknown"
  per_run <- do.call(rbind, lapply(unique(runs), function(run) {
    idx <- which(runs == run)
    pep <- if (!is.na(peptide_col)) {
      unique(as.character(psms[[peptide_col]][idx]))
    } else character()
    prot <- if (!is.na(protein_col)) {
      trimws(unlist(strsplit(
        paste(as.character(psms[[protein_col]][idx]), collapse = ";"),
        ";", fixed = TRUE
      )))
    } else character()
    prot <- unique(prot[nzchar(prot) & !grepl("^rev_", prot, ignore.case = TRUE)])
    data.frame(
      run = run,
      psms = length(idx),
      unique_peptides = length(pep[nzchar(pep) & !is.na(pep)]),
      target_proteins = length(prot),
      median_q = if (any(is.finite(q[idx]))) stats::median(q[idx], na.rm = TRUE) else NA_real_,
      median_precursor_ppm = if (any(is.finite(ppm[idx]))) stats::median(ppm[idx], na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  }))

  charge_table <- if (any(is.finite(charge))) {
    tab <- as.data.frame(table(charge[is.finite(charge)]), stringsAsFactors = FALSE)
    names(tab) <- c("charge", "psms")
    tab$charge <- suppressWarnings(as.numeric(as.character(tab$charge)))
    tab
  } else data.frame(charge = numeric(), psms = integer())

  mass_error <- data.frame(
    run = runs[is.finite(ppm)],
    ppm = ppm[is.finite(ppm)],
    stringsAsFactors = FALSE
  )
  q_value <- data.frame(
    run = runs[is.finite(q)],
    q_value = q[is.finite(q)],
    stringsAsFactors = FALSE
  )
  peptide_length_table <- data.frame(
    run = runs[is.finite(peptide_length)],
    peptide_length = peptide_length[is.finite(peptide_length)],
    stringsAsFactors = FALSE
  )
  missed_table <- if (any(is.finite(missed))) {
    tab <- as.data.frame(table(missed[is.finite(missed)]), stringsAsFactors = FALSE)
    names(tab) <- c("missed_cleavages", "psms")
    tab$missed_cleavages <- suppressWarnings(
      as.numeric(as.character(tab$missed_cleavages))
    )
    tab
  } else data.frame(missed_cleavages = numeric(), psms = integer())
  rt_table <- data.frame(
    run = runs[is.finite(retention_time)],
    retention_time = retention_time[is.finite(retention_time)],
    stringsAsFactors = FALSE
  )

  list(
    summary = summary, per_run = per_run,
    charge = charge_table, mass_error = mass_error,
    q_value = q_value, peptide_length = peptide_length_table,
    missed_cleavages = missed_table, retention_time = rt_table
  )
}

.protvis_dashboard_style <- function() {
  shiny::tags$style(shiny::HTML("
    .pv-qc-shell { padding: 1rem 0 2rem; }
    .pv-qc-metrics { display:grid; grid-template-columns:repeat(5,minmax(150px,1fr));
      gap:.75rem; margin:.25rem 0 1rem; }
    .pv-qc-metric { background:#fff; border:1px solid #dbe8f3; border-radius:14px;
      padding:1rem 1.05rem; box-shadow:0 4px 15px rgba(31,78,109,.05); }
    .pv-qc-metric .value { font-size:1.75rem; font-weight:800; color:#176fa3; }
    .pv-qc-metric .label { color:#607080; font-size:.82rem; }
    .pv-workflow-state { display:flex; align-items:center; gap:.7rem; padding:.55rem .7rem;
      border-bottom:1px solid #edf2f6; }
    .pv-workflow-state:last-child { border-bottom:0; }
    .pv-workflow-dot { width:.7rem; height:.7rem; border-radius:50%; background:#9aa9b5; }
    .pv-workflow-dot.complete { background:#2fb176; }
    .pv-workflow-dot.failed { background:#d9534f; }
    .pv-workflow-dot.invalidated { background:#f0ad4e; }
    @media(max-width:900px){.pv-qc-metrics{grid-template-columns:repeat(2,1fr);}}
  "))
}

#' Project quality-control dashboard UI.
#' @export
protvis_dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    .protvis_dashboard_style(),
    shiny::div(
      class = "pv-qc-shell",
      bslib::card(
        bslib::card_header(
          shiny::div(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            shiny::div(
              shiny::h3("Project QC Dashboard", class = "mb-1"),
              shiny::p(
                "Project-wide identification, completeness, workflow, provenance, and Sage search QC.",
                class = "text-muted mb-0"
              )
            ),
            shiny::div(
              shiny::actionButton(
                ns("resume"), "Resume workflow",
                icon = bsicons::bs_icon("play-circle"),
                class = "btn btn-outline-primary"
              ),
              shiny::downloadButton(
                ns("report"), "HTML report",
                class = "btn btn-outline-secondary"
              )
            )
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("empty_state")),
          shiny::uiOutput(ns("metrics")),
          bslib::layout_columns(
            bslib::card(
              bslib::card_header("Workflow state"),
              bslib::card_body(shiny::uiOutput(ns("workflow_graph")))
            ),
            bslib::card(
              bslib::card_header("Missing values by sample"),
              bslib::card_body(shiny::plotOutput(ns("missing_plot"), height = "280px"))
            ),
            col_widths = c(6, 6)
          ),
          bslib::navset_card_tab(
            bslib::nav_panel("Sample QC", DT::DTOutput(ns("sample_qc"))),
            bslib::nav_panel("Workflow", DT::DTOutput(ns("workflow_table"))),
            bslib::nav_panel("Provenance", DT::DTOutput(ns("provenance"))),
            bslib::nav_panel("Input files", DT::DTOutput(ns("files"))),
            bslib::nav_panel("Sage QC", DT::DTOutput(ns("sage_qc"))),
            bslib::nav_panel("Sage runs", DT::DTOutput(ns("sage_runs")))
          )
        )
      )
    )
  )
}

#' Project quality-control dashboard server.
#' @export
protvis_dashboard_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    current_dataset <- shiny::reactive({
      dataset <- shared_state$dataset
      if (!inherits(dataset, "ProtVis_dataset")) return(NULL)
      tryCatch(protvis_standardize_dataset(dataset), error = function(e) dataset)
    })

    output$empty_state <- shiny::renderUI({
      if (!is.null(current_dataset())) return(NULL)
      shiny::div(
        class = "alert alert-info",
        "No active ProtVis_dataset. Initialize a project or import/search data first."
      )
    })

    output$metrics <- shiny::renderUI({
      object <- current_dataset()
      if (is.null(object)) return(NULL)
      x <- protvis_qc_summary(object)
      metric <- function(value, label) {
        shiny::div(
          class = "pv-qc-metric",
          shiny::div(class = "value", value),
          shiny::div(class = "label", label)
        )
      }
      shiny::div(
        class = "pv-qc-metrics",
        metric(format(x$proteins, big.mark = ","), "Proteins"),
        metric(format(x$samples, big.mark = ","), "Samples"),
        metric(if (is.finite(x$completeness)) {
          paste0(round(100 * x$completeness, 1), "%")
        } else "NA", "Data completeness"),
        metric(if (is.finite(x$median_protein_cv)) {
          paste0(round(100 * x$median_protein_cv, 1), "%")
        } else "NA", "Median protein CV"),
        metric(paste0(x$completed_stages, "/", x$total_stages), "Workflow stages")
      )
    })

    output$workflow_graph <- shiny::renderUI({
      object <- current_dataset()
      if (is.null(object)) return(NULL)
      table <- protvis_workflow_status(object)
      shiny::tagList(lapply(seq_len(nrow(table)), function(i) {
        row <- table[i, , drop = FALSE]
        shiny::div(
          class = "pv-workflow-state",
          shiny::span(class = paste("pv-workflow-dot", row$status)),
          shiny::div(
            shiny::strong(row$label),
            shiny::div(
              class = "small text-muted",
              paste0(
                row$status,
                if (nzchar(row$depends_on)) paste0(" · depends on ", row$depends_on) else ""
              )
            )
          )
        )
      }))
    })

    output$missing_plot <- shiny::renderPlot({
      object <- current_dataset()
      shiny::req(object)
      qc <- protvis_sample_qc(object)
      if (!nrow(qc) || !any(is.finite(qc$missing_fraction))) {
        graphics::plot.new()
        graphics::text(.5, .5, "Quantification QC will appear after Search / import.")
      } else {
        graphics::barplot(
          100 * qc$missing_fraction,
          names.arg = qc$sample_id,
          las = 2, ylab = "Missing values (%)", xlab = "",
          border = NA, cex.names = 0.7
        )
        graphics::abline(h = 50, lty = 2)
      }
    })

    output$sample_qc <- DT::renderDT({
      object <- current_dataset()
      if (is.null(object)) return(data.frame(Message = "No active dataset."))
      DT::datatable(
        protvis_sample_qc(object), rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$workflow_table <- DT::renderDT({
      object <- current_dataset()
      if (is.null(object)) return(data.frame(Message = "No active dataset."))
      DT::datatable(
        protvis_workflow_status(object), rownames = FALSE,
        options = list(dom = "t", scrollX = TRUE)
      )
    })

    output$provenance <- DT::renderDT({
      object <- current_dataset()
      if (is.null(object)) return(data.frame(Message = "No active dataset."))
      DT::datatable(
        protvis_provenance(object)$events, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    output$files <- DT::renderDT({
      object <- current_dataset()
      if (is.null(object)) return(data.frame(Message = "No active dataset."))
      DT::datatable(
        protvis_provenance(object)$files, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    sage_qc <- shiny::reactive({
      object <- current_dataset()
      if (is.null(object)) return(NULL)
      psms <- protvis_assay(object, "psm")
      protvis_sage_qc(psms)
    })

    output$sage_qc <- DT::renderDT({
      value <- sage_qc()
      if (is.null(value)) return(data.frame(Message = "No Sage PSMs available."))
      DT::datatable(value$summary, rownames = FALSE, options = list(dom = "t"))
    })
    output$sage_runs <- DT::renderDT({
      value <- sage_qc()
      if (is.null(value) || !nrow(value$per_run)) {
        return(data.frame(Message = "No run-level Sage QC available."))
      }
      DT::datatable(
        value$per_run, rownames = FALSE,
        options = list(pageLength = 15, scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$resume, {
      object <- current_dataset()
      if (is.null(object)) {
        shiny::showNotification("No active dataset to resume.", type = "warning")
        return(invisible(NULL))
      }
      if (identical(object$metadata$workflow_stage, "Sage_staging") ||
          nrow(object$expression_data) == 0L) {
        shiny::showNotification(
          "The project is waiting for Search. Complete Sage search before resuming downstream analysis.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      directory <- shared_state$workdir %||%
        object$checkpoint_info$directory %||% getwd()
      value <- tryCatch(
        shiny::withProgress(
          message = "Resuming ProtVis workflow", value = 0.2,
          {
            result <- resume_protvis_pipeline(
              object, checkpoint_dir = directory
            )
            shiny::incProgress(0.7)
            result
          }
        ),
        error = function(e) {
          shiny::showNotification(
            paste("Resume failed:", conditionMessage(e)), type = "error"
          )
          NULL
        }
      )
      if (!is.null(value)) {
        value <- protvis_standardize_dataset(value)
        .protvis_ui_sync_state(value, shared_state)
        shiny::showNotification("Workflow resume completed.", type = "message")
      }
    }, ignoreInit = TRUE)

    output$report <- shiny::downloadHandler(
      filename = function() {
        paste0("ProtVis_project_QC_", Sys.Date(), ".html")
      },
      content = function(file) {
        object <- current_dataset()
        shiny::req(object)
        write_protvis_report(object, file)
      }
    )

    invisible(current_dataset)
  })
}
