# Extended normalization workspace -----------------------------------------
# Keeps the existing pipeline contract (Step5 -> Step6) but adds selectable
# Median, Quantile, VSN, Cyclic Loess and RLR methods plus side-by-side
# benchmarking.  This file is collated after data_normalization.R, so the
# public UI/server names below replace the earlier fixed-median implementation.

.protvis_norm_methods <- c(
  "Median" = "median",
  "Quantile" = "quantile",
  "VSN" = "vsn",
  "Cyclic Loess" = "cyclic_loess",
  "RLR" = "rlr"
)

.protvis_norm_matrix <- function(x) {
  df <- base::as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  ids <- base::rownames(df)
  df <- base::as.data.frame(
    base::lapply(df, function(z) suppressWarnings(base::as.numeric(base::as.character(z)))),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  mat <- base::as.matrix(df)
  storage.mode(mat) <- "numeric"
  if (!base::is.null(ids) && base::length(ids) == base::nrow(mat)) {
    base::rownames(mat) <- ids
  }
  mat
}

.protvis_norm_transformation_method <- function(dataset) {
  if (!inherits(dataset, "ProtVis_dataset")) return("Unknown")
  method <- dataset$analysis_results$transformation$method %||% NULL
  if (base::is.null(method)) {
    method <- dataset$process_info$parameters$transformation$method %||% NULL
  }
  if (base::is.null(method) || !base::length(method)) "Unknown" else base::as.character(method[[1L]])
}

.protvis_norm_resolve_scale <- function(input_scale = "auto", transformation_method = "Unknown") {
  input_scale <- base::tolower(base::as.character(input_scale %||% "auto"))
  if (!identical(input_scale, "auto")) return(input_scale)
  method <- base::tolower(base::trimws(base::as.character(transformation_method %||% "unknown")))
  if (method %in% c("log2")) return("log2")
  if (method %in% c("log10")) return("log10")
  if (method %in% c("none", "raw", "unknown", "")) return("raw")
  "other"
}

.protvis_norm_vsn_input <- function(mat, scale) {
  raw <- switch(
    scale,
    log2 = 2 ^ mat,
    log10 = 10 ^ mat,
    raw = mat,
    base::stop(
      "VSN requires raw intensities, or values that can be back-transformed from log2/log10. ",
      "The current transformed scale cannot be safely reconstructed.",
      call. = FALSE
    )
  )
  if (base::any(!base::is.finite(raw), na.rm = TRUE)) {
    base::stop("Back-transformation produced non-finite intensities; VSN was not run.", call. = FALSE)
  }
  finite <- raw[base::is.finite(raw)]
  if (!base::length(finite) || base::any(finite < 0)) {
    base::stop("VSN requires non-negative intensity measurements.", call. = FALSE)
  }
  raw
}

.protvis_norm_rlr <- function(mat) {
  if (!base::requireNamespace("MASS", quietly = TRUE)) {
    base::stop("Package 'MASS' is required for RLR normalization.", call. = FALSE)
  }
  reference <- matrixStats::rowMedians(mat, na.rm = TRUE)
  out <- base::matrix(
    NA_real_, nrow = base::nrow(mat), ncol = base::ncol(mat),
    dimnames = base::dimnames(mat)
  )
  for (j in base::seq_len(base::ncol(mat))) {
    y <- mat[, j]
    keep <- base::is.finite(y) & base::is.finite(reference)
    if (base::sum(keep) < 10L) {
      base::stop("RLR requires at least 10 complete protein measurements per sample.", call. = FALSE)
    }
    fit <- MASS::rlm(y[keep] ~ reference[keep], maxit = 100)
    co <- stats::coef(fit)
    if (base::length(co) < 2L || !base::is.finite(co[[2L]]) || base::abs(co[[2L]]) < 1e-8) {
      base::stop("RLR produced an unstable regression slope for sample ", base::colnames(mat)[j], ".", call. = FALSE)
    }
    out[keep, j] <- (y[keep] - co[[1L]]) / co[[2L]]
  }
  out
}

.protvis_apply_normalization <- function(mat, method = "median", scale = "raw") {
  mat <- .protvis_norm_matrix(mat)
  method <- base::tolower(base::as.character(method %||% "median"))
  if (!base::nrow(mat) || !base::ncol(mat)) {
    base::stop("Expression matrix is empty.", call. = FALSE)
  }

  out <- switch(
    method,
    median = {
      med <- base::apply(mat, 2, stats::median, na.rm = TRUE)
      base::sweep(mat, 2, med, FUN = "-")
    },
    quantile = {
      q <- preprocessCore::normalize.quantiles(mat, copy = TRUE)
      base::dimnames(q) <- base::dimnames(mat)
      q
    },
    vsn = {
      if (!base::requireNamespace("vsn", quietly = TRUE)) {
        base::stop("Package 'vsn' is required for VSN normalization.", call. = FALSE)
      }
      raw <- .protvis_norm_vsn_input(mat, scale)
      v <- vsn::justvsn(raw, verbose = FALSE)
      v <- base::as.matrix(v)
      base::dimnames(v) <- base::dimnames(mat)
      v
    },
    cyclic_loess = {
      v <- limma::normalizeCyclicLoess(mat, method = "fast")
      base::dimnames(v) <- base::dimnames(mat)
      v
    },
    rlr = .protvis_norm_rlr(mat),
    base::stop("Unknown normalization method: ", method, call. = FALSE)
  )
  out <- base::as.matrix(out)
  storage.mode(out) <- "numeric"
  base::dimnames(out) <- base::dimnames(mat)
  out
}

.protvis_norm_group_vector <- function(sample_info, samples) {
  if (base::is.null(sample_info) || !base::nrow(sample_info)) return(NULL)
  info <- base::as.data.frame(sample_info, stringsAsFactors = FALSE, check.names = FALSE)
  if (!"sample_id" %in% base::names(info)) return(NULL)
  index <- base::match(samples, base::as.character(info$sample_id))
  candidates <- base::intersect(c("group", "condition", "tissue", "class"), base::names(info))
  for (column in candidates) {
    values <- base::as.character(info[[column]])[index]
    values <- base::trimws(values)
    ok <- !base::is.na(values) & base::nzchar(values) & values != "Unassigned"
    if (base::length(base::unique(values[ok])) >= 2L) return(values)
  }
  NULL
}

.protvis_norm_metrics <- function(mat, sample_info = NULL, method = "") {
  mat <- .protvis_norm_matrix(mat)
  sample_medians <- base::apply(mat, 2, stats::median, na.rm = TRUE)
  sample_iqr <- base::apply(mat, 2, stats::IQR, na.rm = TRUE)
  cors <- suppressWarnings(stats::cor(mat, use = "pairwise.complete.obs", method = "pearson"))
  cor_values <- if (base::is.matrix(cors) && base::ncol(cors) > 1L) {
    cors[base::lower.tri(cors)]
  } else {
    NA_real_
  }
  groups <- .protvis_norm_group_vector(sample_info, base::colnames(mat))
  within_group_sd <- NA_real_
  if (!base::is.null(groups)) {
    values <- base::numeric()
    for (group in base::unique(groups[!base::is.na(groups) & base::nzchar(groups)])) {
      idx <- base::which(groups == group)
      if (base::length(idx) < 2L) next
      sds <- base::apply(mat[, idx, drop = FALSE], 1, stats::sd, na.rm = TRUE)
      values <- c(values, sds[base::is.finite(sds)])
    }
    if (base::length(values)) within_group_sd <- stats::median(values, na.rm = TRUE)
  }
  base::data.frame(
    method = base::as.character(method),
    sample_median_sd = stats::sd(sample_medians, na.rm = TRUE),
    sample_iqr_sd = stats::sd(sample_iqr, na.rm = TRUE),
    median_pairwise_correlation = stats::median(cor_values, na.rm = TRUE),
    median_within_group_sd = within_group_sd,
    missing_fraction = base::mean(base::is.na(mat)),
    stringsAsFactors = FALSE
  )
}

.protvis_compare_normalizations <- function(mat, sample_info = NULL, scale = "raw",
                                            methods = unname(.protvis_norm_methods)) {
  matrices <- base::list()
  metrics <- base::list()
  status <- base::list()
  for (method in methods) {
    result <- base::tryCatch(
      .protvis_apply_normalization(mat, method = method, scale = scale),
      error = function(e) e
    )
    label <- base::names(.protvis_norm_methods)[base::match(method, .protvis_norm_methods)]
    if (base::is.na(label) || !base::length(label)) label <- method
    if (inherits(result, "error")) {
      status[[method]] <- base::data.frame(
        method = label, status = "Unavailable", message = base::conditionMessage(result),
        stringsAsFactors = FALSE
      )
      next
    }
    matrices[[method]] <- result
    metrics[[method]] <- .protvis_norm_metrics(result, sample_info, label)
    status[[method]] <- base::data.frame(
      method = label, status = "Completed", message = "",
      stringsAsFactors = FALSE
    )
  }
  list(
    matrices = matrices,
    metrics = if (base::length(metrics)) base::do.call(base::rbind, metrics) else base::data.frame(),
    status = if (base::length(status)) base::do.call(base::rbind, status) else base::data.frame()
  )
}

#' Extended Data Normalization UI
#' @inheritParams data_normalization_ui
#' @export
data_normalization_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    protvis_data_input_style(),
    shiny::tags$style(shiny::HTML("\n      .pv-norm-method-note{font-size:12px;color:#657789;line-height:1.5;}\n      .pv-norm-grid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:14px;}\n      @media(max-width:1000px){.pv-norm-grid{grid-template-columns:1fr;}}\n    ")),
    bslib::layout_sidebar(
      class = "pv-mq-shell",
      sidebar = bslib::sidebar(
        width = 335,
        class = "pv-sidebar-card",
        shiny::actionButton(ns("load_data"), "Load data", class = "btn btn-primary fw-bold pv-load-button pv-run-button"),
        shiny::uiOutput(ns("load_status_panel")),
        shiny::hr(),
        shiny::selectInput(
          ns("input_scale"), "Input scale",
          choices = c(
            "Auto from Transformation step" = "auto",
            "Raw intensity" = "raw",
            "log2" = "log2",
            "log10" = "log10"
          ), selected = "auto"
        ),
        shiny::selectInput(
          ns("normalization_method"), "Normalization method",
          choices = c(
            "Auto (source recommended)" = "auto",
            "MaxQuant recommended · median center + row shift" = "maxquant_recommended",
            .protvis_norm_methods
          ),
          selected = "auto"
        ),
        shiny::p(
          "Auto uses the source-specific preset. For MaxQuant it reproduces the archived workflow: sample-wise median subtraction followed by the row-wise +abs(min)+5 shift (exact zeros become 1). Other normalization methods remain available for comparison.",
          class = "pv-norm-method-note"
        ),
        shiny::actionButton(ns("run_normalization"), "Run selected normalization", class = "btn btn-success fw-bold pv-load-button pv-run-button"),
        shiny::uiOutput(ns("normalization_status_panel")),
        shiny::hr(),
        shiny::actionButton(ns("compare_methods"), "Compare all methods", icon = bsicons::bs_icon("bar-chart-line"), class = "btn btn-outline-primary fw-bold pv-load-button pv-run-button"),
        shiny::uiOutput(ns("comparison_status_panel")),
        shiny::hr(),
        colourpicker::colourInput(ns("original_boxplot_color"), "Original boxplot", value = "#B51F9C"),
        colourpicker::colourInput(ns("normalized_boxplot_color"), "Normalized boxplot", value = "#FF7F0E"),
        shiny::numericInput(ns("plot_width"), "Download width (inches)", value = 8, min = 1, max = 40),
        shiny::numericInput(ns("plot_height"), "Download height (inches)", value = 7, min = 1, max = 40),
        shiny::downloadButton(ns("download_original_plot"), "Download original PDF"),
        shiny::downloadButton(ns("download_normalized_plot"), "Download normalized PDF")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Normalization and method benchmarking"),
        bslib::navset_card_tab(
          bslib::nav_panel(
            "Original data",
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("Original matrix"), DT::DTOutput(ns("originalData"))),
              bslib::card(bslib::card_header("Sample distributions"), shiny::plotOutput(ns("originalPlot"), height = "520px"))
            )
          ),
          bslib::nav_panel(
            "Selected method",
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("Normalized matrix"), DT::DTOutput(ns("normalizedData"))),
              bslib::card(bslib::card_header("Normalized distributions"), shiny::plotOutput(ns("normalizedPlot"), height = "520px"))
            )
          ),
          bslib::nav_panel(
            "Method comparison",
            shiny::p(
              "Lower sample-median/IQR dispersion and lower within-group SD indicate tighter technical alignment; higher pairwise correlation indicates greater sample-profile concordance. These metrics are diagnostics, not an automatic method recommendation.",
              class = "pv-norm-method-note"
            ),
            bslib::layout_columns(
              col_widths = c(7, 5),
              bslib::card(bslib::card_header("Comparison metrics"), DT::DTOutput(ns("comparison_table"))),
              bslib::card(bslib::card_header("Method status"), DT::DTOutput(ns("comparison_status_table")))
            ),
            shiny::br(),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("Metric overview"), shiny::plotOutput(ns("comparison_metric_plot"), height = "430px")),
              bslib::card(bslib::card_header("Distribution comparison"), shiny::plotOutput(ns("comparison_distribution_plot"), height = "430px"))
            )
          )
        )
      )
    )
  )
}

#' Extended Data Normalization Server
#' @inheritParams data_normalization_server
#' @export
data_normalization_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      normalization_done = FALSE,
      comparison = NULL,
      transformation_method = "Unknown",
      normalization_method = NULL
    )

    valid_workdir <- function() {
      directory <- shared_state$workdir
      if (base::length(directory) != 1L || base::is.na(directory) ||
          !base::nzchar(base::trimws(base::as.character(directory))) ||
          !base::dir.exists(base::as.character(directory))) {
        base::stop("Set a valid working directory in Project init first.", call. = FALSE)
      }
      base::normalizePath(base::as.character(directory), winslash = "/", mustWork = TRUE)
    }

    original_matrix_numeric <- shiny::reactive({
      shiny::req(rv$expression_matrix)
      .protvis_norm_matrix(rv$expression_matrix)
    })

    resolved_scale <- shiny::reactive({
      .protvis_norm_resolve_scale(input$input_scale %||% "auto", rv$transformation_method)
    })

    shiny::observeEvent(input$load_data, {
      if (!.protvis_begin_run(shared_state, "normalization_load", session, "load_data")) return(invisible(NULL))
      on.exit(.protvis_end_run(shared_state, "normalization_load", session, "load_data"), add = TRUE)
      dataset <- NULL
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        dataset <- shared_state$dataset
      } else {
        workdir <- base::tryCatch(valid_workdir(), error = function(e) NULL)
        if (!base::is.null(workdir)) {
          path <- base::file.path(workdir, "Step5_data_imputation.rda")
          if (base::file.exists(path)) dataset <- .protvis_load_stage_dataset(path, expression_names = "imputed_df")
        }
      }
      if (base::is.null(dataset)) {
        rv$load_success <- FALSE
        shiny::showNotification("No imputed ProtVis_dataset was found. Run Data Imputation first.", type = "error")
        return(invisible(NULL))
      }
      shared_state$dataset <- dataset
      rv$sample_info <- dataset$sample_info
      rv$expression_matrix <- dataset$expression_data
      rv$transformation_method <- .protvis_norm_transformation_method(dataset)
      rv$normalized_matrix <- NULL
      rv$comparison <- NULL
      rv$normalization_done <- FALSE
      rv$normalization_method <- NULL
      rv$load_success <- TRUE
      shiny::showNotification(
        base::paste0("Data loaded. Upstream transformation: ", rv$transformation_method, "."),
        type = "message"
      )
    })

    output$load_status_panel <- shiny::renderUI({
      if (isTRUE(rv$load_success)) {
        shiny::div(class = "pv-status pv-status-ready", base::paste0("✓ Data loaded · transformation: ", rv$transformation_method))
      } else shiny::div(class = "pv-status pv-status-empty", "× Data not loaded")
    })

    output$normalization_status_panel <- shiny::renderUI({
      if (isTRUE(rv$normalization_done)) {
        shiny::div(
          class = "pv-status pv-status-ready",
          base::paste0(
            "✓ ", rv$normalization_method %||% "normalization",
            " completed · scale: ", resolved_scale()
          )
        )
      } else shiny::div(class = "pv-status pv-status-empty", "Normalization not run yet")
    })

    output$comparison_status_panel <- shiny::renderUI({
      if (!base::is.null(rv$comparison)) {
        done <- base::sum(rv$comparison$status$status == "Completed")
        shiny::div(class = "pv-status pv-status-ready", base::paste0("✓ Compared ", done, " methods"))
      } else shiny::div(class = "pv-status pv-status-empty", "Method comparison not run yet")
    })

    shiny::observeEvent(input$run_normalization, {
      if (!.protvis_begin_run(shared_state, "normalization", session, "run_normalization")) return(invisible(NULL))
      on.exit(.protvis_end_run(shared_state, "normalization", session, "run_normalization"), add = TRUE)
      shiny::req(rv$load_success)
      dataset <- if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        shared_state$dataset
      } else {
        create_protvis_dataset(
          original_matrix_numeric(), sample_info = rv$sample_info
        )
      }
      requested_method <- input$normalization_method %||% "auto"
      method <- .protvis_resolve_preprocessing_method(
        dataset, "normalization", requested_method
      )
      result <- base::tryCatch({
        if (method %in% c("maxquant_recommended", "maxquant_default")) {
          .protvis_maxquant_normalize_matrix(
            original_matrix_numeric(), row_shift = 5, zero_value = 1
          )
        } else {
          .protvis_apply_normalization(
            original_matrix_numeric(), method, resolved_scale()
          )
        }
      }, error = function(e) e)
      if (inherits(result, "error")) {
        shiny::showNotification(base::conditionMessage(result), type = "error", duration = 8)
        return(invisible(NULL))
      }
      rv$normalized_matrix <- base::as.data.frame(
        result, stringsAsFactors = FALSE, check.names = FALSE
      )
      rv$normalization_done <- TRUE
      rv$normalization_method <- method

      dataset <- .protvis_update_expression(dataset, rv$normalized_matrix)
      dataset <- .protvis_new_analysis_dataset(
        dataset, "normalization",
        list(
          method = method,
          requested_method = requested_method,
          input_scale = resolved_scale(),
          row_shift = if (identical(method, "maxquant_recommended")) 5 else NULL,
          zero_value = if (identical(method, "maxquant_recommended")) 1 else NULL,
          comparison_methods = base::names(.protvis_norm_methods)
        )
      )
      dataset <- .protvis_store_preprocessing_result(
        dataset,
        stage = "normalization",
        method = method,
        extra = list(
          input_scale = resolved_scale(),
          requested_method = requested_method
        )
      )
      dataset <- .protvis_append_process(
        dataset, "normalization", status = "success",
        parameters = list(
          method = method,
          requested_method = requested_method,
          input_scale = resolved_scale(),
          row_shift = if (identical(method, "maxquant_recommended")) 5 else NULL,
          zero_value = if (identical(method, "maxquant_recommended")) 1 else NULL
        )
      )
      .protvis_ui_sync_state(dataset, shared_state)
      workdir <- valid_workdir()
      .protvis_save_stage_dataset(dataset, base::file.path(workdir, "Step6_data_normalization.rda"))
      shiny::showNotification("Normalization completed and saved to Step6_data_normalization.rda.", type = "message")
    })

    shiny::observeEvent(input$compare_methods, {
      if (!.protvis_begin_run(shared_state, "normalization_compare", session, "compare_methods")) return(invisible(NULL))
      on.exit(.protvis_end_run(shared_state, "normalization_compare", session, "compare_methods"), add = TRUE)
      shiny::req(rv$load_success)
      shiny::withProgress(message = "Comparing normalization methods", value = 0.1, {
        rv$comparison <- .protvis_compare_normalizations(
          original_matrix_numeric(), rv$sample_info, scale = resolved_scale()
        )
        shiny::setProgress(1)
      })
      shiny::showNotification("Normalization method comparison completed.", type = "message")
    })

    output$originalData <- DT::renderDT({
      mat <- base::as.data.frame(original_matrix_numeric(), check.names = FALSE)
      DT::datatable(mat, options = base::list(scrollX = TRUE, pageLength = 10), rownames = TRUE)
    })

    output$normalizedData <- DT::renderDT({
      if (base::is.null(rv$normalized_matrix)) {
        return(DT::datatable(base::data.frame(Message = "Run a selected normalization method first."), rownames = FALSE, options = base::list(dom = "t", paging = FALSE)))
      }
      DT::datatable(rv$normalized_matrix, options = base::list(scrollX = TRUE, pageLength = 10), rownames = TRUE)
    })

    .norm_long <- function(mat) {
      df <- .protvis_rownames_to_column(base::as.data.frame(mat, check.names = FALSE), "ID")
      tidyr::pivot_longer(df, cols = -ID, names_to = "sample_id", values_to = "intensity")
    }

    output$originalPlot <- shiny::renderPlot({
      df <- .norm_long(original_matrix_numeric())
      ggplot2::ggplot(df, ggplot2::aes(x = sample_id, y = intensity)) +
        ggplot2::geom_boxplot(fill = input$original_boxplot_color, outlier.size = 0.15) +
        ggplot2::coord_flip() + ggplot2::theme_bw() + ggplot2::labs(x = NULL, y = "Intensity / transformed value")
    })

    output$normalizedPlot <- shiny::renderPlot({
      shiny::req(rv$normalized_matrix)
      df <- .norm_long(rv$normalized_matrix)
      ggplot2::ggplot(df, ggplot2::aes(x = sample_id, y = intensity)) +
        ggplot2::geom_boxplot(fill = input$normalized_boxplot_color, outlier.size = 0.15) +
        ggplot2::coord_flip() + ggplot2::theme_bw() + ggplot2::labs(x = NULL, y = "Normalized value")
    })

    output$comparison_table <- DT::renderDT({
      table <- if (base::is.null(rv$comparison)) base::data.frame(Message = "Click Compare all methods.") else rv$comparison$metrics
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 10, scrollX = TRUE))
    })

    output$comparison_status_table <- DT::renderDT({
      table <- if (base::is.null(rv$comparison)) base::data.frame(Message = "No comparison run yet.") else rv$comparison$status
      DT::datatable(table, rownames = FALSE, options = base::list(dom = "t", paging = FALSE, scrollX = TRUE))
    })

    output$comparison_metric_plot <- shiny::renderPlot({
      shiny::req(rv$comparison, base::nrow(rv$comparison$metrics) > 0)
      metrics <- rv$comparison$metrics
      cols <- c("sample_median_sd", "sample_iqr_sd", "median_within_group_sd")
      long <- tidyr::pivot_longer(metrics[, c("method", cols), drop = FALSE], cols = -method, names_to = "metric", values_to = "value")
      ggplot2::ggplot(long, ggplot2::aes(x = method, y = value, fill = metric)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)) +
        ggplot2::labs(x = NULL, y = "Diagnostic value", fill = "Metric")
    })

    output$comparison_distribution_plot <- shiny::renderPlot({
      shiny::req(rv$comparison, base::length(rv$comparison$matrices) > 0)
      rows <- base::list()
      for (method in base::names(rv$comparison$matrices)) {
        mat <- rv$comparison$matrices[[method]]
        # Sample up to 1500 proteins per method to keep the plotting object small.
        idx <- base::seq_len(base::min(1500L, base::nrow(mat)))
        sub <- mat[idx, , drop = FALSE]
        tmp <- .norm_long(sub)
        tmp$method <- base::names(.protvis_norm_methods)[base::match(method, .protvis_norm_methods)]
        rows[[method]] <- tmp
      }
      long <- base::do.call(base::rbind, rows)
      ggplot2::ggplot(long, ggplot2::aes(x = intensity, group = interaction(method, sample_id))) +
        ggplot2::geom_density(alpha = 0.2) +
        ggplot2::facet_wrap(~method, scales = "free") +
        ggplot2::theme_bw() + ggplot2::labs(x = "Normalized value", y = "Density")
    })

    .download_boxplot <- function(file, mat, color, title, ylab) {
      grDevices::pdf(file, width = input$plot_width, height = input$plot_height)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::boxplot(mat, las = 2, col = color, main = title, ylab = ylab, outline = FALSE, cex.axis = 0.7)
    }

    output$download_original_plot <- shiny::downloadHandler(
      filename = function() "original_data_boxplot.pdf",
      content = function(file) .download_boxplot(file, original_matrix_numeric(), input$original_boxplot_color, "Original Data", "Value")
    )
    output$download_normalized_plot <- shiny::downloadHandler(
      filename = function() base::paste0("normalized_", input$normalization_method %||% "median", ".pdf"),
      content = function(file) {
        shiny::req(rv$normalized_matrix)
        .download_boxplot(file, rv$normalized_matrix, input$normalized_boxplot_color, "Normalized Data", "Normalized value")
      }
    )

    base::return(rv)
  })
}
