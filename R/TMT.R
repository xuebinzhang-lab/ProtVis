#' TMT User Interface
#' Creates a proteomics-focused user interface for Tandem Mass Tag (TMT)
#' quality control, normalization, exploratory analysis, and visualization.
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @name TMT_ui
#' @export
#'
TMT_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "TMT",
    icon = bsicons::bs_icon("diagram-3"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "Input and annotation",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns("tmt_file"),
            label = "Upload TMT protein/peptide intensity table",
            multiple = FALSE,
            accept = c(".csv", ".tsv", ".txt")
          ),
          shiny::selectInput(ns("id_column"), "Protein / peptide ID column", choices = character(0)),
          shiny::helpText("Numeric columns are treated as TMT reporter channels.")
        ),
        bslib::accordion_panel(
          title = "Preprocessing",
          icon = bsicons::bs_icon("sliders"),
          shiny::checkboxInput(ns("log2_transform"), "Log2 transform intensities", TRUE),
          shiny::checkboxInput(ns("median_normalize"), "Median-center sample channels", TRUE),
          shiny::checkboxInput(ns("impute_missing"), "Impute missing values with half sample minimum", FALSE),
          shiny::numericInput(ns("top_n"), "Top variable features for PCA", value = 500, min = 50, max = 5000, step = 50),
          shiny::actionButton(ns("run_tmt"), "Run TMT QC", class = "btn btn-primary w-100")
        ),
        bslib::accordion_panel(
          title = "Downloads",
          icon = bsicons::bs_icon("download"),
          shiny::selectInput(
            ns("download_plot_type"),
            "Figure to download",
            choices = c(
              "Sample total intensity" = "sample_total",
              "Missing value rate" = "missing_rate",
              "Intensity boxplot" = "boxplot",
              "Intensity density" = "density",
              "Sample correlation" = "correlation",
              "PCA" = "pca",
              "Coefficient of variation" = "cv"
            )
          ),
          shiny::numericInput(ns("width"), "Figure width", value = 8, min = 3),
          shiny::numericInput(ns("height"), "Figure height", value = 6, min = 3),
          shiny::selectInput(ns("units"), "Figure unit", choices = c("in", "cm", "mm", "px"), selected = "in"),
          shiny::downloadButton(ns("download_plot"), "Download figure"),
          shiny::downloadButton(ns("download_processed"), "Download processed matrix")
        )
      ),
      bslib::page_fluid(
        bslib::navset_card_tab(
          bslib::nav_panel("Overview", shiny::verbatimTextOutput(ns("summary_text")), DT::DTOutput(ns("preview_table"))),
          bslib::nav_panel("Sample totals", shiny::plotOutput(ns("sample_total_plot"), height = "430px")),
          bslib::nav_panel("Missing values", shiny::plotOutput(ns("missing_rate_plot"), height = "430px")),
          bslib::nav_panel("Boxplot", shiny::plotOutput(ns("boxplot"), height = "430px")),
          bslib::nav_panel("Density", shiny::plotOutput(ns("density_plot"), height = "430px")),
          bslib::nav_panel("Correlation", shiny::plotOutput(ns("correlation_plot"), height = "500px")),
          bslib::nav_panel("PCA", shiny::plotOutput(ns("pca_plot"), height = "430px")),
          bslib::nav_panel("CV", shiny::plotOutput(ns("cv_plot"), height = "430px"))
        )
      )
    )
  )
}

# -------------------------------------------------------------------------

#' TMT Server Logic
#' Runs common TMT proteomics QC and exploratory visualizations including
#' missingness, sample totals, intensity distributions, correlations, PCA, and CV.
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @importFrom utils read.csv read.delim head write.csv
#' @importFrom ggplot2 ggplot aes geom_col geom_boxplot geom_density geom_point geom_histogram geom_text theme_minimal theme labs element_text ggsave
#' @name TMT_server
#' @export
#'
utils::globalVariables(c("Sample", "Intensity", "MissingRate", "TotalIntensity", "PC1", "PC2", "Group", "CV"))
TMT_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    read_tmt_table <- function(file_info) {
      ext <- base::tolower(tools::file_ext(file_info$name))
      if (ext %in% c("tsv", "txt")) {
        return(utils::read.delim(file_info$datapath, check.names = FALSE, stringsAsFactors = FALSE))
      }
      utils::read.csv(file_info$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    }

    raw_data <- shiny::reactive({
      shiny::req(input$tmt_file)
      read_tmt_table(input$tmt_file)
    })

    shiny::observeEvent(raw_data(), {
      dat <- raw_data()
      numeric_cols <- base::names(dat)[vapply(dat, is.numeric, logical(1))]
      id_default <- base::names(dat)[1]
      shiny::updateSelectInput(session, "id_column", choices = base::names(dat), selected = id_default)
    }, ignoreInit = TRUE)

    numeric_intensity_columns <- shiny::reactive({
      dat <- raw_data()
      numeric_cols <- base::names(dat)[vapply(dat, is.numeric, logical(1))]
      base::setdiff(numeric_cols, input$id_column)
    })

    processed <- shiny::eventReactive(input$run_tmt, {
      dat <- raw_data()
      intensity_cols <- numeric_intensity_columns()
      shiny::validate(
        shiny::need(base::length(intensity_cols) >= 2, "Please provide at least two numeric TMT intensity channels.")
      )
      ids <- if (!base::is.null(input$id_column) && input$id_column %in% base::names(dat)) dat[[input$id_column]] else base::seq_len(base::nrow(dat))
      mat <- as.matrix(dat[intensity_cols])
      storage.mode(mat) <- "numeric"
      raw_mat <- mat
      if (isTRUE(input$log2_transform)) {
        mat[mat < 0] <- NA_real_
        mat <- log2(mat + 1)
      }
      if (isTRUE(input$median_normalize)) {
        sample_medians <- apply(mat, 2, stats::median, na.rm = TRUE)
        grand_median <- stats::median(sample_medians, na.rm = TRUE)
        mat <- sweep(mat, 2, sample_medians, FUN = "-")
        mat <- mat + grand_median
      }
      if (isTRUE(input$impute_missing)) {
        for (j in seq_len(ncol(mat))) {
          col <- mat[, j]
          min_val <- suppressWarnings(min(col, na.rm = TRUE))
          if (is.finite(min_val)) col[is.na(col)] <- min_val / 2
          mat[, j] <- col
        }
      }
      processed_df <- data.frame(ID = ids, mat, check.names = FALSE)
      base::names(processed_df)[-1] <- intensity_cols
      list(
        raw = dat,
        raw_matrix = raw_mat,
        matrix = mat,
        processed_df = processed_df,
        intensity_cols = intensity_cols,
        ids = ids
      )
    })

    long_intensity <- shiny::reactive({
      res <- processed()
      data.frame(
        Sample = rep(res$intensity_cols, each = base::nrow(res$matrix)),
        Intensity = as.vector(res$matrix),
        stringsAsFactors = FALSE
      )
    })

    sample_total_plot <- shiny::reactive({
      res <- processed()
      total_df <- data.frame(
        Sample = res$intensity_cols,
        TotalIntensity = colSums(res$raw_matrix, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      ggplot2::ggplot(total_df, ggplot2::aes(x = Sample, y = TotalIntensity)) +
        ggplot2::geom_col(fill = "#2563eb") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = "TMT channel total intensity", x = NULL, y = "Total raw intensity")
    })

    missing_rate_plot <- shiny::reactive({
      res <- processed()
      missing_df <- data.frame(
        Sample = res$intensity_cols,
        MissingRate = colMeans(is.na(res$raw_matrix)),
        stringsAsFactors = FALSE
      )
      ggplot2::ggplot(missing_df, ggplot2::aes(x = Sample, y = MissingRate)) +
        ggplot2::geom_col(fill = "#dc2626") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = "Missing value rate by TMT channel", x = NULL, y = "Missing rate")
    })

    boxplot_plot <- shiny::reactive({
      ggplot2::ggplot(long_intensity(), ggplot2::aes(x = Sample, y = Intensity)) +
        ggplot2::geom_boxplot(fill = "#38bdf8", outlier.size = 0.6) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = "Normalized TMT intensity distribution", x = NULL, y = "Intensity")
    })

    density_plot <- shiny::reactive({
      ggplot2::ggplot(long_intensity(), ggplot2::aes(x = Intensity, color = Sample)) +
        ggplot2::geom_density(na.rm = TRUE) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = "TMT intensity density", x = "Intensity", y = "Density")
    })

    correlation_plot <- shiny::reactive({
      res <- processed()
      cor_mat <- stats::cor(res$matrix, use = "pairwise.complete.obs")
      old_par <- graphics::par(no.readonly = TRUE)
      on.exit(graphics::par(old_par), add = TRUE)
      graphics::par(mar = c(8, 8, 4, 2))
      graphics::image(seq_len(ncol(cor_mat)), seq_len(nrow(cor_mat)), t(cor_mat[nrow(cor_mat):1, ]), axes = FALSE, col = grDevices::hcl.colors(50, "RdYlBu", rev = TRUE), main = "Sample correlation")
      graphics::axis(1, at = seq_len(ncol(cor_mat)), labels = colnames(cor_mat), las = 2, cex.axis = 0.8)
      graphics::axis(2, at = seq_len(nrow(cor_mat)), labels = rev(rownames(cor_mat)), las = 2, cex.axis = 0.8)
      invisible(cor_mat)
    })

    pca_plot <- shiny::reactive({
      res <- processed()
      mat <- res$matrix
      row_sds <- apply(mat, 1, stats::sd, na.rm = TRUE)
      keep <- is.finite(row_sds) & row_sds > 0
      if (sum(keep) > input$top_n) {
        top_idx <- order(row_sds, decreasing = TRUE)[seq_len(input$top_n)]
        keep <- seq_along(row_sds) %in% top_idx
      }
      pca_mat <- t(mat[keep, , drop = FALSE])
      pca_mat[is.na(pca_mat)] <- 0
      shiny::validate(shiny::need(nrow(pca_mat) >= 2 && ncol(pca_mat) >= 2, "Need at least two samples and two variable features for PCA."))
      pca <- stats::prcomp(pca_mat, center = TRUE, scale. = TRUE)
      var_exp <- round(100 * (pca$sdev^2 / sum(pca$sdev^2))[1:2], 1)
      pca_df <- data.frame(Sample = rownames(pca$x), PC1 = pca$x[, 1], PC2 = pca$x[, 2], stringsAsFactors = FALSE)
      ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2, label = Sample)) +
        ggplot2::geom_point(size = 3) +
        ggplot2::geom_text(vjust = -0.7, size = 3) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = "PCA of TMT channels", x = paste0("PC1 (", var_exp[1], "%)"), y = paste0("PC2 (", var_exp[2], "%)"))
    })

    cv_plot <- shiny::reactive({
      res <- processed()
      row_mean <- rowMeans(res$matrix, na.rm = TRUE)
      row_sd <- apply(res$matrix, 1, stats::sd, na.rm = TRUE)
      cv_df <- data.frame(CV = row_sd / abs(row_mean), stringsAsFactors = FALSE)
      cv_df <- cv_df[is.finite(cv_df$CV), , drop = FALSE]
      ggplot2::ggplot(cv_df, ggplot2::aes(x = CV)) +
        ggplot2::geom_histogram(bins = 50, fill = "#22c55e", color = "white") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = "Feature coefficient of variation", x = "CV", y = "Feature count")
    })

    plot_by_type <- function(type) {
      switch(type,
             sample_total = sample_total_plot(),
             missing_rate = missing_rate_plot(),
             boxplot = boxplot_plot(),
             density = density_plot(),
             pca = pca_plot(),
             cv = cv_plot(),
             sample_total_plot())
    }

    output$summary_text <- shiny::renderPrint({
      res <- processed()
      cat("TMT QC summary\n")
      cat("Rows:", nrow(res$raw), "\n")
      cat("TMT numeric channels:", length(res$intensity_cols), "\n")
      cat("Log2 transform:", isTRUE(input$log2_transform), "\n")
      cat("Median normalization:", isTRUE(input$median_normalize), "\n")
      cat("Missing-value imputation:", isTRUE(input$impute_missing), "\n")
      cat("Overall missing rate:", round(mean(is.na(res$raw_matrix)), 4), "\n")
    })

    output$preview_table <- DT::renderDT({
      DT::datatable(utils::head(processed()$processed_df, 20), options = list(scrollX = TRUE, pageLength = 10))
    })
    output$sample_total_plot <- shiny::renderPlot(print(sample_total_plot()))
    output$missing_rate_plot <- shiny::renderPlot(print(missing_rate_plot()))
    output$boxplot <- shiny::renderPlot(print(boxplot_plot()))
    output$density_plot <- shiny::renderPlot(print(density_plot()))
    output$correlation_plot <- shiny::renderPlot(correlation_plot())
    output$pca_plot <- shiny::renderPlot(print(pca_plot()))
    output$cv_plot <- shiny::renderPlot(print(cv_plot()))

    output$download_plot <- shiny::downloadHandler(
      filename = function() base::paste0("tmt-", input$download_plot_type, "-", base::Sys.Date(), ".png"),
      content = function(file) {
        if (identical(input$download_plot_type, "correlation")) {
          grDevices::png(file, width = input$width, height = input$height, units = input$units, res = 150)
          correlation_plot()
          grDevices::dev.off()
        } else {
          ggplot2::ggsave(file, plot = plot_by_type(input$download_plot_type), width = input$width, height = input$height, units = input$units)
        }
      }
    )

    output$download_processed <- shiny::downloadHandler(
      filename = function() base::paste0("tmt-processed-matrix-", base::Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(processed()$processed_df, file, row.names = FALSE)
    )
  })
}
