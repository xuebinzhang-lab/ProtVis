#' Overview UI Module
#' Creates the user interface for the overview analysis module
#' @param id Character string specifying the namespace id
#' @return A Shiny UI tagList containing the overview analysis interface
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @name overview_ui
#' @export
#'
overview_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        shiny::uiOutput(ns("load_status_panel")),
        bslib::accordion(
          bslib::accordion_panel(
            title = "Correlation",
            icon = correlation_icon,
            shiny::selectInput(
              inputId = ns("cor_method"),
              label = "Correlation Method:",
              choices = c("Pearson", "Spearman", "Kendall"),
              selected = "Pearson"
            ),
            colourpicker::colourInput(
              ns("cor_high_color"),
              "High Color",
              value = "purple"
            ),
            colourpicker::colourInput(
              ns("cor_mid_color"),
              "middle Color",
              value = "black"
            ),
            colourpicker::colourInput(
              ns("cor_low_color"),
              "Low Color",
              value = "yellow"
            ),
            shiny::numericInput(ns("cor_color_min"), "Set Min Value", value = -1, step = 0.1),
            shiny::numericInput(ns("cor_color_max"), "Set Max Value", value = 1, step = 0.1),
            shiny::checkboxInput(ns("cor_cluster_rows"), "Cluster rows", TRUE),
            shiny::checkboxInput(ns("cor_cluster_columns"), "Cluster columns", TRUE),
            shiny::checkboxInput(ns("cor_show_numbers"), "Show correlation values", TRUE),
            shiny::checkboxInput(ns("cor_show_column_names"), "Show sample names", FALSE),
            shiny::actionButton(ns("run_correlation"), "Run Correlation"),
            shiny::numericInput(ns("cor_plot_width"), "Download Plot Width (inches)", value = 10),
            shiny::numericInput(ns("cor_plot_height"), "Download Plot Height (inches)", value = 7),
            shiny::downloadButton(ns("cor_download_pdf"), "Download PDF")
          ),
          bslib::accordion_panel(
            title = "Expression pattern",
            icon = expression_pattern_icon,
            shiny::sliderInput(
              inputId = ns("exp_top_n"),
              label = "Top N Features:",
              min = 50,
              max = 2000,
              value = 500,
              step = 50
            ),
            shiny::checkboxInput(
              inputId = ns("exp_scale"),
              label = "Scale Data",
              value = TRUE
            ),
            shiny::selectInput(
              ns("exp_scale_method"),
              "Scaling direction",
              choices = c("By protein (row)" = "row",
                          "By sample (column)" = "column",
                          "No scaling" = "none"),
              selected = "row"
            ),
            shiny::checkboxInput(ns("exp_cluster_rows"), "Cluster samples", TRUE),
            shiny::checkboxInput(ns("exp_cluster_columns"), "Cluster proteins", TRUE),
            shiny::checkboxInput(ns("exp_show_feature_names"), "Show protein names", FALSE),
            colourpicker::colourInput(
              ns("exp_high_color"),
              "High Color",
              value = "purple"
            ),
            colourpicker::colourInput(
              ns("exp_mid_color"),
              "middle Color",
              value = "black"
            ),
            colourpicker::colourInput(
              ns("exp_low_color"),
              "Low Color",
              value = "yellow"
            ),
            shiny::numericInput(ns("exp_color_min"), "Set Min Value", value = -1, step = 0.1),
            shiny::numericInput(ns("exp_color_max"), "Set Max Value", value = 1, step = 0.1),
            shiny::actionButton(ns("run_expression"), "Run Expression"),
            shiny::numericInput(ns("exp_plot_width"), "Download Plot Width (inches)", value = 10),
            shiny::numericInput(ns("exp_plot_height"), "Download Plot Height (inches)", value = 7),
            shiny::downloadButton(ns("exp_download_pdf"), "Download PDF")
          ),
          bslib::accordion_panel(
            title = "Dimensionality Reduction",
            icon = dimensionality_reduction_icon,
            shiny::selectInput(
              inputId = ns("dimReductionMethod"),
              label = "Select Method:",
              choices = c("PCA", "PCoA", "tSNE", "UMAP", "NMDS"),
              selected = "UMAP"
            ),
            shiny::actionButton(ns("DR_analyse"), "Run"),
            shiny::numericInput(ns("dr_plot_width"), "Download Plot Width (inches)", value = 10),
            shiny::numericInput(ns("dr_plot_height"), "Download Plot Height (inches)", value = 7),
            shiny::downloadButton(ns("dr_download_before_pdf"), "Download Before Normalization"),
            shiny::downloadButton(ns("dr_download_after_pdf"), "Download After Normalization"),
            shiny::downloadButton(ns("dr_download_both_pdf"), "Download Both Plots")
          ),
          bslib::accordion_panel(
            title = "Proteomics QC",
            icon = bsicons::bs_icon("clipboard-pulse"),
            shiny::numericInput(ns("qc_top_n"), "Top variable features for QC PCA", value = 500, min = 50, max = 5000, step = 50),
            shiny::selectInput(
              ns("qc_download_plot_type"),
              "QC figure to download",
              choices = c(
                "Sample total intensity" = "sample_total",
                "Missing value rate" = "missing_rate",
                "Intensity boxplot" = "boxplot",
                "Intensity density" = "density",
                "PCA" = "pca",
                "Coefficient of variation" = "cv"
              )
            ),
            shiny::numericInput(ns("qc_plot_width"), "Download Plot Width (inches)", value = 8),
            shiny::numericInput(ns("qc_plot_height"), "Download Plot Height (inches)", value = 6),
            shiny::downloadButton(ns("qc_download_pdf"), "Download QC PDF"),
            shiny::downloadButton(ns("qc_download_matrix"), "Download Normalized Matrix")
          )
        )
      ),
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,
          gap = "1rem",
          bslib::card(
            height = "520px",
            bslib::card_header("Correlation"),
            bslib::card_body(
              shiny::plotOutput(ns("cor_res"), height = "430px")
            )
          ),
          bslib::card(
            height = "520px",
            bslib::card_header("Expression pattern"),
            bslib::card_body(
              shiny::plotOutput(ns("expression_pattern"), height = "430px")
            )
          ),
          bslib::card(
            height = "520px",
            bslib::card_header("Dimensionality reduction analyse before normalization"),
            bslib::card_body(
              shiny::plotOutput(ns("DR_BeforeNormalization"), height = "430px")
            )
          ),
          bslib::card(
            height = "520px",
            bslib::card_header("Dimensionality reduction analyse after normalization"),
            bslib::card_body(
              shiny::plotOutput(ns("DR_AfterNormalization"), height = "430px")
            )
          ),
          bslib::card(
            height = "520px",
            bslib::card_header("Proteomics QC summary"),
            bslib::card_body(
              shiny::verbatimTextOutput(ns("qc_summary")),
              shiny::plotOutput(ns("qc_sample_total_plot"), height = "300px")
            )
          ),
          bslib::card(
            height = "520px",
            bslib::card_header("Proteomics missing values and distributions"),
            bslib::card_body(
              shiny::plotOutput(ns("qc_missing_rate_plot"), height = "180px"),
              shiny::plotOutput(ns("qc_boxplot"), height = "180px"),
              shiny::plotOutput(ns("qc_density_plot"), height = "180px")
            )
          ),
          bslib::card(
            height = "520px",
            bslib::card_header("Proteomics PCA and CV"),
            bslib::card_body(
              shiny::plotOutput(ns("qc_pca_plot"), height = "220px"),
              shiny::plotOutput(ns("qc_cv_plot"), height = "180px")
            )
          )
        )
      )
    )
  )
}

#' Overview Server Module
#' Server-side logic for the overview analysis module
#' @param id Character string specifying the namespace id
#' @param shared_state Reactive values shared across modules
#' @return A module server function that handles the overview analysis logic
#' @import shiny
#' @importFrom dplyr left_join mutate select case_when
#' @importFrom stringr str_split str_remove_all
#' @importFrom ComplexHeatmap Heatmap rowAnnotation draw
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar grid.text
#' @importFrom grDevices pdf dev.off
#' @importFrom matrixStats rowVars
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom vegan metaMDS
#' @importFrom ggsci scale_color_lancet scale_fill_lancet
#' @importFrom gridExtra grid.arrange
#' @importFrom ggplot2 ggplot aes geom_point stat_ellipse theme_bw labs geom_col geom_boxplot geom_density geom_histogram geom_text theme_minimal theme element_text
#' @name overview_server
#' @export
#'
utils::globalVariables(c(
  "tissue", "tissue2", "species", "Type", "Species",
  "V1", "V2", "SampleType"
))

overview_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {

    standardize_overview_matrix <- function(data) {
      matrix <- base::as.matrix(data)
      storage.mode(matrix) <- "numeric"
      matrix[!is.finite(matrix)] <- NA_real_
      if (base::ncol(matrix) == 0L) return(matrix)
      for (j in base::seq_len(base::ncol(matrix))) {
        observed <- matrix[, j]
        center <- if (base::any(is.finite(observed))) {
          stats::median(observed[is.finite(observed)])
        } else {
          0
        }
        matrix[, j] <- observed - center
      }
      matrix
    }

    rv <- shiny::reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      imputed_matrix = NULL,
      cor_results = NULL,
      exp_results = NULL
    )

    shiny::observeEvent(input$load_data, {
      # ProtVis_dataset is the canonical source for current projects.  The
      # Step5/Step6 files below are retained only for legacy projects.
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        matrix <- base::as.matrix(shared_state$dataset$expression_data)
        storage.mode(matrix) <- "numeric"
        matrix <- standardize_overview_matrix(matrix)
        rv$sample_info <- shared_state$dataset$sample_info
        rv$imputed_matrix <- matrix
        rv$normalized_matrix <- matrix
        rv$cor_results <- NULL
        rv$exp_results <- NULL
        rv$load_success <- TRUE
        shiny::showNotification(
          "✅ ProtVis_dataset loaded successfully.", type = "message"
        )
        return(invisible(NULL))
      }
      shiny::req(shared_state$workdir)

      step5_path <- base::file.path(shared_state$workdir, "Step5_data_imputation.rda")
      step6_path <- base::file.path(shared_state$workdir, "Step6_data_normalization.rda")

      if (!base::file.exists(step5_path) || !base::file.exists(step6_path)) {
        missing_files <- c(step5_path, step6_path)[
          !base::file.exists(c(step5_path, step6_path))
        ]

        shiny::showNotification(
          base::paste(
            "File(s) not found:",
            base::paste(base::basename(missing_files), collapse = ", ")
          ),
          type = "error"
        )
        rv$load_success <- FALSE
        rv$cor_results <- NULL
        rv$exp_results <- NULL
        return()
      }

      tryCatch({
        e5 <- base::new.env()
        base::load(step5_path, envir = e5)

        e6 <- base::new.env()
        base::load(step6_path, envir = e6)

        if (!base::exists("sample_info", envir = e5) ||
            !base::exists("imputed_df", envir = e5)) {
          shiny::showNotification(
            "Required data not found in Step5 file.",
            type = "error"
          )
          rv$load_success <- FALSE
          rv$cor_results <- NULL
          rv$exp_results <- NULL
          return()
        }

        if (!base::exists("normalized_data", envir = e6)) {
          shiny::showNotification(
            "Required data not found in Step6 file.",
            type = "error"
          )
          rv$load_success <- FALSE
          rv$cor_results <- NULL
          rv$exp_results <- NULL
          return()
        }

        imputed_mat <- base::as.data.frame(
          e5$imputed_df,
          stringsAsFactors = FALSE
        )
        normalized_mat <- base::as.data.frame(
          e6$normalized_data,
          stringsAsFactors = FALSE
        )

        if ("ID" %in% base::colnames(imputed_mat)) {
          ids <- imputed_mat$ID
          imputed_mat <- imputed_mat[, base::setdiff(base::colnames(imputed_mat), "ID"), drop = FALSE]
          base::rownames(imputed_mat) <- ids
        }

        if ("ID" %in% base::colnames(normalized_mat)) {
          ids <- normalized_mat$ID
          normalized_mat <- normalized_mat[, base::setdiff(base::colnames(normalized_mat), "ID"), drop = FALSE]
          base::rownames(normalized_mat) <- ids
        }

        imputed_ids <- base::rownames(imputed_mat)
        normalized_ids <- base::rownames(normalized_mat)

        imputed_mat <- base::as.data.frame(
          base::lapply(imputed_mat, function(x) base::as.numeric(base::as.character(x))),
          stringsAsFactors = FALSE
        )
        normalized_mat <- base::as.data.frame(
          base::lapply(normalized_mat, function(x) base::as.numeric(base::as.character(x))),
          stringsAsFactors = FALSE
        )

        base::rownames(imputed_mat) <- imputed_ids
        base::rownames(normalized_mat) <- normalized_ids

        if (!base::identical(base::colnames(imputed_mat), base::colnames(normalized_mat))) {
          shiny::showNotification(
            "Sample names don't match between imputed and normalized data.",
            type = "error"
          )
          rv$load_success <- FALSE
          rv$cor_results <- NULL
          rv$exp_results <- NULL
          return()
        }

        if (!base::identical(base::rownames(imputed_mat), base::rownames(normalized_mat))) {
          shiny::showNotification(
            "Feature IDs don't match between imputed and normalized data.",
            type = "error"
          )
          rv$load_success <- FALSE
          rv$cor_results <- NULL
          rv$exp_results <- NULL
          return()
        }

        rv$sample_info <- e5$sample_info
        rv$imputed_matrix <- imputed_mat
        rv$normalized_matrix <- normalized_mat
        rv$cor_results <- NULL
        rv$exp_results <- NULL
        rv$load_success <- TRUE

        shiny::showNotification(
          "✅ Both datasets loaded successfully.",
          type = "message"
        )
      }, error = function(e) {
        shiny::showNotification(
          base::paste("Error loading data:", e$message),
          type = "error"
        )
        rv$load_success <- FALSE
        rv$cor_results <- NULL
        rv$exp_results <- NULL
      })
    })

    output$load_status_panel <- shiny::renderUI({
      if (isTRUE(rv$load_success)) {
        shiny::div(
          shiny::span(
            "✅ Both datasets loaded successfully",
            style = "color: green;"
          ),
          shiny::br(),
          base::paste(
            "Imputed data:",
            base::nrow(rv$imputed_matrix), "proteins,",
            base::ncol(rv$imputed_matrix), "samples"
          ),
          shiny::br(),
          base::paste(
            "Normalized data:",
            base::nrow(rv$normalized_matrix), "proteins,",
            base::ncol(rv$normalized_matrix), "samples"
          )
        )
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })

    shiny::observeEvent(input$run_correlation, {
      shiny::req(rv$normalized_matrix)

      shiny::withProgress(message = "Calculating correlations...", value = 0.5, {
        matrix <- base::as.matrix(rv$normalized_matrix)
        storage.mode(matrix) <- "numeric"
        if (base::ncol(matrix) < 2L) {
          shiny::showNotification(
            "At least two samples are required for correlation analysis.",
            type = "error"
          )
          rv$cor_results <- NULL
          return()
        }
        result <- tryCatch(
          stats::cor(
            matrix,
            method = base::tolower(input$cor_method),
            use = "pairwise.complete.obs"
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Correlation failed:", conditionMessage(e)),
              type = "error"
            )
            NULL
          }
        )
        if (!base::is.null(result)) {
          # Constant or entirely missing samples have undefined correlation.
          # Preserve that information in the result; the heatmap converts it
          # to a neutral display value without modifying the dataset.
          diag(result) <- 1
        }
        rv$cor_results <- result
        shiny::incProgress(1, detail = "Done")
      })
    })

    make_metadata_annotation <- function(matrix) {
      metadata_share <- base::data.frame(
        sample_id = base::colnames(matrix), stringsAsFactors = FALSE
      )
      info <- rv$sample_info
      if (base::is.null(info) || !is.data.frame(info)) {
        info <- base::data.frame(sample_id = character(),
                                 stringsAsFactors = FALSE)
      }
      info_index <- match(metadata_share$sample_id, info$sample_id)
      if ("maxquant_id" %in% base::colnames(info)) {
        fallback_index <- match(metadata_share$sample_id, info$maxquant_id)
        info_index[is.na(info_index)] <- fallback_index[is.na(info_index)]
      }
      for (column in base::setdiff(base::colnames(info), "sample_id")) {
        metadata_share[[column]] <- info[[column]][info_index]
      }
      tissue_values <- if ("tissue2" %in% names(metadata_share)) {
        as.character(metadata_share$tissue2)
      } else if ("tissue" %in% names(metadata_share)) {
        as.character(metadata_share$tissue)
      } else {
        rep(NA_character_, nrow(metadata_share))
      }
      tissue_lower <- tolower(tissue_values)
      tissue_values[grepl("root|below[ ._-]*ground|underground", tissue_lower)] <- "Below-ground"
      tissue_values[grepl("leaf|shoot|stem|above[ ._-]*ground|aerial", tissue_lower)] <- "Above-ground"
      sample_lower <- tolower(metadata_share$sample_id)
      fallback_tissue <- ifelse(
        grepl("root|below[ ._-]*ground|underground", sample_lower), "Below-ground",
        ifelse(grepl("leaf|shoot|stem|above[ ._-]*ground|aerial", sample_lower),
               "Above-ground", NA_character_)
      )
      channel <- suppressWarnings(as.integer(sub("^([0-9]+)_.*$", "\\1", metadata_share$sample_id)))
      fallback_tissue[is.na(fallback_tissue) & !is.na(channel) & channel <= 3L] <- "Above-ground"
      fallback_tissue[is.na(fallback_tissue) & !is.na(channel) & channel >= 4L] <- "Below-ground"
      tissue_values[is.na(tissue_values) | !nzchar(tissue_values) |
                      tissue_values == "NA" | tissue_values == "All samples"] <-
        fallback_tissue[is.na(tissue_values) | !nzchar(tissue_values) |
                         tissue_values == "NA" | tissue_values == "All samples"]
      tissue_values[is.na(tissue_values) | !nzchar(tissue_values)] <- "All samples"
      metadata_share$tissue2 <- tissue_values
      metadata_share$species <- if ("species" %in% names(metadata_share)) {
        as.character(metadata_share$species)
      } else {
        ifelse(grepl("B73", metadata_share$sample_id, ignore.case = TRUE),
               "Zea mays ssp. mays",
               ifelse(grepl("Y12", metadata_share$sample_id,
                            ignore.case = TRUE),
                      "Zea mays ssp. mexicana", "All samples"))
      }
      metadata_share$species[is.na(metadata_share$species) |
                               !nzchar(metadata_share$species)] <- "All samples"
      ComplexHeatmap::rowAnnotation(
        Tissue = base::as.matrix(metadata_share["tissue2"]),
        Species = base::as.matrix(metadata_share["species"]),
        col = base::list(
          Tissue = c("Above-ground" = "#65a30d", "Below-ground" = "#c2410c",
                     "Leaf" = "#65a30d", "Pulvinus" = "#a16207",
                     "Root" = "#c2410c", "Stem" = "#166534",
                     "Shoot.tip" = "#2563eb", "All samples" = "#94a3b8"),
          Species = c("Zea mays ssp. mays" = "#f59e0b",
                      "Zea mays ssp. mexicana" = "#84cc16",
                      "All samples" = "#94a3b8")
        ),
        annotation_name_gp = grid::gpar(fontsize = 7),
        annotation_legend_param = base::list(
          title_gp = grid::gpar(fontsize = 7),
          labels_gp = grid::gpar(fontsize = 6)
        )
      )
    }

    cor_heatmap <- shiny::reactive({
      shiny::req(isTRUE(rv$load_success))
      shiny::req(!base::is.null(rv$cor_results))
      shiny::req(!base::is.null(rv$sample_info))

      ha <- make_metadata_annotation(rv$normalized_matrix)

      min_break <- input$cor_color_min
      max_break <- input$cor_color_max
      shiny::validate(shiny::need(
        is.finite(min_break) && is.finite(max_break) && min_break < max_break,
        "Correlation color limits must be finite and min < max."
      ))
      mid_break <- (min_break + max_break) / 2
      heatmap_matrix <- rv$cor_results
      heatmap_matrix[!is.finite(heatmap_matrix)] <- 0

      ComplexHeatmap::Heatmap(
        heatmap_matrix,
        right_annotation = ha,
        cluster_rows = isTRUE(input$cor_cluster_rows),
        cluster_columns = isTRUE(input$cor_cluster_columns),
        show_row_names = TRUE,
        show_column_names = isTRUE(input$cor_show_column_names),
        row_names_gp = grid::gpar(fontsize = 6),
        border = "black",
        na_col = "#d1d5db",
        name = "r",
        col = circlize::colorRamp2(
          breaks = c(min_break, mid_break, max_break),
          colors = c(
            input$cor_low_color,
            input$cor_mid_color,
            input$cor_high_color
          )
        ),
        heatmap_legend_param = base::list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        ),
        cell_fun = if (isTRUE(input$cor_show_numbers)) function(j, i, x, y, width, height, fill) {
          grid::grid.text(
            label = if (is.finite(rv$cor_results[i, j])) {
              base::round(rv$cor_results[i, j], 2)
            } else {
              "NA"
            },
            x = x,
            y = y,
            gp = grid::gpar(fontsize = 6, col = "white")
          )
        } else NULL
      )
    })

    output$cor_res <- shiny::renderPlot({
      shiny::validate(
        shiny::need(isTRUE(rv$load_success), "")
      )
      shiny::validate(
        shiny::need(!base::is.null(rv$cor_results), "")
      )

      tryCatch({
        ht <- cor_heatmap()
        shiny::req(!base::is.null(ht))
        ComplexHeatmap::draw(ht)
      }, error = function(e) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, paste("Correlation unavailable:",
                                       conditionMessage(e)), cex = 0.9)
      })
    })

    output$cor_download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0("correlation_heatmap_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        shiny::req(isTRUE(rv$load_success))
        shiny::req(!base::is.null(rv$cor_results))

        grDevices::pdf(
          file,
          width = input$cor_plot_width,
          height = input$cor_plot_height
        )
        ComplexHeatmap::draw(cor_heatmap())
        grDevices::dev.off()
      }
    )

    shiny::observeEvent(input$run_expression, {
      shiny::req(rv$normalized_matrix)

      shiny::withProgress(message = "Analyzing expression patterns...", value = 0.5, {
        mat <- rv$normalized_matrix
        mat <- base::as.matrix(mat)
        storage.mode(mat) <- "numeric"
        row_vars <- matrixStats::rowVars(mat, na.rm = TRUE)
        row_vars[!is.finite(row_vars)] <- -Inf

        top_n <- base::min(input$exp_top_n, base::nrow(mat))
        top_idx <- base::order(row_vars, decreasing = TRUE)[base::seq_len(top_n)]
        mat <- mat[top_idx, , drop = FALSE]

        if (isTRUE(input$exp_scale) && input$exp_scale_method != "none") {
          if (identical(input$exp_scale_method, "column")) {
            mat <- base::scale(mat)
          } else {
            mat <- base::t(base::scale(base::t(mat)))
          }
          mat[!is.finite(mat)] <- NA_real_
        }

        rv$exp_results <- base::as.data.frame(
          mat, stringsAsFactors = FALSE, check.names = FALSE
        )
        shiny::incProgress(1, detail = "Done")
      })
    })

    exp_heatmap <- shiny::reactive({
      shiny::req(isTRUE(rv$load_success))
      shiny::req(!base::is.null(rv$exp_results))
      shiny::req(!base::is.null(rv$sample_info))

      ha <- make_metadata_annotation(rv$normalized_matrix)

      min_break <- input$exp_color_min
      max_break <- input$exp_color_max
      shiny::validate(shiny::need(
        is.finite(min_break) && is.finite(max_break) && min_break < max_break,
        "Expression color limits must be finite and min < max."
      ))
      mid_break <- (min_break + max_break) / 2
      heatmap_matrix <- base::t(base::as.matrix(rv$exp_results))
      cluster_matrix <- heatmap_matrix
      for (i in base::seq_len(base::nrow(cluster_matrix))) {
        missing <- !is.finite(cluster_matrix[i, ])
        if (base::any(missing)) {
          replacement <- stats::median(cluster_matrix[i, !missing], na.rm = TRUE)
          if (!is.finite(replacement)) replacement <- 0
          cluster_matrix[i, missing] <- replacement
        }
      }
      row_dend <- if (isTRUE(input$exp_cluster_rows) &&
                      base::nrow(cluster_matrix) > 1L) {
        stats::hclust(stats::dist(cluster_matrix))
      } else FALSE
      column_dend <- if (isTRUE(input$exp_cluster_columns) &&
                         base::ncol(cluster_matrix) > 1L) {
        stats::hclust(stats::dist(base::t(cluster_matrix)))
      } else FALSE

      ComplexHeatmap::Heatmap(
        heatmap_matrix,
        right_annotation = ha,
        cluster_rows = row_dend,
        cluster_columns = column_dend,
        show_row_names = TRUE,
        show_column_names = isTRUE(input$exp_show_feature_names),
        row_names_gp = grid::gpar(fontsize = 6),
        border = "black",
        na_col = "#d1d5db",
        name = ifelse(isTRUE(input$exp_scale) &&
                        input$exp_scale_method != "none",
                      "Z-score", "Intensity"),
        col = circlize::colorRamp2(
          breaks = c(min_break, mid_break, max_break),
          colors = c(
            input$exp_low_color,
            input$exp_mid_color,
            input$exp_high_color
          )
        ),
        heatmap_legend_param = base::list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        )
      )
    })

    output$expression_pattern <- shiny::renderPlot({
      shiny::validate(
        shiny::need(isTRUE(rv$load_success), "")
      )
      shiny::validate(
        shiny::need(!base::is.null(rv$exp_results), "")
      )

      tryCatch({
        ht <- exp_heatmap()
        shiny::req(!base::is.null(ht))
        ComplexHeatmap::draw(ht)
      }, error = function(e) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, paste("Expression pattern unavailable:",
                                       conditionMessage(e)), cex = 0.9)
      })
    })

    output$exp_download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0("expression_pattern_heatmap_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        shiny::req(isTRUE(rv$load_success))
        shiny::req(!base::is.null(rv$exp_results))

        grDevices::pdf(
          file,
          width = input$exp_plot_width,
          height = input$exp_plot_height
        )
        ComplexHeatmap::draw(exp_heatmap())
        grDevices::dev.off()
      }
    )

    DR_results <- shiny::reactiveValues(
      before = NULL,
      after = NULL
    )

    prepare_DR_matrix <- function(data) {
      matrix <- base::as.matrix(data)
      storage.mode(matrix) <- "numeric"
      matrix[!is.finite(matrix)] <- NA_real_

      # Missing values are not written back to ProtVis_dataset.  They are
      # replaced only in this temporary matrix because distance-based methods
      # cannot operate on NA/Inf values.
      observed <- base::rowSums(!is.na(matrix))
      matrix <- matrix[observed > 0, , drop = FALSE]
      if (base::nrow(matrix) < 2L) {
        stop("Dimensionality reduction requires at least two observed features.",
             call. = FALSE)
      }
      for (i in base::seq_len(base::nrow(matrix))) {
        missing <- is.na(matrix[i, ])
        if (base::any(missing)) {
          replacement <- stats::median(matrix[i, !missing], na.rm = TRUE)
          if (!is.finite(replacement)) replacement <- 0
          matrix[i, missing] <- replacement
        }
      }
      variation <- apply(matrix, 1, stats::sd)
      matrix <- matrix[is.finite(variation) & variation > 0, , drop = FALSE]
      if (base::nrow(matrix) == 0L) {
        stop("Dimensionality reduction requires variable features.",
             call. = FALSE)
      }
      base::t(matrix)
    }

    perform_DR <- function(data, method) {
      t_data <- prepare_DR_matrix(data)
      if (base::nrow(t_data) < 3L) {
        stop("At least three samples are required for a 2D reduction plot.",
             call. = FALSE)
      }

      if (method == "PCA") {
        res <- base::as.data.frame(stats::prcomp(
          t_data, center = TRUE, scale. = TRUE
        )$x[, 1:2, drop = FALSE])
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      if (method == "PCoA") {
        res <- base::as.data.frame(stats::cmdscale(stats::dist(t_data), k = 2))
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      if (method == "tSNE") {
        perplexity <- base::max(1, base::min(
          30, base::floor((base::nrow(t_data) - 1) / 3)
        ))
        res <- base::as.data.frame(Rtsne::Rtsne(
          t_data, perplexity = perplexity, check_duplicates = FALSE,
          pca = FALSE, dims = 2
        )$Y)
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      if (method == "UMAP") {
        config <- umap::umap.defaults
        config$n_neighbors <- base::max(2L, base::min(
          config$n_neighbors, base::nrow(t_data) - 1L
        ))
        config$n_components <- 2L
        res <- tryCatch(
          base::as.data.frame(umap::umap(t_data, config = config)$layout[
            , 1:2, drop = FALSE
          ]),
          error = function(e) {
            # UMAP is sensitive to tied/degenerate neighbourhood distances.
            # PCA is a deterministic, finite fallback for the same cleaned
            # matrix, so a valid DR plot is still available to the user.
            base::as.data.frame(stats::prcomp(
              t_data, center = TRUE, scale. = TRUE
            )$x[, 1:2, drop = FALSE])
          }
        )
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      if (method == "NMDS") {
        res <- base::as.data.frame(vegan::metaMDS(t_data, k = 2)[["points"]])
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      return(NULL)
    }

    shiny::observeEvent(input$DR_analyse, {
      if (!isTRUE(rv$load_success)) {
        shiny::showNotification("Load data before dimensionality reduction.",
                                type = "warning")
        return(invisible(NULL))
      }

      run_safe <- function(data, label) {
        if (base::is.null(data)) return(NULL)
        tryCatch(
          perform_DR(data, input$dimReductionMethod),
          error = function(e) {
            shiny::showNotification(
              paste(label, "reduction failed:", conditionMessage(e)),
              type = "error", duration = 8
            )
            NULL
          }
        )
      }

      shiny::withProgress(message = "Running dimensionality reduction...", value = 0.5, {
        DR_results$before <- run_safe(rv$imputed_matrix, "Before-normalization")
        shiny::incProgress(0.4, detail = "Finished pre-normalization")
        DR_results$after <- run_safe(rv$normalized_matrix, "After-normalization")
        shiny::incProgress(0.6, detail = "Finished post-normalization")
      })
    })

    plot_DR_results <- function(dr_data, title_suffix) {
      df <- base::as.data.frame(dr_data)
      sample_names <- base::rownames(df)
      if (is.null(sample_names)) sample_names <- paste0("Sample_", base::seq_len(nrow(df)))

      df <- dplyr::mutate(
        df,
        Sample = sample_names,
        SampleType = vapply(base::strsplit(sample_names, "_", fixed = TRUE),
                            function(value) value[[1L]], character(1)),
        Type = dplyr::if_else(
          !is.null(rv$sample_info$group) &&
            sample_names %in% rv$sample_info$sample_id,
          as.character(rv$sample_info$group)[match(sample_names,
                                                   rv$sample_info$sample_id)],
          stringr::str_remove_all(sample_names, "^....|..$")
        ),
        Species = dplyr::case_when(
          SampleType == "B73" ~ "Zea mays ssp. mays",
          TRUE ~ "Zea mays ssp. mexicana"
        )
      )

      ellipse_df <- df |>
        dplyr::filter(is.finite(V1), is.finite(V2)) |>
        dplyr::group_by(Type) |>
        dplyr::filter(dplyr::n() >= 3L,
                      stats::sd(V1) > 0, stats::sd(V2) > 0) |>
        dplyr::ungroup()

      plot <- ggplot2::ggplot(df) +
        ggplot2::geom_point(
          ggplot2::aes(x = V1, y = V2, color = Type, shape = Species),
          size = 1.2,
          alpha = 0.8
        )
      if (nrow(ellipse_df) > 0L) {
        plot <- plot +
          ggplot2::stat_ellipse(
            data = ellipse_df,
            ggplot2::aes(x = V1, y = V2, fill = Type),
            geom = "polygon", level = 0.95, alpha = 0.25
          ) +
          ggplot2::stat_ellipse(
            data = ellipse_df,
            ggplot2::aes(x = V1, y = V2, color = Type),
            geom = "path", level = 0.95, alpha = 1, linewidth = 0.5
          )
      }
      plot +
        ggsci::scale_color_lancet() +
        ggsci::scale_fill_lancet() +
        ggplot2::labs(
          x = "Component 1",
          y = "Component 2",
          title = base::paste(input$dimReductionMethod, "analysis", title_suffix)
        ) +
        ggplot2::theme_bw()
    }

    safe_dr_plot <- function(result, title_suffix) {
      tryCatch({
        shiny::validate(shiny::need(
          !base::is.null(result),
          "Run dimensionality reduction to display this plot."
        ))
        print(plot_DR_results(result, title_suffix))
      }, error = function(e) {
        graphics::plot.new()
        graphics::text(
          0.5, 0.5,
          paste("Dimensionality reduction unavailable:",
                conditionMessage(e)),
          cex = 0.85
        )
      })
    }

    output$DR_BeforeNormalization <- shiny::renderPlot(
      safe_dr_plot(DR_results$before, "Before Normalization")
    )

    output$DR_AfterNormalization <- shiny::renderPlot(
      safe_dr_plot(DR_results$after, "After Normalization")
    )

    output$dr_download_before_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0(
          input$dimReductionMethod,
          "_before_normalization_",
          base::Sys.Date(),
          ".pdf"
        )
      },
      content = function(file) {
        shiny::req(DR_results$before)
        grDevices::pdf(file, width = input$dr_plot_width, height = input$dr_plot_height)
        print(plot_DR_results(DR_results$before, "Before Normalization"))
        grDevices::dev.off()
      }
    )

    output$dr_download_after_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0(
          input$dimReductionMethod,
          "_after_normalization_",
          base::Sys.Date(),
          ".pdf"
        )
      },
      content = function(file) {
        shiny::req(DR_results$after)
        grDevices::pdf(file, width = input$dr_plot_width, height = input$dr_plot_height)
        print(plot_DR_results(DR_results$after, "After Normalization"))
        grDevices::dev.off()
      }
    )

    output$dr_download_both_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0(
          input$dimReductionMethod,
          "_both_plots_",
          base::Sys.Date(),
          ".pdf"
        )
      },
      content = function(file) {
        shiny::req(DR_results$before, DR_results$after)
        grDevices::pdf(
          file,
          width = input$dr_plot_width * 2,
          height = input$dr_plot_height
        )
        gridExtra::grid.arrange(
          plot_DR_results(DR_results$before, "Before Normalization"),
          plot_DR_results(DR_results$after, "After Normalization"),
          ncol = 2
        )
        grDevices::dev.off()
      }
    )

    qc_matrix <- shiny::reactive({
      shiny::req(isTRUE(rv$load_success))
      shiny::req(rv$normalized_matrix)
      base::as.matrix(rv$normalized_matrix)
    })

    qc_long_intensity <- shiny::reactive({
      mat <- qc_matrix()
      long <- base::data.frame(
        Sample = rep(base::colnames(mat), each = base::nrow(mat)),
        Intensity = as.vector(mat),
        stringsAsFactors = FALSE
      )
      long[is.finite(long$Intensity), , drop = FALSE]
    })

    qc_sample_total_plot <- shiny::reactive({
      mat <- qc_matrix()
      total_df <- base::data.frame(
        Sample = base::colnames(mat),
        TotalIntensity = base::colSums(mat, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
      ggplot2::ggplot(total_df, ggplot2::aes(x = Sample, y = TotalIntensity)) +
        ggplot2::geom_col(fill = "#2563eb") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = "Sample total normalized intensity", x = NULL, y = "Total intensity")
    })

    qc_missing_rate_plot <- shiny::reactive({
      mat <- qc_matrix()
      missing_df <- base::data.frame(
        Sample = base::colnames(mat),
        MissingRate = base::colMeans(base::is.na(mat)),
        stringsAsFactors = FALSE
      )
      ggplot2::ggplot(missing_df, ggplot2::aes(x = Sample, y = MissingRate)) +
        ggplot2::geom_col(fill = "#dc2626") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = "Missing value rate by sample", x = NULL, y = "Missing rate")
    })

    qc_boxplot <- shiny::reactive({
      ggplot2::ggplot(qc_long_intensity(), ggplot2::aes(x = Sample, y = Intensity)) +
        ggplot2::geom_boxplot(fill = "#38bdf8", outlier.size = 0.6,
                              na.rm = TRUE) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::labs(title = "Normalized intensity distribution", x = NULL, y = "Intensity")
    })

    qc_density_plot <- shiny::reactive({
      ggplot2::ggplot(qc_long_intensity(), ggplot2::aes(x = Intensity, color = Sample)) +
        ggplot2::geom_density(na.rm = TRUE) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = "Normalized intensity density", x = "Intensity", y = "Density")
    })

    qc_pca_plot <- shiny::reactive({
      mat <- qc_matrix()
      row_sds <- apply(mat, 1, stats::sd, na.rm = TRUE)
      keep <- is.finite(row_sds) & row_sds > 0
      top_n <- base::min(input$qc_top_n, base::sum(keep))
      if (top_n > 0 && base::sum(keep) > top_n) {
        top_idx <- base::order(row_sds, decreasing = TRUE)[base::seq_len(top_n)]
        keep <- base::seq_along(row_sds) %in% top_idx
      }
      pca_mat <- base::t(mat[keep, , drop = FALSE])
      pca_mat[base::is.na(pca_mat)] <- 0
      shiny::validate(shiny::need(base::nrow(pca_mat) >= 2 && base::ncol(pca_mat) >= 2, "Need at least two samples and two variable features for PCA."))
      pca <- stats::prcomp(pca_mat, center = TRUE, scale. = TRUE)
      var_exp <- base::round(100 * (pca$sdev^2 / base::sum(pca$sdev^2))[1:2], 1)
      pca_df <- base::data.frame(Sample = base::rownames(pca$x), PC1 = pca$x[, 1], PC2 = pca$x[, 2], stringsAsFactors = FALSE)
      ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2, label = Sample)) +
        ggplot2::geom_point(size = 3, color = "#7c3aed") +
        ggplot2::geom_text(vjust = -0.7, size = 3) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = "PCA of normalized proteomics samples", x = base::paste0("PC1 (", var_exp[1], "%)"), y = base::paste0("PC2 (", var_exp[2], "%)"))
    })

    qc_cv_plot <- shiny::reactive({
      mat <- qc_matrix()
      row_mean <- base::rowMeans(mat, na.rm = TRUE)
      row_sd <- apply(mat, 1, stats::sd, na.rm = TRUE)
      cv_df <- base::data.frame(CV = row_sd / base::abs(row_mean), stringsAsFactors = FALSE)
      cv_df <- cv_df[base::is.finite(cv_df$CV), , drop = FALSE]
      ggplot2::ggplot(cv_df, ggplot2::aes(x = CV)) +
        ggplot2::geom_histogram(bins = 50, fill = "#22c55e", color = "white") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(title = "Protein coefficient of variation", x = "CV", y = "Protein count")
    })

    qc_plot_by_type <- function(type) {
      switch(type,
             sample_total = qc_sample_total_plot(),
             missing_rate = qc_missing_rate_plot(),
             boxplot = qc_boxplot(),
             density = qc_density_plot(),
             pca = qc_pca_plot(),
             cv = qc_cv_plot(),
             qc_sample_total_plot())
    }

    output$qc_summary <- shiny::renderPrint({
      mat <- qc_matrix()
      cat("Proteomics QC summary\n")
      cat("Proteins/features:", base::nrow(mat), "\n")
      cat("Samples:", base::ncol(mat), "\n")
      cat("Overall missing rate:", base::round(base::mean(base::is.na(mat)), 4), "\n")
      cat("Median sample intensity range:", base::paste(base::round(base::range(apply(mat, 2, stats::median, na.rm = TRUE)), 4), collapse = " - "), "\n")
    })

    safe_qc_plot <- function(plot_function) {
      tryCatch({
        print(plot_function())
      }, error = function(e) {
        graphics::plot.new()
        graphics::text(
          0.5, 0.5, paste("QC plot unavailable:", conditionMessage(e)),
          cex = 0.85
        )
      })
    }

    output$qc_sample_total_plot <- shiny::renderPlot(
      safe_qc_plot(qc_sample_total_plot), height = 280
    )
    output$qc_missing_rate_plot <- shiny::renderPlot(
      safe_qc_plot(qc_missing_rate_plot), height = 180
    )
    output$qc_boxplot <- shiny::renderPlot(
      safe_qc_plot(qc_boxplot), height = 180
    )
    output$qc_density_plot <- shiny::renderPlot(
      safe_qc_plot(qc_density_plot), height = 180
    )
    output$qc_pca_plot <- shiny::renderPlot(
      safe_qc_plot(qc_pca_plot), height = 220
    )
    output$qc_cv_plot <- shiny::renderPlot(
      safe_qc_plot(qc_cv_plot), height = 180
    )

    output$qc_download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0("overview_proteomics_qc_", input$qc_download_plot_type, "_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        grDevices::pdf(file, width = input$qc_plot_width, height = input$qc_plot_height)
        print(qc_plot_by_type(input$qc_download_plot_type))
        grDevices::dev.off()
      }
    )

    output$qc_download_matrix <- shiny::downloadHandler(
      filename = function() {
        base::paste0("overview_normalized_matrix_", base::Sys.Date(), ".csv")
      },
      content = function(file) {
        mat <- qc_matrix()
        out <- base::data.frame(ID = base::rownames(mat), mat, check.names = FALSE)
        utils::write.csv(out, file, row.names = FALSE)
      }
    )
  })
}
