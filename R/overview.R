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
          height = 750,
          bslib::card(
            height = "800px",
            bslib::card_header("Correlation"),
            bslib::card_body(
              shiny::plotOutput(ns("cor_res"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Expression pattern"),
            bslib::card_body(
              shiny::plotOutput(ns("expression_pattern"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Dimensionality reduction analyse before normalization"),
            bslib::card_body(
              shiny::plotOutput(ns("DR_BeforeNormalization"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Dimensionality reduction analyse after normalization"),
            bslib::card_body(
              shiny::plotOutput(ns("DR_AfterNormalization"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Proteomics QC summary"),
            bslib::card_body(
              shiny::verbatimTextOutput(ns("qc_summary")),
              shiny::plotOutput(ns("qc_sample_total_plot"), height = "300px")
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Proteomics missing values and distributions"),
            bslib::card_body(
              shiny::plotOutput(ns("qc_missing_rate_plot"), height = "250px"),
              shiny::plotOutput(ns("qc_boxplot"), height = "250px"),
              shiny::plotOutput(ns("qc_density_plot"), height = "250px")
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Proteomics PCA and CV"),
            bslib::card_body(
              shiny::plotOutput(ns("qc_pca_plot"), height = "300px"),
              shiny::plotOutput(ns("qc_cv_plot"), height = "250px")
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

    rv <- shiny::reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      imputed_matrix = NULL,
      cor_results = NULL,
      exp_results = NULL
    )

    shiny::observeEvent(input$load_data, {
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
        rv$cor_results <- stats::cor(
          rv$normalized_matrix,
          method = base::tolower(input$cor_method),
          use = "pairwise.complete.obs"
        )
        shiny::incProgress(1, detail = "Done")
      })
    })

    cor_heatmap <- shiny::reactive({
      shiny::req(isTRUE(rv$load_success))
      shiny::req(!base::is.null(rv$cor_results))
      shiny::req(!base::is.null(rv$sample_info))

      metadata_share <- dplyr::left_join(
        base::data.frame(sample_id = base::colnames(rv$normalized_matrix)),
        rv$sample_info,
        by = "sample_id"
      )

      metadata_share <- dplyr::mutate(
        metadata_share,
        tissue2 = stringr::str_split(tissue, "_", 2, simplify = TRUE)[, 1]
      )

      ha <- ComplexHeatmap::rowAnnotation(
        Tissue = base::as.matrix(dplyr::select(metadata_share, tissue2)),
        Species = base::as.matrix(dplyr::select(metadata_share, species)),
        col = base::list(
          Tissue = c(
            "Leaf" = "green",
            "Pulvinus" = "brown",
            "Root" = "tan",
            "Stem" = "darkgreen",
            "Shoot.tip" = "blue"
          ),
          Species = c(
            "Zea mays ssp. mays" = "orange",
            "Zea mays ssp. mexicana" = "lightgreen"
          )
        ),
        annotation_name_gp = grid::gpar(fontsize = 6),
        annotation_legend_param = base::list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        )
      )

      min_break <- input$cor_color_min
      max_break <- input$cor_color_max
      mid_break <- (min_break + max_break) / 2

      ComplexHeatmap::Heatmap(
        rv$cor_results,
        right_annotation = ha,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = "black",
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
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid::grid.text(
            label = base::round(rv$cor_results[i, j], 2),
            x = x,
            y = y,
            gp = grid::gpar(fontsize = 6, col = "white")
          )
        }
      )
    })

    output$cor_res <- shiny::renderPlot({
      shiny::validate(
        shiny::need(isTRUE(rv$load_success), "")
      )
      shiny::validate(
        shiny::need(!base::is.null(rv$cor_results), "")
      )

      ht <- cor_heatmap()
      shiny::req(!base::is.null(ht))
      ComplexHeatmap::draw(ht)
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
        row_vars <- matrixStats::rowVars(base::as.matrix(mat))

        top_n <- base::min(input$exp_top_n, base::nrow(mat))
        top_idx <- base::order(row_vars, decreasing = TRUE)[base::seq_len(top_n)]
        mat <- mat[top_idx, , drop = FALSE]

        if (isTRUE(input$exp_scale)) {
          mat <- base::t(scale(base::t(mat)))
          mat <- base::as.data.frame(mat, stringsAsFactors = FALSE)
        }

        rv$exp_results <- mat
        shiny::incProgress(1, detail = "Done")
      })
    })

    exp_heatmap <- shiny::reactive({
      shiny::req(isTRUE(rv$load_success))
      shiny::req(!base::is.null(rv$exp_results))
      shiny::req(!base::is.null(rv$sample_info))

      metadata_share <- dplyr::left_join(
        base::data.frame(sample_id = base::colnames(rv$normalized_matrix)),
        rv$sample_info,
        by = "sample_id"
      )

      metadata_share <- dplyr::mutate(
        metadata_share,
        tissue2 = stringr::str_split(tissue, "_", 2, simplify = TRUE)[, 1]
      )

      ha <- ComplexHeatmap::rowAnnotation(
        Tissue = base::as.matrix(dplyr::select(metadata_share, tissue2)),
        Species = base::as.matrix(dplyr::select(metadata_share, species)),
        col = base::list(
          Tissue = c(
            "Leaf" = "green",
            "Pulvinus" = "brown",
            "Root" = "tan",
            "Stem" = "darkgreen",
            "Shoot.tip" = "blue"
          ),
          Species = c(
            "Zea mays ssp. mays" = "orange",
            "Zea mays ssp. mexicana" = "lightgreen"
          )
        ),
        annotation_name_gp = grid::gpar(fontsize = 6),
        annotation_legend_param = base::list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        )
      )

      min_break <- input$exp_color_min
      max_break <- input$exp_color_max
      mid_break <- (min_break + max_break) / 2

      ComplexHeatmap::Heatmap(
        base::t(base::as.matrix(rv$exp_results)),
        right_annotation = ha,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = "black",
        name = ifelse(isTRUE(input$exp_scale), "Z-score", "Intensity"),
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

      ht <- exp_heatmap()
      shiny::req(!base::is.null(ht))
      ComplexHeatmap::draw(ht)
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

    perform_DR <- function(data, method) {
      t_data <- base::t(base::as.matrix(data))

      if (method == "PCA") {
        res <- base::as.data.frame(stats::prcomp(t_data)$x[, 1:2, drop = FALSE])
        base::colnames(res) <- c("V1", "V2")
        return(res)
      }

      if (method == "PCoA") {
        res <- base::as.data.frame(stats::cmdscale(stats::dist(t_data), k = 2))
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      if (method == "tSNE") {
        res <- base::as.data.frame(Rtsne::Rtsne(t_data, perplexity = 5)$Y)
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      if (method == "UMAP") {
        res <- base::as.data.frame(umap::umap(t_data)$layout[, 1:2, drop = FALSE])
        base::colnames(res) <- c("V1", "V2")
        base::rownames(res) <- base::rownames(t_data)
        return(res)
      }

      if (method == "NMDS") {
        res <- base::as.data.frame(vegan::metaMDS(t_data, k = 2)[["points"]])
        base::colnames(res) <- c("V1", "V2")
        return(res)
      }

      return(NULL)
    }

    shiny::observeEvent(input$DR_analyse, {
      shiny::req(rv$sample_info)

      shiny::withProgress(message = "Running dimensionality reduction...", value = 0.5, {
        if (!base::is.null(rv$imputed_matrix)) {
          DR_results$before <- perform_DR(rv$imputed_matrix, input$dimReductionMethod)
          shiny::incProgress(0.3, detail = "Finished pre-normalization")
        }

        if (!base::is.null(rv$normalized_matrix)) {
          DR_results$after <- perform_DR(rv$normalized_matrix, input$dimReductionMethod)
          shiny::incProgress(0.2, detail = "Finished post-normalization")
        }
      })
    })

    plot_DR_results <- function(dr_data, title_suffix) {
      df <- base::as.data.frame(dr_data)

      df <- dplyr::mutate(
        df,
        SampleType = stringr::str_split(base::rownames(df), "_", 2, simplify = TRUE)[, 1],
        Type = stringr::str_remove_all(base::rownames(df), "^....|..$"),
        Species = dplyr::case_when(
          SampleType == "B73" ~ "Zea mays ssp. mays",
          TRUE ~ "Zea mays ssp. mexicana"
        )
      )

      ggplot2::ggplot(df) +
        ggplot2::geom_point(
          ggplot2::aes(x = V1, y = V2, color = Type, shape = Species),
          size = 1.2,
          alpha = 0.8
        ) +
        ggplot2::stat_ellipse(
          ggplot2::aes(x = V1, y = V2, fill = Type),
          geom = "polygon",
          level = 0.95,
          alpha = 0.25
        ) +
        ggplot2::stat_ellipse(
          ggplot2::aes(x = V1, y = V2, color = Type),
          geom = "path",
          level = 0.95,
          alpha = 1,
          linewidth = 0.5
        ) +
        ggsci::scale_color_lancet() +
        ggsci::scale_fill_lancet() +
        ggplot2::labs(
          x = "Component 1",
          y = "Component 2",
          title = base::paste(input$dimReductionMethod, "analysis", title_suffix)
        ) +
        ggplot2::theme_bw()
    }

    output$DR_BeforeNormalization <- shiny::renderPlot({
      shiny::req(DR_results$before)
      print(plot_DR_results(DR_results$before, "Before Normalization"))
    })

    output$DR_AfterNormalization <- shiny::renderPlot({
      shiny::req(DR_results$after)
      print(plot_DR_results(DR_results$after, "After Normalization"))
    })

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
      base::data.frame(
        Sample = rep(base::colnames(mat), each = base::nrow(mat)),
        Intensity = as.vector(mat),
        stringsAsFactors = FALSE
      )
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
        ggplot2::geom_boxplot(fill = "#38bdf8", outlier.size = 0.6) +
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

    output$qc_sample_total_plot <- shiny::renderPlot(print(qc_sample_total_plot()))
    output$qc_missing_rate_plot <- shiny::renderPlot(print(qc_missing_rate_plot()))
    output$qc_boxplot <- shiny::renderPlot(print(qc_boxplot()))
    output$qc_density_plot <- shiny::renderPlot(print(qc_density_plot()))
    output$qc_pca_plot <- shiny::renderPlot(print(qc_pca_plot()))
    output$qc_cv_plot <- shiny::renderPlot(print(qc_cv_plot()))

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
