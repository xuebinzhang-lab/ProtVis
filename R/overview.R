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
              value = "purple"),
            colourpicker::colourInput(
              ns("cor_mid_color"),
              "middle Color",
              value = "black"),
            colourpicker::colourInput(
              ns("cor_low_color"),
              "Low Color",
              value = "yellow"),
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
              value = "purple"),
            colourpicker::colourInput(
              ns("exp_mid_color"),
              "middle Color",
              value = "black"),
            colourpicker::colourInput(
              ns("exp_low_color"),
              "Low Color",
              value = "yellow"),
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
#' @importFrom grid gpar textGrob
#' @importFrom grDevices pdf dev.off
#' @importFrom matrixStats rowVars
#' @importFrom data.table setnames
#' @importFrom magrittr set_rownames
#' @importFrom Rtsne Rtsne
#' @importFrom umap umap
#' @importFrom vegan metaMDS
#' @importFrom ggsci scale_color_lancet scale_fill_lancet
#' @importFrom gridExtra grid.arrange
#' @name overview_server
#' @export
#'

utils::globalVariables(c(
  "tissue", "tissue2", "species", "Type", "Species"
))

overview_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      imputed_matrix = NULL,
      cor_results = NULL,
      exp_results = NULL,
      DR_results = base::list(before = NULL, after = NULL)
    )
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      step5_path <- base::file.path(shared_state$workdir, "Step5_data_imputation.rda")
      step6_path <- base::file.path(shared_state$workdir, "Step6_data_normalization.rda")
      if (!base::file.exists(step5_path) || !base::file.exists(step6_path)) {
        missing_files <- c(step5_path, step6_path)[!base::file.exists(c(step5_path, step6_path))]
        shiny::showNotification(base::paste("File(s) not found:", base::paste(base::basename(missing_files), collapse = ", ")),
                         type = "error")
        rv$load_success = FALSE
        return()
      }
      tryCatch({
        e5 <- base::new.env()
        load(step5_path, envir = e5)
        e6 <- base::new.env()
        load(step6_path, envir = e6)
        if (!base::exists("sample_info", envir = e5) || !base::exists("imputed_df", envir = e5)) {
          shiny::showNotification("Required data not found in Step5 file.", type = "error")
          rv$load_success = FALSE
          return()
        }
        if (!base::exists("normalized_data", envir = e6)) {
          shiny::showNotification("Required data not found in Step6 file.", type = "error")
          rv$load_success = FALSE
          return()
        }
        if (!base::identical(base::colnames(e5$imputed_df), base::colnames(e6$normalized_data))) {
          shiny::showNotification("Sample names don't match between imputed and normalized data.", type = "error")
          rv$load_success = FALSE
          return()
        }
        rv$sample_info <- e5$sample_info
        rv$expression_matrix <- e6$normalized_data
        rv$normalized_matrix <- e6$normalized_data
        rv$imputed_matrix <- e5$imputed_df
        rv$load_success = TRUE
        shiny::showNotification("✅ Both datasets loaded successfully.", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error loading data:", e$message), type = "error")
        rv$load_success = FALSE
      })
    })
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::div(
          shiny::span("✅ Both datasets loaded successfully", style = "color: green;"),
          shiny::br(),
          base::paste("Imputed data:", base::nrow(rv$imputed_matrix), "proteins,",
                      base::ncol(rv$imputed_matrix), "samples"),
          shiny::br(),
          base::paste("Normalized data:", base::nrow(rv$normalized_matrix), "proteins,",
                      base::ncol(rv$normalized_matrix), "samples")
        )
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    shiny::observeEvent(input$run_correlation, {
      shiny::req(rv$normalized_matrix)
      shiny::withProgress(message = 'Calculating correlations...', value = 0.5, {
        rv$cor_results <- stats::cor(
          rv$normalized_matrix,
          method = tolower(input$cor_method)
        )
        shiny::incProgress(1, detail = "Done")
      })
    })
    ht_reactive <- shiny::reactive({
      shiny::req(rv$cor_results)
      shiny::req(rv$sample_info)

      metadata_share <- dplyr::left_join(
        base::data.frame(sample_id = base::colnames(rv$normalized_matrix)),
        rv$sample_info,
        by = "sample_id"
      ) %>%
        dplyr::mutate(tissue2 = stringr::str_split(tissue, "_", 2, TRUE)[, 1])
      left_anno <- ComplexHeatmap::rowAnnotation(
        Tissue = metadata_share %>% dplyr::select(tissue2) %>% base::as.matrix(),
        Species = metadata_share %>% dplyr::select(species) %>% base::as.matrix(),
        col = base::list(
          tissue = c("green", "brown", "tan", "darkgreen", "blue") %>%
            stats::setNames(c("Leaf", "Pulvinus", "Root", "Stem", "Shoot.tip")),
          species = c("orange", "lightgreen") %>%
            stats::setNames(c("Zea mays ssp. mays", "Zea mays ssp. mexicana"))
        ),
        annotation_name_gp = grid::gpar(fontsize = 6),
        annotation_legend_param = list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        )
      )
      min_break <- input$cor_color_min
      max_break <- input$cor_color_max
      mid_break <- (min_break + max_break) / 2
      ComplexHeatmap::Heatmap(
        rv$cor_results,
        right_annotation = left_anno,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = 'black',
        name = "r",
        col = circlize::colorRamp2(
          colors = c(input$cor_low_color, input$cor_mid_color, input$cor_high_color),
          breaks = c(min_break, mid_break, max_break)
        ),
        heatmap_legend_param = base::list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        ),
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid::textGrob(
            label = base::round(rv$cor_results[i, j], 2),
            x = x, y = y,
            gp = grid::gpar(fontsize = 6, col = "white")
          )
        }
      )
    })
    output$cor_res <- shiny::renderPlot({
      ht_reactive()
    })
    output$cor_download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste("correlation_heatmap_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        grDevices::pdf(file, width = input$cor_plot_width, height = input$cor_plot_height)
        ComplexHeatmap::draw(ht_reactive())
        grDevices::dev.off()
      }
    )
    shiny::observeEvent(input$run_expression, {
      shiny::req(rv$normalized_matrix)
      shiny::withProgress(message = 'Analyzing expression patterns...', value = 0.5, {
        mat <- rv$normalized_matrix
        row_vars <- matrixStats::rowVars(mat)
        top_idx <- base::order(row_vars, decreasing = TRUE)[1:input$exp_top_n]
        mat <- mat[top_idx, ]
        if (input$exp_scale) {
          mat <- base::t(scale(t(mat)))
        }
        rv$exp_results <- mat
        shiny::incProgress(1, detail = "Done")
      })
    })
    ht_reactive_exp <- shiny::reactive({
      shiny::req(rv$exp_results)
      shiny::req(rv$sample_info)
      metadata_share <- dplyr::left_join(
        base::data.frame(sample_id = base::colnames(rv$normalized_matrix)),
        rv$sample_info,
        by = "sample_id"
      ) %>%
        dplyr::mutate(tissue2 = stringr::str_split(tissue, "_", 2, TRUE)[, 1])
      left_anno <- ComplexHeatmap::rowAnnotation(
        Tissue = metadata_share %>% dplyr::select(tissue2) %>% base::as.matrix(),
        Species = metadata_share %>% dplyr::select(species) %>% base::as.matrix(),
        col = base::list(
          tissue = c("green", "brown", "tan", "darkgreen", "blue") %>%
            stats::setNames(c("Leaf", "Pulvinus", "Root", "Stem", "Shoot.tip")),
          species = c("orange", "lightgreen") %>%
            stats::setNames(c("Zea mays ssp. mays", "Zea mays ssp. mexicana"))
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
        t(rv$exp_results),
        right_annotation = left_anno,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = 'black',
        name = ifelse(input$exp_scale, "Z-score", "Intensity"),
        col = circlize::colorRamp2(
          colors = c(input$exp_low_color, input$exp_mid_color, input$exp_high_color),
          breaks = c(min_break, mid_break, max_break)
        ),
        heatmap_legend_param = base::list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        ),
        layer_fun = function(j, i, x, y, width, height, fill) {
          if (base::length(i) == 1 && base::length(j) == 1) {
            grid::textGrob(
              label = base::round(rv$exp_results[i, j], 2),
              x = x, y = y,
              gp = grid::gpar(fontsize = 6, col = "white")
            )
          }
        }
      )
    })
    output$expression_pattern <- shiny::renderPlot({
      ht_reactive_exp()
    })
    output$exp_download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste("expression_pattern_heatmap_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        grDevices::pdf(file, width = input$exp_plot_width, height = input$exp_plot_height)
        ComplexHeatmap::draw(ht_reactive_exp())
        grDevices::dev.off()
      }
    )
    DR_results <- shiny::reactiveValues(
      before = NULL,
      after = NULL
    )
    shiny::observeEvent(input$DR_analyse, {
      shiny::req(rv$sample_info)
      shiny::withProgress(message = 'Running dimensionality reduction...', value = 0.5, {
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
    perform_DR <- function(data, method) {
      t_data <- base::t(data)
      switch(method,
             "PCA" = {
               base::as.data.frame(stats::prcomp(t_data)$x[, 1:2]) %>% data.table::setnames(c("V1","V2"))
             },
             "PCoA" = {
               base::as.data.frame(stats::cmdscale(dist(t_data), k = 2))
             },
             "tSNE" = {
               as.data.frame(Rtsne::Rtsne(t_data, perplexity = 5)$Y) %>%
                 magrittr::set_rownames(base::rownames(t_data))
             },
             "UMAP" = {
               base::as.data.frame(umap::umap(t_data)$layout[, 1:2])
             },
             "NMDS" = {
               base::as.data.frame(vegan::metaMDS(t_data, k = 2)[["points"]])%>% data.table::setnames(c("V1","V2"))
             }
      )
    }
    plot_DR_results <- function(dr_data, sample_info, title_suffix) {
      df <- base::as.data.frame(dr_data) %>%
        dplyr::mutate(
          SampleType = stringr::str_split(rownames(.), "_", 2, TRUE)[, 1],
          Type = stringr::str_remove_all(rownames(.), "^....|..$"),
          Species = dplyr::case_when(
            SampleType == "B73" ~ "Zea mays ssp. mays",
            TRUE ~ "Zea mays ssp. mexicana"
          )
        )
      ggplot2::ggplot(df) +
        ggplot2::geom_point(ggplot2::aes(x = V1, y = V2, color = Type, shape = Species),
                   size = 1.2, alpha = 0.8) +
        ggplot2::stat_ellipse(ggplot2::aes(x = V1, y = V2, fill = Type),
                     geom = 'polygon', level = 0.95, alpha = 0.25) +
        ggplot2::stat_ellipse(ggplot2::aes(x = V1, y = V2, color = Type),
                     geom = 'path', level = 0.95, alpha = 1, linewidth = 0.5)+
        ggsci::scale_color_lancet() +
        ggsci::scale_fill_lancet()+
        labs(
          x = "Component 1",
          y = "Component 2",
          title = base::paste(input$dimReductionMethod, "analysis", title_suffix)
        ) +
        ggplot2::theme_bw()
    }
    output$DR_BeforeNormalization <- shiny::renderPlot({
      shiny::req(DR_results$before)
      plot_DR_results(DR_results$before, rv$sample_info, "Before Normalization")
    })
    output$DR_AfterNormalization <- shiny::renderPlot({
      shiny::req(DR_results$after)
      plot_DR_results(DR_results$after, rv$sample_info, "After Normalization")
    })
    output$dr_download_before_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste(input$dimReductionMethod, "_before_normalization_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        shiny::req(DR_results$before)
        grDevices::pdf(file, width = input$dr_plot_width, height = input$dr_plot_height)
        print(plot_DR_results(DR_results$before, rv$sample_info, "Before Normalization"))
        grDevices::dev.off()
      }
    )
    output$dr_download_after_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste(input$dimReductionMethod, "_after_normalization_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        shiny::req(DR_results$after)
        grDevices::pdf(file, width = input$dr_plot_width, height = input$dr_plot_height)
        print(plot_DR_results(DR_results$after, rv$sample_info, "After Normalization"))
        grDevices::dev.off()
      }
    )
    output$dr_download_both_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste(input$dimReductionMethod, "_both_plots_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        shiny::req(DR_results$before, DR_results$after)
        grDevices::pdf(file, width = input$dr_plot_width * 2, height = input$dr_plot_height)
        gridExtra::grid.arrange(
          plot_DR_results(DR_results$before, rv$sample_info, "Before Normalization"),
          plot_DR_results(DR_results$after, rv$sample_info, "After Normalization"),
          ncol = 2
        )
        grDevices::dev.off()
      }
    )
  })
}
