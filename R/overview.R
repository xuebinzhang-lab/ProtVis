#' Overview UI Module
#'
#' Creates the user interface for the overview analysis module
#'
#' @param id Character string specifying the namespace id
#' @return A Shiny UI tagList containing the overview analysis interface
#' @export
#'
overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = sidebar(
        width = 300,
        actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        uiOutput(ns("load_status_panel")),
        accordion(
          accordion_panel(
            title = "Correlation",
            icon = correlation_icon,
            selectInput(
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
            numericInput(ns("cor_color_min"), "Set Min Value", value = -1, step = 0.1),
            numericInput(ns("cor_color_max"), "Set Max Value", value = 1, step = 0.1),
            actionButton(ns("run_correlation"), "Run Correlation"),
            numericInput(ns("cor_plot_width"), "Download Plot Width (inches)", value = 10),
            numericInput(ns("cor_plot_height"), "Download Plot Height (inches)", value = 7),
            downloadButton(ns("cor_download_pdf"), "Download PDF")
          ),
          accordion_panel(
            title = "Expression pattern",
            icon = expression_pattern_icon,
            sliderInput(
              inputId = ns("exp_top_n"),
              label = "Top N Features:",
              min = 50,
              max = 2000,
              value = 500,
              step = 50
            ),
            checkboxInput(
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
            numericInput(ns("exp_color_min"), "Set Min Value", value = -1, step = 0.1),
            numericInput(ns("exp_color_max"), "Set Max Value", value = 1, step = 0.1),
            actionButton(ns("run_expression"), "Run Expression"),
            numericInput(ns("exp_plot_width"), "Download Plot Width (inches)", value = 10),
            numericInput(ns("exp_plot_height"), "Download Plot Height (inches)", value = 7),
            downloadButton(ns("exp_download_pdf"), "Download PDF")
          ),
          accordion_panel(
            title = "Dimensionality Reduction",
            icon = dimensionality_reduction_icon,
            selectInput(
              inputId = ns("dimReductionMethod"),
              label = "Select Method:",
              choices = c("PCA", "PCoA", "tSNE", "UMAP", "NMDS"),
              selected = "UMAP"
            ),
            actionButton(ns("DR_analyse"), "Run"),
            numericInput(ns("dr_plot_width"), "Download Plot Width (inches)", value = 10),
            numericInput(ns("dr_plot_height"), "Download Plot Height (inches)", value = 7),
            downloadButton(ns("dr_download_before_pdf"), "Download Before Normalization"),
            downloadButton(ns("dr_download_after_pdf"), "Download After Normalization"),
            downloadButton(ns("dr_download_both_pdf"), "Download Both Plots")
          )
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 750,
          card(
            height = "800px",
            card_header("Correlation"),
            card_body(
              plotOutput(ns("cor_res"))
            )
          ),
          card(
            height = "800px",
            card_header("Expression pattern"),
            card_body(
              plotOutput(ns("expression_pattern"))
            )
          ),
          card(
            height = "800px",
            card_header("Dimensionality reduction analyse before normalization"),
            card_body(
              plotOutput(ns("DR_BeforeNormalization"))
            )
          ),
          card(
            height = "800px",
            card_header("Dimensionality reduction analyse after normalization"),
            card_body(
              plotOutput(ns("DR_AfterNormalization"))
            )
          )
        )
      )
    )
  )
}

#' Overview Server Module
#'
#' Server-side logic for the overview analysis module
#'
#' @param id Character string specifying the namespace id
#' @param shared_state Reactive values shared across modules
#' @return A module server function that handles the overview analysis logic
#' @export
#'
overview_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      imputed_matrix = NULL,
      cor_results = NULL,
      exp_results = NULL,
      DR_results = list(before = NULL, after = NULL)
    )

    # 加载数据
    observeEvent(input$load_data, {
      req(shared_state$workdir)

      # 定义两个RDA文件路径
      step5_path <- file.path(shared_state$workdir, "Step5_data_imputation.rda")
      step6_path <- file.path(shared_state$workdir, "Step6_data_normalization.rda")

      # 检查文件是否存在
      if (!file.exists(step5_path) || !file.exists(step6_path)) {
        missing_files <- c(step5_path, step6_path)[!file.exists(c(step5_path, step6_path))]
        showNotification(paste("File(s) not found:", paste(basename(missing_files), collapse = ", ")),
                         type = "error")
        rv$load_success = FALSE
        return()
      }

      tryCatch({
        # 加载Step5数据
        e5 <- new.env()
        load(step5_path, envir = e5)

        # 加载Step6数据
        e6 <- new.env()
        load(step6_path, envir = e6)

        # 验证Step5数据
        if (!exists("sample_info", envir = e5) || !exists("imputed_df", envir = e5)) {
          showNotification("Required data not found in Step5 file.", type = "error")
          rv$load_success = FALSE
          return()
        }

        # 验证Step6数据
        if (!exists("normalized_data", envir = e6)) {
          showNotification("Required data not found in Step6 file.", type = "error")
          rv$load_success = FALSE
          return()
        }

        # 检查样本一致性
        if (!identical(colnames(e5$imputed_df), colnames(e6$normalized_data))) {
          showNotification("Sample names don't match between imputed and normalized data.", type = "error")
          rv$load_success = FALSE
          return()
        }

        # 存储数据
        rv$sample_info <- e5$sample_info
        rv$expression_matrix <- e6$normalized_data
        rv$normalized_matrix <- e6$normalized_data
        rv$imputed_matrix <- e5$imputed_df
        rv$load_success = TRUE

        showNotification("✅ Both datasets loaded successfully.", type = "message")

      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
        rv$load_success = FALSE
      })
    })

    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        div(
          span("✅ Both datasets loaded successfully", style = "color: green;"),
          br(),
          paste("Imputed data:", nrow(rv$imputed_matrix), "proteins,",
                ncol(rv$imputed_matrix), "samples"),
          br(),
          paste("Normalized data:", nrow(rv$normalized_matrix), "proteins,",
                ncol(rv$normalized_matrix), "samples")
        )
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # 相关性分析
    observeEvent(input$run_correlation, {
      req(rv$normalized_matrix)

      withProgress(message = 'Calculating correlations...', value = 0.5, {
        rv$cor_results <- cor(
          rv$normalized_matrix,
          method = tolower(input$cor_method)
        )
        incProgress(1, detail = "Done")
      })
    })

    # 创建reactive对象ht来动态生成热图
    ht_reactive <- reactive({
      req(rv$cor_results)
      req(rv$sample_info)

      metadata_share <- dplyr::left_join(
        data.frame(sample_id = colnames(rv$normalized_matrix)),
        rv$sample_info,
        by = "sample_id"
      ) %>%
        dplyr::mutate(tissue2 = stringr::str_split(tissue, "_", 2, TRUE)[, 1])

      left_anno <- ComplexHeatmap::rowAnnotation(
        Tissue = metadata_share %>% dplyr::select(tissue2) %>% as.matrix(),
        Species = metadata_share %>% dplyr::select(species) %>% as.matrix(),
        col = list(
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

      # 获取用户输入的最小值和最大值
      min_break <- input$cor_color_min
      max_break <- input$cor_color_max
      mid_break <- (min_break + max_break) / 2  # 自动计算中间值

      # 创建热图
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
        heatmap_legend_param = list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        ),
        cell_fun = function(j, i, x, y, width, height, fill) {
          # 在每个单元格内显示数字
          grid::textGrob(
            label = round(rv$cor_results[i, j], 2),  # 四舍五入显示两位小数
            x = x, y = y,
            gp = grid::gpar(fontsize = 6, col = "white")  # 设置字体颜色为白色，字体大小为6
          )
        }
      )
    })

    # 显示热图
    output$cor_res <- renderPlot({
      ht_reactive()
    })

    # 下载PDF文件的处理
    output$cor_download_pdf <- downloadHandler(
      filename = function() {
        paste("correlation_heatmap_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # 设置PDF输出的尺寸
        pdf(file, width = input$cor_plot_width, height = input$cor_plot_height)
        ComplexHeatmap::draw(ht_reactive())
        dev.off()
      }
    )

    # 表达模式分析
    observeEvent(input$run_expression, {
      req(rv$normalized_matrix)

      withProgress(message = 'Analyzing expression patterns...', value = 0.5, {
        mat <- rv$normalized_matrix

        # 选择高表达基因
        row_vars <- matrixStats::rowVars(mat)
        top_idx <- order(row_vars, decreasing = TRUE)[1:input$exp_top_n]
        mat <- mat[top_idx, ]

        # 数据标准化
        if (input$exp_scale) {
          mat <- t(scale(t(mat)))
        }

        rv$exp_results <- mat
        incProgress(1, detail = "Done")
      })
    })

    # 创建reactive对象ht来动态生成热图
    ht_reactive_exp <- reactive({
      req(rv$exp_results)
      req(rv$sample_info)

      metadata_share <- dplyr::left_join(
        data.frame(sample_id = colnames(rv$normalized_matrix)),
        rv$sample_info,
        by = "sample_id"
      ) %>%
        dplyr::mutate(tissue2 = stringr::str_split(tissue, "_", 2, TRUE)[, 1])

      left_anno <- ComplexHeatmap::rowAnnotation(
        Tissue = metadata_share %>% dplyr::select(tissue2) %>% as.matrix(),
        Species = metadata_share %>% dplyr::select(species) %>% as.matrix(),
        col = list(
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

      # 获取用户输入的最小值和最大值
      min_break <- input$exp_color_min
      max_break <- input$exp_color_max
      mid_break <- (min_break + max_break) / 2  # 自动计算中间值

      # 使用 `layer_fun` 替代 `cell_fun`
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
        heatmap_legend_param = list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        ),
        layer_fun = function(j, i, x, y, width, height, fill) {
          # 确保条件为单一逻辑值，避免"长度大于1"错误
          if (length(i) == 1 && length(j) == 1) {  # 检查是否是单一的行列索引
            grid::textGrob(
              label = round(rv$exp_results[i, j], 2),  # 四舍五入显示两位小数
              x = x, y = y,
              gp = grid::gpar(fontsize = 6, col = "white")  # 设置字体颜色为白色，字体大小为6
            )
          }
        }
      )
    })

    # 显示热图
    output$expression_pattern <- renderPlot({
      ht_reactive_exp()
    })

    # 下载PDF文件的处理
    output$exp_download_pdf <- downloadHandler(
      filename = function() {
        paste("expression_pattern_heatmap_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # 设置PDF输出的尺寸
        pdf(file, width = input$exp_plot_width, height = input$exp_plot_height)
        ComplexHeatmap::draw(ht_reactive_exp())
        dev.off()
      }
    )

    # 存储降维结果的reactiveValues
    DR_results <- reactiveValues(
      before = NULL,
      after = NULL
    )

    # 观察分析按钮点击事件
    observeEvent(input$DR_analyse, {
      req(rv$sample_info)

      # 显示进度条
      withProgress(message = 'Running dimensionality reduction...', value = 0.5, {
        # 标准化前数据降维
        if (!is.null(rv$imputed_matrix)) {
          DR_results$before <- perform_DR(rv$imputed_matrix, input$dimReductionMethod)
          incProgress(0.3, detail = "Finished pre-normalization")
        }

        # 标准化后数据降维
        if (!is.null(rv$normalized_matrix)) {
          DR_results$after <- perform_DR(rv$normalized_matrix, input$dimReductionMethod)
          incProgress(0.2, detail = "Finished post-normalization")
        }
      })
    })

    # 降维分析函数
    perform_DR <- function(data, method) {
      # 转置数据（样本在行，特征在列）
      t_data <- t(data)

      # 根据选择的方法执行降维
      switch(method,
             "PCA" = {
               as.data.frame(prcomp(t_data)$x[, 1:2]) %>% data.table::setnames(c("V1","V2"))
             },
             "PCoA" = {
               as.data.frame(cmdscale(dist(t_data), k = 2))
             },
             "tSNE" = {
               as.data.frame(Rtsne::Rtsne(t_data, perplexity = 5)$Y) %>%
                 magrittr::set_rownames(rownames(t_data))
             },
             "UMAP" = {
               as.data.frame(umap::umap(t_data)$layout[, 1:2])
             },
             "NMDS" = {
               as.data.frame(vegan::metaMDS(t_data, k = 2)[["points"]])%>% data.table::setnames(c("V1","V2"))
             }
      )
    }

    # 通用绘图函数
    plot_DR_results <- function(dr_data, sample_info, title_suffix) {
      df <- as.data.frame(dr_data) %>%
        dplyr::mutate(
          SampleType = stringr::str_split(rownames(.), "_", 2, TRUE)[, 1],
          Type = stringr::str_remove_all(rownames(.), "^....|..$"),
          Species = case_when(
            SampleType == "B73" ~ "Zea mays ssp. mays",
            TRUE ~ "Zea mays ssp. mexicana"
          )
        )

      ggplot(df) +
        geom_point(aes(x = V1, y = V2, color = Type, shape = Species),
                   size = 1.2, alpha = 0.8) +
        stat_ellipse(aes(x = V1, y = V2, fill = Type),
                     geom = 'polygon', level = 0.95, alpha = 0.25) +
        ggsci::scale_color_lancet() +
        theme_bw() +
        labs(
          x = "Component 1",
          y = "Component 2",
          title = paste(input$dimReductionMethod, "analysis", title_suffix)
        ) +
        theme(
          plot.title = element_text(size = 12, hjust = 0.5),
          panel.border = element_rect(colour = "black", size = 2),
          axis.ticks = element_line(color = "black", linewidth = 2),
          legend.text = element_text(size = 16),
          axis.text = element_text(size = 16, colour = "black"),
          axis.title = element_text(size = 16, colour = "black"),
          panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),
          panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.2)
        )
    }

    # 标准化前降维图
    output$DR_BeforeNormalization <- renderPlot({
      req(DR_results$before)
      plot_DR_results(DR_results$before, rv$sample_info, "Before Normalization")
    })

    # 标准化后降维图
    output$DR_AfterNormalization <- renderPlot({
      req(DR_results$after)
      plot_DR_results(DR_results$after, rv$sample_info, "After Normalization")
    })

    # 下载标准化前降维图的PDF
    output$dr_download_before_pdf <- downloadHandler(
      filename = function() {
        paste(input$dimReductionMethod, "_before_normalization_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        req(DR_results$before)

        # 设置PDF输出的尺寸
        pdf(file, width = input$dr_plot_width, height = input$dr_plot_height)
        print(plot_DR_results(DR_results$before, rv$sample_info, "Before Normalization"))
        dev.off()
      }
    )

    # 下载标准化后降维图的PDF
    output$dr_download_after_pdf <- downloadHandler(
      filename = function() {
        paste(input$dimReductionMethod, "_after_normalization_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        req(DR_results$after)

        # 设置PDF输出的尺寸
        pdf(file, width = input$dr_plot_width, height = input$dr_plot_height)
        print(plot_DR_results(DR_results$after, rv$sample_info, "After Normalization"))
        dev.off()
      }
    )

    # 下载两个降维图的PDF（组合）
    output$dr_download_both_pdf <- downloadHandler(
      filename = function() {
        paste(input$dimReductionMethod, "_both_plots_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        req(DR_results$before, DR_results$after)

        # 设置PDF输出的尺寸
        pdf(file, width = input$dr_plot_width * 2, height = input$dr_plot_height)

        # 创建两行一列的布局
        gridExtra::grid.arrange(
          plot_DR_results(DR_results$before, rv$sample_info, "Before Normalization"),
          plot_DR_results(DR_results$after, rv$sample_info, "After Normalization"),
          ncol = 2
        )
        dev.off()
      }
    )

  })
}
