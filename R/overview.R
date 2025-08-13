overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
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
            actionButton(ns("run_correlation"), "Run Correlation")
          ),
          accordion_panel(
            title = "Expression pattern",
            icon = expression_pattern_icon,
            sliderInput(
              inputId = ns("exp_top_n"),
              label = "Top N Features:",
              min = 50,
              max = 1000,
              value = 500,
              step = 50
            ),
            checkboxInput(
              inputId = ns("exp_scale"),
              label = "Scale Data",
              value = TRUE
            ),
            actionButton(ns("run_expression"), "Run Expression")
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
            actionButton(ns("DR_analyse"), "Run")
          )
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 600,
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
        rv$load_success <- FALSE
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
          rv$load_success <- FALSE
          return()
        }

        # 验证Step6数据
        if (!exists("normalized_data", envir = e6)) {
          showNotification("Required data not found in Step6 file.", type = "error")
          rv$load_success <- FALSE
          return()
        }

        # 检查样本一致性
        if (!identical(colnames(e5$imputed_df), colnames(e6$normalized_data))) {
          showNotification("Sample names don't match between imputed and normalized data.", type = "error")
          rv$load_success <- FALSE
          return()
        }

        # 存储数据
        rv$sample_info <- e5$sample_info
        rv$expression_matrix <- e6$normalized_data
        rv$normalized_matrix <- e6$normalized_data
        rv$imputed_matrix <- e5$imputed_df
        rv$load_success <- TRUE

        showNotification("✅ Both datasets loaded successfully.", type = "message")

      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
        rv$load_success <- FALSE
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

    output$cor_res <- renderPlot({
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

      ht <- ComplexHeatmap::Heatmap(
        rv$cor_results,
        right_annotation = left_anno,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = 'black',
        name = "r",
        col = circlize::colorRamp2(
          colors = c("purple", "black", "yellow"),
          breaks = c(-1, 0, 1)
        ),
        heatmap_legend_param = list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        )
      )
      ComplexHeatmap::draw(ht)
    })

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

    output$expression_pattern <- renderPlot({
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

      ht_expmat <- ComplexHeatmap::Heatmap(
        t(rv$exp_results),
        use_raster = TRUE,
        raster_quality = 2,
        right_annotation = left_anno,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = 'black',
        name = ifelse(input$exp_scale, "Z-score", "Intensity"),
        col = circlize::colorRamp2(
          colors = c("purple", "black", "yellow"),
          breaks = c(-1, 0, 1)
        ),
        heatmap_legend_param = list(
          title_gp = grid::gpar(fontsize = 6),
          labels_gp = grid::gpar(fontsize = 6)
        )
      )
      ComplexHeatmap::draw(ht_expmat)
    })

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
               prcomp(t_data, scale. = TRUE)$x[, 1:2]
             },
             "PCoA" = {
               dist_matrix <- dist(t_data)
               cmdscale(dist_matrix, k = 2)
             },
             "tSNE" = {
               Rtsne::Rtsne(t_data, perplexity = 5)$Y
             },
             "UMAP" = {
               umap::umap(t_data)$layout[, 1:2]
             },
             "NMDS" = {
               vegan::metaMDS(t_data, k = 2)$points
             }
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

    # 通用绘图函数
    plot_DR_results <- function(dr_data, sample_info, title_suffix) {
      df <- as.data.frame(dr_data) %>%
        dplyr::mutate(
          SampleType = str_split(rownames(.), "_", 2, TRUE)[, 1],
          Type = str_remove_all(rownames(.), "^....|..$"),
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


  })
}
