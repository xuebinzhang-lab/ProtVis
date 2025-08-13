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
          ),
          accordion_panel(
            title = "Expression pattern",
            icon = expression_pattern_icon,
          ),
          accordion_panel(
            title = "Dimensionality Reduction Analysis",
            icon = dimensionality_reduction_analysis_icon,
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
            card_header("PCA before normalization"),
            card_body(
              plotOutput(ns("pcaBeforeNormalization"))
            )
          ),

          card(
            height = "800px",
            card_header("PCA after normalization"),
            card_body(
              plotOutput(ns("pcaAfterNormalization"))
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
      normalized_matrix = NULL
    )

    # 加载数据
    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step6_data_normalization.rda")

      if (!file.exists(rda_path)) {
        rv$load_success <- FALSE
        showNotification("Step6_data_normalization.rda not found in the specified directory.", type = "error")
        return()
      }

      tryCatch({
        e <- new.env()
        load(rda_path, envir = e)

        if (!exists("sample_info", envir = e) || !exists("normalized_data", envir = e)) {
          showNotification("Required data (sample_info or normalized_data) not found in the RDA file.", type = "error")
          rv$load_success <- FALSE
          return()
        }

        # 验证数据
        if (!is.data.frame(e$sample_info) || !is.matrix(e$normalized_data)) {
          showNotification("Loaded data has incorrect format.", type = "error")
          rv$load_success <- FALSE
          return()
        }

        rv$sample_info <- e$sample_info
        rv$expression_matrix <- e$normalized_data
        rv$normalized_matrix <- e$normalized_data  # 保持一致性
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")

      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
        rv$load_success <- FALSE
      })
    })

    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # 相关性图
    output$cor_res <- renderPlot({
      req(rv$sample_info)
      req(rv$normalized_matrix)
      sample_info <- rv$sample_info
      expmat_share <- rv$normalized_matrix
      # calculate
      cor <- cor(expmat_share)

      # 生成 metadata_share 和 left_anno
      metadata_share <- dplyr::left_join(
        data.frame(sample_id = colnames(expmat_share)), sample_info, by = "sample_id"
      ) %>% dplyr::mutate(tissue2 = stringr::str_split(tissue, "_", 2, TRUE)[, 1])

      left_anno = ComplexHeatmap::rowAnnotation(
        Tissue = metadata_share %>% dplyr::select(tissue2) %>% as.matrix(),
        Species = metadata_share %>% dplyr::select(species) %>% as.matrix(),
        col = list(
          tissue = c("green", "brown", "tan", "darkgreen", "blue") %>%
            stats::setNames(c("Leaf", "Pulvinus", "Root", "Stem", "Shoot.tip")),
          species = c("orange", "lightgreen") %>%
            stats::setNames(c("Zea mays ssp. mays", "Zea mays ssp. mexicana"))
        ),
        annotation_name_gp = grid::gpar(fontsize = 6),  # 注释名称字体大小
        annotation_legend_param = list(title_gp = grid::gpar(fontsize = 6),
                                       labels_gp = grid::gpar(fontsize = 6))  # 注释图例字体大小
      )

      # 生成 Heatmap 对象
      ht <- ComplexHeatmap::Heatmap(
        cor,
        right_annotation = left_anno,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = 'black',
        name = "r",
        col = circlize::colorRamp2(colors = c("purple", "black", "yellow"), breaks = c(-1, 0, 1)),
        heatmap_legend_param = list(
          title_gp = grid::gpar(fontsize = 6),  # 图例标题字体大小
          labels_gp = grid::gpar(fontsize = 6)  # 图例标签字体大小
        )
      )
      ComplexHeatmap::draw(ht)
    })

    # 表达模式
    output$expression_pattern <- renderPlot({
      req(rv$sample_info)
      req(rv$normalized_matrix)
      sample_info <- rv$sample_info
      expmat_share <- rv$normalized_matrix
      # plot
      metadata_share <- dplyr::left_join(
        data.frame(sample_id = colnames(expmat_share)), sample_info, by = "sample_id"
      ) %>% dplyr::mutate(tissue2 = stringr::str_split(tissue, "_", 2, TRUE)[, 1])
      left_anno = ComplexHeatmap::rowAnnotation(
        Tissue = metadata_share %>% dplyr::select(tissue2) %>% as.matrix(),
        Species = metadata_share %>% dplyr::select(species) %>% as.matrix(),
        col = list(
          tissue = c("green", "brown", "tan", "darkgreen", "blue") %>%
            stats::setNames(c("Leaf", "Pulvinus", "Root", "Stem", "Shoot.tip")),
          species = c("orange", "lightgreen") %>%
            stats::setNames(c("Zea mays ssp. mays", "Zea mays ssp. mexicana"))
        ),
        annotation_name_gp = grid::gpar(fontsize = 6),  # 注释名称字体大小
        annotation_legend_param = list(title_gp = grid::gpar(fontsize = 6),
                                       labels_gp = grid::gpar(fontsize = 6))  # 注释图例字体大小
      )
      ht_expmat <- ComplexHeatmap::Heatmap(
        expmat_share[rowSums(expmat_share) > 0, ] %>% t(),  # 直接矩阵操作更高效
        use_raster = TRUE,  # 明确启用栅格化
        raster_quality = 2,  # 提高栅格质量（1-4，越大质量越高）
        right_annotation = left_anno,
        show_row_names = TRUE,
        show_column_names = FALSE,
        row_names_gp = grid::gpar(fontsize = 6),
        border = 'black',
        name = "Scaled \nintensity",
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



  })
}
