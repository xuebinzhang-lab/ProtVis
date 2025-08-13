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
            icon = HTML('<svg viewBox="0 0 16 16" width="20" height="20" fill="currentColor">
        <rect x="2" y="2" width="12" height="12" stroke="currentColor" fill="none"/>
        <!-- Points below the line -->
        <circle cx="4" cy="12" r="1"/>
        <circle cx="5" cy="10.5" r="1"/>
        <circle cx="7" cy="9" r="1"/>
        <circle cx="9" cy="7.5" r="1"/>
        <circle cx="11" cy="6" r="1"/>
        <!-- Points above the line -->
        <circle cx="4.5" cy="9" r="1"/>
        <circle cx="6" cy="7.5" r="1"/>
        <circle cx="8" cy="5.5" r="1"/>
        <circle cx="10" cy="4" r="1"/>
        <circle cx="12" cy="2.5" r="1"/>
        <!-- Diagonal line -->
        <line x1="3" y1="13" x2="13" y2="3" stroke="currentColor" stroke-width="1.5"/>
    </svg>')
          ),
          accordion_panel(
            title = "Expression pattern",
            icon = HTML('<svg viewBox="0 0 16 16" width="20" height="20">
        <!-- 4x4 heatmap grid with varying grayscale -->
        <rect x="1" y="1" width="3" height="3" fill="#000000"/>
        <rect x="5" y="1" width="3" height="3" fill="#333333"/>
        <rect x="9" y="1" width="3" height="3" fill="#666666"/>
        <rect x="13" y="1" width="3" height="3" fill="#999999"/>

        <rect x="1" y="5" width="3" height="3" fill="#333333"/>
        <rect x="5" y="5" width="3" height="3" fill="#666666"/>
        <rect x="9" y="5" width="3" height="3" fill="#999999"/>
        <rect x="13" y="5" width="3" height="3" fill="#cccccc"/>

        <rect x="1" y="9" width="3" height="3" fill="#666666"/>
        <rect x="5" y="9" width="3" height="3" fill="#999999"/>
        <rect x="9" y="9" width="3" height="3" fill="#cccccc"/>
        <rect x="13" y="9" width="3" height="3" fill="#eeeeee"/>

        <rect x="1" y="13" width="3" height="3" fill="#999999"/>
        <rect x="5" y="13" width="3" height="3" fill="#cccccc"/>
        <rect x="9" y="13" width="3" height="3" fill="#eeeeee"/>
        <rect x="13" y="13" width="3" height="3" fill="#ffffff"/>
    </svg>')
          ),
          accordion_panel(
            title = "Dimensionality Reduction Analysis",
            icon = HTML('<svg viewBox="0 0 16 16" width="20" height="20" fill="currentColor">
        <!-- Square outline -->
        <rect x="2" y="2" width="12" height="12" stroke="currentColor" fill="none"/>

        <!-- Left vertical ellipse with 3 points (taller than wide) -->
        <ellipse cx="4.5" cy="8" rx="1.5" ry="3" stroke="currentColor" fill="none"/>
        <circle cx="4" cy="6.5" r="0.8"/>  <!-- Top point -->
        <circle cx="4.5" cy="8" r="0.8"/>   <!-- Center point -->
        <circle cx="5" cy="9.5" r="0.8"/>   <!-- Bottom point -->

        <!-- Right vertical ellipse with 3 points (spaced farther) -->
        <ellipse cx="11.5" cy="8" rx="1.5" ry="3" stroke="currentColor" fill="none"/>
        <circle cx="11" cy="6.5" r="0.8"/>  <!-- Top point -->
        <circle cx="11.5" cy="8" r="0.8"/>  <!-- Center point -->
        <circle cx="12" cy="9.5" r="0.8"/>   <!-- Bottom point -->
    </svg>')
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
