DEG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = sidebar(
        width = 300,
        fileInput(ns("data_count_file"), "Upload Gene Expression Data", accept = c(".xlsx")),
        helpText("Note: The first column name should be 'GeneID'"),
        actionButton(ns("generate_plot"), "Run"),
        accordion(
          accordion_panel(
            title = "PCA",
            icon = correlation_icon,
            numericInput(ns("download_width_pca"), "Width of PCA Plot Download (inches)", value = 7),
            numericInput(ns("download_height_pca"), "Height of PCA Plot Download (inches)", value = 7),
            downloadButton(ns("download_pca"), "Download PCA Plot PDF")  # PCA下载按钮
          )
        ),
        accordion(
          accordion_panel(
            title = "Volcano Map",
            icon = volcano_icon,
            colourpicker::colourInput(ns("color_up"), "Color for Up", value = "salmon"),
            colourpicker::colourInput(ns("color_down"), "Color for Down", value = "lightblue"),
            colourpicker::colourInput(ns("color_not_sig"), "Color for Not Significant", value = "grey"),
            downloadButton(ns("download_pdf"), "Download Volcano Plot PDF"),
            numericInput(ns("download_width_voc"), "Width of Volcano Plot Download (inches)", value = 7),
            numericInput(ns("download_height_voc"), "Height of Volcano Plot Download (inches)", value = 7)
          )
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 750,
          card(
            height = "800px",
            card_header("PCA"),
            card_body(
              plotOutput(ns("pca_plot"))  # 用于展示PCA图
            )
          ),
          card(
            height = "800px",
            card_header("Volcano Map"),
            card_body(
              uiOutput(ns("progress_ui")),
              plotOutput(ns("voc_plot"))
            )
          )
        )
      )
    )
  )
}
DEG_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 反应性加载数据
    data_count <- reactive({
      req(input$data_count_file)
      readxl::read_xlsx(input$data_count_file$datapath)
    })

    # 创建进度条 UI - 可选
    output$progress_ui <- renderUI({
      req(input$generate_plot)
      withProgress(message = 'Running Differential Expression Analysis...', value = 0, {
        incProgress(0.5, detail = "Loading data...")
        incProgress(0.5, detail = "Generating volcano plot...")
      })
    })

    # 进行差异分析
    res_tbl <- reactive({
      count_data <- data_count()

      # 处理count数据并做差异分析
      data_count.mat <- count_data %>%
        column_to_rownames("GeneID") %>%
        mutate_if(is.numeric, ceiling) %>% as.matrix()

      colData = data.frame(
        row.names = colnames(data_count.mat),
        group = rep(c("B73", "Y12"), each = 3) %>% factor(levels = c("B73", "Y12"))
      )

      dds <- DESeqDataSetFromMatrix(countData = data_count.mat, colData = colData, design = ~ group)
      dds <- DESeq(dds)
      res <- results(dds, contrast = c("group", "B73", "Y12"))

      res_tbl <- res %>%
        as.data.frame() %>%
        tibble::rownames_to_column("GeneID") %>%
        mutate(regular = case_when(
          padj < 0.05 & log2FoldChange > 1 ~ "up",
          padj < 0.05 & log2FoldChange < -1 ~ "down",
          TRUE ~ "not sig"
        ))

      return(res_tbl)
    })

    # PCA 计算和可视化
    pca_res <- reactive({
      count_data <- data_count()
      data_count.mat <- count_data %>%
        column_to_rownames("GeneID")

      # 创建分组信息
      group <- data.frame(
        group = c("B73","B73","B73","Y12","Y12","Y12")
      )
      rownames(group) <- colnames(data_count.mat)

      # 进行PCA分析
      pca_result <- PCAtools::pca(data_count.mat, metadata = group)

      return(pca_result)
    })

    # 保存PCA图的reactive值
    pca_plot_ready <- reactiveVal(FALSE)  # 控制PCA图的显示

    # 在点击"Run"按钮后生成PCA图
    observeEvent(input$generate_plot, {
      pca_plot_ready(TRUE)
    })

    # 绘制PCA图并保存到变量中
    pca_plot_obj <- reactive({
      req(pca_plot_ready())  # 确保PCA图只有在点击按钮后才显示
      pca_result <- pca_res()
      pca_plot <- PCAtools::biplot(pca_result,
                                   x = "PC1",
                                   y = "PC2",
                                   colby = "group",        # 按组别着色
                                   legendPosition = "right",  # 图例位置
                                   lab = NULL,               # 不显示样本标签
                                   encircle = TRUE,          # 添加分组椭圆
                                   encircleFill = TRUE)      # 填充椭圆区域
      return(pca_plot)
    })

    # 绘制PCA图
    output$pca_plot <- renderPlot({
      req(pca_plot_ready())  # 只有点击后才绘制图形
      pca_plot_obj()  # 使用保存的变量进行渲染
    })

    # 绘制火山图并保存到变量中
    voc_plot_obj <- reactive({
      req(input$generate_plot)
      res_tbl_data <- res_tbl()  # 获取差异分析结果

      # 绘制火山图
      voc_plot <- ggplot(res_tbl_data, aes(x = log2FoldChange, y = -log10(padj))) +
        geom_point(aes(color = regular, size = -log10(padj)), alpha = 0.7) +
        scale_color_manual(values = c(
          "up" = input$color_up,
          "down" = input$color_down,
          "not sig" = input$color_not_sig
        )) +
        scale_size(range = c(0, 1.5)) +
        geom_hline(aes(yintercept = -log10(0.05)), linewidth = 0.3, linetype = "dashed", color = "black") +
        geom_vline(aes(xintercept = -1), linewidth = 0.3, linetype = "dashed", color = "black") +
        geom_vline(aes(xintercept = 1), linewidth = 0.3, linetype = "dashed", color = "black") +
        coord_cartesian(ylim = c(0, 50)) +
        theme_bw() +
        theme(
          line = element_line(linewidth = 0.5, colour = "black"),
          text = element_text(size = 7, colour = "black"),
          axis.title = element_text(size = 7, colour = "black"),
          rect = element_rect(linewidth = 0.5, colour = "black"),
          panel.grid = element_blank(),
          legend.position = "none"
        )
      return(voc_plot)
    })

    # 绘制火山图
    output$voc_plot <- renderPlot({
      voc_plot_obj()  # 使用保存的变量进行渲染
    })

    # 生成 PCA 图下载
    output$download_pca <- downloadHandler(
      filename = function() {
        paste("PCA_plot", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = pca_plot_obj(), device = "pdf", width = input$download_width_pca, height = input$download_height_pca) # 使用用户输入的尺寸
      }
    )

    # 生成 Volcano Plot 下载
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste("volcano_plot", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = voc_plot_obj(), device = "pdf", width = input$download_width_voc, height = input$download_height_voc) # 使用用户输入的尺寸
      }
    )
  })
}
