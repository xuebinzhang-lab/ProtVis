# UI 部分
DEG_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = sidebar(
        width = 300,
        fileInput(ns("data_count_file"), "Upload Gene Expression Data", accept = c(".xlsx")),
        helpText("Note: The first column name in expression file should be 'GeneID'"),
        fileInput(ns("group_file"), "Upload Group Information", accept = c(".xlsx")),
        helpText("Note: The first column in the group file should be 'Sample', and the second column should be 'Group'"),
        actionButton(ns("generate_plot"), "Run Analysis", class = "btn-primary"),

        hr(),

        accordion(
          accordion_panel(
            title = "PCA Settings",
            icon = pca_icon,
            # PCA图形设置
            selectInput(ns("pca_colby"), "Color by:",
                        choices = c("None" = "none"),
                        selected = "none"),
            selectInput(ns("pca_shapeby"), "Shape by:",
                        choices = c("None" = "none"),
                        selected = "none"),
            selectInput(ns("pca_pointsize"), "Point Size:",
                        choices = c("Small" = 2, "Medium" = 3, "Large" = 4),
                        selected = 3),

            # 动态组颜色设置
            uiOutput(ns("group_colors_ui")),

            colourpicker::colourInput(ns("pca_base_color"), "Base Color (when no grouping)", value = "#2E86AB"),
            checkboxInput(ns("pca_show_labels"), "Show Sample Labels", value = FALSE),
            checkboxInput(ns("pca_encircle"), "Encircle Groups", value = TRUE),
            checkboxInput(ns("pca_show_ellipse"), "Show Confidence Ellipse", value = TRUE),
            numericInput(ns("pca_ellipse_alpha"), "Ellipse Transparency",
                         value = 0.2, min = 0, max = 1, step = 0.1),
            numericInput(ns("pca_legend_size"), "Legend Text Size",
                         value = 12, min = 8, max = 20, step = 1),

            hr(),
            numericInput(ns("download_width_pca"), "Width of PCA Plot (inches)",
                         value = 8, min = 3, max = 20),
            numericInput(ns("download_height_pca"), "Height of PCA Plot (inches)",
                         value = 7, min = 3, max = 20),
            downloadButton(ns("download_pca"), "Download PCA Plot PDF", class = "btn-sm"),
            downloadButton(ns("download_pca_data"), "Download PCA Data", class = "btn-sm")
          ),
          accordion_panel(
            title = "Volcano Plot Settings",
            icon = volcano_icon,
            colourpicker::colourInput(ns("color_up"), "Color for Up-regulated", value = "salmon"),
            colourpicker::colourInput(ns("color_down"), "Color for Down-regulated", value = "lightblue"),
            colourpicker::colourInput(ns("color_not_sig"), "Color for Not Significant", value = "grey"),
            numericInput(ns("volcano_point_size"), "Point Size",
                         value = 2, min = 1, max = 5, step = 0.5),
            sliderInput(ns("volcano_alpha"), "Point Transparency",
                        min = 0.1, max = 1, value = 0.7, step = 0.1),
            checkboxInput(ns("volcano_show_grid"), "Show Grid", value = FALSE),
            hr(),
            numericInput(ns("download_width_voc"), "Width of Volcano Plot (inches)",
                         value = 8, min = 3, max = 20),
            numericInput(ns("download_height_voc"), "Height of Volcano Plot (inches)",
                         value = 7, min = 3, max = 20),
            downloadButton(ns("download_pdf"), "Download Volcano Plot PDF", class = "btn-sm"),
            downloadButton(ns("download_deg_data"), "Download DEG Data", class = "btn-sm")
          )
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 750,
          card(
            height = "800px",
            card_header("PCA Analysis", icon = shiny::icon("chart-pie")),
            card_body(
              tabsetPanel(
                type = "tabs",
                tabPanel("Plot",
                         plotOutput(ns("pca_plot"), height = "650px")
                ),
                tabPanel("PCA Data",
                         div(
                           style = "margin-bottom: 10px;",
                           downloadButton(ns("download_pca_table"), "Download as CSV",
                                          class = "btn-sm btn-success", style = "float: right;")
                         ),
                         DT::DTOutput(ns("pca_data_table"), height = "600px")
                )
              )
            )
          ),
          card(
            height = "800px",
            card_header("Volcano Plot", icon = shiny::icon("fire")),
            card_body(
              tabsetPanel(
                type = "tabs",
                tabPanel("Plot",
                         plotOutput(ns("voc_plot"), height = "650px")
                ),
                tabPanel("DEG Results",
                         div(
                           style = "margin-bottom: 10px;",
                           downloadButton(ns("download_degs"), "Download as CSV",
                                          class = "btn-sm btn-success", style = "float: right;")
                         ),
                         DT::DTOutput(ns("deg_table"), height = "600px")
                ),
                tabPanel("Statistics",
                         card(
                           card_header("DEG Summary Statistics"),
                           tableOutput(ns("deg_stats"))
                         ),
                         card(
                           card_header("Top DEGs"),
                           DT::DTOutput(ns("top_degs_table"), height = "300px")
                         )
                )
              )
            )
          )
        )
      )
    )
  )
}

# Server 部分
DEG_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 状态管理
    analysis_ready <- reactiveVal(FALSE)

    # 反应性加载数据
    expression_data <- reactive({
      req(input$data_count_file)
      df <- readxl::read_xlsx(input$data_count_file$datapath)
      validate(
        need("GeneID" %in% colnames(df), "Error: Expression file must contain 'GeneID' column"),
        need(ncol(df) > 1, "Error: Expression file must contain sample columns")
      )
      return(df)
    })

    # 加载分组信息
    group_data <- reactive({
      req(input$group_file)
      df <- readxl::read_xlsx(input$group_file$datapath)
      validate(
        need("Sample" %in% colnames(df), "Error: Group file must contain 'Sample' column"),
        need("Group" %in% colnames(df), "Error: Group file must contain 'Group' column")
      )
      return(df)
    })

    # 获取分组数据的列名（用于PCA颜色和形状选择）
    group_columns <- reactive({
      req(group_data())
      cols <- colnames(group_data())
      # 排除Sample列
      cols <- cols[cols != "Sample"]
      return(cols)
    })

    # 获取当前分组变量的不同组别
    selected_groups <- reactive({
      req(group_data(), input$pca_colby)
      if (input$pca_colby != "none") {
        groups <- unique(group_data()[[input$pca_colby]])
        return(sort(as.character(groups)))  # 确保是字符型并排序
      }
      return(NULL)
    })

    # 观察分组数据变化，更新PCA设置选项
    observeEvent(group_data(), {
      cols <- group_columns()
      if(length(cols) > 0) {
        # 更新颜色选择
        updateSelectInput(session, "pca_colby",
                          choices = c("None" = "none", cols),
                          selected = "Group")
        # 更新形状选择
        updateSelectInput(session, "pca_shapeby",
                          choices = c("None" = "none", cols),
                          selected = "none")
      }
    })

    # 观察分组列选择变化
    observeEvent(input$pca_colby, {
      if (input$pca_colby != "none" && !is.null(group_data())) {
        # 清除之前可能存在的颜色输入
        removeUI(
          selector = paste0("#", ns("group_colors_title")),
          immediate = TRUE
        )
      }
    })

    # 生成动态颜色选择器
    output$group_colors_ui <- renderUI({
      groups <- selected_groups()

      if (is.null(groups) || input$pca_colby == "none") {
        return(NULL)  # 如果没有选择分组或分组为"none"，不显示颜色选择器
      }

      # 生成一组美观的默认颜色
      default_colors <- c(
        "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
        "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
        "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
        "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA"
      )

      # 为每个组创建颜色选择器
      color_pickers <- lapply(seq_along(groups), function(i) {
        group <- groups[i]
        default_color <- default_colors[(i-1) %% length(default_colors) + 1]

        # 为每个组创建唯一的ID
        group_id <- gsub("[^A-Za-z0-9]", "_", group)

        tagList(
          colourpicker::colourInput(
            ns(paste0("color_", group_id)),
            label = paste("Color for:", group),
            value = default_color
          )
        )
      })

      # 添加一个重置按钮
      reset_button <- actionButton(
        ns("reset_colors"),
        "Reset Colors to Default",
        icon = icon("refresh"),
        class = "btn-sm btn-outline-secondary"
      )

      tagList(
        h5("Customize Group Colors:", id = ns("group_colors_title")),
        br(),
        color_pickers,
        br(),
        reset_button
      )
    })

    # 处理颜色重置按钮
    observeEvent(input$reset_colors, {
      groups <- selected_groups()
      if (!is.null(groups)) {
        default_colors <- c(
          "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
          "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
          "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
          "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA"
        )

        for (i in seq_along(groups)) {
          group <- groups[i]
          default_color <- default_colors[(i-1) %% length(default_colors) + 1]
          group_id <- gsub("[^A-Za-z0-9]", "_", group)
          colourpicker::updateColourInput(
            session,
            paste0("color_", group_id),
            value = default_color
          )
        }
      }
    })

    # 获取用户选择的颜色
    get_group_colors <- reactive({
      groups <- selected_groups()
      if (is.null(groups) || input$pca_colby == "none") {
        return(NULL)
      }

      colors <- character(0)

      for (group in groups) {
        group_id <- gsub("[^A-Za-z0-9]", "_", group)
        color_input <- paste0("color_", group_id)

        if (!is.null(input[[color_input]])) {
          colors <- c(colors, input[[color_input]])
        } else {
          # 如果颜色未设置，使用默认颜色
          default_colors <- c(
            "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
            "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5"
          )
          default_color <- default_colors[(which(groups == group) - 1) %% length(default_colors) + 1]
          colors <- c(colors, default_color)
        }
      }

      names(colors) <- groups
      return(colors)
    })

    # 创建样本信息
    sample_info <- reactive({
      req(group_data())
      col_data <- group_data() %>%
        tibble::column_to_rownames("Sample")
      return(col_data)
    })

    # 运行分析
    observeEvent(input$generate_plot, {
      # 验证数据
      validate(
        need(!is.null(expression_data()), "Please upload expression data"),
        need(!is.null(sample_info()), "Please upload group information"),
        need(nrow(expression_data()) > 0, "Expression data is empty"),
        need(nrow(sample_info()) > 0, "Group information is empty")
      )

      # 检查样本名称是否匹配
      expr_samples <- colnames(expression_data())[-1]  # 排除GeneID列
      group_samples <- rownames(sample_info())

      validate(
        need(all(expr_samples %in% group_samples),
             paste("Error: Sample names in expression data do not match group data.\n",
                   "Expression samples:", paste(expr_samples, collapse = ", "), "\n",
                   "Group samples:", paste(group_samples, collapse = ", ")))
      )

      analysis_ready(TRUE)
    })

    # 进行PCA分析
    pca_result <- reactive({
      req(analysis_ready(), expression_data(), sample_info())

      # 提取表达矩阵
      expr_mat <- expression_data() %>%
        tibble::column_to_rownames("GeneID") %>%
        as.matrix()

      # 确保样本顺序一致
      expr_mat <- expr_mat[, rownames(sample_info()), drop = FALSE]

      # 运行PCA
      pca <- PCAtools::pca(expr_mat, metadata = sample_info(), removeVar = 0.1)
      return(pca)
    })

    # 绘制PCA图
    pca_plot_obj <- reactive({
      req(pca_result())

      # 获取颜色和形状设置
      colby <- input$pca_colby
      shapeby <- input$pca_shapeby
      show_labels <- input$pca_show_labels
      encircle <- input$pca_encircle
      show_ellipse <- input$pca_show_ellipse
      ellipse_alpha <- input$pca_ellipse_alpha
      point_size <- as.numeric(input$pca_pointsize)
      base_color <- input$pca_base_color
      legend_size <- input$pca_legend_size

      # 基础PCA图设置
      pca_args <- list(
        pca_result(),
        x = "PC1",
        y = "PC2",
        legendPosition = "right",
        legendLabSize = legend_size,
        legendIconSize = 6,
        pointSize = point_size,
        title = "PCA Plot",
        subtitle = "Principal Component Analysis"
      )

      # 设置颜色
      if (colby != "none") {
        pca_args$colby <- colby

        # 获取用户定义的颜色
        group_colors <- get_group_colors()
        if (length(group_colors) > 0) {
          pca_args$colkey <- group_colors
        }
      } else {
        pca_args$colby <- NULL
        pca_args$colkey <- base_color
      }

      # 设置形状
      if (shapeby != "none") {
        pca_args$shape <- shapeby
      } else {
        pca_args$shape <- NULL
      }

      # 设置样本标签
      if (show_labels) {
        pca_args$lab <- rownames(pca_result()$metadata)
      } else {
        pca_args$lab <- NULL
      }

      # 设置椭圆
      if (encircle && colby != "none" && show_ellipse) {
        pca_args$encircle <- TRUE
        pca_args$encircleFill <- TRUE
        pca_args$encircleAlpha <- ellipse_alpha
        pca_args$encircleLineSize <- 1
      } else {
        pca_args$encircle <- FALSE
      }

      # 绘制图形
      pca_plot <- do.call(PCAtools::biplot, pca_args)

      return(pca_plot)
    })

    # 准备PCA数据表格 (显示pca_result$rotated)
    pca_rotated_data <- reactive({
      req(pca_result())

      # 获取旋转后的坐标
      rotated_data <- as.data.frame(pca_result()$rotated)
      rotated_data <- rotated_data[, 1:min(10, ncol(rotated_data))]  # 只显示前10个主成分

      # 添加样本名
      rotated_data <- cbind(
        Sample = rownames(rotated_data),
        rotated_data
      )

      # 添加分组信息
      if (!is.null(sample_info())) {
        rotated_data <- cbind(
          rotated_data,
          sample_info()
        )
      }

      return(rotated_data)
    })

    # 进行差异表达分析
    deseq_results <- eventReactive(input$generate_plot, {
      req(expression_data(), sample_info())

      withProgress(message = 'Running DESeq2 analysis...', value = 0.3, {
        # 准备计数矩阵
        count_mat <- expression_data() %>%
          tibble::column_to_rownames("GeneID") %>%
          mutate(across(everything(), ceiling)) %>%
          as.matrix()

        # 确保样本顺序一致
        count_mat <- count_mat[, rownames(sample_info()), drop = FALSE]

        # 创建DESeq2对象
        incProgress(0.2, detail = "Creating DESeq2 object...")
        dds <- DESeqDataSetFromMatrix(
          countData = count_mat,
          colData = sample_info(),
          design = ~ Group
        )

        # 运行DESeq2
        incProgress(0.3, detail = "Running DESeq2...")
        dds <- DESeq(dds)

        # 获取结果
        incProgress(0.2, detail = "Extracting results...")
        res <- results(dds, contrast = c("Group", "B73", "Y12"))

        # 整理结果
        res_tbl <- res %>%
          as.data.frame() %>%
          tibble::rownames_to_column("GeneID") %>%
          mutate(
            regular = case_when(
              padj < 0.05 & log2FoldChange > 1 ~ "up",
              padj < 0.05 & log2FoldChange < -1 ~ "down",
              TRUE ~ "not sig"
            ),
            significant = ifelse(padj < 0.05 & abs(log2FoldChange) > 1, "yes", "no"),
            Regulation = case_when(
              regular == "up" ~ "Up-regulated",
              regular == "down" ~ "Down-regulated",
              TRUE ~ "Not significant"
            )
          ) %>%
          arrange(padj, desc(abs(log2FoldChange)))

        return(res_tbl)
      })
    })

    # 获取DEG统计信息
    deg_stats <- reactive({
      req(deseq_results())

      res_tbl <- deseq_results()

      stats <- list(
        total_genes = nrow(res_tbl),
        up_regulated = sum(res_tbl$regular == "up", na.rm = TRUE),
        down_regulated = sum(res_tbl$regular == "down", na.rm = TRUE),
        significant = sum(res_tbl$regular %in% c("up", "down"), na.rm = TRUE),
        percent_sig = round(sum(res_tbl$regular %in% c("up", "down"), na.rm = TRUE) / nrow(res_tbl) * 100, 2)
      )

      return(stats)
    })

    # 获取top DEGs
    top_degs <- reactive({
      req(deseq_results())

      res_tbl <- deseq_results()

      # 获取显著差异表达的基因
      sig_genes <- res_tbl %>%
        filter(regular %in% c("up", "down")) %>%
        arrange(padj, desc(abs(log2FoldChange))) %>%
        head(20)  # 显示前20个

      return(sig_genes)
    })

    # 绘制火山图
    voc_plot_obj <- reactive({
      req(deseq_results())
      res_tbl <- deseq_results()

      # 计算统计信息用于副标题
      stats <- deg_stats()

      # 创建火山图
      p <- ggplot(res_tbl, aes(x = log2FoldChange, y = -log10(padj))) +
        geom_point(aes(color = regular),
                   size = input$volcano_point_size,
                   alpha = input$volcano_alpha) +
        scale_color_manual(
          values = c(
            "up" = input$color_up,
            "down" = input$color_down,
            "not sig" = input$color_not_sig
          ),
          name = "Expression"
        ) +
        geom_hline(
          yintercept = -log10(0.05),
          linetype = "dashed",
          color = "black",
          alpha = 0.5
        ) +
        geom_vline(
          xintercept = c(-1, 1),
          linetype = "dashed",
          color = "black",
          alpha = 0.5
        ) +
        labs(
          title = "Volcano Plot",
          subtitle = paste(
            "Up-regulated:", stats$up_regulated,
            "| Down-regulated:", stats$down_regulated,
            "| Total significant:", stats$significant,
            paste0("(", stats$percent_sig, "%)")
          ),
          x = "log2(Fold Change)",
          y = "-log10(Adjusted p-value)"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          plot.subtitle = element_text(size = 12, color = "gray50"),
          axis.title = element_text(size = 12),
          legend.position = "right",
          panel.grid = if(input$volcano_show_grid) element_line(color = "gray90") else element_blank(),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5)
        ) +
        coord_cartesian(ylim = c(0, max(-log10(res_tbl$padj[is.finite(-log10(res_tbl$padj))]), na.rm = TRUE) * 1.1))

      return(p)
    })

    # 渲染PCA图
    output$pca_plot <- renderPlot({
      req(pca_plot_obj())
      pca_plot_obj()
    })

    # 渲染火山图
    output$voc_plot <- renderPlot({
      req(voc_plot_obj())
      voc_plot_obj()
    })

    # 渲染PCA数据表格 (显示pca_result$rotated)
    output$pca_data_table <- DT::renderDT({
      req(pca_rotated_data())

      DT::datatable(
        pca_rotated_data(),
        extensions = c('Buttons', 'Scroller'),
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = 550,
          scroller = TRUE
        ),
        rownames = FALSE,
        class = 'display compact'
      )
    })

    # 渲染DEG结果表格 (显示res_tbl)
    output$deg_table <- DT::renderDT({
      req(deseq_results())

      res_tbl <- deseq_results() %>%
        select(GeneID, baseMean, log2FoldChange, lfcSE, stat, pvalue, padj, Regulation) %>%
        mutate(
          across(where(is.numeric), ~ round(., 4)),
          padj = format(padj, scientific = TRUE, digits = 3)
        )

      DT::datatable(
        res_tbl,
        extensions = c('Buttons', 'Scroller'),
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = 550,
          scroller = TRUE
        ),
        rownames = FALSE,
        class = 'display compact'
      ) %>%
        DT::formatStyle(
          'Regulation',
          backgroundColor = DT::styleEqual(
            c('Up-regulated', 'Down-regulated', 'Not significant'),
            c('#FFCCCC', '#CCE5FF', '#F2F2F2')
          )
        )
    })

    # 渲染DEG统计表格
    output$deg_stats <- renderTable({
      req(deg_stats())

      stats <- deg_stats()

      data.frame(
        Statistic = c("Total Genes", "Up-regulated", "Down-regulated",
                      "Total Significant", "Percentage Significant"),
        Value = c(
          stats$total_genes,
          paste(stats$up_regulated, "genes"),
          paste(stats$down_regulated, "genes"),
          paste(stats$significant, "genes"),
          paste(stats$percent_sig, "%")
        )
      )
    }, align = 'lr')

    # 渲染Top DEGs表格
    output$top_degs_table <- DT::renderDT({
      req(top_degs())

      top_genes <- top_degs() %>%
        select(GeneID, log2FoldChange, padj, Regulation) %>%
        mutate(
          log2FoldChange = round(log2FoldChange, 3),
          padj = format(padj, scientific = TRUE, digits = 3)
        )

      DT::datatable(
        top_genes,
        extensions = c('Buttons', 'Scroller'),
        options = list(
          pageLength = 5,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'display compact'
      ) %>%
        DT::formatStyle(
          'Regulation',
          backgroundColor = DT::styleEqual(
            c('Up-regulated', 'Down-regulated', 'Not significant'),
            c('#FFCCCC', '#CCE5FF', '#F2F2F2')
          )
        )
    })

    # 下载PCA图
    output$download_pca <- downloadHandler(
      filename = function() {
        paste("PCA_plot_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        req(pca_plot_obj())
        pdf(file, width = input$download_width_pca, height = input$download_height_pca)
        print(pca_plot_obj())
        dev.off()
      }
    )

    # 下载火山图
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste("volcano_plot_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        req(voc_plot_obj())
        pdf(file, width = input$download_width_voc, height = input$download_height_voc)
        print(voc_plot_obj())
        dev.off()
      }
    )

    # 下载PCA数据 (pca_result$rotated)
    output$download_pca_table <- downloadHandler(
      filename = function() {
        paste("pca_rotated_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(pca_rotated_data())
        write.csv(pca_rotated_data(), file, row.names = FALSE)
      }
    )

    # 下载PCA数据 (从侧边栏按钮)
    output$download_pca_data <- downloadHandler(
      filename = function() {
        paste("pca_rotated_data_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(pca_rotated_data())
        write.csv(pca_rotated_data(), file, row.names = FALSE)
      }
    )

    # 下载DEG数据 (res_tbl) - 从侧边栏按钮
    output$download_deg_data <- downloadHandler(
      filename = function() {
        paste("deg_analysis_results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(deseq_results())
        write.csv(deseq_results(), file, row.names = FALSE)
      }
    )

    # 下载DEG数据 (res_tbl) - 从表格内按钮
    output$download_degs <- downloadHandler(
      filename = function() {
        paste("deg_results_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(deseq_results())
        write.csv(deseq_results(), file, row.names = FALSE)
      }
    )
  })
}
