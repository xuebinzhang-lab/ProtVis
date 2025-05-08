#' Multivariate Summary UI Module
#' @description Multivariate Summary UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title mv_summary_ui
#' @name mv_summary_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
mv_summary_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Multivariate Summary',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(ns("file"), "Upload CSV file", accept = ".csv"),
          fileInput(
            inputId = ns('GroupInfo'),
            label = 'Group information',
            multiple = FALSE,
            accept = '.csv'
          )
        )
      ),
      # 2x2 布局
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # boxplot -----------------------------------------------------------------
            title = "Boxplot",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(ns("boxplot_color"),
                                          "Color:",
                                          value = "black"),
                actionButton(ns("boxplot"), "Run")
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("boxplot"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                numericInput(ns("boxplot_height"),
                             label = "Height:",
                             value = 7,
                             step = 0.1),
                numericInput(ns("boxplot_width"),label = "Width:",
                             value = 7, step = 1),
                downloadButton(ns("download_boxplot"), "Download")
              )
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs", # This allows tab navigation
                tabPanel(
                  title = "Figure before normalization",
                  plotOutput(ns("boxplotbeforeshow"))
                ),
                tabPanel(
                  title = "Figure after normalization",
                  plotOutput(ns("boxplotaftershow"))
                )
              )
            )
          ),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # PCA ---------------------------------------------------------------------
            title = "PCA",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Run',
                actionButton(ns("run_btn_PCA"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                downloadButton(ns("download_PCA_table"), label = "Output Table", icon = icon("download"))
              )
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs", # This allows tab navigation
                tabPanel(
                  title = "Figure",
                  plotOutput(ns("PCAplotshow"))
                ),
                tabPanel(
                  title = "Table",
                  DT::DTOutput(ns("PCA_dataTable"))
                )
              )
            )
          ),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # correlation -------------------------------------------------------------
            title = "Correlation",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput("method", "Correlation Method",
                            choices = c("pearson", "kendall", "spearman"), selected = "pearson")
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("calculate"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                numericInput(ns("correlationPlotHeight"), "Height:",
                             value = 8, min = 1, max = 100, step = 1),
                numericInput(ns("correlationPlotWidth"), "Width:",
                             value = 8, min = 1, max = 100, step = 1),
                downloadButton(ns("Figure_correlation_download"), label = "Download Figure",
                               icon = icon("download")),
                downloadButton(ns("table_correlation_download"), label = "Download Table",
                               icon = icon("download"))
              )
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs", # This allows tab navigation
                tabPanel(
                  title = "Figure",
                  plotOutput(ns("correlationPlot"))
                ),
                tabPanel(
                  title = "Table",
                  DT::DTOutput(ns("correlation_dataTable"))
                )
              )
            )
          ),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # heatmap -----------------------------------------------------------------
            title = "Heatmap",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                radioButtons(inputId = ns("Logical_value2"),
                             label = "Logical value",
                             choices = c("TRUE", "FALSE"),
                             selected = "TRUE")
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("plotheatmap"), "Run")
              )
            ),
            mainPanel(
              plotOutput(ns("correlationPlotshow"))
            )
          )
        )
      )
    )
  )
}

#' @title mv_summary_server
#' @name mv_summary_server
#' @param id description
#' @param input description
#' @param output description
#' @param session description
#' @import utils
#' @import shiny
#' @import tidyr
#' @import ggplot2
#' @import grDevices
#' @import dplyr
#' @import tidyverse
#' @import stats
#' @export
#'
utils::globalVariables(c("value","Sample","Group"))

mv_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # boxplot -----------------------------------------------------------------
    # 上传表达量
    data <- reactive({
      req(input$file)
      read.csv(input$file$datapath, row.names = 1)
    })
    # 上传分组信息
    group <- reactive({
      req(input$GroupInfo)
      read.csv(input$GroupInfo$datapath)
    })
    # 绘制箱线图1
    observeEvent(input$boxplot, {
      # 绘制相关性图
      output$boxplotbeforeshow <- renderPlot({
        req(data())
        req(group())
        # 数据整理
        boxplot <- data() %>%
          pivot_longer(cols = everything(),
                       names_to = "Sample",
                       values_to = "value") %>%
          dplyr::mutate(value = log10(value)) %>%
          dplyr::left_join(group(),by = "Sample")
        # 画图
        ggplot(boxplot, aes(x = Sample, y = value, fill = Group)) +
          geom_boxplot(color = input$boxplot_color) +
          labs(x = "", y = "Log10(Protein Abundance)") +
          theme_bw() +
          theme(
            panel.grid.major = element_blank(),         # 去除主网格线
            panel.grid.minor = element_blank(),         # 去除次网格线
            plot.title = element_text(color = "black"), # 标题字体颜色
            axis.title = element_text(color = "black"), # 坐标轴标题颜色
            axis.text = element_text(color = "black"),  # 坐标轴刻度文字颜色
            legend.text = element_text(color = "black"),# 图例文字颜色
            legend.title = element_text(color = "black")# 图例标题颜色
          )
      })
    })
    # 绘制箱线图2
    observeEvent(input$boxplot, {
      # 绘制相关性图
      output$boxplotaftershow <- renderPlot({
        req(data())
        req(group())
        # 数据整理
        boxplot <- data() %>%
          as.matrix() %>%
          preprocessCore::normalize.quantiles() %>%
          as.data.frame() %>%
          setNames(colnames(data())) %>%
          pivot_longer(cols = everything(),
                       names_to = "Sample",
                       values_to = "value") %>%
          dplyr::mutate(value = log10(value)) %>%
          dplyr::left_join(group(),by = "Sample")
        # 画图
        ggplot(boxplot, aes(x = Sample, y = value, fill = Group)) +
          geom_boxplot(color = input$boxplot_color) +
          labs(x = "", y = "Log10(Protein Abundance)") +
          theme_bw() +
          theme(
            panel.grid.major = element_blank(),         # 去除主网格线
            panel.grid.minor = element_blank(),         # 去除次网格线
            plot.title = element_text(color = "black"), # 标题字体颜色
            axis.title = element_text(color = "black"), # 坐标轴标题颜色
            axis.text = element_text(color = "black"),  # 坐标轴刻度文字颜色
            legend.text = element_text(color = "black"),# 图例文字颜色
            legend.title = element_text(color = "black")# 图例标题颜色
          )
      })
    })
    output$download_boxplot <- downloadHandler(
      filename = function() {
        paste("boxplot_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # 设置文件输出为PDF格式
        pdf(file, width = input$boxplot_width, height = input$boxplot_height)
        req(data())  # 确保数据已加载
        req(group())  # 确保分组信息已加载
        # before
        boxplot <- data() %>%
          pivot_longer(cols = everything(),
                       names_to = "Sample",
                       values_to = "value") %>%
          dplyr::mutate(value = log10(value)) %>%
          dplyr::left_join(group(),by = "Sample")
        # 画图
        p1 <- ggplot(boxplot, aes(x = Sample, y = value, fill = Group)) +
          geom_boxplot(color = input$boxplot_color) +
          labs(x = "", y = "Log10(Protein Abundance)") +
          theme_bw() +
          theme(
            panel.grid.major = element_blank(),         # 去除主网格线
            panel.grid.minor = element_blank(),         # 去除次网格线
            plot.title = element_text(color = "black"), # 标题字体颜色
            axis.title = element_text(color = "black"), # 坐标轴标题颜色
            axis.text = element_text(color = "black"),  # 坐标轴刻度文字颜色
            legend.text = element_text(color = "black"),# 图例文字颜色
            legend.title = element_text(color = "black")# 图例标题颜色
          )
        # after
        boxplot <- data() %>%
          as.matrix() %>%
          preprocessCore::normalize.quantiles() %>%
          as.data.frame() %>%
          setNames(colnames(data())) %>%
          pivot_longer(cols = everything(), names_to = "Sample", values_to = "value") %>%
          dplyr::mutate(value = log10(value)) %>%
          dplyr::left_join(group(), by = "Sample")
        # 绘制箱线图
        p2 <- ggplot(boxplot, aes(x = Sample, y = value, fill = Group)) +
          geom_boxplot(color = input$boxplot_color) +
          labs(x = "", y = "Log10(Protein Abundance)") +
          theme_bw() +
          theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(color = "black"),
            axis.title = element_text(color = "black"),
            axis.text = element_text(color = "black"),
            legend.text = element_text(color = "black"),
            legend.title = element_text(color = "black")
          )
        print(p1)  # 打印图形到pdf
        print(p2)
        # 关闭设备，保存PDF文件
        dev.off()  # 确保关闭图形设备
      }
    )

    # PCA ---------------------------------------------------------------------
    observeEvent(input$run_btn_PCA, {
      # 绘制相关性图
      output$PCAplotshow <- renderPlot({
        req(data())
        req(group())
        class <- group() %>%
          tibble::column_to_rownames("Sample")
        expr <- log10(data())
        pca_data <- t(expr)
        pca <- PCAtools::pca(expr, metadata = class)
        PCAtools::biplot(pca,
               x = "PC1",
               y = "PC2",
               colby = "Group",
               legendPosition = "right",
               lab = NULL,
               encircle = TRUE,
               encircleFill = TRUE
        )
      })
      # 显示上传的表格
      output$PCA_dataTable <- DT::renderDT({
        req(data())
        req(group())
        class <- group() %>%
          tibble::column_to_rownames("Sample")
        expr <- log10(data())
        pca_data <- t(expr)
        pca <- PCAtools::pca(expr, metadata = class)
        DT::datatable(data.frame(pca[["rotated"]]))
      })
    })
    # 下载表格
    output$download_PCA_table <- downloadHandler(
      filename = function() {
        paste("PCA_rotated_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(data())
        req(group())
        class <- group() %>%
          tibble::column_to_rownames("Sample")
        expr <- log10(data())
        pca_data <- t(expr)
        pca <- PCAtools::pca(expr, metadata = class)

        # 保存旋转矩阵为CSV文件
        write.csv(pca[["rotated"]], file, row.names = TRUE)
      }
    )

    # correlation ---------------------------------------------------------------------
    # 计算并显示相关性矩阵
    observeEvent(input$calculate, {
      req(data())
      method <- input$method
      correlation_matrix <- cor(data(), method = method, use = "complete.obs")

      # 显示上传的表格
      output$correlation_dataTable <- DT::renderDT({
        DT::datatable(correlation_matrix)
      })

      # 绘制相关性图
      output$correlationPlot <- renderPlot({
        req(input$calculate)  # 只有点击"Calculate"后才会渲染图形
        corrplot::corrplot(corr=correlation_matrix,order = "AOE",type="upper",tl.pos = "d")
        corrplot::corrplot(corr = correlation_matrix,add=TRUE, type="lower", method="number",
                 order="AOE",diag=FALSE,tl.pos="n", cl.pos="n",number.cex = 0.7)
      })
    })

    # 下载相关性图（PDF）
    output$Figure_correlation_download <- downloadHandler(
      filename = function() {
        paste("correlation_plot", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        req(input$calculate)  # 确保图形只有在计算后下载
        # 保存图形为PDF文件
        pdf(file, width = input$correlationPlotWidth,
            height = input$correlationPlotHeight)  # 设置PDF输出的宽度和高度
        # corrplot(cor(data(), method = input$method, use = "complete.obs"),
        #          method = "circle",type = "full", tl.col = "black",
        #          tl.srt = 45, mar = c(0, 0, 1, 0))
        corrplot::corrplot(cor(data(),method = input$method),order = "AOE",type="upper",tl.pos = "d")
        corrplot::corrplot(cor(data(),method = input$method),add=TRUE, type="lower", method="number",
                 order="AOE",diag=FALSE,tl.pos="n", cl.pos="n",number.cex = 0.7)
        dev.off()
      }
    )

    # 下载相关性表格（CSV）
    output$table_correlation_download <- downloadHandler(
      filename = function() {
        paste("correlation_table", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(input$calculate)  # 确保只有在计算后下载表格
        # 将相关性矩阵写入CSV文件
        write.csv(cor(data(), method = input$method, use = "complete.obs"), file, row.names = TRUE)
      }
    )
    # heatmap ---------------------------------------------------------------------
    # 绘制热图
    observeEvent(input$plotheatmap, {
      # 绘制相关性图
      output$correlationPlotshow <- renderPlot({
        req(data())
        data_normalized <- data() %>%
          head(10) %>%
          as.matrix() %>%
          t() %>%
          scale() %>%
          t()
        ComplexHeatmap::Heatmap(
          data_normalized,
          col = circlize::colorRamp2(c(-2,0,2),c("green","white","red"))
          )
      })
    })


  })
}



