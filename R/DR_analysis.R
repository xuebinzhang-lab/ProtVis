#' Dimension reduction analysis UI Module
#' @description Dimension reduction analysis UI Module
#' @param id A unique identifier for the Shiny namespace, Dimension reduction analysis.
#' @title DR_analysis_ui
#' @name DR_analysis_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
DR_analysis_ui <- function(id) {
  ns <- NS(id)
  # UI 结构
  nav_panel(
    title = 'Dimensionality Reduction Analysis',
    icon = bs_icon("alexa"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('file'),
            label = 'Expression matrix',
            multiple = FALSE,
            accept = '.csv'
          ),
          fileInput(
            inputId = ns('GroupInfo'),
            label = 'Group information',
            multiple = FALSE,
            accept = '.csv'
          )
        ),
        accordion_panel(
          title = "Method",
          icon = bs_icon("view-stacked"),
          open = TRUE,
          selectInput(ns("dropdown"), "Choose a Dimensionality Reduction Method:",
                      choices = c("PCA", "PCoA", "tSNE", "UMAP", "NMDS"))
        )
      ),
      # PCA ---------------------------------------------------------------------
      # 使用 conditionalPanel，仅在选择 "PCA" 时显示右侧面板
      conditionalPanel(
        condition = "input.dropdown == 'PCA'",
        ns = ns,  # 确保为该模块设置命名空间
        page_fluid(
          layout_column_wrap(
            width = 1,
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "PCA",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  uiOutput(ns("colorSelectors_PCA"))
                ),
                accordion_panel(
                  title = 'Run',
                  actionButton(ns("run_btn_PCA"), "Run")
                ),
                accordion_panel(
                  title = 'Download',
                  icon = bs_icon('download'),
                  downloadButton(ns("download_PCA_Figure"), label = "Figure", icon = icon("download")),
                  br(),
                  downloadButton(ns("download_PCA_table"), label = "Table", icon = icon("download"))
                )
              ),
              mainPanel(
                tabsetPanel(
                  type = "tabs", # 允许标签页切换
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
            )
          )
        )
      ),
      # PCoA ---------------------------------------------------------------------
      # 使用 conditionalPanel，仅在选择 "PCoA" 时显示右侧面板
      conditionalPanel(
        condition = "input.dropdown == 'PCoA'",
        ns = ns,  # 确保为该模块设置命名空间
        page_fluid(
          layout_column_wrap(
            width = 1,
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "PCoA",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  uiOutput(ns("colorSelectors_PCoA"))
                ),
                accordion_panel(
                  title = 'Run',
                  actionButton(ns("run_btn_PCoA"), "Run")
                ),
                accordion_panel(
                  title = 'Download',
                  icon = bs_icon('download'),
                  downloadButton(ns("download_PCoA_Figure"), label = "Figure", icon = icon("download")),
                  br(),
                  downloadButton(ns("download_PCoA_table"), label = "Table", icon = icon("download"))
                )
              ),
              mainPanel(
                tabsetPanel(
                  type = "tabs", # 允许标签页切换
                  tabPanel(
                    title = "Figure",
                    plotOutput(ns("PCoAplotshow"))
                  ),
                  tabPanel(
                    title = "Table",
                    DT::DTOutput(ns("PCoA_dataTable"))
                  )
                )
              )
            )
          )
        )
      ),
      # tSNE ---------------------------------------------------------------------
      # 使用 conditionalPanel，仅在选择 "tSNE" 时显示右侧面板
      conditionalPanel(
        condition = "input.dropdown == 'tSNE'",
        ns = ns,  # 确保为该模块设置命名空间
        page_fluid(
          layout_column_wrap(
            width = 1,
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "tSNE",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  uiOutput(ns("colorSelectors_tSNE"))
                ),
                accordion_panel(
                  title = 'Run',
                  actionButton(ns("run_btn_tSNE"), "Run")
                ),
                accordion_panel(
                  title = 'Download',
                  icon = bs_icon('download'),
                  downloadButton(ns("download_tSNE_Figure"), label = "Figure", icon = icon("download")),
                  br(),
                  downloadButton(ns("download_tSNE_table"), label = "Table", icon = icon("download"))
                )
              ),
              mainPanel(
                tabsetPanel(
                  type = "tabs", # 允许标签页切换
                  tabPanel(
                    title = "Figure",
                    plotOutput(ns("tSNEplotshow"))
                  ),
                  tabPanel(
                    title = "Table",
                    DT::DTOutput(ns("tSNE_dataTable"))
                  )
                )
              )
            )
          )
        )
      ),
    # UMAP --------------------------------------------------------------------
    # 使用 conditionalPanel，仅在选择 "UMAP" 时显示右侧面板
    conditionalPanel(
      condition = "input.dropdown == 'UMAP'",
      ns = ns,  # 确保为该模块设置命名空间
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "UMAP",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("colorSelectors_UMAP"))
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run_btn_UMAP"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                downloadButton(ns("download_UMAP_Figure"), label = "Figure", icon = icon("download")),
                br(),
                downloadButton(ns("download_UMAP_table"), label = "Table", icon = icon("download"))
              )
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs", # 允许标签页切换
                tabPanel(
                  title = "Figure",
                  plotOutput(ns("UMAPplotshow"))
                ),
                tabPanel(
                  title = "Table",
                  DT::DTOutput(ns("UMAP_dataTable"))
                )
              )
            )
          )
        )
      )
    ),
    # NMDS --------------------------------------------------------------------
    # 使用 conditionalPanel，仅在选择 "NMDS" 时显示右侧面板
    conditionalPanel(
      condition = "input.dropdown == 'NMDS'",
      ns = ns,  # 确保为该模块设置命名空间
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "NMDS",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("colorSelectors_NMDS"))
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run_btn_NMDS"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                downloadButton(ns("download_NMDS_Figure"), label = "Figure", icon = icon("download")),
                br(),
                downloadButton(ns("download_NMDS_table"), label = "Table", icon = icon("download"))
              )
            ),
            mainPanel(
              tabsetPanel(
                type = "tabs", # 允许标签页切换
                tabPanel(
                  title = "Figure",
                  plotOutput(ns("NMDSplotshow"))
                ),
                tabPanel(
                  title = "Table",
                  DT::DTOutput(ns("NMDS_dataTable"))
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

#' Dimension reduction analysis Server Module
#' @description Server logic for Dimension reduction analysis
#' @param id The module ID. This is used to namespace the inputs and outputs in the UI and server components.
#' @param input Standard Shiny server arguments: a list of input values from the UI.
#' @param output Standard Shiny server arguments: a list of outputs to send back to the UI.
#' @param session Standard Shiny server arguments: the session object that maintains the state of the app.
#' @import shiny
#' @import utils
#' @import ggplot2
#' @name DR_analysis_server
#' @title DR_analysis_server
#' @export
#'
utils::globalVariables(c("V1", "V2", "TSNE1","TSNE2", "UMAP1","UMAP2", "NMDS1","NMDS2"))
DR_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 上传表达量数据
    data <- reactive({
      req(input$file)
      read.csv(input$file$datapath, row.names = 1)
    })

    # 上传分组信息
    group <- reactive({
      req(input$GroupInfo)
      read.csv(input$GroupInfo$datapath)
    })

    # 动态生成PCA的颜色选择器
    observeEvent(input$GroupInfo, {
      req(input$GroupInfo)
      group_data <- group()

      # 确保 Group 列存在
      if (!"Group" %in% colnames(group_data)) {
        stop("Group column is missing in the group information.")
      }

      output$colorSelectors_PCA <- renderUI({
        base::lapply(unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_PCA_", g)),
            label = paste("Select Color for", g),
            value = sample(colors(), 1)  # 默认颜色
          )
        })
      })
    })

    # 动态生成PCoA的颜色选择器
    observeEvent(input$GroupInfo, {
      req(input$GroupInfo)
      group_data <- group()

      # 确保 Group 列存在
      if (!"Group" %in% colnames(group_data)) {
        stop("Group column is missing in the group information.")
      }

      output$colorSelectors_PCoA <- renderUI({
        base::lapply(unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_PCoA_", g)),
            label = paste("Select Color for", g),
            value = sample(colors(), 1)  # 默认颜色
          )
        })
      })

      output$colorSelectors_tSNE <- renderUI({
        base::lapply(unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_tSNE_", g)),
            label = paste("Select Color for", g),
            value = sample(colors(), 1)  # 默认颜色
          )
        })
      })

      output$colorSelectors_UMAP <- renderUI({
        base::lapply(unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_UMAP_", g)),
            label = paste("Select Color for", g),
            value = sample(colors(), 1)  # 默认颜色
          )
        })
      })

      output$colorSelectors_NMDS <- renderUI({
        base::lapply(unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_NMDS_", g)),
            label = paste("Select Color for", g),
            value = sample(colors(), 1)  # 默认颜色
          )
        })
      })

    })
    # PCA ---------------------------------------------------------------------
    # PCA 绘图
    observeEvent(input$run_btn_PCA, {
      req(input$dropdown == "PCA")
      # 获取分组信息
      group_data <- group()
      # 获取用户选择的颜色
      color_map_PCA <- base::sapply(unique(group_data$Group), function(g) {
        color_input_id <- paste0("color_PCA_", g)
        input[[color_input_id]]  # 获取颜色
      }, USE.NAMES = TRUE)

      # 绘制PCA图
      output$PCAplotshow <- renderPlot({
        req(data())
        req(group())

        class <- group_data %>%
          tibble::column_to_rownames("Sample")  # 确保分组信息与表达数据对齐
        expr <- log10(data())  # 对数据进行 log10 转换
        pca_data <- t(expr)  # 转置数据以适应PCA函数
        pca <- PCAtools::pca(expr, metadata = class)
        PCAtools::biplot(pca,
               x = "PC1",
               y = "PC2",
               colby = "Group",  # 基于分组进行颜色编码
               colkey = color_map_PCA,  # 使用动态生成的颜色映射
               legendPosition = "right",
               lab = NULL,
               encircle = TRUE,
               encircleFill = TRUE
        )
      })
    })
    # PCA显示上传的表格
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
    # 添加下载PCA图像为PDF的功能
    output$download_PCA_Figure <- downloadHandler(
      filename = function() {
        paste("PCA_plot_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # 获取分组信息
        group_data <- group()
        # 获取用户选择的颜色
        color_map_PCA <- sapply(unique(group_data$Group), function(g) {
          color_input_id <- paste0("color_PCA_", g)
          input[[color_input_id]]  # 获取颜色
        }, USE.NAMES = TRUE)
        class <- group_data %>%
          tibble::column_to_rownames("Sample")  # 确保分组信息与表达数据对齐
        expr <- log10(data())  # 对数据进行 log10 转换
        pca_data <- t(expr)  # 转置数据以适应PCA函数
        pca <- PCAtools::pca(expr, metadata = class)
        pca_plot <- PCAtools::biplot(pca,
               x = "PC1",
               y = "PC2",
               colby = "Group",  # 基于分组进行颜色编码
               colkey = color_map_PCA,  # 使用动态生成的颜色映射
               legendPosition = "right",
               lab = NULL,
               encircle = TRUE,
               encircleFill = TRUE
        )
        # 保存图像为PDF文件
        ggsave(file, plot = pca_plot, device = "pdf", width = 8, height = 6)
      }
    )
    # PCoA --------------------------------------------------------------------
    # PCoA 绘图
    observeEvent(input$run_btn_PCoA, {
      req(input$dropdown == "PCoA")

      # 获取分组信息
      group_data <- group()

      # 获取用户选择的颜色
      color_map_PCoA <- base::sapply(unique(group_data$Group), function(g) {
        color_input_id <- paste0("color_PCoA_", g)
        input[[color_input_id]]  # 获取颜色
      }, USE.NAMES = TRUE)

      # 绘制PCoA图
      output$PCoAplotshow <- renderPlot({
        req(data())
        req(group())

        dist = vegan::vegdist(t(data()), method = "bray", diag = TRUE, upper = TRUE)
        pcoa = stats::cmdscale(dist, eig = TRUE)
        eig = summary(vegan::eigenvals(pcoa))
        axis = paste0("PCoA", 1:ncol(eig))
        eig = data.frame(Axis = axis, t(eig)[, -3])
        pco1 <- round(eig[1, 3] * 100, 2)  # 第一主坐标解释率
        pco2 <- round(eig[2, 3] * 100, 2)  # 第二主坐标解释率
        xlab <- paste0("PCoA1 (", pco1, "%)")
        ylab <- paste0("PCoA2 (", pco2, "%)")
        pcoa_points <- as.data.frame(pcoa$points) %>%
          tibble::rownames_to_column("index") %>%
          dplyr::left_join(group() %>% dplyr::rename(index = Sample), by = "index") %>%
          tibble::column_to_rownames("index")
        ggplot(pcoa_points, aes(V1, V2)) +
          geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
          stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
          scale_color_manual(values = color_map_PCoA) +  # 使用动态颜色映射
          scale_fill_manual(values = color_map_PCoA)+
          labs(x = xlab, y = ylab)+
          theme_bw()+
          ggplot2::theme(
            plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
            panel.border = element_rect(colour = "black", size = 2),
            axis.ticks = element_line(color = "black", size = 2),
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 16,colour = "black"),
            axis.title = element_text(size = 16,colour = "black"),
            panel.grid.major = element_line(color = "#EBEBEB", size = 0.5),  # 去除主网格线
            panel.grid.minor = element_line(color = "#EBEBEB", size = 0.2)   # 去除次网格线
          )
      })
      # PCoA显示上传的表格
      output$PCoA_dataTable <- DT::renderDT({
        req(data())
        req(group())
        dist = vegan::vegdist(t(data()), method = "bray", diag = TRUE, upper = TRUE)
        pcoa = cmdscale(dist, eig = TRUE)
        DT::datatable(data.frame(pcoa$points) %>% setNames(c("PCoA1","PCoA2")))
      })
      # PCoA下载表格
      output$download_PCoA_table <- downloadHandler(
        filename = function() {
          paste("PCoA_result_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          dist = vegan::vegdist(t(data()), method = "bray", diag = TRUE, upper = TRUE)
          pcoa = cmdscale(dist, eig = TRUE)
          DT::datatable(data.frame(pcoa$points) %>% setNames(c("PCoA1","PCoA2")))
          write.csv(data.frame(pcoa$points) %>% setNames(c("PCoA1","PCoA2")),
                    file, row.names = TRUE)
        }
      )
      # 添加下载PCoA图像为PDF的功能
      output$download_PCoA_Figure <- downloadHandler(
        filename = function() {
          paste("PCoA_plot_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          dist = vegan::vegdist(t(data()), method = "bray", diag = TRUE, upper = TRUE)
          pcoa = cmdscale(dist, eig = TRUE)
          eig = summary(vegan::eigenvals(pcoa))
          axis = paste0("PCoA", 1:ncol(eig))
          eig = data.frame(Axis = axis, t(eig)[, -3])
          pco1 <- round(eig[1, 3] * 100, 2)  # 第一主坐标解释率
          pco2 <- round(eig[2, 3] * 100, 2)  # 第二主坐标解释率
          xlab <- paste0("PCoA1 (", pco1, "%)")
          ylab <- paste0("PCoA2 (", pco2, "%)")
          pcoa_points <- as.data.frame(pcoa$points) %>%
            tibble::rownames_to_column("index") %>%
            left_join(group() %>% dplyr::rename(index = Sample), by = "index") %>%
            tibble::column_to_rownames("index")
          pcoa_plot <- ggplot(pcoa_points, aes(V1, V2)) +
            geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
            stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
            scale_color_manual(values = color_map_PCoA) +  # 使用动态颜色映射
            scale_fill_manual(values = color_map_PCoA)+
            labs(x = xlab, y = ylab)+
            theme_bw()+
            ggplot2::theme(
              plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
              panel.border = element_rect(colour = "black", size = 2),
              axis.ticks = element_line(color = "black", size = 2),
              legend.text = element_text(size = 16),
              axis.text = element_text(size = 16,colour = "black"),
              axis.title = element_text(size = 16,colour = "black"),
              panel.grid.major = element_line(color = "#EBEBEB", size = 0.5),  # 去除主网格线
              panel.grid.minor = element_line(color = "#EBEBEB", size = 0.2)   # 去除次网格线
            )
          # 保存图像为PDF文件
          ggsave(file, plot = pcoa_plot, device = "pdf", width = 8, height = 6)
        }
      )
    })
    # tSNE --------------------------------------------------------------------
    observeEvent(input$run_btn_tSNE, {
      req(input$dropdown == "tSNE")

      # 获取分组信息
      group_data <- group()
      # 获取用户选择的颜色
      color_map_tSNE <- sapply(unique(group_data$Group), function(g) {
        color_input_id <- paste0("color_tSNE_", g)
        input[[color_input_id]]  # 获取颜色
      }, USE.NAMES = TRUE)
      # print(color_map_tSNE)

      # tSNE分析
      output$tSNEplotshow <- renderPlot({
        req(data())
        req(group())
        data_t <- data() %>%
          t() %>%
          as.matrix()
        # print(data_t)
        set.seed(123)
        tsne_out <- Rtsne::Rtsne(
          data_t,
          dims = 2,
          pca = FALSE,
          perplexity = (nrow(data_t) - 1)/3,
          theta = 0.0,
          max_iter = 1000
        )
        tsne_plot_data <- data.frame(tsne_out$Y) %>%
          setNames(c("TSNE1","TSNE2")) %>%
          mutate(Group = group()$Group)
        # print(tsne_plot_data)
        tsne_plot <- ggplot(tsne_plot_data, aes(TSNE1, TSNE2)) +
          geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
          stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
          scale_color_manual(values = color_map_tSNE) +  # 使用动态颜色映射
          scale_fill_manual(values = color_map_tSNE)+
          # labs(x = xlab, y = ylab)+
          theme_bw()+
          ggplot2::theme(
            plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
            panel.border = element_rect(colour = "black", size = 2),
            axis.ticks = element_line(color = "black", linewidth = 2),
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 16,colour = "black"),
            axis.title = element_text(size = 16,colour = "black"),
            panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),  # 去除主网格线
            panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.2)   # 去除次网格线
          )
        tsne_plot

        })
      # tSNE显示上传的表格
      output$tSNE_dataTable <- DT::renderDT({
        req(data())
        req(group())
        data_t <- data() %>%
          t() %>%
          as.matrix()
        # print(data_t)
        set.seed(123)
        tsne_out <- Rtsne::Rtsne(
          data_t,
          dims = 2,
          pca = FALSE,
          perplexity = (nrow(data_t) - 1)/3,
          theta = 0.0,
          max_iter = 1000
        )
        tsne_plot_data <- data.frame(tsne_out$Y) %>%
          setNames(c("TSNE1","TSNE2")) %>%
          mutate(Group = group()$Group)
        DT::datatable(tsne_plot_data)
      })
      # tSNE下载表格
      output$download_tSNE_table <- downloadHandler(
        filename = function() {
          paste("tSNE_result_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          data_t <- data() %>%
            t() %>%
            as.matrix()
          # print(data_t)
          set.seed(123)
          tsne_out <- Rtsne::Rtsne(
            data_t,
            dims = 2,
            pca = FALSE,
            perplexity = (nrow(data_t) - 1)/3,
            theta = 0.0,
            max_iter = 1000
          )
          tsne_plot_data <- data.frame(tsne_out$Y) %>%
            setNames(c("TSNE1","TSNE2")) %>%
            mutate(Group = group()$Group)
          write.csv(tsne_plot_data,
                    file, row.names = TRUE)
        }
      )
      # 添加下载tSNE图像为PDF的功能
      output$download_tSNE_Figure <- downloadHandler(
        filename = function() {
          paste("tSNE_plot_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          data_t <- data() %>%
            t() %>%
            as.matrix()
          # print(data_t)
          set.seed(123)
          tsne_out <- Rtsne::Rtsne(
            data_t,
            dims = 2,
            pca = FALSE,
            perplexity = (nrow(data_t) - 1)/3,
            theta = 0.0,
            max_iter = 1000
          )
          tsne_plot_data <- data.frame(tsne_out$Y) %>%
            setNames(c("TSNE1","TSNE2")) %>%
            mutate(Group = group()$Group)
          # print(tsne_plot_data)
          tsne_plot <- ggplot(tsne_plot_data, aes(TSNE1, TSNE2)) +
            geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
            stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
            scale_color_manual(values = color_map_tSNE) +  # 使用动态颜色映射
            scale_fill_manual(values = color_map_tSNE)+
            # labs(x = xlab, y = ylab)+
            theme_bw()+
            ggplot2::theme(
              plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
              panel.border = element_rect(colour = "black", size = 2),
              axis.ticks = element_line(color = "black", linewidth = 2),
              legend.text = element_text(size = 16),
              axis.text = element_text(size = 16,colour = "black"),
              axis.title = element_text(size = 16,colour = "black"),
              panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),  # 去除主网格线
              panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.2)   # 去除次网格线
            )
          # 保存图像为PDF文件
          ggsave(file, plot = tsne_plot, device = "pdf", width = 8, height = 6)
        }
      )
      })
    # UMAP --------------------------------------------------------------------
    observeEvent(input$run_btn_UMAP, {
      req(input$dropdown == "UMAP")
      # 获取分组信息
      group_data <- group()
      # 获取用户选择的颜色
      color_map_UMAP <- sapply(unique(group_data$Group), function(g) {
        color_input_id <- paste0("color_UMAP_", g)
        input[[color_input_id]]  # 获取颜色
      }, USE.NAMES = TRUE)
      # UMAP分析
      output$UMAPplotshow <- renderPlot({
        req(data())
        req(group())
        data_t <- data() %>%
          t() %>%
          as.matrix()
        # print(data_t)
        umap <- umap(data_t,n_neighbors = 10)
        umap_plot_data <- data.frame(umap$layout) %>%
          setNames(c("UMAP1","UMAP2")) %>%
          mutate(Group = group()$Group)
        umap_plot <- ggplot(umap_plot_data, aes(UMAP1, UMAP2)) +
          geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
          stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
          scale_color_manual(values = color_map_UMAP) +  # 使用动态颜色映射
          scale_fill_manual(values = color_map_UMAP)+
          # labs(x = xlab, y = ylab)+
          theme_bw()+
          ggplot2::theme(
            plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
            panel.border = element_rect(colour = "black", size = 2),
            axis.ticks = element_line(color = "black", linewidth = 2),
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 16,colour = "black"),
            axis.title = element_text(size = 16,colour = "black"),
            panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),  # 去除主网格线
            panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.2)   # 去除次网格线
          )
        umap_plot
        })
      # UMAP显示上传的表格
      output$UMAP_dataTable <- DT::renderDT({
        req(data())
        req(group())
        data_t <- data() %>%
          t() %>%
          as.matrix()
        # print(data_t)
        umap <- umap(data_t,n_neighbors = 10)
        umap_plot_data <- data.frame(umap$layout) %>%
          setNames(c("UMAP1","UMAP2")) %>%
          mutate(Group = group()$Group)
        DT::datatable(umap_plot_data)
      })
      # UMAP下载表格
      output$download_UMAP_table <- downloadHandler(
        filename = function() {
          paste("UMAP_result_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          data_t <- data() %>%
            t() %>%
            as.matrix()
          # print(data_t)
          umap <- umap(data_t,n_neighbors = 10)
          umap_plot_data <- data.frame(umap$layout) %>%
            setNames(c("UMAP1","UMAP2")) %>%
            mutate(Group = group()$Group)
          write.csv(umap_plot_data,
                    file, row.names = TRUE)
        }
      )
      # 添加下载UMAP图像为PDF的功能
      output$download_UMAP_Figure <- downloadHandler(
        filename = function() {
          paste("UMAP_plot_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          data_t <- data() %>%
            t() %>%
            as.matrix()
          # print(data_t)
          umap <- umap(data_t,n_neighbors = 10)
          umap_plot_data <- data.frame(umap$layout) %>%
            setNames(c("UMAP1","UMAP2")) %>%
            mutate(Group = group()$Group)
          umap_plot <- ggplot(umap_plot_data, aes(UMAP1, UMAP2)) +
            geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
            stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
            scale_color_manual(values = color_map_UMAP) +  # 使用动态颜色映射
            scale_fill_manual(values = color_map_UMAP)+
            # labs(x = xlab, y = ylab)+
            theme_bw()+
            ggplot2::theme(
              plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
              panel.border = element_rect(colour = "black", size = 2),
              axis.ticks = element_line(color = "black", linewidth = 2),
              legend.text = element_text(size = 16),
              axis.text = element_text(size = 16,colour = "black"),
              axis.title = element_text(size = 16,colour = "black"),
              panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),  # 去除主网格线
              panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.2)   # 去除次网格线
            )
          # 保存图像为PDF文件
          ggsave(file, plot = umap_plot, device = "pdf", width = 8, height = 6)
        }
      )
      })
    # NMDS --------------------------------------------------------------------
    observeEvent(input$run_btn_NMDS, {
      req(input$dropdown == "NMDS")
      # 获取分组信息
      group_data <- group()
      # 获取用户选择的颜色
      color_map_NMDS <- sapply(unique(group_data$Group), function(g) {
        color_input_id <- paste0("color_NMDS_", g)
        input[[color_input_id]]  # 获取颜色
      }, USE.NAMES = TRUE)
      # UMAP分析
      output$NMDSplotshow <- renderPlot({
        req(data())
        req(group())
        data_t <- data() %>%
          t() %>%
          as.matrix()
        otu.distance <- vegan::vegdist(data_t, method = 'bray')
        df_nmds <- vegan::metaMDS(otu.distance, k = 2)
        df_nmds_stress <- df_nmds$stress
        df_points <- as.data.frame(df_nmds$points)
        df_points$samples <- row.names(df_points)
        names(df_points)[1:2] <- c('NMDS1', 'NMDS2')
        # print(group())
        group <- group() %>%
          dplyr::rename(samples = Sample )
        # print(group)
        nmds_plot_data <- df_points %>%
          left_join(group,by = "samples")
        # print(nmds_plot_data)
        nmds_plot <- ggplot(nmds_plot_data, aes(NMDS1, NMDS2)) +
          geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
          stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
          scale_color_manual(values = color_map_NMDS) +  # 使用动态颜色映射
          scale_fill_manual(values = color_map_NMDS)+
          # labs(x = xlab, y = ylab)+
          theme_bw()+
          ggplot2::theme(
            plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
            panel.border = element_rect(colour = "black", size = 2),
            axis.ticks = element_line(color = "black", linewidth = 2),
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 16,colour = "black"),
            axis.title = element_text(size = 16,colour = "black"),
            panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),  # 去除主网格线
            panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.2)   # 去除次网格线
          )+
          ggtitle(paste('Stress=',round(df_nmds_stress,3)))
        nmds_plot
        }
        )
      # NMDS显示上传的表格
      output$NMDS_dataTable <- DT::renderDT({
        req(data())
        req(group())
        data_t <- data() %>%
          t() %>%
          as.matrix()
        otu.distance <- vegan::vegdist(data_t, method = 'bray')
        df_nmds <- vegan::metaMDS(otu.distance, k = 2)
        df_nmds_stress <- df_nmds$stress
        df_points <- as.data.frame(df_nmds$points)
        df_points$samples <- row.names(df_points)
        names(df_points)[1:2] <- c('NMDS1', 'NMDS2')
        # print(group())
        group <- group() %>%
          dplyr::rename(samples = Sample )
        nmds_plot_data <- df_points %>%
          left_join(group,by = "samples")
        DT::datatable(nmds_plot_data)
      })
      # NMDS下载表格
      output$download_NMDS_table <- downloadHandler(
        filename = function() {
          paste("NMDS_result_table_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          data_t <- data() %>%
            t() %>%
            as.matrix()
          otu.distance <- vegan::vegdist(data_t, method = 'bray')
          df_nmds <- vegan::metaMDS(otu.distance, k = 2)
          df_nmds_stress <- df_nmds$stress
          df_points <- as.data.frame(df_nmds$points)
          df_points$samples <- row.names(df_points)
          names(df_points)[1:2] <- c('NMDS1', 'NMDS2')
          # print(group())
          group <- group() %>%
            dplyr::rename(samples = Sample )
          nmds_plot_data <- df_points %>%
            left_join(group,by = "samples")
          write.csv(nmds_plot_data,
                    file, row.names = TRUE)
        }
      )
      # 添加下载NMDS图像为PDF的功能
      output$download_NMDS_Figure <- downloadHandler(
        filename = function() {
          paste("NMDS_plot_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          req(data())
          req(group())
          data_t <- data() %>%
            t() %>%
            as.matrix()
          otu.distance <- vegan::vegdist(data_t, method = 'bray')
          df_nmds <- vegan::metaMDS(otu.distance, k = 2)
          df_nmds_stress <- df_nmds$stress
          df_points <- as.data.frame(df_nmds$points)
          df_points$samples <- row.names(df_points)
          names(df_points)[1:2] <- c('NMDS1', 'NMDS2')
          # print(group())
          group <- group() %>%
            dplyr::rename(samples = Sample )
          # print(group)
          nmds_plot_data <- df_points %>%
            left_join(group,by = "samples")
          # print(nmds_plot_data)
          nmds_plot <- ggplot(nmds_plot_data, aes(NMDS1, NMDS2)) +
            geom_point(aes(color = Group), size = 4) +  # 按分组画点的颜色
            stat_ellipse(aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
            scale_color_manual(values = color_map_NMDS) +  # 使用动态颜色映射
            scale_fill_manual(values = color_map_NMDS)+
            # labs(x = xlab, y = ylab)+
            theme_bw()+
            ggplot2::theme(
              plot.title = element_text(size = 12, hjust = 0.5),  # 图标题居中
              panel.border = element_rect(colour = "black", size = 2),
              axis.ticks = element_line(color = "black", linewidth = 2),
              legend.text = element_text(size = 16),
              axis.text = element_text(size = 16,colour = "black"),
              axis.title = element_text(size = 16,colour = "black"),
              panel.grid.major = element_line(color = "#EBEBEB", linewidth = 0.5),  # 去除主网格线
              panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.2)   # 去除次网格线
            )+
            ggtitle(paste('Stress=',round(df_nmds_stress,3)))
          # 保存图像为PDF文件
          ggsave(file, plot = nmds_plot, device = "pdf", width = 8, height = 6)
        }
      )
      }
      )
  })
}




