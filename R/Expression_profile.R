#' Expression Profile User Interface
#'
#' Creates a user interface for displaying expression profiles in a Shiny application.
#'
#' @param id A unique identifier for the Shiny namespace.
#' @title Expression_profile_ui
#' @name Expression_profile_ui
#' @import shiny
#' @import bslib
#' @import bsicons
#' @export
#'
Expression_profile_ui <- function(id) {
  ns <- NS(id)
  # UI 结构
  nav_panel(
    title = 'Expression profile',
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
          )
        ),
        accordion_panel(
          title = "Method",
          icon = bs_icon("view-stacked"),
          open = TRUE,
          selectInput(ns("dropdown"), "Choose a Method:",
                      choices = c("Kmeans","Heatmap"))
        )
      ),
      # Kmeans ---------------------------------------------------------------------
      # 使用 conditionalPanel，仅在选择 "Kmeans" 时显示右侧面板
      conditionalPanel(
        condition = "input.dropdown == 'Kmeans'",
        ns = ns,  # 确保为该模块设置命名空间
        page_fluid(
          layout_column_wrap(
            width = 1,
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Kmeans",
              sidebar = accordion(
                open = 'closed',
                accordion_panel(
                  title = 'Parameter',
                  colourpicker::colourInput(ns("color_select"), "select color", value = "#FF5733"),
                  numericInput(ns("centers"), "centers:", value = 6, min = 0)
                ),
                accordion_panel(
                  title = 'Run',
                  actionButton(ns("run_btn_Kmeans"), "Run")
                ),
                accordion_panel(
                  title = 'Download',
                  icon = bs_icon('download'),
                  numericInput(ns("Kmeans_width"), "width:", value = 8, min = 0),
                  numericInput(ns("Kmeans_height"), "height:", value = 6, min = 0),
                  downloadButton(ns("download_Kmeans_Figure"), label = "Figure", icon = icon("download")),
                  br(),
                  downloadButton(ns("download_Kmeans_table"), label = "Table", icon = icon("download"))
                )
              ),
              mainPanel(
                tabsetPanel(
                  type = "tabs", # 允许标签页切换
                  tabPanel(
                    title = "Figure",
                    plotOutput(ns("Kmeansplotshow"))
                  ),
                  tabPanel(
                    title = "Table",
                    DT::DTOutput(ns("Kmeans_dataTable"))
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


# server ------------------------------------------------------------------
# Server
#' @import shiny
#' @import ggplot2
#' @import bslib
#' @import bsicons
#' @name Expression_profile_server
#' @title Expression_profile_server
#' @export

utils::globalVariables(c("Cluster", "Cluster_Count", "variable",
                         "index","Cluster","Var2","Var1"))

Expression_profile_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data <- reactive({
      req(input$file)
      read.csv(input$file$datapath, row.names = 1)
    })
    # Kmeans ---------------------------------------------------------------------
    # Kmeans 绘图
    observeEvent(input$run_btn_Kmeans, {
      req(input$dropdown == "Kmeans")
      data <- data()
      data_scale <- data.frame(round(t(apply(data, 1, scale)), 2))
      colnames(data_scale) <- colnames(data)
      cl <- kmeans(data_scale, centers = input$centers)
      data_new <- data.frame("index" = rownames(data_scale), "Cluster" = cl$cluster, data_scale) %>%
        as_tibble() %>%
        dplyr::mutate("Cluster" = paste0("Cluster", Cluster))
      # print(data_new)
      output$Kmeansplotshow <- renderPlot({
        req(data_scale)
        req(cl)
        data_new <- data.frame("index" = rownames(data_scale), Cluster = cl$cluster, data_scale) %>%
          as_tibble() %>%
          dplyr::mutate(Cluster = paste0("Cluster", Cluster)) %>%
          dplyr::count(Cluster) %>%
          dplyr::rename(Cluster_Count = n) %>%
          dplyr::right_join(data_new, by = "Cluster") %>%
          dplyr::mutate(Cluster = paste0(Cluster,":",Cluster_Count)) %>%
          dplyr::select(-Cluster_Count)
        # print(data_new)
        data_new = reshape2::melt(data_new) %>% as_tibble()
        centers_line <- reshape2::melt(cl$centers)
        centers_line <- split(centers_line, centers_line$Var1)
        # 定义绘图数据
        plot_data <- split(data_new, data_new$Cluster)
        # Generate a palette of distinct colors
        num_clusters <- length(unique(data_new$Cluster))  # 获取聚类集群的数量
        colors <- rainbow(num_clusters)  # 使用RColorBrewer生成一组颜色
        # Create a named vector of colors
        color_vector <- setNames(colors, unique(data_new$Cluster))
        # Modify the plotting code to use the color_vector
        plots <- purrr::map2(plot_data, centers_line, function(df, centers) {
          ggplot2::ggplot(df, aes(x = variable, y = value, group = index, color = Cluster)) +
            geom_line(show.legend = FALSE) +
            labs(x = "", y = "Standardised value") +
            labs(title = df$Cluster)+
            # scale_color_manual(values = color_vector) +  # 使用自定义的颜色向量
            scale_color_manual(values = input$color_select) +  # 使用自定义的颜色向量
            theme_bw() +
            theme(plot.title = element_text(hjust = 0.5)) +
            theme(panel.grid = element_blank()) +
            theme(axis.text = element_text(colour = 'black'))+
            theme(text=element_text(size=11,  family="serif"))+
            geom_line(data = centers, aes(x = Var2,
                                          y = value,
                                          group = factor(Var1)),
                      col = "black", linewidth = 1)+
            ggprism::theme_prism(border = TRUE)+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
        })
        patchwork_plot <- patchwork::wrap_plots(plots)
        print(patchwork_plot)
      })
      # kmeans显示上传的表格
      output$Kmeans_dataTable <- DT::renderDT({
        req(data_new)
        DT::datatable(data_new)
      })
      # 下载表格
      output$download_Kmeans_table <- downloadHandler(
        filename = function() {
          paste("Kmeans_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          req(data_new)
          # 保存旋转矩阵为CSV文件
          write.csv(data_new, file, row.names = TRUE)
        }
      )
      # 添加下载Kmeans图像为PDF的功能
      output$download_Kmeans_Figure <- downloadHandler(
        filename = function() {
          paste("Kmeans_plot_", Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          req(data_scale)
          req(cl)
          data_new <- data.frame("index" = rownames(data_scale), Cluster = cl$cluster, data_scale) %>%
            as_tibble() %>%
            dplyr::mutate(Cluster = paste0("Cluster", Cluster)) %>%
            dplyr::count(Cluster) %>%
            dplyr::rename(Cluster_Count = n) %>%
            dplyr::right_join(data_new, by = "Cluster") %>%
            dplyr::mutate(Cluster = paste0(Cluster,":",Cluster_Count)) %>%
            dplyr::select(-Cluster_Count)
          # print(data_new)
          data_new = reshape2::melt(data_new) %>% as_tibble()
          centers_line <- reshape2::melt(cl$centers)
          centers_line <- split(centers_line, centers_line$Var1)
          # 定义绘图数据
          plot_data <- split(data_new, data_new$Cluster)
          # Generate a palette of distinct colors
          num_clusters <- length(unique(data_new$Cluster))  # 获取聚类集群的数量
          colors <- rainbow(num_clusters)  # 使用RColorBrewer生成一组颜色
          # Create a named vector of colors
          color_vector <- setNames(colors, unique(data_new$Cluster))
          # Modify the plotting code to use the color_vector
          plots <- purrr::map2(plot_data, centers_line, function(df, centers) {
            ggplot(df, aes(x = variable, y = value, group = index, color = Cluster)) +
              geom_line(show.legend = FALSE) +
              labs(x = "", y = "Standardised value") +
              labs(title = df$Cluster)+
              # scale_color_manual(values = color_vector) +  # 使用自定义的颜色向量
              scale_color_manual(values = input$color_select) +  # 使用自定义的颜色向量
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5)) +
              theme(panel.grid = element_blank()) +
              theme(axis.text = element_text(colour = 'black'))+
              theme(text=element_text(size=11,  family="serif"))+
              geom_line(data = centers, aes(x = Var2,
                                            y = value,
                                            group = factor(Var1)),
                        col = "black", linewidth = 1)+
              ggprism::theme_prism(border = TRUE)+
              theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
          })
          patchwork_plot <- patchwork::wrap_plots(plots)
          print(patchwork_plot)
          # 保存图像为PDF文件
          ggsave(file, plot = patchwork_plot, device = "pdf", width = input$Kmeans_width, height = input$Kmeans_height)
        }
      )

      }
      )


  }
  )
}

