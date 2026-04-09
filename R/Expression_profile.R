#' Expression Profile User Interface
#' Creates a user interface for displaying expression profiles in a Shiny application.
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
  bslib::nav_panel(
    title = 'Expression profile',
    icon = bsicons::bs_icon("alexa"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('file'),
            label = 'Expression matrix',
            multiple = FALSE,
            accept = '.csv'
          )
        ),
        bslib::accordion_panel(
          title = "Method",
          icon = bsicons::bs_icon("view-stacked"),
          open = TRUE,
          shiny::selectInput(ns("dropdown"), "Choose a Method:",
                             choices = c("Kmeans","Heatmap"))
        )
      ),
      shiny::conditionalPanel(
        condition = "input.dropdown == 'Kmeans'",
        ns = ns,
        bslib::page_fluid(
          bslib::layout_column_wrap(
            width = 1,
            height = 600,
            bslib::navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Kmeans",
              sidebar = bslib::accordion(
                open = 'closed',
                bslib::accordion_panel(
                  title = 'Parameter',
                  colourpicker::colourInput(ns("color_select"), "select color", value = "#FF5733"),
                  shiny::numericInput(ns("centers"), "centers:", value = 6, min = 0)
                ),
                bslib::accordion_panel(
                  title = 'Run',
                  shiny::actionButton(ns("run_btn_Kmeans"), "Run")
                ),
                bslib::accordion_panel(
                  title = 'Download',
                  icon = bsicons::bs_icon('download'),
                  shiny::numericInput(ns("Kmeans_width"), "width:", value = 8, min = 0),
                  shiny::numericInput(ns("Kmeans_height"), "height:", value = 6, min = 0),
                  shiny::downloadButton(ns("download_Kmeans_Figure"), label = "Figure", icon = shiny::icon("download")),
                  shiny::br(),
                  shiny::downloadButton(ns("download_Kmeans_table"), label = "Table", icon = shiny::icon("download"))
                )
              ),
              mainPanel(
                shiny::tabsetPanel(
                  type = "tabs",
                  shiny::tabPanel(
                    title = "Figure",
                    shiny::plotOutput(ns("Kmeansplotshow"))
                  ),
                  shiny::tabPanel(
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
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- shiny::reactive({
      shiny::req(input$file)
      utils::read.csv(input$file$datapath, row.names = 1)
    })
    shiny::observeEvent(input$run_btn_Kmeans, {
      shiny::req(input$dropdown == "Kmeans")
      data <- data()
      data_scale <- base::data.frame(base::round(base::t(base::apply(data, 1, scale)), 2))
      base::colnames(data_scale) <- base::colnames(data)
      cl <- stats::kmeans(data_scale, centers = input$centers)
      data_new <- data.frame("index" = rownames(data_scale), "Cluster" = cl$cluster, data_scale) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate("Cluster" = base::paste0("Cluster", Cluster))
      output$Kmeansplotshow <- shiny::renderPlot({
        shiny::req(data_scale)
        shiny::req(cl)
        data_new <- data.frame("index" = base::rownames(data_scale), Cluster = cl$cluster, data_scale) %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(Cluster = base::paste0("Cluster", Cluster)) %>%
          dplyr::count(Cluster) %>%
          dplyr::rename(Cluster_Count = n) %>%
          dplyr::right_join(data_new, by = "Cluster") %>%
          dplyr::mutate(Cluster = base::paste0(Cluster,":",Cluster_Count)) %>%
          dplyr::select(-Cluster_Count)
        data_new = reshape2::melt(data_new) %>% dplyr::as_tibble()
        centers_line <- reshape2::melt(cl$centers)
        centers_line <- base::split(centers_line, centers_line$Var1)
        plot_data <- base::split(data_new, data_new$Cluster)
        # Generate a palette of distinct colors
        num_clusters <- base::length(base::unique(data_new$Cluster))
        colors <- grDevices::rainbow(num_clusters)
        # Create a named vector of colors
        color_vector <- stats::setNames(colors, base::unique(data_new$Cluster))
        # Modify the plotting code to use the color_vector
        plots <- purrr::map2(plot_data, centers_line, function(df, centers) {
          ggplot2::ggplot(df, ggplot2::aes(x = variable, y = value, group = index, color = Cluster)) +
            ggplot2::geom_line(show.legend = FALSE) +
            ggplot2::labs(x = "", y = "Standardised value") +
            ggplot2::labs(title = df$Cluster)+
            # scale_color_manual(values = color_vector) +  # 使用自定义的颜色向量
            ggplot2::scale_color_manual(values = input$color_select) +  # 使用自定义的颜色向量
            ggplot2::theme_bw() +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
            ggplot2::theme(panel.grid = ggplot2::element_blank()) +
            ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black'))+
            ggplot2::theme(text=ggplot2::element_text(size=11,  family="serif"))+
            ggplot2::geom_line(data = centers, ggplot2::aes(x = Var2,
                                                            y = value,
                                                            group = factor(Var1)),
                               col = "black", linewidth = 1)+
            ggprism::theme_prism(border = TRUE)+
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
        })
        patchwork_plot <- patchwork::wrap_plots(plots)
        print(patchwork_plot)
      })
      # kmeans显示上传的表格
      output$Kmeans_dataTable <- DT::renderDT({
        shiny::req(data_new)
        DT::datatable(data_new)
      })
      # 下载表格
      output$download_Kmeans_table <- shiny::downloadHandler(
        filename = function() {
          base::paste("Kmeans_", base::Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          shiny::req(data_new)
          utils::write.csv(data_new, file, row.names = TRUE)
        }
      )
      output$download_Kmeans_Figure <- shiny::downloadHandler(
        filename = function() {
          base::paste("Kmeans_plot_", base::Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          shiny::req(data_scale)
          shiny::req(cl)
          data_new <- data.frame("index" = rownames(data_scale), Cluster = cl$cluster, data_scale) %>%
            dplyr::as_tibble() %>%
            dplyr::mutate(Cluster = base::paste0("Cluster", Cluster)) %>%
            dplyr::count(Cluster) %>%
            dplyr::rename(Cluster_Count = n) %>%
            dplyr::right_join(data_new, by = "Cluster") %>%
            dplyr::mutate(Cluster = base::paste0(Cluster,":",Cluster_Count)) %>%
            dplyr::select(-Cluster_Count)
          # print(data_new)
          data_new = reshape2::melt(data_new) %>% dplyr::as_tibble()
          centers_line <- reshape2::melt(cl$centers)
          centers_line <- base::split(centers_line, centers_line$Var1)
          plot_data <- base::split(data_new, data_new$Cluster)
          # Generate a palette of distinct colors
          num_clusters <- base::length(base::unique(data_new$Cluster))
          colors <- grDevices::rainbow(num_clusters)
          # Create a named vector of colors
          color_vector <- stats::setNames(colors, base::unique(data_new$Cluster))
          # Modify the plotting code to use the color_vector
          plots <- purrr::map2(plot_data, centers_line, function(df, centers) {
            ggplot2::ggplot(df, ggplot2::aes(x = variable, y = value, group = index, color = Cluster)) +
              ggplot2::geom_line(show.legend = FALSE) +
              ggplot2::labs(x = "", y = "Standardised value") +
              ggplot2::labs(title = df$Cluster)+
              # scale_color_manual(values = color_vector) +  # 使用自定义的颜色向量
              ggplot2::scale_color_manual(values = input$color_select) +  # 使用自定义的颜色向量
              ggplot2::theme_bw() +
              ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
              ggplot2::theme(panel.grid = ggplot2::element_blank()) +
              ggplot2::theme(axis.text = ggplot2::element_text(colour = 'black'))+
              ggplot2::theme(text=ggplot2::element_text(size=11,  family="serif"))+
              ggplot2::geom_line(data = centers, ggplot2::aes(x = Var2,
                                                              y = value,
                                                              group = factor(Var1)),
                                 col = "black", linewidth = 1)+
              ggprism::theme_prism(border = TRUE)+
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5))
          })
          patchwork_plot <- patchwork::wrap_plots(plots)
          print(patchwork_plot)
          ggplot2::ggsave(file, plot = patchwork_plot, device = "pdf", width = input$Kmeans_width, height = input$Kmeans_height)
        }
      )
    }
    )
  }
  )
}
