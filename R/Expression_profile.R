#' Expression Profile User Interface
#' Creates a user interface for displaying expression profiles in a Shiny application.
#' @param id A unique identifier for the Shiny namespace.
#' @title Expression_profile_ui
#' @name Expression_profile_ui
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom colourpicker colourInput
#' @export
#'
Expression_profile_ui <- function(id) {
  ns <- NS(id)

  sidebar_ui <- bslib::accordion(
    bslib::accordion_panel(
      title = "File Upload",
      icon = bsicons::bs_icon("upload"),
      shiny::fileInput(
        inputId = ns("file"),
        label = "Expression matrix (optional; built-in example is used when empty)",
        multiple = FALSE,
        accept = ".csv"
      )
    ),
    bslib::accordion_panel(
      title = "Method",
      icon = bsicons::bs_icon("view-stacked"),
      open = TRUE,
      shiny::selectInput(
        ns("dropdown"),
        "Choose a Method:",
        choices = c("Kmeans", "Heatmap")
      )
    )
  )

  kmeans_ui <- shiny::conditionalPanel(
    condition = "input.dropdown == 'Kmeans'",
    ns = ns,
    bslib::layout_column_wrap(
      width = 1,
      height = 800,
      bslib::navset_card_tab(
        height = 800,
        full_screen = TRUE,
        title = "Kmeans",
        sidebar = bslib::accordion(
          open = "closed",
          bslib::accordion_panel(
            title = "Parameter",
            colourpicker::colourInput(ns("color_select"), "select color", value = "#FF5733"),
            shiny::numericInput(ns("centers"), "centers:", value = 6, min = 0)
          ),
          bslib::accordion_panel(
            title = "Run",
            shiny::actionButton(ns("run_btn_Kmeans"), "Run")
          ),
          bslib::accordion_panel(
            title = "Download",
            icon = bsicons::bs_icon("download"),
            shiny::numericInput(ns("Kmeans_width"), "width:", value = 8, min = 0),
            shiny::numericInput(ns("Kmeans_height"), "height:", value = 6, min = 0),
            shiny::downloadButton(ns("download_Kmeans_Figure"), label = "Figure", icon = shiny::icon("download")),
            shiny::br(),
            shiny::downloadButton(ns("download_Kmeans_table"), label = "Table", icon = shiny::icon("download"))
          )
        ),
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel("Figure", shiny::plotOutput(ns("Kmeansplotshow"))),
          shiny::tabPanel("Table", DT::DTOutput(ns("Kmeans_dataTable")))
        )
      )
    )
  )

  heatmap_ui <- shiny::conditionalPanel(
    condition = "input.dropdown == 'Heatmap'",
    ns = ns,
    bslib::layout_column_wrap(
      width = 1,
      height = 800,
      bslib::navset_card_tab(
        height = 800,
        full_screen = TRUE,
        title = "Heatmap trend analysis (K-means)",
        sidebar = bslib::accordion(
          open = "Parameter",
          bslib::accordion_panel(
            title = "Parameter",
            shiny::numericInput(ns("heatmap_centers"), "K-means clusters:", value = 6, min = 2),
            shiny::selectInput(
              ns("heatmap_scale"),
              "Scale:",
              choices = c("Row (trend)" = "row", "Column" = "column", "None" = "none"),
              selected = "row"
            )
          ),
          bslib::accordion_panel(
            title = "Run",
            shiny::actionButton(ns("run_btn_heatmap"), "Run heatmap", class = "btn btn-primary")
          ),
          bslib::accordion_panel(
            title = "Download",
            icon = bsicons::bs_icon("download"),
            shiny::downloadButton(ns("download_heatmap_table"), label = "Cluster table", icon = shiny::icon("download"))
          )
        ),
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel("Figure", shiny::plotOutput(ns("heatmap_plot"), height = "680px")),
          shiny::tabPanel("Table", DT::DTOutput(ns("heatmap_table")))
        )
      )
    )
  )

  bslib::layout_sidebar(
    sidebar = sidebar_ui,
    shiny::tagList(kmeans_ui, heatmap_ui)
  )
}


#' @import shiny
#' @importFrom utils read.csv write.csv
#' @importFrom dplyr as_tibble mutate count rename right_join select
#' @importFrom reshape2 melt
#' @importFrom purrr map2
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 ggsave
#' @importFrom ggprism theme_prism
#' @name Expression_profile_server
#' @title Expression_profile_server
#' @export
#'

utils::globalVariables(c("Cluster_Count", "variable",
                         "index","Cluster","Var2","Var1"))
Expression_profile_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    builtin_expression_profile <- local({
      set.seed(20260910)
      values <- matrix(rnorm(105 * 8), nrow = 105, ncol = 8)
      values[1:35, 4:8] <- values[1:35, 4:8] + 1.2
      values[36:70, 1:4] <- values[36:70, 1:4] + 1.0
      values[71:105, c(2, 5, 7)] <- values[71:105, c(2, 5, 7)] - 1.1
      colnames(values) <- c("TA", "TB", "TC", "TD", "A", "B", "C", "D")
      rownames(values) <- sprintf("m%03d", seq_len(nrow(values)))
      function() as.data.frame(values, check.names = FALSE)
    })
    data <- shiny::reactive({
      if (!is.null(input$file)) {
        return(utils::read.csv(input$file$datapath, row.names = 1,
                               check.names = FALSE))
      }
      builtin_expression_profile()
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
            # scale_color_manual(values = color_vector) +
            ggplot2::scale_color_manual(values = input$color_select) +
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
              # scale_color_manual(values = color_vector) +
              ggplot2::scale_color_manual(values = input$color_select) +
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

    shiny::observeEvent(input$run_btn_heatmap, {
      shiny::req(input$dropdown == "Heatmap")
      mat <- data()
      mat <- as.matrix(mat)
      storage.mode(mat) <- "numeric"
      mat <- mat[stats::complete.cases(mat), , drop = FALSE]
      shiny::validate(shiny::need(nrow(mat) >= input$heatmap_centers, "The matrix must contain at least as many rows as K-means clusters."))

      scaled_mat <- switch(
        input$heatmap_scale,
        row = t(scale(t(mat))),
        column = scale(mat),
        none = mat
      )
      scaled_mat[is.na(scaled_mat)] <- 0
      km <- stats::kmeans(scaled_mat, centers = input$heatmap_centers)
      cluster_table <- data.frame(ID = rownames(scaled_mat), Cluster = paste0("Cluster", km$cluster), scaled_mat, check.names = FALSE)
      cluster_table <- cluster_table[order(cluster_table$Cluster), , drop = FALSE]
      plot_mat <- as.matrix(cluster_table[, setdiff(colnames(cluster_table), c("ID", "Cluster")), drop = FALSE])
      rownames(plot_mat) <- cluster_table$ID

      output$heatmap_plot <- shiny::renderPlot({
        stats::heatmap(
          plot_mat,
          Rowv = NA,
          Colv = NA,
          scale = "none",
          labRow = NA,
          margins = c(8, 6),
          main = "Expression trend heatmap (K-means ordered)"
        )
      })

      output$heatmap_table <- DT::renderDT({
        DT::datatable(cluster_table, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
      })

      output$download_heatmap_table <- shiny::downloadHandler(
        filename = function() paste0("heatmap_kmeans_clusters_", Sys.Date(), ".csv"),
        content = function(file) utils::write.csv(cluster_table, file, row.names = FALSE)
      )
    })

  }
  )
}
