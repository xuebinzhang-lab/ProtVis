#' Dimension reduction analysis UI Module
#' @description Dimension reduction analysis UI Module
#' @param id A unique identifier for the Shiny namespace, Dimension reduction analysis.
#' @title DR_analysis_ui
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom colourpicker colourInput
#' @name DR_analysis_ui
#' @export
#'
DR_analysis_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'Dimensionality Reduction Analysis',
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
          ),
          shiny::fileInput(
            inputId = ns('GroupInfo'),
            label = 'Group information',
            multiple = FALSE,
            accept = '.csv'
          )
        ),
        bslib::accordion_panel(
          title = "Method",
          icon = bsicons::bs_icon("view-stacked"),
          open = TRUE,
          shiny::selectInput(ns("dropdown"), "Choose a Dimensionality Reduction Method:",
                             choices = c("PCA", "PCoA", "tSNE", "UMAP", "NMDS"))
        )
      ),
      # PCA ---------------------------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.dropdown == 'PCA'",
        ns = ns,
        # 👇 删掉 page_fluid，直接写内容
        bslib::layout_column_wrap(
          width = 1,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "PCA",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::uiOutput(ns("colorSelectors_PCA"))
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("run_btn_PCA"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("download_PCA_Figure"), label = "Figure", icon = shiny::icon("download")),
                shiny::br(),
                shiny::downloadButton(ns("download_PCA_table"), label = "Table", icon = shiny::icon("download"))
              )
            ),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                title = "Figure",
                shiny::plotOutput(ns("PCAplotshow"))
              ),
              shiny::tabPanel(
                title = "Table",
                DT::DTOutput(ns("PCA_dataTable"))
              )
            )
          )
        )
      ),
      # PCoA ---------------------------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.dropdown == 'PCoA'",
        ns = ns,
        bslib::layout_column_wrap(
          width = 1,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "PCoA",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::uiOutput(ns("colorSelectors_PCoA"))
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("run_btn_PCoA"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("download_PCoA_Figure"), label = "Figure", icon = shiny::icon("download")),
                shiny::br(),
                shiny::downloadButton(ns("download_PCoA_table"), label = "Table", icon = shiny::icon("download"))
              )
            ),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                title = "Figure",
                shiny::plotOutput(ns("PCoAplotshow"))
              ),
              shiny::tabPanel(
                title = "Table",
                DT::DTOutput(ns("PCoA_dataTable"))
              )
            )
          )
        )
      ),
      # tSNE ---------------------------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.dropdown == 'tSNE'",
        ns = ns,
        bslib::layout_column_wrap(
          width = 1,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "tSNE",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::uiOutput(ns("colorSelectors_tSNE"))
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("run_btn_tSNE"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("download_tSNE_Figure"), label = "Figure", icon = shiny::icon("download")),
                shiny::br(),
                shiny::downloadButton(ns("download_tSNE_table"), label = "Table", icon = shiny::icon("download"))
              )
            ),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                title = "Figure",
                shiny::plotOutput(ns("tSNEplotshow"))
              ),
              shiny::tabPanel(
                title = "Table",
                DT::DTOutput(ns("tSNE_dataTable"))
              )
            )
          )
        )
      ),
      # UMAP --------------------------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.dropdown == 'UMAP'",
        ns = ns,
        bslib::layout_column_wrap(
          width = 1,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "UMAP",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::uiOutput(ns("colorSelectors_UMAP"))
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("run_btn_UMAP"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("download_UMAP_Figure"), label = "Figure", icon = shiny::icon("download")),
                shiny::br(),
                shiny::downloadButton(ns("download_UMAP_table"), label = "Table", icon = shiny::icon("download"))
              )
            ),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                title = "Figure",
                shiny::plotOutput(ns("UMAPplotshow"))
              ),
              shiny::tabPanel(
                title = "Table",
                DT::DTOutput(ns("UMAP_dataTable"))
              )
            )
          )
        )
      ),
      # NMDS --------------------------------------------------------------------
      shiny::conditionalPanel(
        condition = "input.dropdown == 'NMDS'",
        ns = ns,
        bslib::layout_column_wrap(
          width = 1,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "NMDS",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::uiOutput(ns("colorSelectors_NMDS"))
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("run_btn_NMDS"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("download_NMDS_Figure"), label = "Figure", icon = shiny::icon("download")),
                shiny::br(),
                shiny::downloadButton(ns("download_NMDS_table"), label = "Table", icon = shiny::icon("download"))
              )
            ),
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel(
                title = "Figure",
                shiny::plotOutput(ns("NMDSplotshow"))
              ),
              shiny::tabPanel(
                title = "Table",
                DT::DTOutput(ns("NMDS_dataTable"))
              )
            )
          )
        )
      )
    )
  )
}

#' Dimension reduction analysis Server Module
#' @title DR_analysis_server
#' @description Server logic for Dimension reduction analysis
#' @param id The module ID. This is used to namespace the inputs and outputs in the UI and server components.
#' @import shiny
#' @importFrom utils read.csv write.csv
#' @importFrom colourpicker colourInput
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom dplyr left_join rename
#' @importFrom DT renderDT datatable
#' @importFrom PCAtools pca biplot
#' @importFrom vegan vegdist eigenvals
#' @importFrom ggplot2 ggsave
#' @name DR_analysis_server
#' @export
#'
utils::globalVariables(c("V1", "V2", "TSNE1","TSNE2", "UMAP1","UMAP2", "NMDS1","NMDS2"))

DR_analysis_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Upload expression data
    data <- shiny::reactive({
      shiny::req(input$file)
      utils::read.csv(input$file$datapath, row.names = 1)
    })
    # Upload grouping information
    group <- shiny::reactive({
      shiny::req(input$GroupInfo)
      utils::read.csv(input$GroupInfo$datapath)
    })
    # Color selector for dynamically generating PCA
    shiny::observeEvent(input$GroupInfo, {
      shiny::req(input$GroupInfo)
      group_data <- group()
      # Ensure that the Group column exists.
      if (!"Group" %in% colnames(group_data)) {
        stop("Group column is missing in the group information.")
      }
      output$colorSelectors_PCA <- shiny::renderUI({
        base::lapply(unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(base::paste0("color_PCA_", g)),
            label = paste("Select Color for", g),
            value = base::sample(colors(), 1)  # Default color
          )
        })
      })
    })
    # Color selector for dynamically generating PCoA
    shiny::observeEvent(input$GroupInfo, {
      shiny::req(input$GroupInfo)
      group_data <- group()
      # Ensure that the Group column exists.
      if (!"Group" %in% colnames(group_data)) {
        stop("Group column is missing in the group information.")
      }
      output$colorSelectors_PCoA <- shiny::renderUI({
        base::lapply(base::unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(base::paste0("color_PCoA_", g)),
            label = base::paste("Select Color for", g),
            value = base::sample(colors(), 1)  # Default color
          )
        })
      })
      output$colorSelectors_tSNE <- shiny::renderUI({
        base::lapply(base::unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(base::paste0("color_tSNE_", g)),
            label = base::paste("Select Color for", g),
            value = base::sample(colors(), 1)  # Default color
          )
        })
      })

      output$colorSelectors_UMAP <- shiny::renderUI({
        base::lapply(base::unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(base::paste0("color_UMAP_", g)),
            label = base::paste("Select Color for", g),
            value = base::sample(colors(), 1)  # Default color
          )
        })
      })

      output$colorSelectors_NMDS <- shiny::renderUI({
        base::lapply(base::unique(group_data$Group), function(g) {
          colourpicker::colourInput(
            inputId = ns(base::paste0("color_NMDS_", g)),
            label = base::paste("Select Color for", g),
            value = base::sample(colors(), 1)  # Default color
          )
        })
      })

    })
    # PCA ---------------------------------------------------------------------
    shiny::observeEvent(input$run_btn_PCA, {
      shiny::req(input$dropdown == "PCA")
      group_data <- group()
      color_map_PCA <- base::sapply(base::unique(group_data$Group), function(g) {
        color_input_id <- base::paste0("color_PCA_", g)
        input[[color_input_id]]
      }, USE.NAMES = TRUE)
      output$PCAplotshow <- shiny::renderPlot({
        shiny::req(data())
        shiny::req(group())
        class <- group_data %>%
          tibble::column_to_rownames("Sample")
        expr <- base::log10(data())
        pca_data <- base::t(expr)
        pca <- PCAtools::pca(expr, metadata = class)
        PCAtools::biplot(pca,
                         x = "PC1",
                         y = "PC2",
                         colby = "Group",
                         colkey = color_map_PCA,
                         legendPosition = "right",
                         lab = NULL,
                         encircle = TRUE,
                         encircleFill = TRUE
        )
      })
    })
    output$PCA_dataTable <- DT::renderDT({
      shiny::req(data())
      shiny::req(group())
      class <- group() %>%
        tibble::column_to_rownames("Sample")
      expr <- base::log10(data())
      pca_data <- base::t(expr)
      pca <- PCAtools::pca(expr, metadata = class)
      DT::datatable(data.frame(pca[["rotated"]]))
    })
    output$download_PCA_table <- shiny::downloadHandler(
      filename = function() {
        base::paste("PCA_rotated_", base::Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(data())
        shiny::req(group())
        class <- group() %>%
          tibble::column_to_rownames("Sample")
        expr <- base::log10(data())
        pca_data <- base::t(expr)
        pca <- PCAtools::pca(expr, metadata = class)
        utils::write.csv(pca[["rotated"]], file, row.names = TRUE)
      }
    )
    output$download_PCA_Figure <- shiny::downloadHandler(
      filename = function() {
        base::paste("PCA_plot_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        group_data <- group()
        color_map_PCA <- base::sapply(base::unique(group_data$Group), function(g) {
          color_input_id <- paste0("color_PCA_", g)
          input[[color_input_id]]
        }, USE.NAMES = TRUE)
        class <- group_data %>%
          tibble::column_to_rownames("Sample")
        expr <- base::log10(data())
        pca_data <- base::t(expr)
        pca <- PCAtools::pca(expr, metadata = class)
        pca_plot <- PCAtools::biplot(pca,
                                     x = "PC1",
                                     y = "PC2",
                                     colby = "Group",
                                     colkey = color_map_PCA,
                                     legendPosition = "right",
                                     lab = NULL,
                                     encircle = TRUE,
                                     encircleFill = TRUE
        )
        ggplot2::ggsave(file, plot = pca_plot, device = "pdf", width = 8, height = 6)
      }
    )
    # PCoA --------------------------------------------------------------------
    shiny::observeEvent(input$run_btn_PCoA, {
      shiny::req(input$dropdown == "PCoA")
      group_data <- group()
      color_map_PCoA <- base::sapply(unique(group_data$Group), function(g) {
        color_input_id <- base::paste0("color_PCoA_", g)
        input[[color_input_id]]
      }, USE.NAMES = TRUE)
      output$PCoAplotshow <- shiny::renderPlot({
        shiny::req(data())
        shiny::req(group())
        dist = vegan::vegdist(base::t(data()), method = "bray", diag = TRUE, upper = TRUE)
        pcoa = stats::cmdscale(dist, eig = TRUE)
        eig = base::summary(vegan::eigenvals(pcoa))
        axis = base::paste0("PCoA", 1:ncol(eig))
        eig = base::data.frame(Axis = axis, base::t(eig)[, -3])
        pco1 <- base::round(eig[1, 3] * 100, 2)
        pco2 <- base::round(eig[2, 3] * 100, 2)
        xlab <- base::paste0("PCoA1 (", pco1, "%)")
        ylab <- base::paste0("PCoA2 (", pco2, "%)")
        pcoa_points <- base::as.data.frame(pcoa$points) %>%
          tibble::rownames_to_column("index") %>%
          dplyr::left_join(group() %>% dplyr::rename(index = Sample), by = "index") %>%
          tibble::column_to_rownames("index")
        ggplot2::ggplot(pcoa_points, ggplot2::aes(V1, V2)) +
          ggplot2::geom_point(ggplot2::aes(color = Group), size = 4) +  # 按分组画点的颜色
          ggplot2::stat_ellipse(ggplot2::aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
          ggplot2::scale_color_manual(values = color_map_PCoA) +  # 使用动态颜色映射
          ggplot2::scale_fill_manual(values = color_map_PCoA)+
          ggplot2::labs(x = xlab, y = ylab)+
          ggplot2::theme_bw()+
          ggplot2::theme(
            plot.title = ggplot2::element_text(size = 12, hjust = 0.5),  # 图标题居中
            panel.border = ggplot2::element_rect(colour = "black", size = 2),
            axis.ticks = ggplot2::element_line(color = "black", size = 2),
            legend.text = ggplot2::element_text(size = 16),
            axis.text = ggplot2::element_text(size = 16,colour = "black"),
            axis.title = ggplot2::element_text(size = 16,colour = "black"),
            panel.grid.major = ggplot2::element_line(color = "#EBEBEB", size = 0.5),  # 去除主网格线
            panel.grid.minor = ggplot2::element_line(color = "#EBEBEB", size = 0.2)   # 去除次网格线
          )
      })
      output$PCoA_dataTable <- DT::renderDT({
        shiny::req(data())
        shiny::req(group())
        dist = vegan::vegdist(base::t(data()), method = "bray", diag = TRUE, upper = TRUE)
        pcoa = cmdscale(dist, eig = TRUE)
        DT::datatable(base::data.frame(pcoa$points) %>% stats::setNames(c("PCoA1","PCoA2")))
      })
      output$download_PCoA_table <- shiny::downloadHandler(
        filename = function() {
          base::paste("PCoA_result_table_", base::Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          shiny::req(data())
          shiny::req(group())
          dist = vegan::vegdist(base::t(data()), method = "bray", diag = TRUE, upper = TRUE)
          pcoa = stats::cmdscale(dist, eig = TRUE)
          DT::datatable(base::data.frame(pcoa$points) %>% stats::setNames(c("PCoA1","PCoA2")))
          utils::write.csv(base::data.frame(pcoa$points) %>% stats::setNames(c("PCoA1","PCoA2")),
                    file, row.names = TRUE)
        }
      )
      output$download_PCoA_Figure <- shiny::downloadHandler(
        filename = function() {
          base::paste("PCoA_plot_", base::Sys.Date(), ".pdf", sep = "")
        },
        content = function(file) {
          shiny::req(data())
          shiny::req(group())
          dist = vegan::vegdist(base::t(data()), method = "bray", diag = TRUE, upper = TRUE)
          pcoa = stats::cmdscale(dist, eig = TRUE)
          eig = base::summary(vegan::eigenvals(pcoa))
          axis = base::paste0("PCoA", 1:ncol(eig))
          eig = base::data.frame(Axis = axis, base::t(eig)[, -3])
          pco1 <- base::round(eig[1, 3] * 100, 2)
          pco2 <- base::round(eig[2, 3] * 100, 2)
          xlab <- base::paste0("PCoA1 (", pco1, "%)")
          ylab <- base::paste0("PCoA2 (", pco2, "%)")
          pcoa_points <- base::as.data.frame(pcoa$points) %>%
            tibble::rownames_to_column("index") %>%
            dplyr::left_join(group() %>% dplyr::rename(index = Sample), by = "index") %>%
            tibble::column_to_rownames("index")
          pcoa_plot <- ggplot2::ggplot(pcoa_points, ggplot2::aes(V1, V2)) +
            ggplot2::geom_point(aes(color = Group), size = 4) +
            ggplot2::stat_ellipse(ggplot2::aes(fill = Group), geom = 'polygon', level = 0.95, alpha = 0.25) +
            ggplot2::scale_color_manual(values = color_map_PCoA) +
            ggplot2::scale_fill_manual(values = color_map_PCoA)+
            ggplot2::labs(x = xlab, y = ylab)+
            ggplot2::theme_bw()+
            ggplot2::theme(
              plot.title = ggplot2::element_text(size = 12, hjust = 0.5),
              panel.border = ggplot2::element_rect(colour = "black", size = 2),
              axis.ticks = ggplot2::element_line(color = "black", size = 2),
              legend.text = ggplot2::element_text(size = 16),
              axis.text = ggplot2::element_text(size = 16,colour = "black"),
              axis.title = ggplot2::element_text(size = 16,colour = "black"),
              panel.grid.major = ggplot2::element_line(color = "#EBEBEB", size = 0.5),
              panel.grid.minor = ggplot2::element_line(color = "#EBEBEB", size = 0.2)
            )
          ggplot2::ggsave(file, plot = pcoa_plot, device = "pdf", width = 8, height = 6)
        }
      )
    })
  })
}
