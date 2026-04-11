#' Multivariate Summary UI Module
#' @description Multivariate Summary UI Module
#' @param id A unique identifier for the Shiny namespace.
#' @title mv_summary_ui
#' @name mv_summary_ui
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom colourpicker colourInput
#' @export
#'

mv_summary_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    # title = 'Multivariate Summary',
    title = 'Data normalization',
    icon = bsicons::bs_icon("play-circle"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(ns("file"), "Upload CSV file", accept = ".csv"),
          shiny::fileInput(
            inputId = ns('GroupInfo'),
            label = 'Group information',
            multiple = FALSE,
            accept = '.csv'
          )
        )
      ),
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # boxplot -----------------------------------------------------------------
            title = "Boxplot",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(ns("boxplot_color"),
                                          "Color:",
                                          value = "black"),
                shiny::actionButton(ns("boxplot"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("boxplot"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                shiny::numericInput(ns("boxplot_height"),
                             label = "Height:",
                             value = 7,
                             step = 0.1),
                shiny::numericInput(ns("boxplot_width"),label = "Width:",
                             value = 7, step = 1),
                shiny::downloadButton(ns("download_boxplot"), "Download")
              )
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                type = "tabs", # This allows tab navigation
                shiny::tabPanel(
                  title = "Figure before normalization",
                  shiny::plotOutput(ns("boxplotbeforeshow"))
                ),
                shiny::tabPanel(
                  title = "Figure after normalization",
                  shiny::plotOutput(ns("boxplotaftershow"))
                )
              )
            )
          ),
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # PCA ---------------------------------------------------------------------
            title = "PCA",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Run',
                actionButton(ns("run_btn_PCA"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("download_PCA_table"), label = "Output Table", icon = shiny::icon("download"))
              )
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                type = "tabs", # This allows tab navigation
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
          ),
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # correlation -------------------------------------------------------------
            title = "Correlation",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::selectInput("method", "Correlation Method",
                            choices = c("pearson", "kendall", "spearman"), selected = "pearson")
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("calculate"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::numericInput(ns("correlationPlotHeight"), "Height:",
                             value = 8, min = 1, max = 100, step = 1),
                shiny::numericInput(ns("correlationPlotWidth"), "Width:",
                             value = 8, min = 1, max = 100, step = 1),
                shiny::downloadButton(ns("Figure_correlation_download"), label = "Download Figure",
                               icon = icon("download")),
                shiny::downloadButton(ns("table_correlation_download"), label = "Download Table",
                               icon = icon("download"))
              )
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                type = "tabs", # This allows tab navigation
                shiny::tabPanel(
                  title = "Figure",
                  shiny::plotOutput(ns("correlationPlot"))
                ),
                shiny::tabPanel(
                  title = "Table",
                  DT::DTOutput(ns("correlation_dataTable"))
                )
              )
            )
          ),
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            # heatmap -----------------------------------------------------------------
            title = "Heatmap",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::radioButtons(inputId = ns("Logical_value2"),
                             label = "Logical value",
                             choices = c("TRUE", "FALSE"),
                             selected = "TRUE")
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("plotheatmap"), "Run")
              )
            ),
            shiny::mainPanel(
              shiny::plotOutput(ns("correlationPlotshow"))
            )
          )
        )
      )
    )
  )
}

#' @title mv_summary_server
#' @name mv_summary_server
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @importFrom utils read.csv write.csv head
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr everything mutate left_join
#' @importFrom tibble column_to_rownames
#' @importFrom DT renderDT datatable
#' @importFrom grDevices pdf dev.off
#' @importFrom preprocessCore normalize.quantiles
#' @importFrom PCAtools pca biplot
#' @importFrom corrplot corrplot
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom circlize colorRamp2
#' @export
#'

utils::globalVariables(c("value","Sample","Group"))
mv_summary_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # boxplot -----------------------------------------------------------------
    data <- shiny::reactive({
      shiny::req(input$file)
      utils::read.csv(input$file$datapath, row.names = 1)
    })
    group <- shiny::reactive({
      shiny::req(input$GroupInfo)
      utils::read.csv(input$GroupInfo$datapath)
    })
    shiny::observeEvent(input$boxplot, {
      output$boxplotbeforeshow <- shiny::renderPlot({
        shiny::req(data())
        shiny::req(group())
        boxplot <- data() %>%
          tidyr::pivot_longer(cols = dplyr::everything(),
                              names_to = "Sample",
                              values_to = "value") %>%
          dplyr::mutate(value = base::log10(value)) %>%
          dplyr::left_join(group(),by = "Sample")
        ggplot2::ggplot(boxplot, ggplot2::aes(x = Sample, y = value, fill = Group)) +
          ggplot2::geom_boxplot(color = input$boxplot_color) +
          ggplot2::labs(x = "", y = "Log10(Protein Abundance)") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(color = "black"),
            axis.title = ggplot2::element_text(color = "black"),
            axis.text = ggplot2::element_text(color = "black"),
            legend.text = ggplot2::element_text(color = "black"),
            legend.title = ggplot2::element_text(color = "black")
          )
      })
    })
    shiny::observeEvent(input$boxplot, {
      output$boxplotaftershow <- shiny::renderPlot({
        shiny::req(data())
        shiny::req(group())
        boxplot <- data() %>%
          base::as.matrix() %>%
          preprocessCore::normalize.quantiles() %>%
          base::as.data.frame() %>%
          stats::setNames(base::colnames(data())) %>%
          tidyr::pivot_longer(cols = dplyr::everything(),
                              names_to = "Sample",
                              values_to = "value") %>%
          dplyr::mutate(value = base::log10(value)) %>%
          dplyr::left_join(group(),by = "Sample")
        ggplot2::ggplot(boxplot, ggplot2::aes(x = Sample, y = value, fill = Group)) +
          ggplot2::geom_boxplot(color = input$boxplot_color) +
          ggplot2::labs(x = "", y = "Log10(Protein Abundance)") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(color = "black"),
            axis.title = ggplot2::element_text(color = "black"),
            axis.text = ggplot2::element_text(color = "black"),
            legend.text = ggplot2::element_text(color = "black"),
            legend.title = ggplot2::element_text(color = "black")
          )
      })
    })
    output$download_boxplot <- shiny::downloadHandler(
      filename = function() {
        base::paste("boxplot_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        grDevices::pdf(file, width = input$boxplot_width, height = input$boxplot_height)
        shiny::req(data())
        shiny::req(group())
        # before
        boxplot <- data() %>%
          tidyr::pivot_longer(cols = dplyr::everything(),
                              names_to = "Sample",
                              values_to = "value") %>%
          dplyr::mutate(value = base::log10(value)) %>%
          dplyr::left_join(group(),by = "Sample")
        p1 <- ggplot2::ggplot(boxplot, ggplot2::aes(x = Sample, y = value, fill = Group)) +
          ggplot2::geom_boxplot(color = input$boxplot_color) +
          ggplot2::labs(x = "", y = "Log10(Protein Abundance)") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(color = "black"),
            axis.title = ggplot2::element_text(color = "black"),
            axis.text = ggplot2::element_text(color = "black"),
            legend.text = ggplot2::element_text(color = "black"),
            legend.title = ggplot2::element_text(color = "black")
          )
        # after
        boxplot <- data() %>%
          base::as.matrix() %>%
          preprocessCore::normalize.quantiles() %>%
          base::as.data.frame() %>%
          stats::setNames(colnames(data())) %>%
          tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Sample", values_to = "value") %>%
          dplyr::mutate(value = base::log10(value)) %>%
          dplyr::left_join(group(), by = "Sample")
        p2 <- ggplot2::ggplot(boxplot, ggplot2::aes(x = Sample, y = value, fill = Group)) +
          ggplot2::geom_boxplot(color = input$boxplot_color) +
          ggplot2::labs(x = "", y = "Log10(Protein Abundance)") +
          ggplot2::theme_bw() +
          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(color = "black"),
            axis.title = ggplot2::element_text(color = "black"),
            axis.text = ggplot2::element_text(color = "black"),
            legend.text = ggplot2::element_text(color = "black"),
            legend.title = ggplot2::element_text(color = "black")
          )
        print(p1)
        print(p2)
        grDevices::dev.off()
      }
    )

    # PCA ---------------------------------------------------------------------
    shiny::observeEvent(input$run_btn_PCA, {
      output$PCAplotshow <- shiny::renderPlot({
        shiny::req(data())
        shiny::req(group())
        class <- group() %>%
          tibble::column_to_rownames("Sample")
        expr <- base::log10(data())
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
      output$PCA_dataTable <- DT::renderDT({
        shiny::req(data())
        shiny::req(group())
        class <- group() %>%
          tibble::column_to_rownames("Sample")
        expr <- base::log10(data())
        pca_data <- t(expr)
        pca <- PCAtools::pca(expr, metadata = class)
        DT::datatable(data.frame(pca[["rotated"]]))
      })
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
    # correlation ---------------------------------------------------------------------
    shiny::observeEvent(input$calculate, {
      shiny::req(data())
      method <- input$method
      correlation_matrix <- stats::cor(data(), method = method, use = "complete.obs")
      output$correlation_dataTable <- DT::renderDT({
        DT::datatable(correlation_matrix)
      })
      output$correlationPlot <- shiny::renderPlot({
        req(input$calculate)
        corrplot::corrplot(corr=correlation_matrix,order = "AOE",type="upper",tl.pos = "d")
        corrplot::corrplot(corr = correlation_matrix,add=TRUE, type="lower", method="number",
                           order="AOE",diag=FALSE,tl.pos="n", cl.pos="n",number.cex = 0.7)
      })
    })
    output$Figure_correlation_download <- shiny::downloadHandler(
      filename = function() {
        base::paste("correlation_plot", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        shiny::req(input$calculate)
        grDevices::pdf(file, width = input$correlationPlotWidth,
            height = input$correlationPlotHeight)
        corrplot::corrplot(cor(data(),method = input$method),order = "AOE",type="upper",tl.pos = "d")
        corrplot::corrplot(cor(data(),method = input$method),add=TRUE, type="lower", method="number",
                           order="AOE",diag=FALSE,tl.pos="n", cl.pos="n",number.cex = 0.7)
        grDevices::dev.off()
      }
    )
    output$table_correlation_download <- shiny::downloadHandler(
      filename = function() {
        base::paste("correlation_table", base::Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(input$calculate)
        utils::write.csv(stats::cor(data(), method = input$method, use = "complete.obs"), file, row.names = TRUE)
      }
    )
    # heatmap ---------------------------------------------------------------------
    shiny::observeEvent(input$plotheatmap, {
      output$correlationPlotshow <- shiny::renderPlot({
        shiny::req(data())
        data_normalized <- data() %>%
          utils::head(10) %>%
          base::as.matrix() %>%
          base::t() %>%
          base::scale() %>%
          base::t()
        ComplexHeatmap::Heatmap(
          data_normalized,
          col = circlize::colorRamp2(c(-2,0,2),c("green","white","red"))
        )
      })
    })
  })
}
