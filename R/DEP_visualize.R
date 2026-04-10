#' DEP Visualization User Interface
#' Creates a user interface for visualizing DEP (Differentially Expressed Proteins)
#' in a Shiny application.
#' @title DEP_visualize_ui
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom colourpicker colourInput
#' @name DEP_visualize_ui
#' @export
#'
DEP_visualize_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = 'DEP visualize',
    icon = bsicons::bs_icon("play-circle"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('SampleInfo'),
            label = 'Expression matrix',
            multiple = FALSE,
            accept = '.csv'
          ),
          shiny::fileInput(
            inputId = ns('GroupInfo'),
            label = 'Group information',
            multiple = FALSE,
            accept = '.csv'
          ),
          shiny::textInput(
            inputId = ns("LogFC"),
            label = "LogFC:",
            value = "1"
          ),
          shiny::textInput(
            inputId = ns("Pvalue"),
            label = "Pvalue:",
            value = "0.05"
          ),
          # selectInput(
          #   inputId = ns("param_select"),
          #   label = "Select Parameter",
          #   choices = c("Parameter 1", "Parameter 2", "Parameter 3"),
          #   selected = "Parameter 1"
          # ),
          shiny::actionButton(ns("run_button"), "Run")
        ),
        bslib::accordion_panel(
          title = "Download Figure",
          icon = bsicons::bs_icon("download"),
          shiny::textInput(
            inputId = ns("height"),
            label = "Height",
            placeholder = "Enter height..."
          ),
          shiny::textInput(
            inputId = ns("width"),
            label = "Width",
            placeholder = "Enter width..."
          ),
          shiny::selectInput(
            inputId = ns("Units"),
            label = "Select Unit",
            choices = c("in", "cm", "mm", "px"),
            selected = "in"
          ),
          shiny::downloadButton(ns("download_data"), "Download")
        )
      ),
      # 2x2 布局
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,
          height = 600,
          shiny::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Volcano plot",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::selectInput(ns("pvalue_col"), "Select P-value column:", choices = NULL),
                shiny::selectInput(ns("log2fc_col"), "Select log2 Fold Change column:", choices = NULL),
                shiny::selectInput(ns("vip_col"), "Select VIP column:", choices = NULL),
                shiny::numericInput(ns("pvalue_threshold"), "P-value threshold:", value = 0.05),
                colourpicker::colourInput(ns("pvalue_line_color"), "P-value Line Color:", value = "black"),  # P-value line color (default black)
                shiny::numericInput(ns("log2fc_threshold"), "log2 Fold Change threshold:", value = 1),
                colourpicker::colourInput(ns("log2fc_line_color"), "log2 Fold Change Line Color:", value = "black"),  # log2 Fold Change line color (default black)

                # Point color inputs for upregulated and downregulated points
                colourpicker::colourInput(ns("upregulated_color"), "Upregulated Points Color:", value = "red"),  # Upregulated points color
                colourpicker::colourInput(ns("downregulated_color"), "Downregulated Points Color:", value = "blue"),  # Downregulated points color
                colourpicker::colourInput(ns("not_significant_color"), "Not Significant Points Color:", value = "gray"),  # Not significant points color
                # Axis range toggle and inputs
                shiny::checkboxInput(ns("use_x_range"), "Set X-axis range", value = FALSE),
                shiny::conditionalPanel(
                  condition = paste0("input['", ns("use_x_range"), "']"),
                  shiny::numericInput(ns("x_min"), "X-axis minimum:", value = -3),
                  shiny::numericInput(ns("x_max"), "X-axis maximum:", value = 3)
                ),
                shiny::checkboxInput(ns("use_y_range"), "Set Y-axis range", value = FALSE),
                shiny::conditionalPanel(
                  condition = paste0("input['", ns("use_y_range"), "']"),
                  shiny::numericInput(ns("y_min"), "Y-axis minimum:", value = 0),
                  shiny::numericInput(ns("y_max"), "Y-axis maximum:", value = 10)
                ),
                # Run button
                shiny::actionButton(ns("run_btn_Volcano"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                # Download format selection
                shiny::radioButtons(ns("file_format"), "Choose file format:",
                                    choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
                                    selected = "png"),
                # Download button
                shiny::downloadButton(ns("download_plot"), label = "Download")
              )
            ),
            mainPanel(
              shiny::plotOutput(ns("output_ui"))
            )
          ),
          shiny::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Fold Change Density Plot",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::selectInput(ns("log2fc_col_Density"), "Select log2 Fold Change column:", choices = NULL),
                shiny::actionButton(ns("run_btn_Density"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("fig2_download"), label = "Output Table", icon = shiny::icon("download"))
              )
            ),
            mainPanel(
              shiny::plotOutput(ns("output_ui2"))
            )
          ),
          shiny::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Venn Diagram of DEP Between Groups",
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
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("fig2_download"), label = "Output Table", icon = shiny::icon("download"))
              )
            ),
            mainPanel(
              shiny::plotOutput(ns("plot2"))
            )
          ),
          shiny::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "DEP Functional Enrichment Analysis",
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
                title = 'Download',
                icon = bsicons::bs_icon('download'),
                shiny::downloadButton(ns("fig2_download"), label = "Output Table", icon = shiny::icon("download"))
              )
            ),
            mainPanel(
              shiny::plotOutput(ns("plot2"))
            )
          )
        )
      )
    )
  )
}
#' TMT Server Logic
#'
#' Defines the server-side logic for the TMT (Tandem Mass Tag) analysis section
#' in a Shiny application, including plot generation based on user input.
#'
#' @param input Shiny input values.
#' @param output Shiny output values.
#' @param session Shiny session object.
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @name DEP_visualize_server
#' @export
#'
utils::globalVariables(c("mtcars", "wt", "hp", "drat"))

DEP_visualize_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 获取命名空间
    # Reactive value to store the data
    data <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$SampleInfo, {
      shiny::req(input$SampleInfo)
      # data(read.csv(input$SampleInfo$datapath))
      # 使用 reactive 处理数据加载
      data <- reactive({
        shiny::req(input$SampleInfo)  # 确保输入文件存在
        utils::read.csv(input$SampleInfo$datapath)
      })
      # Update select input choices based on data columns
      shiny::updateSelectInput(session, ns("pvalue_col"), choices = base::names(data()))
      shiny::updateSelectInput(session, ns("log2fc_col"), choices = base::names(data()))
      shiny::updateSelectInput(session, ns("vip_col"), choices = base::names(data()))
      shiny::updateSelectInput(session, ns("log2fc_col_Density"), choices = base::names(data()))
    })
    # Generate the volcano plot based on user inputs
    plot_reactive <- shiny::reactive({
      shiny::req(input$pvalue_col, input$log2fc_col, input$vip_col)
      # Data cleaning
      data_clean <- data()
      data_clean[[input$pvalue_col]] <- base::suppressWarnings(base::as.numeric(base::as.character(data_clean[[input$pvalue_col]])))
      data_clean[[input$log2fc_col]] <- base::suppressWarnings(base::as.numeric(base::as.character(data_clean[[input$log2fc_col]])))
      data_clean[[input$vip_col]] <- base::suppressWarnings(base::as.numeric(base::as.character(data_clean[[input$vip_col]])))
      # Check if any selected columns contain NA after conversion
      if (base::any(base::is.na(data_clean[[input$pvalue_col]]))) {
        stop("Error: P-value column contains non-numeric values that could not be converted.")
      }
      if (base::any(base::is.na(data_clean[[input$log2fc_col]]))) {
        stop("Error: log2 Fold Change column contains non-numeric values that could not be converted.")
      }
      if (base::any(base::is.na(data_clean[[input$vip_col]]))) {
        stop("Error: VIP column contains non-numeric values that could not be converted.")
      }
      # Categorize points based on thresholds
      data_clean$category <- ifelse(data_clean[[input$log2fc_col]] > input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Up",
                                    ifelse(data_clean[[input$log2fc_col]] < -input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Down", "Not Significant"))

      # Start building the plot
      plot <- ggplot2::ggplot(data_clean, ggplot2::aes_string(x = input$log2fc_col, y = paste0("-log10(", input$pvalue_col, ")"), size = input$vip_col, color = "category")) +
        ggplot2::geom_point() +
        ggplot2::scale_color_manual(values = c("Up" = input$upregulated_color, "Down" = input$downregulated_color, "Not Significant" = input$not_significant_color)) +
        ggplot2::geom_hline(yintercept = -log10(input$pvalue_threshold), linetype = "dashed", color = input$pvalue_line_color) +  # P-value threshold line
        ggplot2::geom_vline(xintercept = c(-input$log2fc_threshold, input$log2fc_threshold), linetype = "dashed", color = input$log2fc_line_color) +  # log2 Fold Change threshold lines
        ggplot2::theme_bw() +
        ggplot2::labs(x = "log2 Fold Change", y = "-log10(P-value)",
                      title = "Volcano Plot", color = "Category", size = "VIP")
      # Apply X-axis range if toggle is enabled
      if (input$use_x_range) {
        plot <- plot + ggplot2::xlim(input$x_min, input$x_max)
      }

      # Apply Y-axis range if toggle is enabled
      if (input$use_y_range) {
        plot <- plot + ggplot2::ylim(input$y_min, input$y_max)
      }
      plot
    })

    # Render the volcano plot
    output$output_ui <- shiny::renderPlot({
      shiny::req(input$run_btn_Volcano)  # 确保按下 Run 按钮后才渲染
      plot_reactive()  # 直接返回图形对象
    })
    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        base::paste("volcano_plot", base::Sys.Date(), ".", input$file_format, sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = plot_reactive(), device = input$file_format, width = 8, height = 6, units = "in")
      }
    )
    # Fold Change Density Plot ------------------------------------------------
    plot_reactive2 <- shiny::reactive({
      shiny::req(input$log2fc_col_Density)
      # data clean
      data_clean <- data()
      data_clean[[input$log2fc_col_Density]] <- base::suppressWarnings(base::as.numeric(base::as.character(data_clean[[input$log2fc_col_Density]])))
      if (base::any(base::is.na(data_clean[[input$log2fc_col_Density]]))) {
        stop("Error: log2 Fold Change column for density plot contains non-numeric values that could not be converted.")
      }
      plot <- ggplot2::ggplot(data_clean, ggplot2::aes(x = data_clean[[input$log2fc_col_Density]])) +
        ggplot2::geom_density(fill = "blue", alpha = 0.5) +
        ggplot2::labs(x = "log2FC", y = "Density") +
        ggplot2::theme_bw()
      plot
    })

    output$output_ui2 <- shiny::renderPlot({
      shiny::req(input$run_btn_Density)
      plot_reactive2()
    })
  })
}
