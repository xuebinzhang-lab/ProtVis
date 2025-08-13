#' DEP Visualization User Interface
#'
#' Creates a user interface for visualizing DEP (Differentially Expressed Proteins)
#' in a Shiny application.
#' @name DEP_visualize_ui
#' @title DEP_visualize_ui
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @export
DEP_visualize_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'DEP visualize',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('SampleInfo'),
            label = 'Expression matrix',
            multiple = FALSE,
            accept = '.csv'
          ),
          fileInput(
            inputId = ns('GroupInfo'),
            label = 'Group information',
            multiple = FALSE,
            accept = '.csv'
          ),
          textInput(
            inputId = "LogFC",
            label = "LogFC:",
            value = "1"
          ),
          textInput(
            inputId = "Pvalue",
            label = "Pvalue:",
            value = "0.05"
          ),
          # selectInput(
          #   inputId = ns("param_select"),
          #   label = "Select Parameter",
          #   choices = c("Parameter 1", "Parameter 2", "Parameter 3"),
          #   selected = "Parameter 1"
          # ),
          actionButton(ns("run_button"), "Run")
        ),
        accordion_panel(
          title = "Download Figure",
          icon = bs_icon("download"),
          textInput(
            inputId = ns("height"),
            label = "Height",
            placeholder = "Enter height..."
          ),
          textInput(
            inputId = ns("width"),
            label = "Width",
            placeholder = "Enter width..."
          ),
          selectInput(
            inputId = ns("Units"),
            label = "Select Unit",
            choices = c("in", "cm", "mm", "px"),
            selected = "in"
          ),
          downloadButton(ns("download_data"), "Download")
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
            title = "Volcano plot",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput(ns("pvalue_col"), "Select P-value column:", choices = NULL),
                selectInput(ns("log2fc_col"), "Select log2 Fold Change column:", choices = NULL),
                selectInput(ns("vip_col"), "Select VIP column:", choices = NULL),
                numericInput(ns("pvalue_threshold"), "P-value threshold:", value = 0.05),
                colourpicker::colourInput(ns("pvalue_line_color"), "P-value Line Color:", value = "black"),  # P-value line color (default black)
                numericInput(ns("log2fc_threshold"), "log2 Fold Change threshold:", value = 1),
                colourpicker::colourInput(ns("log2fc_line_color"), "log2 Fold Change Line Color:", value = "black"),  # log2 Fold Change line color (default black)

                # Point color inputs for upregulated and downregulated points
                colourpicker::colourInput(ns("upregulated_color"), "Upregulated Points Color:", value = "red"),  # Upregulated points color
                colourpicker::colourInput(ns("downregulated_color"), "Downregulated Points Color:", value = "blue"),  # Downregulated points color
                colourpicker::colourInput(ns("not_significant_color"), "Not Significant Points Color:", value = "gray"),  # Not significant points color
                # Axis range toggle and inputs
                checkboxInput(ns("use_x_range"), "Set X-axis range", value = FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("use_x_range"), "']"),
                  numericInput(ns("x_min"), "X-axis minimum:", value = -3),
                  numericInput(ns("x_max"), "X-axis maximum:", value = 3)
                ),
                checkboxInput(ns("use_y_range"), "Set Y-axis range", value = FALSE),
                conditionalPanel(
                  condition = paste0("input['", ns("use_y_range"), "']"),
                  numericInput(ns("y_min"), "Y-axis minimum:", value = 0),
                  numericInput(ns("y_max"), "Y-axis maximum:", value = 10)
                ),
                # Run button
                actionButton(ns("run_btn_Volcano"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                # Download format selection
                radioButtons(ns("file_format"), "Choose file format:",
                             choices = c("PDF" = "pdf", "PNG" = "png", "JPG" = "jpg", "SVG" = "svg"),
                             selected = "png"),
                # Download button
                downloadButton(ns("download_plot"), label = "Download")
              )
            ),
            mainPanel(
              plotOutput(ns("output_ui"))
            )
          ),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Fold Change Density Plot",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                selectInput(ns("log2fc_col_Density"), "Select log2 Fold Change column:", choices = NULL),
                actionButton(ns("run_btn_Density"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                icon = bs_icon('download'),
                downloadButton(ns("fig2_download"), label = "Output Table", icon = icon("download"))
              )
            ),
            mainPanel(
              plotOutput(ns("output_ui2"))
            )
          ),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Venn Diagram of DEP Between Groups",
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
                title = 'Download',
                icon = bs_icon('download'),
                downloadButton(ns("fig2_download"), label = "Output Table", icon = icon("download"))
              )
            ),
            mainPanel(
              plotOutput(ns("plot2"))
            )
          ),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "DEP Functional Enrichment Analysis",
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
                title = 'Download',
                icon = bs_icon('download'),
                downloadButton(ns("fig2_download"), label = "Output Table", icon = icon("download"))
              )
            ),
            mainPanel(
              plotOutput(ns("plot2"))
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
utils::globalVariables(c("mtcars", "wt", "hp", "drat"))
DEP_visualize_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 获取命名空间
    # Reactive value to store the data
    data <- reactiveVal(NULL)
    observeEvent(input$SampleInfo, {
      req(input$SampleInfo)
      # data(read.csv(input$SampleInfo$datapath))
      # 使用 reactive 处理数据加载
      data <- reactive({
        req(input$SampleInfo)  # 确保输入文件存在
        read.csv(input$SampleInfo$datapath)
      })
      # Update select input choices based on data columns
      updateSelectInput(session, "pvalue_col", choices = names(data()))
      updateSelectInput(session, "log2fc_col", choices = names(data()))
      updateSelectInput(session, "vip_col", choices = names(data()))
      updateSelectInput(session, "log2fc_col_Density", choices = names(data()))
    })
    # Generate the volcano plot based on user inputs
    plot_reactive <- reactive({
      req(input$pvalue_col, input$log2fc_col, input$vip_col)

      # Data cleaning
      data_clean <- data()
      data_clean[[input$pvalue_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$pvalue_col]])))
      data_clean[[input$log2fc_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$log2fc_col]])))
      data_clean[[input$vip_col]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$vip_col]])))

      # Check if any selected columns contain NA after conversion
      if (any(is.na(data_clean[[input$pvalue_col]]))) {
        stop("Error: P-value column contains non-numeric values that could not be converted.")
      }
      if (any(is.na(data_clean[[input$log2fc_col]]))) {
        stop("Error: log2 Fold Change column contains non-numeric values that could not be converted.")
      }
      if (any(is.na(data_clean[[input$vip_col]]))) {
        stop("Error: VIP column contains non-numeric values that could not be converted.")
      }

      # Categorize points based on thresholds
      data_clean$category <- ifelse(data_clean[[input$log2fc_col]] > input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Up",
                                    ifelse(data_clean[[input$log2fc_col]] < -input$log2fc_threshold & data_clean[[input$pvalue_col]] < input$pvalue_threshold, "Down", "Not Significant"))

      # Start building the plot
      plot <- ggplot(data_clean, aes_string(x = input$log2fc_col, y = paste0("-log10(", input$pvalue_col, ")"), size = input$vip_col, color = "category")) +
        geom_point() +
        scale_color_manual(values = c("Up" = input$upregulated_color, "Down" = input$downregulated_color, "Not Significant" = input$not_significant_color)) +
        geom_hline(yintercept = -log10(input$pvalue_threshold), linetype = "dashed", color = input$pvalue_line_color) +  # P-value threshold line
        geom_vline(xintercept = c(-input$log2fc_threshold, input$log2fc_threshold), linetype = "dashed", color = input$log2fc_line_color) +  # log2 Fold Change threshold lines
        theme_bw() +
        labs(x = "log2 Fold Change", y = "-log10(P-value)",
             title = "Volcano Plot", color = "Category", size = "VIP")

      # Apply X-axis range if toggle is enabled
      if (input$use_x_range) {
        plot <- plot + xlim(input$x_min, input$x_max)
      }

      # Apply Y-axis range if toggle is enabled
      if (input$use_y_range) {
        plot <- plot + ylim(input$y_min, input$y_max)
      }

      plot
    })

    # Render the volcano plot
    output$output_ui <- renderPlot({
      req(input$run_btn_Volcano)  # 确保按下 Run 按钮后才渲染
      plot_reactive()  # 直接返回图形对象
    })
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("volcano_plot", Sys.Date(), ".", input$file_format, sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot_reactive(), device = input$file_format, width = 8, height = 6, units = "in")
      }
    )

# Fold Change Density Plot ------------------------------------------------
    plot_reactive2 <- reactive({
      req(input$log2fc_col_Density)

      # 数据清理
      data_clean <- data()
      data_clean[[input$log2fc_col_Density]] <- suppressWarnings(as.numeric(as.character(data_clean[[input$log2fc_col_Density]])))

      # 检查是否存在 NA
      if (any(is.na(data_clean[[input$log2fc_col_Density]]))) {
        stop("Error: log2 Fold Change column for density plot contains non-numeric values that could not be converted.")
      }

      # 绘制折线密度图
      plot <- ggplot(data_clean, aes(x = data_clean[[input$log2fc_col_Density]])) +
        geom_density(fill = "blue", alpha = 0.5) +
        labs(x = "log2FC", y = "Density") +
        theme_bw()

      plot
    })

    output$output_ui2 <- renderPlot({
      req(input$run_btn_Density)  # 确保按下 Run 按钮后才渲染
      plot_reactive2()  # 直接返回图形对象
    })

  })
}
