#' TMT User Interface
#'
#' Creates a user interface for the TMT (Tandem Mass Tag) analysis section
#' in a Shiny application. This includes file upload, parameter selection,
#' and visualization options.
#'
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @export
TMT_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'TMT',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('SampleInfo'),
            label = 'Upload Table',
            multiple = FALSE,
            accept = '.csv'
          ),
          selectInput(
            inputId = ns("param_select"),
            label = "Select Parameter",
            choices = c("Parameter 1", "Parameter 2", "Parameter 3"),
            selected = "Parameter 1"
          ),
          actionButton(ns("run_button"), "Run")
        ),
        accordion_panel(
          title = "Download Figure",
          icon = bs_icon("download"),
          textInput(
            inputId = ns("height"),
            label = "Height",
            value = 6,
            placeholder = "Enter height..."
          ),
          textInput(
            inputId = ns("width"),
            label = "Width",
            value = 6,
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
      page_fluid(
        navset_card_tab(
          title = "Tab 1",
          sidebar = accordion(
            accordion_panel(
              title = 'Parameter',
              radioButtons(inputId = ns("Logical_value1"),
                           label = "Logical value",
                           choices = c("TRUE", "FALSE"),
                           selected = "TRUE")
            ),
            accordion_panel(
              title = 'Download',
              icon = bs_icon('download'),
              downloadButton(ns("fig1_download"), label = "Output Table", icon = icon("download"))
            )
          ),
          mainPanel(
            plotOutput(ns("plot1"))
          )
        ),
        navset_card_tab(
          title = "Tab 2",
          sidebar = accordion(
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
}


# -------------------------------------------------------------------------

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
#' @importFrom utils head read.csv write.csv
#' @name TMT_server
#' @export
utils::globalVariables(c("mtcars", "wt", "hp", "drat"))
TMT_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # 获取命名空间

    # 读取上传的 CSV 文件
    data_uploaded <- reactive({
      req(input$SampleInfo)  # 确保文件已上传

      # 打印文件路径，帮助调试
      print(paste("File uploaded:", input$SampleInfo$datapath))

      # 尝试读取 CSV 文件
      tryCatch({
        data <- read.csv(input$SampleInfo$datapath)
        print("File loaded successfully!")
        return(data)
      }, error = function(e) {
        print(paste("Error in reading file:", e))
        return(NULL)
      })
    })

    # 渲染 plot1
    output$plot1 <- renderPlot({
      req(input$run_button)  # 确保按钮被点击
      data <- data_uploaded()  # 获取上传的数据

      # 打印上传的文件前几行
      print(head(data))  # 用于调试，检查文件是否正确上传

      # 检查数据是否为空
      req(nrow(data) > 0)

      # 检查列名是否存在，假设列名为 wt 和 hp（根据实际情况调整）
      req("wt" %in% colnames(data), "hp" %in% colnames(data))  # 如果没有这些列，则报错

      ggplot2::ggplot(data, aes(x = wt, y = hp)) +
        geom_point() +
        ggtitle("Plot 1: Weight vs Horsepower")
    })

    # 渲染 plot2
    output$plot2 <- renderPlot({
      req(input$run_button)  # 确保按钮被点击
      data <- data_uploaded()  # 获取上传的数据

      # 检查数据是否为空
      req(nrow(data) > 0)

      # 检查列名是否存在，假设列名为 wt 和 drat（根据实际情况调整）
      req("wt" %in% colnames(data), "drat" %in% colnames(data))  # 如果没有这些列，则报错

      ggplot(data, aes(x = wt, y = drat)) +
        geom_point(col = "red") +
        ggtitle("Plot 2: Weight vs Drat")
    })

    # 下载图形1
    output$fig1_download <- downloadHandler(
      filename = function() {
        paste("plot1-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), width = as.numeric(input$width), height = as.numeric(input$height), units = input$Units)
      }
    )

    # 下载图形2
    output$fig2_download <- downloadHandler(
      filename = function() {
        paste("plot2-", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), width = as.numeric(input$width), height = as.numeric(input$height), units = input$Units)
      }
    )

    # 下载数据文件
    output$download_data <- downloadHandler(
      filename = function() {
        paste("uploaded_data-", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(data_uploaded(), file)
      }
    )
  })
}





