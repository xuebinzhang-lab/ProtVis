#' Missing value filling UI Module
#' @description Missing value filling UI Module
#' @param id A unique identifier for the Shiny namespace, Missing value filling.
#' @title mv_imputation_ui
#' @name mv_imputation_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
mv_imputation_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    # title = 'Multivariate Imputation',
    title = 'Data transformation',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('file'),
            label = 'Upload Expression Matrix',
            multiple = FALSE,
            accept = '.csv'
          )
        ),
        accordion_panel(
          title = "Method of Data Interpolation",
          selectInput(ns("choice_method"), "Select a method:",
                      choices = c("kNN", "RF", "Mean", "Median", "Zero", "Minimum"),
                      selected = "Mean")
        ),
        accordion_panel(
          title = "Run",
          actionButton(ns("runButton"), "Run")
        ),
        accordion_panel(
          title = "Download",
          downloadButton(ns("downloadData"), "Download")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Original Data",
            mainPanel(
              DT::DTOutput(ns("originalData"))
            )
            ),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Original Data visualize",
            mainPanel(
              plotOutput(ns("originalPlot"))
            )),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Imputed Data",
            mainPanel(
              DT::DTOutput(ns("imputedData"))
            )),
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Imputed Data visualize",
            mainPanel(
              plotOutput(ns("imputedPlot"))
            )
            )
          )
        )
    )
  )
}

#' Missing value filling Server Module
#' @description Server logic for Missing value filling
#' @title mv_imputation_server
#' @param id The module ID.
#' @name mv_imputation_server
#' @import shiny
#' @import utils
#' @import tidyverse
#' @import ggplot2
#' @export
#'
mv_imputation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 上传文件并读取数据
    data <- reactive({
      req(input$file)
      read.csv(input$file$datapath) %>%
        tibble::column_to_rownames("Accession")
    })

    # 点击按钮进行数据插补
    imputed_data <- eventReactive(input$runButton, {
      req(data())  # 确保数据存在
      method <- input$choice_method
      df <- as.data.frame(data())  # 转换成 data.frame，避免 mutate 报错

      if (method == "kNN") {
        return(as.data.frame(VIM::kNN(df, k = 5)))
      } else if (method == "RF") {
        return(as.data.frame(missForest::missForest(df)$ximp))
      } else if (method == "Mean") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))))
      } else if (method == "Median") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(is.na(.), median(., na.rm = TRUE), .))))
      } else if (method == "Zero") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(is.na(.), 0, .))))
      } else if (method == "Minimum") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(is.na(.), min(., na.rm = TRUE), .))))
      }
    })

    # 原始数据表
    output$originalData <- DT::renderDT({
      req(data())
      DT::datatable(data(), options = list(pageLength = 10, scrollX = TRUE))
    })

    # 原始数据可视化
    output$originalPlot <- renderPlot({
      req(data())
      visdat::vis_dat(as.data.frame(data())) +
        scale_fill_manual(
          values = c(
            "character" = "skyblue",
            "factor" = "lightgreen",
            "numeric" = "#E0F3F8",
            "logical" = "lightyellow",
            "NA" = "#BEBEBE"
          )
        )
    })

    # 插补后数据表（点击按钮后显示）
    output$imputedData <- DT::renderDT({
      req(imputed_data())
      DT::datatable(imputed_data(), options = list(pageLength = 10, scrollX = TRUE))
    })

    # 插补后数据可视化（点击按钮后显示）
    output$imputedPlot <- renderPlot({
      req(imputed_data())
      visdat::vis_dat(as.data.frame(imputed_data())) +
        scale_fill_manual(
          values = c(
            "character" = "skyblue",
            "factor" = "lightgreen",
            "numeric" = "#E0F3F8",
            "logical" = "lightyellow",
            "NA" = "#BEBEBE"
          )
        )
    })

    # 下载插补后的数据
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("imputed_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(imputed_data(), file, row.names = TRUE)
      }
    )
  })
}
