#' Veen plot UI Module
#' @description Veen plot UI Module
#' @param id A unique identifier for the Shiny namespace, Veen plot
#' @title veen_ui
#' @name veen_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#'
veen_ui <- function(id){
  ns <- NS(id)
  nav_panel(
    title = 'Venn',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('file'),
            label = 'File',
            multiple = FALSE,
            accept = '.csv'
          )
          )
        ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Venn plot",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                uiOutput(ns("colorSelectors"))
                ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                downloadButton(ns("downloadPlot"), "Download")
              )
              ),
            mainPanel(
              plotOutput(ns("veen_plot"))
            )
            )
          )
        )
      )
    )
}



#' Veen plot Server Module
#' @description Server logic for Veen plot
#' @title veen_server
#' @name veen_server
#' @param id Standard shiny server arguments
#' @import shiny
#' @import utils
#' @import tidyr
#' @export
#'
utils::globalVariables(c("Name", "Set"))
veen_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 保存reactive数据
    reactive_data <- reactiveValues()

    # 动态生成颜色选择器
    observeEvent(input$file, {
      req(input$file)
      data <- utils::read.csv(input$file$datapath)
      output$colorSelectors <- renderUI({
        lapply(seq_along(colnames(data)), function(i) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_", i)),
            label = paste("Select Color for", colnames(data)[i]),
            value = sample(colors(), 1)
          )
        })
      })
    })

    observeEvent(input$run, {
      req(input$file)
      data <- utils::read.csv(input$file$datapath)

      # Prepare data for plotting
      long_df <- data %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Set", values_to = "Name") %>%
        dplyr::distinct(Name, Set) %>%
        tidyr::pivot_wider(names_from = Set, values_from = Set, values_fill = list(Set = "0")) %>%
        dplyr::mutate(across(-Name, ~ifelse(. == "0", 0, 1))) %>%
        tibble::column_to_rownames("Name")

      # Save reactive data
      reactive_data$set_list <- as.list(data)
      reactive_data$upset_data <- long_df
      reactive_data$colors <- sapply(seq_along(colnames(data)), function(i) input[[paste0("color_", i)]])

      # Render plot based on color count
      output$veen_plot <- renderPlot({
        if (length(reactive_data$colors) <= 4) {
          ggvenn::ggvenn(reactive_data$set_list, fill_color = reactive_data$colors)
        } else {
          venn::venn(
            reactive_data$upset_data,
            ilabels = TRUE, box = FALSE, ilcs = 1, sncs = 1.2,
            lwd = 4, lty = 1, col = reactive_data$colors, zcolor = reactive_data$colors
          )
        }
      })
    })

    # 下载PDF功能
    output$downloadPlot <- downloadHandler(
      filename = function() paste("veen_plot", Sys.Date(), ".pdf", sep = ""),
      content = function(file) {
        if (length(reactive_data$colors) <= 4) {
          ggplot2::ggsave(file, plot = ggvenn::ggvenn(reactive_data$set_list, fill_color = reactive_data$colors),
                 width = 8, height = 6, dpi = 300, device = "pdf")
        } else {
          grDevices::pdf(file, width = 8, height = 6)
          venn::venn(reactive_data$upset_data, ilabels = TRUE, box = FALSE, ilcs = 1, sncs = 1.2,
               lwd = 4, lty = 1, col = reactive_data$colors, zcolor = reactive_data$colors)
          dev.off()
        }
      }
    )
  })
}
