#' Venn Diagram UI Module
#'
#' Creates a user interface for generating Venn diagrams from uploaded CSV data.
#' Supports both traditional Venn diagrams (up to 4 sets) and UpSet-style diagrams
#' (5+ sets), with customizable colors and download functionality.
#'
#' @description Venn diagram visualization UI with file upload, color customization,
#'   and plot download capabilities.
#' @param id A unique identifier for the Shiny namespace.
#' @title venn_ui
#' @name venn_ui
#' @return A Shiny UI nav_panel containing the Venn diagram interface.
#' @import bsicons
#' @import shiny
#' @import bslib
#' @export
#' @examples
#' \dontrun{
#' ui <- venn_ui("venn_diagram")
#' }
venn_ui <- function(id){
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'Venn',
    icon = bsicons::bs_icon("play-circle"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('file'),
            label = 'File',
            multiple = FALSE,
            accept = '.csv'
          )
        )
      ),
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Venn plot",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                shiny::uiOutput(ns("colorSelectors"))
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("run"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                shiny::downloadButton(ns("downloadPlot"), "Download")
              )
            ),
            shiny::mainPanel(
              shiny::plotOutput(ns("venn_plot"))
            )
          )
        )
      )
    )
  )
}

#' Venn Diagram Server Module
#'
#' Server-side logic for the Venn diagram module. Handles CSV file processing,
#' dynamic color selection, Venn diagram generation (using ggvenn for ≤4 sets
#' or venn package for 5+ sets), and PDF download functionality.
#'
#' @description Server logic for generating Venn diagrams with automatic
#'   detection of set count to choose appropriate visualization method.
#' @title venn_server
#' @name venn_server
#' @param id Standard shiny server identifier.
#' @return A Shiny server module function.
#' @import shiny
#' @import utils
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @import grDevices
#' @import tibble
#' @export
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   venn_server("venn_diagram")
#' }
#' }

utils::globalVariables(c("Name", "Set", "everything", "across"))

venn_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 保存reactive数据
    reactive_data <- shiny::reactiveValues()

    # 动态生成颜色选择器
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      data <- utils::read.csv(input$file$datapath)
      output$colorSelectors <- shiny::renderUI({
        lapply(base::seq_along(base::colnames(data)), function(i) {
          colourpicker::colourInput(
            inputId = ns(base::paste0("color_", i)),
            label = base::paste("Select Color for", base::colnames(data)[i]),
            value = base::sample(base::colors(), 1)
          )
        })
      })
    })

    shiny::observeEvent(input$run, {
      shiny::req(input$file)
      data <- utils::read.csv(input$file$datapath)

      # Prepare data for plotting
      long_df <- data %>%
        tidyr::pivot_longer(cols = tidyr::everything(), names_to = "Set", values_to = "Name") %>%
        dplyr::distinct(Name, Set) %>%
        tidyr::pivot_wider(names_from = Set, values_from = Set, values_fill = base::list(Set = "0")) %>%
        dplyr::mutate(dplyr::across(-Name, ~base::ifelse(. == "0", 0, 1))) %>%
        tibble::column_to_rownames("Name")

      # Save reactive data
      reactive_data$set_list <- base::as.list(data)
      reactive_data$upset_data <- long_df
      reactive_data$colors <- base::sapply(base::seq_along(base::colnames(data)), function(i) input[[base::paste0("color_", i)]])

      # Render plot based on color count
      output$venn_plot <- shiny::renderPlot({
        if (base::length(reactive_data$colors) <= 4) {
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
    output$downloadPlot <- shiny::downloadHandler(
      filename = function() base::paste("venn_plot", base::Sys.Date(), ".pdf", sep = ""),
      content = function(file) {
        if (base::length(reactive_data$colors) <= 4) {
          ggplot2::ggsave(file, plot = ggvenn::ggvenn(reactive_data$set_list, fill_color = reactive_data$colors),
                          width = 8, height = 6, dpi = 300, device = "pdf")
        } else {
          grDevices::pdf(file, width = 8, height = 6)
          venn::venn(reactive_data$upset_data, ilabels = TRUE, box = FALSE, ilcs = 1, sncs = 1.2,
                     lwd = 4, lty = 1, col = reactive_data$colors, zcolor = reactive_data$colors)
          grDevices::dev.off()
        }
      }
    )
  })
}
