#' TMT User Interface
#' Creates a user interface for the TMT (Tandem Mass Tag) analysis section
#' in a Shiny application. This includes file upload, parameter selection,
#' and visualization options.
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @name TMT_ui
#' @export
#'
TMT_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'TMT',
    icon = bsicons::bs_icon("play-circle"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('SampleInfo'),
            label = 'Upload Table',
            multiple = FALSE,
            accept = '.csv'
          ),
          shiny::selectInput(
            inputId = ns("param_select"),
            label = "Select Parameter",
            choices = c("Parameter 1", "Parameter 2", "Parameter 3"),
            selected = "Parameter 1"
          ),
          shiny::actionButton(ns("run_button"), "Run")
        ),
        bslib::accordion_panel(
          title = "Download Figure",
          icon = bsicons::bs_icon("download"),
          shiny::textInput(
            inputId = ns("height"),
            label = "Height",
            value = 6,
            placeholder = "Enter height..."
          ),
          shiny::textInput(
            inputId = ns("width"),
            label = "Width",
            value = 6,
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
      bslib::page_fluid(
        bslib::navset_card_tab(
          title = "Tab 1",
          sidebar = bslib::accordion(
            bslib::accordion_panel(
              title = 'Parameter',
              shiny::radioButtons(inputId = ns("Logical_value1"),
                           label = "Logical value",
                           choices = c("TRUE", "FALSE"),
                           selected = "TRUE")
            ),
            bslib::accordion_panel(
              title = 'Download',
              icon = bsicons::bs_icon('download'),
              shiny::downloadButton(ns("fig1_download"), label = "Output Table", icon = shiny::icon("download"))
            )
          ),
          shiny::mainPanel(
            shiny::plotOutput(ns("plot1"))
          )
        ),
        bslib::navset_card_tab(
          title = "Tab 2",
          sidebar = bslib::accordion(
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
          shiny::mainPanel(
            shiny::plotOutput(ns("plot2"))
          )
        )
      )
    )
  )
}


# -------------------------------------------------------------------------

#' TMT Server Logic
#' Defines the server-side logic for the TMT (Tandem Mass Tag) analysis section
#' in a Shiny application, including plot generation based on user input.
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @importFrom utils read.csv head write.csv
#' @importFrom ggplot2 ggsave last_plot
#' @name TMT_server
#' @export
#'
utils::globalVariables(c("mtcars", "wt", "hp", "drat"))
TMT_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data_uploaded <- shiny::reactive({
      shiny::req(input$SampleInfo)
      print(base::paste("File uploaded:", input$SampleInfo$datapath))
      tryCatch({
        data <- utils::read.csv(input$SampleInfo$datapath)
        print("File loaded successfully!")
        return(data)
      }, error = function(e) {
        print(base::paste("Error in reading file:", e))
        return(NULL)
      })
    })
    output$plot1 <- shiny::renderPlot({
      shiny::req(input$run_button)
      data <- data_uploaded()
      print(utils::head(data))
      shiny::req(base::nrow(data) > 0)
      shiny::req("wt" %in% base::colnames(data), "hp" %in% colnames(data))
      ggplot2::ggplot(data, ggplot2::aes(x = wt, y = hp)) +
        ggplot2::geom_point() +
        ggplot2::ggtitle("Plot 1: Weight vs Horsepower")
    })
    output$plot2 <- shiny::renderPlot({
      shiny::req(input$run_button)
      data <- data_uploaded()
      shiny::req(base::nrow(data) > 0)
      shiny::req("wt" %in% base::colnames(data), "drat" %in% base::colnames(data))
      ggplot2::ggplot(data, ggplot2::aes(x = wt, y = drat)) +
        ggplot2::geom_point(col = "red") +
        ggplot2::ggtitle("Plot 2: Weight vs Drat")
    })
    output$fig1_download <- shiny::downloadHandler(
      filename = function() {
        base::paste("plot1-", base::Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = ggplot2::last_plot(),
          width = base::as.numeric(input$width),
          height = base::as.numeric(input$height),
          units = input$Units)
      }
    )
    output$fig2_download <- shiny::downloadHandler(
      filename = function() {
        base::paste("plot2-", base::Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = ggplot2::last_plot(),
          width = base::as.numeric(input$width),
          height = base::as.numeric(input$height),
          units = input$Units)
      }
    )
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        base::paste("uploaded_data-", base::Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        utils::write.csv(data_uploaded(), file)
      }
    )
  })
}
