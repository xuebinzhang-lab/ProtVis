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
          shiny::selectInput(ns("x_column"), "X-axis numeric column", choices = character(0)),
          shiny::selectInput(ns("y_column"), "Y-axis numeric column", choices = character(0)),
          shiny::selectInput(ns("color_column"), "Optional color/group column", choices = c("None" = "")),
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
#' @importFrom ggplot2 ggsave
#' @name TMT_server
#' @export
#'
utils::globalVariables(c("x_value", "y_value", "color_value"))
TMT_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
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

    shiny::observeEvent(data_uploaded(), {
      data <- data_uploaded()
      shiny::req(data)
      numeric_cols <- base::names(data)[vapply(data, is.numeric, logical(1))]
      shiny::updateSelectInput(session, "x_column", choices = numeric_cols, selected = numeric_cols[1])
      shiny::updateSelectInput(session, "y_column", choices = numeric_cols, selected = numeric_cols[min(2, base::length(numeric_cols))])
      shiny::updateSelectInput(session, "color_column", choices = c("None" = "", base::names(data)), selected = "")
    }, ignoreInit = TRUE)

    tmt_plot <- shiny::reactive({
      data <- data_uploaded()
      shiny::req(base::nrow(data) > 0)
      shiny::req(input$x_column %in% base::colnames(data), input$y_column %in% base::colnames(data))
      plot_df <- data.frame(
        x_value = data[[input$x_column]],
        y_value = data[[input$y_column]],
        stringsAsFactors = FALSE
      )
      if (!base::is.null(input$color_column) && input$color_column != "" && input$color_column %in% base::colnames(data)) {
        plot_df$color_value <- base::as.factor(data[[input$color_column]])
        return(
          ggplot2::ggplot(plot_df, ggplot2::aes(x = x_value, y = y_value, color = color_value)) +
            ggplot2::geom_point() +
            ggplot2::labs(x = input$x_column, y = input$y_column, color = input$color_column, title = "TMT numeric column comparison")
        )
      }
      ggplot2::ggplot(plot_df, ggplot2::aes(x = x_value, y = y_value)) +
        ggplot2::geom_point() +
        ggplot2::labs(x = input$x_column, y = input$y_column, title = "TMT numeric column comparison")
    })

    output$plot1 <- shiny::renderPlot({
      shiny::req(input$run_button)
      print(utils::head(data_uploaded()))
      print(tmt_plot())
    })
    output$plot2 <- shiny::renderPlot({
      shiny::req(input$run_button)
      print(tmt_plot())
    })
    output$fig1_download <- shiny::downloadHandler(
      filename = function() {
        base::paste("plot1-", base::Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          plot = tmt_plot(),
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
          plot = tmt_plot(),
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
