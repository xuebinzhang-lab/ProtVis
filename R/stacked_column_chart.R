#' Stacked Bar Chart UI Module
#' Creates a user interface for generating customizable stacked bar charts
#' from uploaded CSV data. Supports color customization, coordinate flipping,
#' axis labeling, and chart download.
#' @param id A unique identifier for the Shiny namespace.
#' @return A Shiny UI tagList containing the stacked bar chart interface.
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @name stacked_column_chart_ui
#' @export
#'
stacked_column_chart_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::h3("Stacked Bar Chart"),
        # File upload control
        shiny::fileInput(ns("file_input"), "Upload Data", accept = ".csv"),
        # Checkbox to flip coordinates
        shiny::checkboxInput(ns("flip_coords"), "Flip Coordinates", value = FALSE),
        # Color inputs for Down, Not_Significant, and Up categories
        colourpicker::colourInput(ns("color_down"), "Color for Down", value = "#0011FF"),
        colourpicker::colourInput(ns("color_not_significant"), "Color for Not Significant", value = "gray"),
        colourpicker::colourInput(ns("color_up"), "Color for Up", value = "red"),
        # Custom labels for x, y, and fill
        shiny::textInput(ns("x_label"), "X-Axis Label", value = "Tissue"),
        shiny::textInput(ns("y_label"), "Y-Axis Label", value = "Protein Count"),
        shiny::textInput(ns("fill_label"), "Fill Legend Label", value = ""),
        # Set chart download size
        shiny::numericInput(ns("width"), "Chart Width (in inches)", value = 8, min = 1),
        shiny::numericInput(ns("height"), "Chart Height (in inches)", value = 6, min = 1),
        # Download button
        shiny::downloadButton(ns("download_plot"), "Download")
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns("protein_plot"))
      )
    )
  )
}

#' Stacked Bar Chart Server Module
#' Server-side logic for the stacked bar chart module. Processes uploaded CSV data,
#' generates customizable stacked bar charts using ggplot2, and handles plot downloads.
#' @param id A unique identifier for the Shiny namespace.
#' @return A Shiny server module function.
#' @import shiny
#' @importFrom data.table fread
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggsave
#' @name stacked_column_chart_server
#' @export
#'

utils::globalVariables(c("Tissue", "variable", "value"))

stacked_column_chart_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Reactive expression to load uploaded CSV file
    data <- shiny::reactive({
      shiny::req(input$file_input)
      data.table::fread(input$file_input$datapath)  # Read file
    })
    # Process data and generate stacked bar chart
    output$protein_plot <- shiny::renderPlot({
      df <- data()  # Get data
      shiny::req(df)  # Ensure data is valid
      # Reshape data to long format
      data_long <- reshape2::melt(df, id.vars = "Tissue")
      # Set colors based on user input
      data_long$variable <- base::factor(data_long$variable,
                                         levels = c("Down", "Not_Significant", "Up"))
      # Create the stacked bar chart with custom colors
      p <- ggplot2::ggplot(data_long, ggplot2::aes(x = Tissue, y = value, fill = variable)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::scale_fill_manual(values = c(
          "Down" = input$color_down,
          "Not_Significant" = input$color_not_significant,
          "Up" = input$color_up
        )) +
        ggplot2::labs(
          x = input$x_label,  # Custom X-axis label
          y = input$y_label,  # Custom Y-axis label
          fill = input$fill_label  # Custom fill legend label
        ) +
        ggplot2::theme_bw()
      # If the user selected to flip coordinates, add coord_flip()
      if (input$flip_coords) {
        p <- p + ggplot2::coord_flip()
      }
      # Render the plot
      p
    })
    # Set up the download functionality
    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        base::paste("protein_plot_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Save the plot using ggsave
        ggplot2::ggsave(file, plot = ggplot2::last_plot(),
                        width = input$width, height = input$height, units = "in")
      }
    )
  })
}
