#' Stacked Bar Chart UI Module
#'
#' @param id A unique identifier for the module.
#'
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @import data.table
#' @import reshape2
#' @import colourpicker
#'
#' @description
#' This UI function creates a stacked bar chart interface with a file upload, color input options,
#' label customization, and chart download capabilities.
#' @export
#'

stacked_column_chart_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h3("Stacked Bar Chart"),
        # File upload control
        fileInput(ns("file_input"), "Upload Data", accept = ".csv"),
        # Checkbox to flip coordinates
        checkboxInput(ns("flip_coords"), "Flip Coordinates", value = FALSE),
        # Color inputs for Down, Not_Significant, and Up categories
        colourInput(ns("color_down"), "Color for Down", value = "#0011FF"),
        colourInput(ns("color_not_significant"), "Color for Not Significant", value = "gray"),
        colourInput(ns("color_up"), "Color for Up", value = "red"),
        # Custom labels for x, y, and fill
        textInput(ns("x_label"), "X-Axis Label", value = "Tissue"),
        textInput(ns("y_label"), "Y-Axis Label", value = "Protein Count"),
        textInput(ns("fill_label"), "Fill Legend Label", value = ""),
        # Set chart download size
        numericInput(ns("width"), "Chart Width (in inches)", value = 8, min = 1),
        numericInput(ns("height"), "Chart Height (in inches)", value = 6, min = 1),
        # Download button
        downloadButton(ns("download_plot"), "Download")
      ),
      mainPanel(
        plotOutput(ns("protein_plot"))
      )
    )
  )
}

#' Stacked Bar Chart Server Module
#'
#' @param id A unique identifier for the module.
#'
#' @import shiny
#' @import data.table
#' @import ggplot2
#' @import reshape2
#' @import colourpicker
#'
#' @description
#' This server function processes data, generates the stacked bar chart, and handles dynamic plot updates,
#' custom labels, and download functionality.
#' @export
#'

stacked_column_chart_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive expression to load uploaded CSV file
    data <- reactive({
      req(input$file_input)
      fread(input$file_input$datapath)  # Read file
    })

    # Process data and generate stacked bar chart
    output$protein_plot <- renderPlot({
      df <- data()  # Get data
      req(df)  # Ensure data is valid

      # Reshape data to long format
      data_long <- melt(df, id.vars = "Tissue")

      # Set colors based on user input
      data_long$variable <- factor(data_long$variable, levels = c("Down", "Not_Significant", "Up"))

      # Create the stacked bar chart with custom colors
      p <- ggplot(data_long, aes(x = Tissue, y = value, fill = variable)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c(
          "Down" = input$color_down,
          "Not_Significant" = input$color_not_significant,
          "Up" = input$color_up
        )) +
        labs(
          x = input$x_label,  # Custom X-axis label
          y = input$y_label,  # Custom Y-axis label
          fill = input$fill_label  # Custom fill legend label
        ) +
        theme_bw()

      # If the user selected to flip coordinates, add coord_flip()
      if (input$flip_coords) {
        p <- p + coord_flip()
      }

      # Render the plot
      p
    })

    # Set up the download functionality
    output$download_plot <- downloadHandler(
      filename = function() {
        paste("protein_plot_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Save the plot using ggsave
        ggsave(file, plot = last_plot(), width = input$width, height = input$height, units = "in")
      }
    )
  })
}
