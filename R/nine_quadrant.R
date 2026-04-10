#' Creates the user interface for the Nine-Quadrant Plot module.
#' This module allows users to upload a CSV/XLSX file, select numeric columns for X/Y axes,
#' set cutoffs, customize colors for each quadrant, and download the plot as a PDF.
#' @param id A character string specifying the namespace of the module.
#' @return A Shiny UI tagList containing the sidebar layout with input controls and main plot area.
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @name nine_quadrant_ui
#' @export
#'
nine_quadrant_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      bslib::accordion(
        bslib::accordion_panel(
          "Data Input",
          shiny::fileInput(ns("file"), "Upload CSV/XLSX File", accept = c(".csv", ".xlsx")),
          shiny::uiOutput(ns("col_select_ui"))
        ),
        bslib::accordion_panel(
          "Plot Settings",
          shiny::numericInput(ns("fc_cutoff"), "Log2FC Cutoff", value = 1, step = 0.1),
          shiny::checkboxInput(ns("show_counts"), "Show Quadrant Counts", value = TRUE)
        ),
        bslib::accordion_panel(
          "Color Settings",
          colourpicker::colourInput(ns("col_up_up"), "Up_Up", "red"),
          colourpicker::colourInput(ns("col_down_down"), "Down_Down", "blue"),
          colourpicker::colourInput(ns("col_up_down"), "Up_Down", "purple"),
          colourpicker::colourInput(ns("col_down_up"), "Down_Up", "orange"),
          colourpicker::colourInput(ns("col_up_ns"), "Up_NS", "pink"),
          colourpicker::colourInput(ns("col_down_ns"), "Down_NS", "skyblue"),
          colourpicker::colourInput(ns("col_ns_up"), "NS_Up", "brown"),
          colourpicker::colourInput(ns("col_ns_down"), "NS_Down", "darkgreen"),
          colourpicker::colourInput(ns("col_ns_ns"), "NS_NS", "grey70")
        ),
        bslib::accordion_panel(
          "Download Settings",
          shiny::numericInput(ns("pdf_width"), "PDF Width (inches)", value = 8, min = 1, max = 20),
          shiny::numericInput(ns("pdf_height"), "PDF Height (inches)", value = 6, min = 1, max = 20),
          shiny::downloadButton(ns("download_pdf"), "Download PDF")
        )
      )
    ),
    mainPanel(
      shiny::plotOutput(ns("plot"), height = "700px")
    )
  )
}

#' Implements the server-side logic for the Nine-Quadrant Plot module.
#' This includes reading uploaded files, dynamically generating UI for numeric column selection,
#' classifying points into quadrants, customizing colors, rendering the plot, and providing PDF download.
#' @param id A character string specifying the namespace of the module.
#' @return A Shiny module server that manages the Nine-Quadrant Plot interactivity.
#' @import shiny
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when group_by summarise n
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_point scale_color_manual
#' @importFrom ggplot2 theme_bw labs coord_cartesian geom_label scale_fill_manual
#' @importFrom grDevices cairo_pdf pdf dev.off
#' @name nine_quadrant_server
#' @export

utils::globalVariables(c("Omic1_status", "Omic2_status", "Quadrant", "x_center", "y_center"))

nine_quadrant_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # ---- Reactive: Read uploaded file ----
    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        utils::read.csv(input$file$datapath, check.names = FALSE)
      } else if (ext == "xlsx") {
        readxl::read_xlsx(input$file$datapath)
      } else {
        shiny::validate("Unsupported file type")
      }
    })
    # ---- Reactive UI: Automatically detect numeric columns ----
    output$col_select_ui <- shiny::renderUI({
      df <- data()
      num_cols <- base::names(df)[base::sapply(df, is.numeric)]
      shiny::tagList(
        shiny::selectInput(ns("col_x"), "Select Omic1 (X-axis)", choices = num_cols),
        shiny::selectInput(ns("col_y"), "Select Omic2 (Y-axis)", choices = num_cols)
      )
    })
    # ---- Reactive: Classify points into quadrants ----
    processed <- shiny::reactive({
      shiny::req(input$col_x, input$col_y)
      df <- data()
      fc <- input$fc_cutoff
      df <- df %>%
        dplyr::mutate(
          Omic1_status = dplyr::case_when(
            .data[[input$col_x]] > fc  ~ "Up",
            .data[[input$col_x]] < -fc ~ "Down",
            TRUE ~ "NS"
          ),
          Omic2_status = dplyr::case_when(
            .data[[input$col_y]] > fc  ~ "Up",
            .data[[input$col_y]] < -fc ~ "Down",
            TRUE ~ "NS"
          ),
          Quadrant = base::paste(Omic1_status, Omic2_status, sep = "_")
        )
      df
    })
    # ---- Reactive: Color mapping for quadrants ----
    colors <- shiny::reactive({
      c(
        "Up_Up" = input$col_up_up,
        "Down_Down" = input$col_down_down,
        "Up_Down" = input$col_up_down,
        "Down_Up" = input$col_down_up,
        "Up_NS" = input$col_up_ns,
        "Down_NS" = input$col_down_ns,
        "NS_Up" = input$col_ns_up,
        "NS_Down" = input$col_ns_down,
        "NS_NS" = input$col_ns_ns
      )
    })

    # ---- Function: Generate plot ----
    create_plot <- function() {
      df <- processed()
      fc <- input$fc_cutoff

      # Summarize counts per quadrant
      quadrant_data <- df %>%
        dplyr::group_by(Quadrant) %>%
        dplyr::summarise(
          count = dplyr::n(),
          x_center = stats::median(.data[[input$col_x]], na.rm = TRUE),
          y_center = stats::median(.data[[input$col_y]], na.rm = TRUE)
        )
      # Define axis limits
      x_limits <- c(min(df[[input$col_x]], na.rm = TRUE), max(df[[input$col_x]], na.rm = TRUE))
      y_limits <- c(min(df[[input$col_y]], na.rm = TRUE), max(df[[input$col_y]], na.rm = TRUE))
      # Base plot
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$col_x]], y = .data[[input$col_y]], color = Quadrant)) +
        ggplot2::geom_hline(yintercept = c(-fc, fc), linetype = "dashed", color = "grey50") +
        ggplot2::geom_vline(xintercept = c(-fc, fc), linetype = "dashed", color = "grey50") +
        ggplot2::geom_point(size = 2, alpha = 0.8) +
        ggplot2::scale_color_manual(values = colors()) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(
          x = paste(input$col_x, "(Log2FC)"),
          y = paste(input$col_y, "(Log2FC)")
        ) +
        ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits)
      # Optional: show quadrant counts
      if (input$show_counts) {
        p <- p +
          ggplot2::geom_label(
            data = quadrant_data,
            ggplot2::aes(
              x = x_center,
              y = y_center,
              label = base::paste(Quadrant, ":", count),
              fill = Quadrant
            ),
            color = "white",
            size = 5,
            fontface = "bold",
            show.legend = FALSE,
            alpha = 0.7
          ) +
          ggplot2::scale_fill_manual(values = colors())
      }
      return(p)
    }
    # ---- Render plot ----
    output$plot <- shiny::renderPlot({
      create_plot()
    })
    # ---- Download PDF ----
    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0("Nine_Quadrant_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (capabilities("cairo")) {
          grDevices::cairo_pdf(file, width = input$pdf_width, height = input$pdf_height)
        } else {
          grDevices::pdf(file, width = input$pdf_width, height = input$pdf_height)
        }
        print(create_plot())
        grDevices::dev.off()
      }
    )
  })
}
