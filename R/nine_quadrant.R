# ==== Nine-Quadrant Plot Module UI ====
#'
#' Creates the user interface for the Nine-Quadrant Plot module.
#' This module allows users to upload a CSV/XLSX file, select numeric columns for X/Y axes,
#' set cutoffs, customize colors for each quadrant, and download the plot as a PDF.
#'
#' @param id A character string specifying the namespace of the module.
#' @return A Shiny UI tagList containing the sidebar layout with input controls and main plot area.
#' @export
nine_quadrant_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      accordion(
        accordion_panel(
          "Data Input",
          fileInput(ns("file"), "Upload CSV/XLSX File", accept = c(".csv", ".xlsx")),
          uiOutput(ns("col_select_ui"))
        ),
        accordion_panel(
          "Plot Settings",
          numericInput(ns("fc_cutoff"), "Log2FC Cutoff", value = 1, step = 0.1),
          checkboxInput(ns("show_counts"), "Show Quadrant Counts", value = TRUE)
        ),
        accordion_panel(
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
        accordion_panel(
          "Download Settings",
          numericInput(ns("pdf_width"), "PDF Width (inches)", value = 8, min = 1, max = 20),
          numericInput(ns("pdf_height"), "PDF Height (inches)", value = 6, min = 1, max = 20),
          downloadButton(ns("download_pdf"), "Download PDF")
        )
      )
    ),
    mainPanel(
      plotOutput(ns("plot"), height = "700px")
    )
  )
}

# ==== Nine-Quadrant Plot Module Server ====
#'
#' Implements the server-side logic for the Nine-Quadrant Plot module.
#' This includes reading uploaded files, dynamically generating UI for numeric column selection,
#' classifying points into quadrants, customizing colors, rendering the plot, and providing PDF download.
#'
#' @param id A character string specifying the namespace of the module.
#' @return A Shiny module server that manages the Nine-Quadrant Plot interactivity.
#' @export
nine_quadrant_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive: Read uploaded file ----
    data <- reactive({
      req(input$file)
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        read.csv(input$file$datapath, check.names = FALSE)
      } else if (ext == "xlsx") {
        readxl::read_xlsx(input$file$datapath)
      } else {
        validate("Unsupported file type")
      }
    })

    # ---- Reactive UI: Automatically detect numeric columns ----
    output$col_select_ui <- renderUI({
      df <- data()
      num_cols <- names(df)[sapply(df, is.numeric)]
      tagList(
        selectInput(ns("col_x"), "Select Omic1 (X-axis)", choices = num_cols),
        selectInput(ns("col_y"), "Select Omic2 (Y-axis)", choices = num_cols)
      )
    })

    # ---- Reactive: Classify points into quadrants ----
    processed <- reactive({
      req(input$col_x, input$col_y)
      df <- data()
      fc <- input$fc_cutoff
      df <- df %>%
        mutate(
          Omic1_status = case_when(
            .data[[input$col_x]] > fc  ~ "Up",
            .data[[input$col_x]] < -fc ~ "Down",
            TRUE ~ "NS"
          ),
          Omic2_status = case_when(
            .data[[input$col_y]] > fc  ~ "Up",
            .data[[input$col_y]] < -fc ~ "Down",
            TRUE ~ "NS"
          ),
          Quadrant = paste(Omic1_status, Omic2_status, sep = "_")
        )
      df
    })

    # ---- Reactive: Color mapping for quadrants ----
    colors <- reactive({
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
        group_by(Quadrant) %>%
        summarise(
          count = n(),
          x_center = median(.data[[input$col_x]], na.rm = TRUE),
          y_center = median(.data[[input$col_y]], na.rm = TRUE)
        )

      # Define axis limits
      x_limits <- c(min(df[[input$col_x]], na.rm = TRUE), max(df[[input$col_x]], na.rm = TRUE))
      y_limits <- c(min(df[[input$col_y]], na.rm = TRUE), max(df[[input$col_y]], na.rm = TRUE))

      # Base plot
      p <- ggplot(df, aes(x = .data[[input$col_x]], y = .data[[input$col_y]], color = Quadrant)) +
        geom_hline(yintercept = c(-fc, fc), linetype = "dashed", color = "grey50") +
        geom_vline(xintercept = c(-fc, fc), linetype = "dashed", color = "grey50") +
        geom_point(size = 2, alpha = 0.8) +
        scale_color_manual(values = colors()) +
        theme_bw(base_size = 14) +
        labs(
          x = paste(input$col_x, "(Log2FC)"),
          y = paste(input$col_y, "(Log2FC)")
        ) +
        coord_cartesian(xlim = x_limits, ylim = y_limits)

      # Optional: show quadrant counts
      if (input$show_counts) {
        p <- p +
          geom_label(
            data = quadrant_data,
            aes(
              x = x_center,
              y = y_center,
              label = paste(Quadrant, ":", count),
              fill = Quadrant
            ),
            color = "white",
            size = 5,
            fontface = "bold",
            show.legend = FALSE,
            alpha = 0.7
          ) +
          scale_fill_manual(values = colors())
      }
      return(p)
    }

    # ---- Render plot ----
    output$plot <- renderPlot({
      create_plot()
    })

    # ---- Download PDF ----
    output$download_pdf <- downloadHandler(
      filename = function() {
        paste0("Nine_Quadrant_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        if (capabilities("cairo")) {
          cairo_pdf(file, width = input$pdf_width, height = input$pdf_height)
        } else {
          pdf(file, width = input$pdf_width, height = input$pdf_height)
        }
        print(create_plot())
        dev.off()
      }
    )
  })
}
