utils::globalVariables(c("Tissue", "variable", "value"))

#' Stacked Column Chart UI Module
#'
#' UI module for generating a ProtVis-style stacked column chart from uploaded
#' CSV data or demo data. Supports run-based rendering, dynamic color controls,
#' data preview, and multi-format plot download.
#'
#' @param id A unique module id.
#'
#' @return A Shiny UI tagList.
#'
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs enable disable
#' @importFrom colourpicker colourInput
#' @export
stacked_column_chart_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),

    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 340,

        bslib::accordion(
          open = c("Data Input", "Plot Settings", "Download"),

          bslib::accordion_panel(
            "Data Input",

            shiny::fileInput(
              ns("file_input"),
              "Upload CSV File",
              accept = c(".csv")
            ),

            shiny::div(style = "height: 8px;"),

            shiny::actionButton(
              ns("use_demo"),
              "Use Demo Data",
              class = "btn btn-outline-primary btn-sm"
            ),

            shiny::div(style = "height: 8px;"),

            shiny::downloadButton(
              ns("download_demo"),
              "Download Demo CSV",
              class = "btn btn-outline-secondary btn-sm"
            ),

            shiny::div(style = "height: 12px;"),

            shiny::uiOutput(ns("data_status"))
          ),

          bslib::accordion_panel(
            "Plot Settings",

            shiny::checkboxInput(
              ns("flip_coords"),
              "Flip Coordinates",
              value = FALSE
            ),

            shiny::checkboxInput(
              ns("show_border"),
              "Show Bar Border",
              value = FALSE
            ),

            shiny::textInput(
              ns("x_label"),
              "X-Axis Label",
              value = "Tissue"
            ),

            shiny::textInput(
              ns("y_label"),
              "Y-Axis Label",
              value = "Count"
            ),

            shiny::textInput(
              ns("fill_label"),
              "Legend Title",
              value = "Category"
            ),

            shiny::numericInput(
              ns("base_size"),
              "Base Font Size",
              value = 12,
              min = 8,
              max = 24,
              step = 1
            ),

            shiny::uiOutput(ns("dynamic_colors")),

            shiny::div(style = "height: 10px;"),

            shiny::actionButton(
              ns("run_plot"),
              "Run",
              class = "btn btn-success"
            )
          ),

          bslib::accordion_panel(
            "Download",

            shiny::radioButtons(
              ns("download_format"),
              "Download Format",
              choices = c(
                "PDF" = "pdf",
                "PNG" = "png",
                "JPG" = "jpg",
                "SVG" = "svg"
              ),
              selected = "pdf",
              inline = TRUE
            ),

            shiny::numericInput(
              ns("width"),
              "Width (inches)",
              value = 8,
              min = 1,
              step = 0.5
            ),

            shiny::numericInput(
              ns("height"),
              "Height (inches)",
              value = 6,
              min = 1,
              step = 0.5
            ),

            shiny::numericInput(
              ns("dpi"),
              "DPI (for PNG/JPG)",
              value = 300,
              min = 72,
              step = 50
            ),

            shiny::downloadButton(
              ns("download_plot"),
              "Download Plot"
            )
          )
        )
      ),

      shiny::mainPanel(
        bslib::layout_columns(
          col_widths = c(8, 4),

          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Stacked Column Chart"),
            bslib::card_body(
              shiny::uiOutput(ns("plot_ui"))
            )
          ),

          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Uploaded Data Preview"),
            bslib::card_body(
              shiny::uiOutput(ns("preview_info")),
              shiny::tableOutput(ns("data_preview"))
            )
          )
        )
      )
    )
  )
}

#' Stacked Column Chart Server Module
#'
#' Server module for generating a ProtVis-style stacked column chart from
#' uploaded CSV data or demo data. Supports controlled rendering via a Run
#' button, dynamic category colors, data preview, and multi-format export.
#'
#' @param id A unique module id.
#'
#' @return A Shiny module server function.
#'
#' @import shiny
#' @importFrom data.table fread
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual labs theme_bw theme
#' @importFrom ggplot2 element_text element_blank coord_flip ggsave
#' @export
stacked_column_chart_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    demo_data <- shiny::reactive({
      data.frame(
        Tissue = c("Root", "Stem", "Leaf", "Panicle"),
        Down = c(120, 95, 140, 80),
        Not_Significant = c(300, 280, 260, 210),
        Up = c(150, 170, 190, 130),
        check.names = FALSE
      )
    })

    rv <- shiny::reactiveValues(
      current_data = NULL,
      data_source = NULL,
      has_run = FALSE,
      plot_obj = NULL,
      plot_data = NULL
    )

    default_palette <- function(n) {
      base_colors <- c(
        "#3B5BDB", "#ADB5BD", "#E03131", "#2F9E44", "#F08C00",
        "#862E9C", "#0C8599", "#D6336C", "#5C940D", "#495057"
      )

      if (n <= length(base_colors)) {
        return(base_colors[seq_len(n)])
      }

      grDevices::colorRampPalette(base_colors)(n)
    }

    shiny::observeEvent(input$file_input, {
      shiny::req(input$file_input)

      df <- data.table::fread(
        input$file_input$datapath,
        data.table = FALSE
      )

      rv$current_data <- df
      rv$data_source <- paste0("Uploaded file: ", input$file_input$name)
      rv$has_run <- FALSE
      rv$plot_obj <- NULL
      rv$plot_data <- NULL

      shinyjs::disable("download_plot")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$use_demo, {
      rv$current_data <- demo_data()
      rv$data_source <- "Demo data loaded"
      rv$has_run <- FALSE
      rv$plot_obj <- NULL
      rv$plot_data <- NULL

      shinyjs::disable("download_plot")
    }, ignoreInit = TRUE)

    output$data_status <- shiny::renderUI({
      if (is.null(rv$current_data)) {
        return(
          shiny::div(
            style = "padding: 8px 10px; background: #F8F9FA; border-radius: 8px; color: #666;",
            "No dataset loaded."
          )
        )
      }

      shiny::div(
        style = "padding: 8px 10px; background: #F1F3F5; border-radius: 8px;",
        shiny::strong("Current data: "),
        rv$data_source
      )
    })

    numeric_category_columns <- shiny::reactive({
      df <- rv$current_data

      if (is.null(df)) {
        return(character(0))
      }

      if (!"Tissue" %in% base::colnames(df)) {
        return(character(0))
      }

      cols <- base::setdiff(base::colnames(df), "Tissue")

      cols[base::vapply(df[, cols, drop = FALSE], is.numeric, logical(1))]
    })

    output$dynamic_colors <- shiny::renderUI({
      cols <- numeric_category_columns()

      if (length(cols) == 0) {
        return(
          shiny::div(
            style = "padding: 8px 10px; background: #FFF3CD; border-radius: 8px; color: #856404;",
            "Color settings will appear after valid data is loaded."
          )
        )
      }

      pal <- default_palette(length(cols))

      shiny::tagList(
        shiny::hr(),
        shiny::h5("Category Colors"),
        lapply(seq_along(cols), function(i) {
          colourpicker::colourInput(
            inputId = ns(paste0("color_", cols[i])),
            label = paste("Color for", cols[i]),
            value = pal[i]
          )
        })
      )
    })

    shiny::observeEvent(input$run_plot, {
      shiny::req(rv$current_data)

      df <- rv$current_data

      if (!"Tissue" %in% base::colnames(df)) {
        shiny::showNotification(
          "The dataset must contain a 'Tissue' column.",
          type = "error"
        )
        rv$has_run <- FALSE
        rv$plot_obj <- NULL
        rv$plot_data <- NULL
        shinyjs::disable("download_plot")
        return(NULL)
      }

      num_cols <- numeric_category_columns()

      if (length(num_cols) == 0) {
        shiny::showNotification(
          "No numeric category columns available for plotting.",
          type = "error"
        )
        rv$has_run <- FALSE
        rv$plot_obj <- NULL
        rv$plot_data <- NULL
        shinyjs::disable("download_plot")
        return(NULL)
      }

      df2 <- df[, c("Tissue", num_cols), drop = FALSE]

      data_long <- reshape2::melt(
        df2,
        id.vars = "Tissue",
        variable.name = "variable",
        value.name = "value"
      )

      data_long$Tissue <- base::as.character(data_long$Tissue)
      data_long$variable <- base::factor(data_long$variable, levels = num_cols)

      color_values <- stats::setNames(
        base::vapply(
          num_cols,
          function(x) {
            input[[paste0("color_", x)]]
          },
          character(1)
        ),
        num_cols
      )

      border_color <- if (isTRUE(input$show_border)) "black" else NA

      p <- ggplot2::ggplot(
        data_long,
        ggplot2::aes(x = Tissue, y = value, fill = variable)
      ) +
        ggplot2::geom_bar(
          stat = "identity",
          color = border_color,
          width = 0.72
        ) +
        ggplot2::scale_fill_manual(
          values = color_values,
          drop = FALSE
        ) +
        ggplot2::labs(
          x = input$x_label,
          y = input$y_label,
          fill = input$fill_label
        ) +
        ggplot2::theme_bw(base_size = input$base_size) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(color = "black"),
          axis.text.y = ggplot2::element_text(color = "black"),
          axis.title = ggplot2::element_text(color = "black"),
          legend.title = ggplot2::element_text(color = "black"),
          legend.text = ggplot2::element_text(color = "black")
        )

      if (isTRUE(input$flip_coords)) {
        p <- p + ggplot2::coord_flip()
      }

      rv$plot_data <- data_long
      rv$plot_obj <- p
      rv$has_run <- TRUE

      shinyjs::enable("download_plot")

      shiny::showNotification(
        "Plot generated successfully.",
        type = "message"
      )
    }, ignoreInit = TRUE)

    output$plot_ui <- shiny::renderUI({
      if (is.null(rv$current_data)) {
        return(
          shiny::div(
            style = "padding: 40px 20px; text-align: center; color: #666;",
            shiny::h5("No data available"),
            shiny::p("Please upload a CSV file or click 'Use Demo Data', then click Run.")
          )
        )
      }

      if (!isTRUE(rv$has_run) || is.null(rv$plot_obj)) {
        return(
          shiny::div(
            style = "padding: 40px 20px; text-align: center; color: #666;",
            shiny::h5("Ready to run"),
            shiny::p("Data has been loaded. Adjust parameters if needed, then click Run.")
          )
        )
      }

      shiny::plotOutput(ns("protein_plot"), height = "560px")
    })

    output$protein_plot <- shiny::renderPlot({
      shiny::req(rv$has_run)
      shiny::req(rv$plot_obj)

      rv$plot_obj
    })

    output$preview_info <- shiny::renderUI({
      if (is.null(rv$current_data)) {
        return(
          shiny::div(
            style = "color: #666;",
            "No data loaded."
          )
        )
      }

      df <- rv$current_data

      shiny::div(
        style = "margin-bottom: 10px;",
        shiny::tags$span(
          style = "display: inline-block; padding: 4px 10px; background: #F1F3F5; border-radius: 999px; margin-right: 8px;",
          paste0("Rows: ", nrow(df))
        ),
        shiny::tags$span(
          style = "display: inline-block; padding: 4px 10px; background: #F1F3F5; border-radius: 999px; margin-right: 8px;",
          paste0("Columns: ", ncol(df))
        ),
        shiny::tags$span(
          style = "display: inline-block; padding: 4px 10px; background: #F1F3F5; border-radius: 999px;",
          rv$data_source
        )
      )
    })

    output$data_preview <- shiny::renderTable({
      if (is.null(rv$current_data)) {
        return(NULL)
      }

      utils::head(rv$current_data, 8)
    }, striped = TRUE, bordered = TRUE, spacing = "s", width = "100%")

    output$download_demo <- shiny::downloadHandler(
      filename = function() {
        "stacked_column_chart_demo.csv"
      },
      content = function(file) {
        utils::write.csv(demo_data(), file, row.names = FALSE)
      }
    )

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        paste0("stacked_column_chart_", Sys.Date(), ".", input$download_format)
      },
      content = function(file) {
        shiny::req(rv$has_run)
        shiny::req(rv$plot_obj)

        p <- rv$plot_obj

        if (identical(input$download_format, "pdf")) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = input$width,
            height = input$height,
            units = "in",
            device = grDevices::pdf
          )
        } else if (identical(input$download_format, "png")) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = input$width,
            height = input$height,
            units = "in",
            dpi = input$dpi,
            device = "png"
          )
        } else if (identical(input$download_format, "jpg")) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = input$width,
            height = input$height,
            units = "in",
            dpi = input$dpi,
            device = "jpeg"
          )
        } else if (identical(input$download_format, "svg")) {
          ggplot2::ggsave(
            filename = file,
            plot = p,
            width = input$width,
            height = input$height,
            units = "in",
            device = "svg"
          )
        }
      }
    )

    shiny::observeEvent(rv$has_run, {
      if (!isTRUE(rv$has_run)) {
        shinyjs::disable("download_plot")
      }
    }, ignoreInit = FALSE)
  })
}
