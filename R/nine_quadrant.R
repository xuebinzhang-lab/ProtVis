#' Creates the user interface for the Nine-Quadrant Plot module.
#' This module allows users to upload a CSV/XLSX file, select numeric columns for X/Y axes,
#' set cutoffs, customize colors for each quadrant, preview uploaded data, and download the plot as a PDF.
#' Below the plot, expandable panels are provided for all nine quadrants, each with its own data table and download button.
#' @param id A character string specifying the namespace of the module.
#' @return A Shiny UI tagList containing the sidebar layout with input controls and main plot area.
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @importFrom DT DTOutput
#' @name nine_quadrant_ui
#' @export
#'
nine_quadrant_ui <- function(id) {
  ns <- NS(id)

  quadrant_names <- c(
    "Up_Up", "Up_NS", "Up_Down",
    "NS_Up", "NS_NS", "NS_Down",
    "Down_Up", "Down_NS", "Down_Down"
  )

  quadrant_panels <- lapply(quadrant_names, function(q) {
    pretty_title <- gsub("_", " / ", q)

    bslib::accordion_panel(
      title = pretty_title,
      shiny::div(
        style = "margin-bottom: 12px;",
        shiny::downloadButton(
          ns(paste0("download_", q)),
          paste("Download", q),
          class = "btn-success btn-sm"
        )
      ),
      DT::DTOutput(ns(paste0("table_", q)))
    )
  })

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      open = "open",
      gap = "12px",
      bslib::accordion(
        always_open = TRUE,

        bslib::accordion_panel(
          "Data Input",
          shiny::fileInput(
            ns("file"),
            "Upload CSV/XLSX File",
            accept = c(".csv", ".xlsx")
          ),
          shiny::uiOutput(ns("col_select_ui"))
        ),

        bslib::accordion_panel(
          "Plot Settings",
          shiny::numericInput(
            ns("fc_cutoff"),
            "Log2FC Cutoff",
            value = 1,
            step = 0.1
          ),
          shiny::checkboxInput(
            ns("show_counts"),
            "Show Quadrant Counts",
            value = TRUE
          ),
          shiny::div(
            style = "margin-top: 12px;",
            shiny::actionButton(
              ns("run_plot"),
              "Run",
              class = "btn-primary",
              width = "100%"
            )
          )
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
          shiny::numericInput(
            ns("pdf_width"),
            "PDF Width (inches)",
            value = 8,
            min = 1,
            max = 20
          ),
          shiny::numericInput(
            ns("pdf_height"),
            "PDF Height (inches)",
            value = 6,
            min = 1,
            max = 20
          ),
          shiny::downloadButton(
            ns("download_pdf"),
            "Download PDF",
            class = "btn-success",
            width = "100%"
          )
        )
      )
    ),

    bslib::layout_columns(
      col_widths = c(5, 7),

      bslib::card(
        full_screen = TRUE,
        style = "min-height: 760px;",
        bslib::card_header(
          shiny::div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            shiny::div(
              style = "font-weight: 600; font-size: 18px;",
              "Uploaded Data Preview"
            ),
            shiny::uiOutput(ns("data_info"))
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("data_preview_ui"))
        )
      ),

      bslib::card(
        full_screen = TRUE,
        style = "min-height: 760px;",
        bslib::card_header(
          shiny::div(
            style = "display:flex; justify-content:space-between; align-items:center;",
            shiny::div(
              style = "font-weight: 600; font-size: 18px;",
              "Nine-Quadrant Plot"
            ),
            shiny::uiOutput(ns("plot_status"))
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("plot_ui"), height = "700px"),
          shiny::tags$hr(style = "margin: 18px 0;"),
          shiny::uiOutput(ns("quadrant_tables_ui"))
        )
      )
    )
  )
}

#' Implements the server-side logic for the Nine-Quadrant Plot module.
#' This includes reading uploaded files, dynamically generating UI for numeric column selection,
#' previewing uploaded data, classifying points into quadrants, customizing colors,
#' rendering the plot, providing PDF download, and showing per-quadrant tables with per-quadrant CSV downloads.
#' @param id A character string specifying the namespace of the module.
#' @return A Shiny module server that manages the Nine-Quadrant Plot interactivity.
#' @import shiny
#' @importFrom shinyjs disable enable
#' @importFrom tools file_ext
#' @importFrom utils read.csv write.csv
#' @importFrom readxl read_xlsx
#' @importFrom dplyr mutate case_when group_by summarise n filter
#' @importFrom ggplot2 ggplot aes geom_hline geom_vline geom_point scale_color_manual
#' @importFrom ggplot2 theme_bw labs coord_cartesian geom_label scale_fill_manual
#' @importFrom grDevices cairo_pdf pdf dev.off
#' @importFrom DT renderDT datatable DTOutput
#' @name nine_quadrant_server
#' @export
#'
utils::globalVariables(c("Omic1_status", "Omic2_status", "Quadrant", "x_center", "y_center"))

nine_quadrant_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    quadrant_names <- c(
      "Up_Up", "Up_NS", "Up_Down",
      "NS_Up", "NS_NS", "NS_Down",
      "Down_Up", "Down_NS", "Down_Down"
    )

    shinyjs::disable("download_pdf")
    lapply(quadrant_names, function(q) shinyjs::disable(paste0("download_", q)))

    data <- shiny::reactive({
      shiny::req(input$file)
      ext <- tools::file_ext(input$file$name)

      if (ext == "csv") {
        utils::read.csv(input$file$datapath, check.names = FALSE)
      } else if (ext == "xlsx") {
        as.data.frame(readxl::read_xlsx(input$file$datapath))
      } else {
        shiny::validate(shiny::need(FALSE, "Unsupported file type"))
      }
    })

    output$data_info <- shiny::renderUI({
      if (is.null(input$file)) {
        shiny::tags$span(
          style = paste(
            "display:inline-block;",
            "padding:4px 10px;",
            "border-radius:999px;",
            "background:#f3f4f6;",
            "color:#6b7280;",
            "font-size:12px;",
            "font-weight:500;"
          ),
          "No file uploaded"
        )
      } else {
        df <- data()
        shiny::tags$span(
          style = paste(
            "display:inline-block;",
            "padding:4px 10px;",
            "border-radius:999px;",
            "background:#eef2ff;",
            "color:#374151;",
            "font-size:12px;",
            "font-weight:500;"
          ),
          paste0(nrow(df), " rows × ", ncol(df), " columns")
        )
      }
    })

    output$plot_status <- shiny::renderUI({
      if (is.null(input$run_plot) || input$run_plot == 0) {
        shiny::tags$span(
          style = paste(
            "display:inline-block;",
            "padding:4px 10px;",
            "border-radius:999px;",
            "background:#fef3c7;",
            "color:#92400e;",
            "font-size:12px;",
            "font-weight:600;"
          ),
          "Not run"
        )
      } else {
        shiny::tags$span(
          style = paste(
            "display:inline-block;",
            "padding:4px 10px;",
            "border-radius:999px;",
            "background:#dcfce7;",
            "color:#166534;",
            "font-size:12px;",
            "font-weight:600;"
          ),
          "Ready"
        )
      }
    })

    output$col_select_ui <- shiny::renderUI({
      shiny::req(input$file)
      df <- data()
      num_cols <- base::names(df)[base::sapply(df, is.numeric)]

      if (length(num_cols) < 2) {
        return(
          shiny::div(
            style = paste(
              "margin-top:10px;",
              "padding:10px 12px;",
              "border-radius:10px;",
              "background:#fff7ed;",
              "color:#9a3412;",
              "font-size:13px;"
            ),
            "At least two numeric columns are required."
          )
        )
      }

      shiny::tagList(
        shiny::selectInput(
          ns("col_x"),
          "Select Omic1 (X-axis)",
          choices = num_cols
        ),
        shiny::selectInput(
          ns("col_y"),
          "Select Omic2 (Y-axis)",
          choices = num_cols,
          selected = if (length(num_cols) >= 2) num_cols[2] else num_cols[1]
        )
      )
    })

    output$data_preview_ui <- shiny::renderUI({
      if (is.null(input$file)) {
        shiny::div(
          style = paste(
            "min-height: 680px;",
            "display: flex;",
            "align-items: center;",
            "justify-content: center;",
            "padding: 30px;"
          ),
          shiny::div(
            style = paste(
              "max-width: 520px;",
              "width: 100%;",
              "text-align: center;",
              "padding: 32px 24px;",
              "border: 1px solid #e5e7eb;",
              "border-radius: 16px;",
              "background: #f8fafc;",
              "box-shadow: 0 2px 8px rgba(0,0,0,0.05);"
            ),
            shiny::tags$div(
              style = "font-size: 20px; font-weight: 600; margin-bottom: 10px; color: #1f2937;",
              "Uploaded Data Preview"
            ),
            shiny::tags$div(
              style = "font-size: 14px; line-height: 1.7; color: #4b5563;",
              "Please upload a CSV or XLSX file to preview the dataset here."
            )
          )
        )
      } else {
        DT::DTOutput(ns("data_preview"))
      }
    })

    output$data_preview <- DT::renderDT({
      shiny::req(input$file)
      df <- data()

      DT::datatable(
        df,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100),
          scrollX = TRUE,
          scrollY = "620px",
          autoWidth = TRUE
        )
      )
    })

    processed <- shiny::eventReactive(input$run_plot, {
      shiny::req(input$file, input$col_x, input$col_y)

      df <- data()
      fc <- input$fc_cutoff

      df <- dplyr::mutate(
        df,
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
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$run_plot, {
      shinyjs::enable("download_pdf")
      lapply(quadrant_names, function(q) shinyjs::enable(paste0("download_", q)))
    })

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

    create_plot <- function() {
      df <- processed()
      fc <- input$fc_cutoff

      quadrant_data <- df %>%
        dplyr::group_by(Quadrant) %>%
        dplyr::summarise(
          count = dplyr::n(),
          x_center = stats::median(.data[[input$col_x]], na.rm = TRUE),
          y_center = stats::median(.data[[input$col_y]], na.rm = TRUE),
          .groups = "drop"
        )

      x_limits <- c(
        min(df[[input$col_x]], na.rm = TRUE),
        max(df[[input$col_x]], na.rm = TRUE)
      )
      y_limits <- c(
        min(df[[input$col_y]], na.rm = TRUE),
        max(df[[input$col_y]], na.rm = TRUE)
      )

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
          x = .data[[input$col_x]],
          y = .data[[input$col_y]],
          color = Quadrant
        )
      ) +
        ggplot2::geom_hline(
          yintercept = c(-fc, fc),
          linetype = "dashed",
          color = "grey50"
        ) +
        ggplot2::geom_vline(
          xintercept = c(-fc, fc),
          linetype = "dashed",
          color = "grey50"
        ) +
        ggplot2::geom_point(size = 2, alpha = 0.8) +
        ggplot2::scale_color_manual(values = colors()) +
        ggplot2::theme_bw(base_size = 14) +
        ggplot2::labs(
          x = paste(input$col_x, "(Log2FC)"),
          y = paste(input$col_y, "(Log2FC)")
        ) +
        ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits)

      if (isTRUE(input$show_counts)) {
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

      p
    }

    output$plot_ui <- shiny::renderUI({
      if (is.null(input$file)) {
        shiny::div(
          style = paste(
            "height: 680px;",
            "display: flex;",
            "align-items: center;",
            "justify-content: center;",
            "padding: 30px;"
          ),
          shiny::div(
            style = paste(
              "max-width: 560px;",
              "width: 100%;",
              "text-align: center;",
              "padding: 36px 28px;",
              "border: 1px solid #e5e7eb;",
              "border-radius: 16px;",
              "background: #f8fafc;",
              "box-shadow: 0 2px 8px rgba(0,0,0,0.05);"
            ),
            shiny::tags$div(
              style = "font-size: 22px; font-weight: 600; margin-bottom: 12px; color: #1f2937;",
              "Nine-Quadrant Plot"
            ),
            shiny::tags$div(
              style = "font-size: 15px; line-height: 1.7; color: #4b5563;",
              "Please upload a CSV/XLSX file first."
            )
          )
        )
      } else if (is.null(input$run_plot) || input$run_plot == 0) {
        shiny::div(
          style = paste(
            "height: 680px;",
            "display: flex;",
            "align-items: center;",
            "justify-content: center;",
            "padding: 30px;"
          ),
          shiny::div(
            style = paste(
              "max-width: 560px;",
              "width: 100%;",
              "text-align: center;",
              "padding: 36px 28px;",
              "border: 1px solid #e5e7eb;",
              "border-radius: 16px;",
              "background: #f8fafc;",
              "box-shadow: 0 2px 8px rgba(0,0,0,0.05);"
            ),
            shiny::tags$div(
              style = "font-size: 22px; font-weight: 600; margin-bottom: 12px; color: #1f2937;",
              "Nine-Quadrant Plot"
            ),
            shiny::tags$div(
              style = "font-size: 15px; line-height: 1.7; color: #4b5563;",
              "Please select the X and Y columns, adjust the cutoff and display settings, then click ",
              shiny::tags$b("Run"),
              " to generate the plot."
            )
          )
        )
      } else {
        shiny::plotOutput(ns("plot"), height = "680px")
      }
    })

    output$plot <- shiny::renderPlot({
      shiny::req(input$run_plot > 0)
      create_plot()
    })

    output$quadrant_tables_ui <- shiny::renderUI({
      if (is.null(input$file)) {
        return(
          shiny::div(
            style = paste(
              "padding: 18px 20px;",
              "border: 1px solid #e5e7eb;",
              "border-radius: 12px;",
              "background: #f8fafc;",
              "color: #4b5563;"
            ),
            "Please upload a file first to view quadrant tables."
          )
        )
      }

      if (is.null(input$run_plot) || input$run_plot == 0) {
        return(
          shiny::div(
            style = paste(
              "padding: 18px 20px;",
              "border: 1px solid #e5e7eb;",
              "border-radius: 12px;",
              "background: #f8fafc;",
              "color: #4b5563;"
            ),
            "Quadrant tables will appear here after clicking Run."
          )
        )
      }

      bslib::accordion(
        id = ns("quadrant_accordion"),
        multiple = TRUE,
        open = FALSE,
        !!!lapply(quadrant_names, function(q) {
          pretty_title <- gsub("_", " / ", q)

          bslib::accordion_panel(
            title = pretty_title,
            shiny::div(
              style = "margin-bottom: 12px;",
              shiny::downloadButton(
                ns(paste0("download_", q)),
                paste("Download", q),
                class = "btn-success btn-sm"
              )
            ),
            DT::DTOutput(ns(paste0("table_", q)))
          )
        })
      )
    })

    quadrant_data_list <- shiny::reactive({
      shiny::req(input$run_plot > 0)
      df <- processed()
      out <- stats::setNames(vector("list", length(quadrant_names)), quadrant_names)

      for (q in quadrant_names) {
        out[[q]] <- dplyr::filter(df, Quadrant == q)
      }
      out
    })

    for (q in quadrant_names) {
      local({
        quadrant <- q

        output[[paste0("table_", quadrant)]] <- DT::renderDT({
          shiny::req(input$run_plot > 0)
          df_q <- quadrant_data_list()[[quadrant]]

          DT::datatable(
            df_q,
            rownames = FALSE,
            filter = "top",
            options = list(
              pageLength = 10,
              lengthMenu = c(10, 25, 50, 100),
              scrollX = TRUE,
              autoWidth = TRUE
            )
          )
        })

        output[[paste0("download_", quadrant)]] <- shiny::downloadHandler(
          filename = function() {
            paste0(quadrant, "_", Sys.Date(), ".csv")
          },
          content = function(file) {
            shiny::req(input$run_plot > 0)
            utils::write.csv(
              quadrant_data_list()[[quadrant]],
              file,
              row.names = FALSE
            )
          }
        )
      })
    }

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste0("Nine_Quadrant_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        shiny::req(input$run_plot > 0)

        if (capabilities("cairo")) {
          grDevices::cairo_pdf(
            file,
            width = input$pdf_width,
            height = input$pdf_height
          )
        } else {
          grDevices::pdf(
            file,
            width = input$pdf_width,
            height = input$pdf_height
          )
        }

        print(create_plot())
        grDevices::dev.off()
      }
    )
  })
}
