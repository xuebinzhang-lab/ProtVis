#' Correlation Chord Diagram UI
#' UI module for correlation chord diagram visualization.
#' @param id Module id.
#' @return Shiny UI.
#' @import shiny
#' @import bslib
#' @importFrom DT DTOutput
#' @importFrom colourpicker colourInput
#' @importFrom bsicons bs_icon
#' @name correlation_chord_ui
#' @export
#'
correlation_chord_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        open = "open",
        gap = "12px",

        shiny::div(
          style = "margin-bottom: 8px;",
          shiny::h4("Correlation Chord Diagram", style = "margin-bottom: 6px;"),
          shiny::p(
            "Visualize pairwise correlations among selected numeric variables using a chord diagram.",
            style = "color:#666; font-size:13px; margin-bottom:0;"
          )
        ),

        bslib::accordion(
          open = c("Input Data", "Analysis Settings"),

          bslib::accordion_panel(
            "Input Data",
            shiny::fileInput(
              ns("file"),
              "Upload Data",
              accept = c(".csv", ".tsv", ".txt")
            ),

            shiny::div(
              style = "display:flex; gap:8px; flex-wrap:wrap; margin-top:6px;",
              shiny::actionButton(
                ns("run"),
                "Run",
                icon = bsicons::bs_icon("play-fill"),
                class = "btn-primary"
              ),
              shiny::actionButton(
                ns("clear_all"),
                "Clear",
                icon = bsicons::bs_icon("x-circle")
              )
            ),

            shiny::div(style = "margin-top:12px;"),
            shiny::uiOutput(ns("column_ui"))
          ),

          bslib::accordion_panel(
            "Analysis Settings",
            shiny::selectInput(
              ns("method"),
              "Correlation Method",
              choices = c("pearson", "spearman", "kendall"),
              selected = "pearson"
            ),

            shiny::numericInput(
              ns("threshold"),
              "Absolute Correlation Threshold",
              value = 0.6,
              min = 0,
              max = 1,
              step = 0.05
            ),

            shiny::sliderInput(
              ns("max_links"),
              "Maximum Number of Links",
              min = 10,
              max = 200,
              value = 50,
              step = 5
            )
          ),

          bslib::accordion_panel(
            "Display Settings",
            shiny::sliderInput(
              ns("label_cex"),
              "Label Size",
              min = 0.4,
              max = 1.4,
              value = 0.75,
              step = 0.05
            ),

            shiny::sliderInput(
              ns("transparency"),
              "Link Transparency",
              min = 0,
              max = 0.9,
              value = 0.35,
              step = 0.05
            ),

            colourpicker::colourInput(
              ns("pos_color"),
              "Positive Correlation Color",
              value = "#D73027"
            ),

            colourpicker::colourInput(
              ns("neg_color"),
              "Negative Correlation Color",
              value = "#4575B4"
            )
          ),

          bslib::accordion_panel(
            "Download",
            shiny::div(
              style = "display:flex; flex-direction:column; gap:10px;",
              shiny::downloadButton(
                ns("download_pdf"),
                "Download PDF"
              ),
              shiny::downloadButton(
                ns("download_png"),
                "Download PNG"
              ),
              shiny::downloadButton(
                ns("download_matrix"),
                "Download Correlation Matrix"
              )
            )
          )
        )
      ),

      shiny::div(
        style = "display:flex; flex-direction:column; gap:14px;",

        bslib::layout_columns(
          col_widths = c(3, 3, 3, 3),

          bslib::card(
            full_screen = FALSE,
            min_height = 120,
            style = "border-radius: 14px;",
            bslib::card_body(
              shiny::div(
                style = "font-size:12px; color:#666;",
                "Variables"
              ),
              shiny::div(
                style = "font-size:28px; font-weight:700; margin-top:6px;",
                shiny::textOutput(ns("metric_variables"), container = shiny::span)
              )
            )
          ),

          bslib::card(
            full_screen = FALSE,
            min_height = 120,
            style = "border-radius: 14px;",
            bslib::card_body(
              shiny::div(
                style = "font-size:12px; color:#666;",
                "Links Shown"
              ),
              shiny::div(
                style = "font-size:28px; font-weight:700; margin-top:6px;",
                shiny::textOutput(ns("metric_links"), container = shiny::span)
              )
            )
          ),

          bslib::card(
            full_screen = FALSE,
            min_height = 120,
            style = "border-radius: 14px;",
            bslib::card_body(
              shiny::div(
                style = "font-size:12px; color:#666;",
                "Positive Links"
              ),
              shiny::div(
                style = "font-size:28px; font-weight:700; margin-top:6px; color:#D73027;",
                shiny::textOutput(ns("metric_pos"), container = shiny::span)
              )
            )
          ),

          bslib::card(
            full_screen = FALSE,
            min_height = 120,
            style = "border-radius: 14px;",
            bslib::card_body(
              shiny::div(
                style = "font-size:12px; color:#666;",
                "Negative Links"
              ),
              shiny::div(
                style = "font-size:28px; font-weight:700; margin-top:6px; color:#4575B4;",
                shiny::textOutput(ns("metric_neg"), container = shiny::span)
              )
            )
          )
        ),

        bslib::layout_columns(
          col_widths = c(6, 6),

          bslib::card(
            full_screen = TRUE,
            min_height = 250,
            style = "border-radius: 14px;",
            bslib::card_header("Uploaded Data Preview"),
            bslib::card_body(
              DT::DTOutput(ns("data_preview"))
            )
          ),

          bslib::card(
            full_screen = TRUE,
            min_height = 250,
            style = "border-radius: 14px;",
            bslib::card_header("Correlation Matrix Preview"),
            bslib::card_body(
              DT::DTOutput(ns("cor_table"))
            )
          )
        ),

        bslib::layout_columns(
          col_widths = c(8, 4),

          bslib::card(
            full_screen = TRUE,
            min_height = 250,
            style = "border-radius: 14px;",
            bslib::card_header("Selected Links Preview"),
            bslib::card_body(
              DT::DTOutput(ns("edge_table"))
            )
          ),

          bslib::card(
            full_screen = FALSE,
            min_height = 250,
            style = "border-radius: 14px;",
            bslib::card_header("Analysis Summary"),
            bslib::card_body(
              shiny::htmlOutput(ns("summary_info"))
            )
          )
        ),

        bslib::card(
          full_screen = TRUE,
          min_height = 820,
          style = "border-radius: 14px;",
          bslib::card_header(
            shiny::div(
              style = "display:flex; justify-content:space-between; align-items:center;",
              shiny::span("Chord Diagram"),
              shiny::tags$div(
                style = "display:flex; gap:14px; align-items:center; font-size:12px; color:#666;",
                shiny::tags$span(
                  shiny::tags$span(
                    style = "display:inline-block; width:10px; height:10px; background:#D73027; border-radius:50%; margin-right:6px;"
                  ),
                  "Positive"
                ),
                shiny::tags$span(
                  shiny::tags$span(
                    style = "display:inline-block; width:10px; height:10px; background:#4575B4; border-radius:50%; margin-right:6px;"
                  ),
                  "Negative"
                )
              )
            )
          ),
          bslib::card_body(
            shiny::div(
              style = "font-size:13px; color:#666; margin-bottom:10px;",
              "The diagram displays the strongest pairwise correlations that pass the selected threshold and maximum-link limit."
            ),
            shiny::plotOutput(ns("chord_plot"), height = "730px")
          )
        )
      )
    )
  )
}

#' Correlation Chord Diagram Server
#' Server module for correlation chord diagram visualization.
#' @param id Module id.
#' @return Shiny server module.
#' @import shiny
#' @importFrom DT renderDT datatable
#' @importFrom circlize chordDiagram circos.clear circos.par circos.trackPlotRegion get.cell.meta.data circos.text
#' @importFrom grDevices pdf png dev.off adjustcolor hcl.colors
#' @importFrom utils read.csv read.delim write.csv head
#' @name correlation_chord_server
#' @export
#'
correlation_chord_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      data = NULL,
      cor_mat = NULL,
      edge_df = NULL
    )

    read_input_data <- function(path, filename) {
      ext <- tolower(tools::file_ext(filename))
      if (ext == "csv") {
        utils::read.csv(path, check.names = FALSE)
      } else {
        utils::read.delim(path, check.names = FALSE)
      }
    }

    rescale_to <- function(x, to = c(1, 8)) {
      if (length(x) == 0) {
        return(numeric(0))
      }
      if (all(is.na(x))) {
        return(rep(mean(to), length(x)))
      }
      rng <- range(x, na.rm = TRUE)
      if (diff(rng) == 0) {
        return(rep(mean(to), length(x)))
      }
      (x - rng[1]) / diff(rng) * (to[2] - to[1]) + to[1]
    }

    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      rv$data <- read_input_data(input$file$datapath, input$file$name)
      rv$cor_mat <- NULL
      rv$edge_df <- NULL
    })

    shiny::observeEvent(input$clear_all, {
      rv$data <- NULL
      rv$cor_mat <- NULL
      rv$edge_df <- NULL

      shiny::showNotification(
        "Inputs and results have been cleared.",
        type = "message",
        duration = 2
      )
    })

    shiny::observe({
      shiny::req(rv$data)

      numeric_cols <- names(rv$data)[vapply(rv$data, is.numeric, logical(1))]

      output$column_ui <- shiny::renderUI({
        shiny::selectizeInput(
          session$ns("columns"),
          "Select Numeric Columns",
          choices = numeric_cols,
          selected = head(numeric_cols, min(length(numeric_cols), 20)),
          multiple = TRUE,
          options = list(
            placeholder = "Select numeric columns"
          )
        )
      })
    })

    output$data_preview <- DT::renderDT({
      if (is.null(rv$data)) {
        return(
          DT::datatable(
            data.frame(Message = "Upload a file to preview the dataset."),
            options = list(dom = "t", paging = FALSE),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        utils::head(rv$data, 10),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
    })

    shiny::observeEvent(input$run, {
      shiny::req(rv$data)
      shiny::req(input$columns)
      shiny::req(length(input$columns) >= 2)

      shiny::withProgress(message = "Running correlation analysis...", value = 0, {
        shiny::incProgress(0.15, detail = "Preparing selected variables...")

        df <- rv$data[, input$columns, drop = FALSE]
        df <- df[, vapply(df, is.numeric, logical(1)), drop = FALSE]

        shiny::validate(
          shiny::need(ncol(df) >= 2, "Please select at least two numeric columns.")
        )

        shiny::incProgress(0.30, detail = "Calculating correlation matrix...")

        cor_mat <- stats::cor(
          df,
          method = input$method,
          use = "pairwise.complete.obs"
        )

        shiny::incProgress(0.55, detail = "Filtering strongest links...")

        cor_df <- as.data.frame(as.table(cor_mat), stringsAsFactors = FALSE)
        colnames(cor_df) <- c("from", "to", "correlation")

        cor_df <- cor_df[cor_df$from != cor_df$to, , drop = FALSE]

        cor_df$key <- apply(cor_df[, c("from", "to")], 1, function(x) {
          paste(sort(x), collapse = "___")
        })
        cor_df <- cor_df[!duplicated(cor_df$key), , drop = FALSE]
        cor_df$key <- NULL

        cor_df <- cor_df[abs(cor_df$correlation) >= input$threshold, , drop = FALSE]

        if (nrow(cor_df) > 0) {
          cor_df <- cor_df[order(abs(cor_df$correlation), decreasing = TRUE), , drop = FALSE]
          cor_df <- utils::head(cor_df, input$max_links)
        }

        cor_df$link_color <- ifelse(
          cor_df$correlation >= 0,
          input$pos_color,
          input$neg_color
        )

        cor_df$link_lwd <- rescale_to(abs(cor_df$correlation), to = c(1.2, 8))

        shiny::incProgress(0.85, detail = "Updating outputs...")

        rv$cor_mat <- cor_mat
        rv$edge_df <- cor_df

        shiny::incProgress(1, detail = "Completed.")
      })

      shiny::showNotification(
        "Correlation chord diagram generated successfully.",
        type = "message",
        duration = 2
      )
    })

    output$cor_table <- DT::renderDT({
      if (is.null(rv$cor_mat)) {
        return(
          DT::datatable(
            data.frame(Message = "Run the analysis to preview the correlation matrix."),
            options = list(dom = "t", paging = FALSE),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        round(rv$cor_mat, 3),
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          autoWidth = TRUE
        )
      )
    })

    output$edge_table <- DT::renderDT({
      if (is.null(rv$edge_df)) {
        return(
          DT::datatable(
            data.frame(Message = "Run the analysis to preview selected correlation links."),
            options = list(dom = "t", paging = FALSE),
            rownames = FALSE
          )
        )
      }

      show_df <- rv$edge_df[, c("from", "to", "correlation"), drop = FALSE]
      show_df$correlation <- round(show_df$correlation, 3)

      DT::datatable(
        show_df,
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          autoWidth = TRUE
        ),
        rownames = FALSE
      )
    })

    output$summary_info <- shiny::renderUI({
      if (is.null(rv$data) || is.null(rv$edge_df) || is.null(rv$cor_mat)) {
        return(
          shiny::HTML(
            paste(
              "<div style='color:#666; font-size:14px; line-height:1.95;'>",
              "<b>Status:</b> Waiting for analysis.<br/>",
              "<b>Variables selected:</b> -<br/>",
              "<b>Total links shown:</b> -<br/>",
              "<b>Threshold:</b> -<br/>",
              "<b>Method:</b> -<br/>",
              "<b>Recommendation:</b> Upload data, select variables, and click Run.",
              "</div>"
            )
          )
        )
      }

      pos_n <- sum(rv$edge_df$correlation > 0)
      neg_n <- sum(rv$edge_df$correlation < 0)

      shiny::HTML(
        paste0(
          "<div style='font-size:14px; line-height:1.95;'>",
          "<b>Status:</b> Ready<br/>",
          "<b>Variables selected:</b> ", ncol(rv$cor_mat), "<br/>",
          "<b>Total links shown:</b> ", nrow(rv$edge_df), "<br/>",
          "<b>Positive links:</b> <span style='color:#D73027;'>", pos_n, "</span><br/>",
          "<b>Negative links:</b> <span style='color:#4575B4;'>", neg_n, "</span><br/>",
          "<b>Threshold:</b> ", input$threshold, "<br/>",
          "<b>Method:</b> ", input$method,
          "</div>"
        )
      )
    })

    output$metric_variables <- shiny::renderText({
      if (is.null(rv$cor_mat)) "-" else ncol(rv$cor_mat)
    })

    output$metric_links <- shiny::renderText({
      if (is.null(rv$edge_df)) "-" else nrow(rv$edge_df)
    })

    output$metric_pos <- shiny::renderText({
      if (is.null(rv$edge_df)) "-" else sum(rv$edge_df$correlation > 0)
    })

    output$metric_neg <- shiny::renderText({
      if (is.null(rv$edge_df)) "-" else sum(rv$edge_df$correlation < 0)
    })

    draw_chord <- function(edge_df, label_cex, transparency) {
      if (is.null(edge_df) || nrow(edge_df) == 0) {
        plot.new()
        graphics::text(
          0.5, 0.5,
          "No correlations passed the current threshold/filter."
        )
        return(invisible(NULL))
      }

      circlize::circos.clear()

      n_sectors <- length(unique(c(edge_df$from, edge_df$to)))
      gap_val <- if (n_sectors <= 10) 6 else if (n_sectors <= 20) 4 else 2

      circlize::circos.par(
        start.degree = 90,
        gap.degree = gap_val,
        track.margin = c(0.01, 0.01),
        cell.padding = c(0, 0, 0, 0)
      )

      sectors <- unique(c(edge_df$from, edge_df$to))
      grid_cols <- stats::setNames(
        grDevices::hcl.colors(length(sectors), palette = "Set 3"),
        sectors
      )

      link_cols <- mapply(
        function(col, tr) grDevices::adjustcolor(col, alpha.f = 1 - tr),
        edge_df$link_color,
        transparency,
        SIMPLIFY = TRUE
      )

      circlize::chordDiagram(
        x = edge_df[, c("from", "to", "correlation")],
        grid.col = grid_cols,
        col = link_cols,
        transparency = transparency,
        annotationTrack = "grid",
        preAllocateTracks = list(track.height = 0.14),
        link.sort = TRUE,
        link.decreasing = FALSE,
        link.lwd = edge_df$link_lwd,
        directional = 0
      )

      circlize::circos.trackPlotRegion(
        track.index = 1,
        bg.border = NA,
        panel.fun = function(x, y) {
          sector.name <- circlize::get.cell.meta.data("sector.index")
          xlim <- circlize::get.cell.meta.data("xlim")
          ylim <- circlize::get.cell.meta.data("ylim")

          circlize::circos.text(
            x = mean(xlim),
            y = ylim[1] + 0.7,
            labels = sector.name,
            facing = "clockwise",
            niceFacing = TRUE,
            adj = c(0, 0.5),
            cex = label_cex
          )
        }
      )
    }

    output$chord_plot <- shiny::renderPlot({
      if (is.null(rv$edge_df)) {
        plot.new()
        graphics::text(
          0.5, 0.5,
          "Upload data and click Run to generate the chord diagram."
        )
        return()
      }

      draw_chord(
        edge_df = rv$edge_df,
        label_cex = input$label_cex,
        transparency = input$transparency
      )
    })

    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        "correlation_chord_diagram.pdf"
      },
      content = function(file) {
        grDevices::pdf(file, width = 10, height = 10)
        draw_chord(
          edge_df = rv$edge_df,
          label_cex = input$label_cex,
          transparency = input$transparency
        )
        grDevices::dev.off()
      }
    )

    output$download_png <- shiny::downloadHandler(
      filename = function() {
        "correlation_chord_diagram.png"
      },
      content = function(file) {
        grDevices::png(file, width = 3000, height = 3000, res = 300)
        draw_chord(
          edge_df = rv$edge_df,
          label_cex = input$label_cex,
          transparency = input$transparency
        )
        grDevices::dev.off()
      }
    )

    output$download_matrix <- shiny::downloadHandler(
      filename = function() {
        "correlation_matrix.csv"
      },
      content = function(file) {
        if (is.null(rv$cor_mat)) {
          utils::write.csv(
            data.frame(Message = "No correlation matrix available."),
            file,
            row.names = FALSE
          )
        } else {
          utils::write.csv(rv$cor_mat, file, row.names = TRUE)
        }
      }
    )
  })
}
