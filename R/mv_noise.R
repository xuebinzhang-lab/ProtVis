#' Remove noise data UI Module
#' @description Remove noise from proteomics data
#' @param id A unique identifier for the Shiny namespace
#' @title mv_noise_ui
#' @name mv_noise_ui
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @export
#'
mv_noise_ui <- function(id) {
  ns <- NS(id)

  bslib::nav_panel(
    title = 'Missing value interpolation',
    icon = bsicons::bs_icon("c-square"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('file'),
            label = 'Upload Expression Matrix',
            multiple = FALSE,
            accept = c('.csv', '.tsv', '.txt')
          ),
          shiny::helpText("First column should contain protein IDs")
        ),
        bslib::accordion_panel(
          title = "Filter Settings",
          icon = bsicons::bs_icon("sliders"),
          shiny::numericInput(
            ns("na_threshold"),
            "Missing Value Threshold (%)",
            value = 50,
            min = 0,
            max = 100,
            step = 5
          ),
          shiny::helpText("Rows with higher % of missing values will be removed")
        ),
        bslib::accordion_panel(
          title = "Actions",
          icon = bsicons::bs_icon("gear"),
          shiny::actionButton(
            ns("runButton"),
            "Run",
            icon = shiny::icon("play"),
            class = "btn-primary"
          ),
          shiny::downloadButton(
            ns("downloadData"),
            "Download",
            class = "btn-success"
          )
        )
      ),
      bslib::page_fluid(
        shiny::h3("Proteomics Data Quality Control"),
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::navset_card_tab(
            height = "500px",
            full_screen = TRUE,
            title = "Raw Data",
            bslib::nav_panel(
              "Table",
              DT::DTOutput(ns("rawTable"))
            ),
            bslib::nav_panel(
              "Statistics",
              shiny::verbatimTextOutput(ns("rawStats"))
            ),
            bslib::nav_panel(
              "Distribution",
              shiny::plotOutput(ns("rawPlot"))
            ),
            bslib::nav_panel(
              "Missing Values",
              shiny::plotOutput(ns("rawMissingPlot"))
            )
          ),
          bslib::navset_card_tab(
            height = "500px",
            full_screen = TRUE,
            title = "Processed Data",
            bslib::nav_panel(
              "Table",
              DT::DTOutput(ns("cleanTable"))
            ),
            bslib::nav_panel(
              "Statistics",
              shiny::verbatimTextOutput(ns("cleanStats"))
            ),
            bslib::nav_panel(
              "Distribution",
              shiny::plotOutput(ns("cleanPlot"))
            ),
            bslib::nav_panel(
              "Missing Values",
              shiny::plotOutput(ns("cleanMissingPlot"))
            )
          )
        )
      )
    )
  )
}

#' Remove noise data server Module
#' @description Remove noise from proteomics data
#' @param id A unique identifier for the Shiny namespace
#' @title mv_noise_server
#' @name mv_noise_server
#' @import bsicons
#' @import shiny
#' @import bslib
#' @import ggplot2
#' @export
#'
mv_noise_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value for raw data
    raw_data <- shiny::reactive({
      shiny::req(input$file)
      tryCatch({
        df <- read.csv(input$file$datapath, row.names = 1, check.names = FALSE)
        shiny::validate(
          shiny::need(is.matrix(df) || is.data.frame(df), "Invalid data format"),
          shiny::need(nrow(df) > 0 && ncol(df) > 0, "Empty dataset")
        )
        as.matrix(df)
      }, error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
        NULL
      })
    })

    # Reactive value for cleaned data
    cleaned_data <- shiny::eventReactive(input$runButton, {
      shiny::req(raw_data())
      shiny::withProgress({
        shiny::setProgress(message = "Processing data...")

        data <- raw_data()
        na_percent <- rowSums(is.na(data)) / ncol(data) * 100
        threshold <- input$na_threshold
        data[na_percent <= threshold, ]
      })
    })

    # Raw data table
    output$rawTable <- DT::renderDT({
      shiny::req(raw_data())
      DT::datatable(
        raw_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv')
        ),
        extensions = 'Buttons',
        rownames = TRUE,
        caption = "Raw expression data"
      )
    })

    # Cleaned data table
    output$cleanTable <- DT::renderDT({
      shiny::req(cleaned_data())
      DT::datatable(
        cleaned_data(),
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv')
        ),
        extensions = 'Buttons',
        rownames = TRUE,
        caption = "Processed expression data"
      )
    })

    # Raw data statistics
    output$rawStats <- shiny::renderPrint({
      shiny::req(raw_data())
      data <- raw_data()

      cat("=== Raw Data Summary ===\n")
      cat("Proteins:", nrow(data), "\n")
      cat("Samples:", ncol(data), "\n")
      cat("Total missing values:", sum(is.na(data)), "\n")
      cat("Missing value percentage:",
          round(sum(is.na(data))/length(data)*100, 2), "%\n")
      cat("\nMissing values per sample:\n")
      print(summary(colSums(is.na(data))))
    })

    # Cleaned data statistics
    output$cleanStats <- shiny::renderPrint({
      shiny::req(cleaned_data())
      data <- cleaned_data()

      cat("=== Processed Data Summary ===\n")
      cat("Proteins remaining:", nrow(data), "\n")
      cat("Samples:", ncol(data), "\n")
      cat("Total missing values:", sum(is.na(data)), "\n")
      cat("Missing value percentage:",
          round(sum(is.na(data))/length(data)*100, 2), "%\n")
      cat("\nMissing values per sample:\n")
      print(summary(colSums(is.na(data))))
    })

    # Raw data distribution plot
    output$rawPlot <- shiny::renderPlot({
      shiny::req(raw_data())
      data <- raw_data()

      plot_data <- reshape2::melt(data)
      colnames(plot_data) <- c("Protein", "Sample", "Value")

      ggplot(plot_data, aes(x = Value)) +
        geom_histogram(fill = "steelblue", bins = 30) +
        facet_wrap(~Sample, scales = "free") +
        labs(title = "Raw Data Distribution",
             x = "Expression Value",
             y = "Count") +
        theme_minimal()
    })

    # Cleaned data distribution plot
    output$cleanPlot <- shiny::renderPlot({
      shiny::req(cleaned_data())
      data <- cleaned_data()

      plot_data <- reshape2::melt(data)
      colnames(plot_data) <- c("Protein", "Sample", "Value")

      ggplot(plot_data, aes(x = Value)) +
        geom_histogram(fill = "darkgreen", bins = 30) +
        facet_wrap(~Sample, scales = "free") +
        labs(title = "Processed Data Distribution",
             x = "Expression Value",
             y = "Count") +
        theme_minimal()
    })

    # Raw data missing value heatmap
    output$rawMissingPlot <- shiny::renderPlot({
      shiny::req(raw_data())
      data <- raw_data()

      heatmap_data <- is.na(data) * 1

      graphics::par(mar = c(5, 4, 4, 2) + 0.1)
      image(
        x = 1:ncol(heatmap_data),
        y = 1:nrow(heatmap_data),
        z = t(heatmap_data),
        col = c("white", "red"),
        xlab = "Samples",
        ylab = "Proteins",
        main = "Missing Values in Raw Data (Red = Missing)"
      )
    })

    # Cleaned data missing value heatmap
    output$cleanMissingPlot <- shiny::renderPlot({
      shiny::req(cleaned_data())
      data <- cleaned_data()

      heatmap_data <- is.na(data) * 1

      graphics::par(mar = c(5, 4, 4, 2) + 0.1)
      image(
        x = 1:ncol(heatmap_data),
        y = 1:nrow(heatmap_data),
        z = t(heatmap_data),
        col = c("white", "red"),
        xlab = "Samples",
        ylab = "Proteins",
        main = "Missing Values in Processed Data (Red = Missing)"
      )
    })

    # Download handler
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        paste0("processed_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        shiny::req(cleaned_data())
        write.csv(cleaned_data(), file)
      }
    )
  })
}
