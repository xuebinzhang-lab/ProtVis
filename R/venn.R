#' Venn Diagram UI Module
#' Creates a user interface for generating Venn diagrams or UpSet plots
#' from uploaded CSV data, with table display and download functionality.
#' @description Venn/UpSet visualization UI with file upload, plot type selection,
#'   color customization, processed table display, and plot download.
#' @param id A unique identifier for the Shiny namespace.
#' @title venn_ui
#' @return A Shiny UI nav_panel containing the Venn/UpSet interface.
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @name venn_ui
#' @export
#'
venn_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::fileInput(
          inputId = ns("file"),
          label = "File",
          multiple = FALSE,
          accept = ".csv"
        ),
        shiny::actionButton(ns("load_example"), "Use example data", class = "btn btn-outline-primary w-100"),
        shiny::radioButtons(
          inputId = ns("plot_type"),
          label = "Choose plot type",
          choices = c("Auto" = "auto", "Venn" = "venn", "UpSet" = "upset"),
          selected = "auto"
        ),
        shiny::uiOutput(ns("colorSelectors")),
        shiny::actionButton(ns("run"), "Run"),
        shiny::br(),
        shiny::br(),
        shiny::downloadButton(ns("downloadPlot"), "Download")
      ),
      bslib::page_fluid(
        shiny::uiOutput(ns("plot_notice")),
        bslib::card(
          full_screen = TRUE,
          style = "margin-bottom: 20px;",
          bslib::card_header("Venn / UpSet Plot"),
          bslib::card_body(
            shiny::plotOutput(ns("venn_plot"), height = "500px")
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Processed Table"),
          bslib::card_body(
            DT::DTOutput(ns("processed_table"))
          )
        )
      )
    )
}

#' Venn Diagram Server Module
#' Server-side logic for the Venn/UpSet module. Handles CSV file processing,
#' dynamic color selection, plot generation, processed table display,
#' and PDF download functionality.
#' @description Server logic for generating Venn diagrams or UpSet plots
#'   with automatic or manual plot type selection.
#'   Uses Venn for <= 6 sets and UpSet for > 6 sets in Auto mode.
#' @title venn_server
#' @param id Standard shiny server identifier.
#' @return A Shiny server module function.
#' @import shiny
#' @importFrom utils read.csv
#' @importFrom colourpicker colourInput
#' @importFrom tidyr pivot_longer everything pivot_wider
#' @importFrom dplyr distinct mutate across
#' @importFrom tibble column_to_rownames
#' @importFrom ggplot2 ggsave
#' @importFrom grDevices pdf dev.off colors
#' @importFrom DT renderDT datatable
#' @name venn_server
#' @export
#'

utils::globalVariables(c("Name", "Set", "everything", "across"))

venn_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      set_list = NULL,
      upset_data = NULL,
      upset_table = NULL,
      colors = NULL,
      plot_mode = NULL,
      example_df = NULL
    )

    make_example_venn_data <- function() {
      data.frame(
        Proteome = c("P53", "MAPK1", "AKT1", "MTOR", "STAT3", "EGFR", NA),
        Transcriptome = c("P53", "MAPK1", "HIF1A", "STAT3", "JUN", NA, NA),
        Phosphoproteome = c("AKT1", "MTOR", "MAPK1", "EGFR", "SRC", "JUN", NA),
        stringsAsFactors = FALSE
      )
    }

    parsed_data <- shiny::reactive({
      if (!is.null(rv$example_df)) {
        df <- rv$example_df
      } else {
        shiny::req(input$file)
        df <- utils::read.csv(
          file = input$file$datapath,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }

      df[] <- lapply(df, function(x) {
        x <- as.character(x)
        x <- trimws(x)
        x[x == ""] <- NA
        x
      })

      set_list <- lapply(df, function(x) unique(stats::na.omit(x)))

      long_df <- df %>%
        tidyr::pivot_longer(
          cols = tidyr::everything(),
          names_to = "Set",
          values_to = "Name"
        ) %>%
        dplyr::distinct(Name, Set) %>%
        dplyr::mutate(
          Name = trimws(as.character(Name)),
          Set = trimws(as.character(Set))
        )

      long_df <- long_df[!is.na(long_df$Name) & long_df$Name != "", , drop = FALSE]

      bin_df <- long_df %>%
        tidyr::pivot_wider(
          names_from = Set,
          values_from = Set,
          values_fill = list(Set = "0")
        ) %>%
        dplyr::mutate(
          dplyr::across(-Name, ~ ifelse(. == "0", 0, 1))
        )

      row_mat <- bin_df %>%
        tibble::column_to_rownames("Name")

      list(
        set_list = set_list,
        bin_df = bin_df,
        row_mat = row_mat
      )
    })

    refresh_color_selectors <- function() {
      dat <- parsed_data()

      output$colorSelectors <- shiny::renderUI({
        lapply(seq_along(names(dat$set_list)), function(i) {
          colourpicker::colourInput(
            inputId = session$ns(paste0("color_", i)),
            label = paste("Select Color for", names(dat$set_list)[i]),
            value = grDevices::hcl.colors(length(dat$set_list), "Set2")[i]
          )
        })
      })
    }

    shiny::observeEvent(input$load_example, {
      rv$example_df <- make_example_venn_data()
      refresh_color_selectors()
      shiny::showNotification("Example Venn/UpSet data loaded. Click Run to draw the plot.", type = "message")
    })

    shiny::observeEvent(input$file, {
      rv$example_df <- NULL
      dat <- parsed_data()
      refresh_color_selectors()
    })

    get_plot_mode <- shiny::reactive({
      dat <- parsed_data()
      n_sets <- length(dat$set_list)

      if (input$plot_type == "auto") {
        if (n_sets <= 6) {
          "venn"
        } else {
          "upset"
        }
      } else {
        input$plot_type
      }
    })

    shiny::observeEvent(input$run, {
      dat <- parsed_data()

      rv$set_list <- dat$set_list
      rv$upset_data <- dat$row_mat
      rv$upset_table <- dat$bin_df
      rv$colors <- sapply(seq_along(names(dat$set_list)), function(i) {
        selected_color <- input[[paste0("color_", i)]]
        if (is.null(selected_color) || identical(selected_color, "")) {
          grDevices::hcl.colors(length(dat$set_list), "Set2")[i]
        } else {
          selected_color
        }
      })
      rv$plot_mode <- get_plot_mode()
    })

    output$plot_notice <- shiny::renderUI({
      shiny::req(!is.null(input$file) || !is.null(rv$example_df))

      dat <- parsed_data()
      n_sets <- length(dat$set_list)

      if (input$plot_type == "venn" && n_sets > 6) {
        shiny::div(
          style = "padding:10px 14px; margin-bottom:12px; background:#fff3cd; color:#856404; border-radius:8px;",
          "Current dataset contains more than 6 sets. Please use Auto or UpSet."
        )
      } else {
        shiny::div(
          style = "padding:8px 12px; margin-bottom:12px; background:#f8f9fa; color:#495057; border-radius:8px;",
          paste("Detected", n_sets, "sets. Current mode:", toupper(get_plot_mode()))
        )
      }
    })

    output$venn_plot <- shiny::renderPlot({
      shiny::req(input$run > 0)
      shiny::req(rv$set_list, rv$upset_data, rv$colors, rv$plot_mode)

      if (rv$plot_mode == "venn") {
        shiny::validate(
          shiny::need(
            length(rv$set_list) <= 6,
            "More than 6 sets detected. Please switch to Auto or UpSet."
          )
        )

        ggvenn::ggvenn(
          data = rv$set_list,
          fill_color = rv$colors
        )
      } else {
        shiny::validate(
          shiny::need(
            ncol(rv$upset_data) >= 1,
            "No valid set data available for UpSet plot."
          )
        )

        UpSetR::upset(
          data = as.data.frame(rv$upset_data),
          sets = colnames(rv$upset_data),
          keep.order = TRUE,
          order.by = "freq"
        )
      }
    })

    output$processed_table <- DT::renderDT({
      shiny::req(input$run > 0)
      shiny::req(rv$upset_table)

      DT::datatable(
        rv$upset_table,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
    })

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("venn_upset_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        shiny::req(rv$set_list, rv$upset_data, rv$colors, rv$plot_mode)

        if (rv$plot_mode == "venn") {
          ggplot2::ggsave(
            filename = file,
            plot = ggvenn::ggvenn(
              data = rv$set_list,
              fill_color = rv$colors
            ),
            width = 8,
            height = 6,
            dpi = 300,
            device = "pdf"
          )
        } else {
          grDevices::pdf(file, width = 10, height = 7)
          UpSetR::upset(
            data = as.data.frame(rv$upset_data),
            sets = colnames(rv$upset_data),
            keep.order = TRUE,
            order.by = "freq"
          )
          grDevices::dev.off()
        }
      }
    )
  })
}
