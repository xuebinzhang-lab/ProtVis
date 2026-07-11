#' Post-Translational Modification (PTM) Module UI
#' @description
#' This function defines the user interface for the Post-Translational Modification (PTM)
#' analysis module. It provides a comprehensive sidebar with accordion panels for
#' data uploading, ion modification settings, peptide selection, and visualization
#' parameters. The main panel displays the resulting PTM status or visualization.
#' @param id A character string representing the Shiny module namespace ID.
#' @return A Shiny UI tag list containing a sidebar layout with accordion controls
#' and a main display card.
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @name PTM_ui
#' @export

PTM_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        bslib::accordion(
          bslib::accordion_panel(
            title = "Upload File",
            shiny::fileInput(ns("ptm_file"),
                             "Upload PTM Data",
                             accept = c(".csv", ".xlsx"),
                             multiple = FALSE)
          ),
          bslib::accordion_panel(
            title = "Ion Modifications",
            shiny::selectInput(ns("add_modification"),
                               label = "Select Ion(s) to Add",
                               choices = base::list(
                                 "Add H⁺ (Protonation)" = "add_H_plus",
                                 "Add H₂O (Hydration)" = "add_H2O",
                                 "Add NH₃ (Ammonia)" = "add_NH3",
                                 "Add CH₃ (Methylation)" = "add_methyl",
                                 "Add H₂ (Hydrogenation)" = "add_H2",
                                 "Add CO₂ (Decarboxylation)" = "add_CO2"
                               ),
                               multiple = TRUE
            ),
            shiny::selectInput(ns("remove_modification"),
                               label = "Select Ion(s) to Remove",
                               choices = base::list(
                                 "Remove H⁺ (Deprotonation)" = "remove_H_plus",
                                 "Remove H₂O (Dehydration)" = "remove_H2O",
                                 "Remove NH₃ (Deammoniation)" = "remove_NH3",
                                 "Remove CH₃ (Demethylation)" = "remove_methyl",
                                 "Remove H₂ (Dehydrogenation)" = "remove_H2",
                                 "Remove CO₂ (Decarboxylation)" = "remove_CO2"
                               ),
                               multiple = TRUE
            )
          ),
          bslib::accordion_panel(
            title = "Peptide Selection",
            shiny::selectInput(ns("Peptide"),
                               "Select Peptide",
                               choices = c(
                                 "GIVDQSQQAYQEAFEISK",
                                 "ACCPLEGVRPSPQQTEYR",
                                 "GLTMLDHEQVTPEDPGAQFLIR",
                                 "ACFLMAHNGWVMGDDPLR",
                                 "EILVGDVGQTVDDPYATFVK"),
                               selected = "ACCPLEGVRPSPQQTEYR")
          ),
          bslib::accordion_panel(
            title = "Modification Type",
            shiny::selectInput(ns("mod_type"),
                               "Select Modification Type",
                               choices = c("Phosphorylation", "Ubiquitination", "Acetylation", "Methylation", "Glycosylation"),
                               selected = "Phosphorylation")
          ),
          bslib::accordion_panel(
            title = "Color Selection for Ions",
            shiny::fluidRow(
              shiny::column(6,
                            tags$div(style = "font-weight: bold; text-align: left;", "b ion"),
                            colourpicker::colourInput(ns("color_up"),
                                                      "",
                                                      value = "blue")),
              shiny::column(6,
                            tags$div(style = "font-weight: bold; text-align: left;", "y ion"),
                            colourpicker::colourInput(ns("color_down"),
                                                      "",
                                                      value = "red"))
            )
          ),
          bslib::accordion_panel(
            title = "Visualization",
            shiny::actionButton(ns("visualize"),
                                "VISUALIZE PTM"),
            shiny::downloadButton(ns("download_ptm_plot"),
                                  "DOWNLOAD")
          )
        )
      ),

      main = bslib::card(
        height = "500px",
        bslib::card_header("Protein Post-translational Modifications (PTMs) Visualization"),
        bslib::card_body(
          shiny::textOutput(ns("status_message"))
        )
      )
    )
  )
}

#' Post-Translational Modification (PTM) Module Server
#' @description
#' Provides minimal server-side behavior for the PTM UI so uploaded data,
#' visualization requests, and downloads are wired into the Shiny app.
#' @param id A character string representing the Shiny module namespace ID.
#' @return None. Called for side effects in the Shiny session.
#' @import shiny
#' @importFrom ggplot2 ggplot aes geom_col theme_minimal labs ggsave
#' @name PTM_server
#' @export
PTM_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ptm_data <- shiny::reactive({
      shiny::req(input$ptm_file)
      ext <- base::tolower(tools::file_ext(input$ptm_file$name))
      if (ext == "csv") {
        return(utils::read.csv(input$ptm_file$datapath, stringsAsFactors = FALSE, check.names = FALSE))
      }
      if (ext %in% c("xlsx", "xls")) {
        return(as.data.frame(readxl::read_excel(input$ptm_file$datapath), check.names = FALSE))
      }
      stop("Unsupported PTM file format. Please upload a csv, xlsx, or xls file.", call. = FALSE)
    })

    ptm_plot <- shiny::eventReactive(input$visualize, {
      dat <- ptm_data()
      mod_label <- if (base::is.null(input$mod_type)) "Modification" else input$mod_type
      if (base::nrow(dat) == 0 || base::ncol(dat) == 0) {
        stop("Uploaded PTM data is empty.", call. = FALSE)
      }
      value_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      if (base::length(value_cols) == 0) {
        plot_df <- data.frame(
          Category = names(dat),
          Count = vapply(dat, function(x) sum(!is.na(x) & x != ""), numeric(1)),
          stringsAsFactors = FALSE
        )
      } else {
        plot_df <- data.frame(
          Category = value_cols,
          Count = vapply(dat[value_cols], function(x) sum(!is.na(x)), numeric(1)),
          stringsAsFactors = FALSE
        )
      }
      ggplot2::ggplot(plot_df, ggplot2::aes(x = Category, y = Count)) +
        ggplot2::geom_col(fill = if (base::is.null(input$color_up)) "blue" else input$color_up) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::labs(
          title = base::paste("PTM data overview:", mod_label),
          x = "Column",
          y = "Observed values"
        )
    })

    output$status_message <- shiny::renderText({
      if (base::is.null(input$ptm_file)) {
        return("Upload PTM data, choose modification settings, then click VISUALIZE PTM.")
      }
      dat <- ptm_data()
      base::paste(
        "Loaded", input$ptm_file$name,
        "with", base::nrow(dat), "rows and", base::ncol(dat), "columns.",
        "Selected peptide:", input$Peptide,
        "Modification:", input$mod_type
      )
    })

    output$download_ptm_plot <- shiny::downloadHandler(
      filename = function() {
        base::paste0("ptm-overview-", base::Sys.Date(), ".png")
      },
      content = function(file) {
        ggplot2::ggsave(file, plot = ptm_plot(), width = 7, height = 5, units = "in")
      }
    )
  })
}
