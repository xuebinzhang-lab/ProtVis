#' Protein Structure Analysis UI Module
#' Creates a user interface for protein secondary structure analysis using
#' bio3d package to perform Normal Mode Analysis (NMA) on PDB files.
#' @title Protein Structure UI
#' @param id A unique identifier for the Shiny namespace
#' @import shiny
#' @import bslib
#' @name protein_structure_ui
#' @export
#'
protein_structure_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = 'Secondary structure of protein',
    icon = bsicons::bs_icon("play-circle"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('file'),
            label = 'File(.pdb)',
            multiple = FALSE,
            accept = '.pdb'
          )
        )
      ),
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1,
          height = 750,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Secondary structure of protein",
            sidebar = bslib::accordion(
              open = 'closed',
              bslib::accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(
                  inputId = ns("sheet_color"),
                  label = "beta sheet color",
                  value = "orange"),
                colourpicker::colourInput(
                  inputId = ns("helix_color"),
                  label = "alpha helix color",
                  value = "purple")
              ),
              bslib::accordion_panel(
                title = 'Run',
                shiny::actionButton(ns("run"), "Run")
              ),
              bslib::accordion_panel(
                title = 'Download',
                shiny::numericInput(
                  inputId = ns("plot_width"),
                  label = "Plot width (inches)",
                  value = 10,
                  min = 5,
                  max = 20,
                  step = 0.5
                ),
                shiny::numericInput(
                  inputId = ns("plot_height"),
                  label = "Plot height (inches)",
                  value = 6,
                  min = 4,
                  max = 15,
                  step = 0.5
                ),
                shiny::selectInput(
                  inputId = ns("plot_units"),
                  label = "Plot units",
                  choices = c("inches" = "in", "centimeters" = "cm", "millimeters" = "mm"),
                  selected = "in"
                ),
                shiny::numericInput(
                  inputId = ns("plot_dpi"),
                  label = "Resolution (DPI)",
                  value = 300,
                  min = 72,
                  max = 1200,
                  step = 1
                ),
                shiny::selectInput(
                  inputId = ns("plot_format"),
                  label = "File format",
                  choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "SVG" = "svg"),
                  selected = "pdf"
                ),
                shiny::hr(),
                shiny::downloadButton(ns("downloadPlot"), "Download Plot"),
                shiny::p(style = "font-size: 12px; color: #666; margin-top: 10px;",
                         "Note: PDF format supports vector graphics, PNG/JPEG/TIFF are raster images.")
              )
            ),
            shiny::mainPanel(
              shiny::plotOutput(ns("protein_2"))
            )
          )
        )
      )
    )
  )
}

#' Protein Structure Analysis Server Module
#'
#' Server-side logic for protein secondary structure analysis. Performs
#' Normal Mode Analysis (NMA) on uploaded PDB files and generates fluctuation
#' plots with secondary structure elements highlighted.
#'
#' @description Server logic for protein_structure module
#' @param id Standard shiny server identifier
#' @return A Shiny server module function
#' @import shiny
#' @name protein_structure_server
#' @export

protein_structure_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (!base::exists("elements") || !base::exists("atom.index")) {
      utils::data(elements, package = "bio3d")
      utils::data(atom.index, package = "bio3d")
    }
    plot_data <- shiny::reactiveValues(
      modes = NULL,
      pdb = NULL
    )
    shiny::observeEvent(input$run, {
      shiny::req(input$file)
      shiny::showNotification("Reading PDB file...", type = "message", duration = 2)
      tryCatch({
        pdb <- bio3d::read.pdb(input$file$datapath)
        shiny::showNotification("Performing Normal Mode Analysis...", type = "message", duration = 2)
        modes <- bio3d::nma(pdb)
        plot_data$modes <- modes
        plot_data$pdb <- pdb
        output$protein_2 <- shiny::renderPlot({
          if (!base::is.null(modes) && !base::is.null(modes$fluctuations)) {
            bio3d::plot.bio3d(
              modes$fluctuations,
              sse = pdb,
              sheet.col = input$sheet_color,
              helix.col = input$helix_color,
              typ = "l",
              lwd = 3,
              ylab = "Fluctuations from NMA (Å)"
            )
          } else {
            graphics::plot.new()
            graphics::text(0.5, 0.5, "Analysis failed or no fluctuations data",
                           cex = 1.2, col = "red")
          }
        })
        output$downloadPlot <- shiny::downloadHandler(
          filename = function() {
            base_name <- base::paste0("protein_structure_", Sys.Date())
            format <- input$plot_format
            switch(format,
                   "pdf" = paste0(base_name, ".pdf"),
                   "png" = paste0(base_name, ".png"),
                   "jpeg" = paste0(base_name, ".jpeg"),
                   "tiff" = paste0(base_name, ".tiff"),
                   "svg" = paste0(base_name, ".svg"))
          },
          content = function(file) {
            shiny::req(plot_data$modes, plot_data$pdb)
            format <- input$plot_format
            width <- base::as.numeric(input$plot_width)
            height <- base::as.numeric(input$plot_height)
            units <- input$plot_units
            dpi <- base::as.numeric(input$plot_dpi)
            if (format == "pdf") {
              grDevices::pdf(file, width = width, height = height)
            } else if (format == "png") {
              grDevices::png(file, width = width, height = height,
                             units = units, res = dpi)
            } else if (format == "jpeg") {
              grDevices::jpeg(file, width = width, height = height,
                              units = units, res = dpi, quality = 100)
            } else if (format == "tiff") {
              grDevices::tiff(file, width = width, height = height,
                              units = units, res = dpi, compression = "lzw")
            } else if (format == "svg") {
              grDevices::svg(file, width = width, height = height)
            }
            bio3d::plot.bio3d(
              plot_data$modes$fluctuations,
              sse = plot_data$pdb,
              sheet.col = input$sheet_color,
              helix.col = input$helix_color,
              typ = "l",
              lwd = 3,
              ylab = "Fluctuations from NMA (Å)"
            )
            graphics::title(main = "Protein Secondary Structure Analysis",
                            sub = base::paste("Generated:", base::Sys.Date()))
            grDevices::dev.off()
          },
          contentType = switch(input$plot_format,
                               "pdf" = "application/pdf",
                               "png" = "image/png",
                               "jpeg" = "image/jpeg",
                               "tiff" = "image/tiff",
                               "svg" = "image/svg+xml")
        )
        shiny::showNotification("Analysis completed successfully!",
                                type = "message", duration = 3)
      }, error = function(e) {
        output$protein_2 <- shiny::renderPlot({
          graphics::plot.new()
          graphics::text(0.5, 0.5, paste("Error:", e$message),
                         cex = 1, col = "red")
        })
        shiny::showNotification(paste("Error:", e$message),
                                type = "error", duration = 5)
      })
    })
  })
}
