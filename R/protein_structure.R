#' protein_structure
#' @title protein_structure_ui
#' @name protein_structure_ui
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @export
#'
protein_structure_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Secondary structure of protein',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('file'),
            label = 'File(.pdb)',
            multiple = FALSE,
            accept = '.pdb'
          )
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 600,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Secondary structure of protein",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
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
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                downloadButton(ns("downloadPlot"), "Download PDF")
              )
            ),
            mainPanel(
              plotOutput(ns("protein_2"))
            )
          )
        )
      )
    )
  )
}
#' protein_structure Server Module
#' @description Server logic for protein_structure
#' @param id Standard shiny server arguments
#' @import shiny
#' @export
#'
protein_structure_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$run, {
      req(input$file)
      pdb <- bio3d::read.pdb(input$file$datapath)
      modes <- bio3d::nma(pdb)

      output$protein_2 <- renderPlot({
        bio3d::plot.bio3d(
          modes$fluctuations,
          sse = pdb,
          sheet.col = input$sheet_color,
          helix.col = input$helix_color,
          typ = "l",
          lwd = 3,
          ylab = "Fluctuations from NMA (custom label)"
        )
      })

      output$downloadPlot <- downloadHandler(
        filename = function() {
          paste0("protein_plot_", Sys.Date(), ".pdf")
        },
        content = function(file) {
          pdf(file, width = 8, height = 6)
          bio3d::plot.bio3d(
            modes$fluctuations,
            sse = pdb,
            sheet.col = input$sheet_color,
            helix.col = input$helix_color,
            typ = "l",
            lwd = 3,
            ylab = "Fluctuations from NMA (custom label)"
          )
          dev.off()
        }
      )
    })
  })
}
