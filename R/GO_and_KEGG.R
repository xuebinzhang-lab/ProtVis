#' GO and KEGG Database User Interface
#'
#' Creates a user interface for accessing GO (Gene Ontology) and KEGG (Kyoto Encyclopedia of Genes and Genomes) databases
#' in a Shiny application.
#' @title GO_and_KEGG_ui
#' @name GO_and_KEGG_ui
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @export
#'
GO_and_KEGG_ui <- function(id) {
  ns <- NS(id)
  shiny::nav_panel(
    title = 'GO and KEGG analyze',
    icon = shiny::icon("play-circle"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = shiny::icon("upload"),
          shiny::fileInput(
            inputId = ns('file'),
            label = 'Background (.xlsx)',
            multiple = FALSE,
            accept = '.xlsx'
          ),
          shiny::fileInput(
            inputId = ns('gene_list'),
            label = 'Gene list (.xlsx)',
            multiple = FALSE,
            accept = '.xlsx'
          ),
          shiny::radioButtons(
            inputId = ns("choice"),
            label = "Select a function:",
            choices = c("GO" = "choice_go",
                        "KEGG" = "choice_KEGG"),
            selected = "GO"
          ),
          shiny::actionButton(ns("run"), "Run")
        )
      ),
      bslib::page_fluid(
        shiny::fluidRow(
          # Left panel for plot
          shiny::column(
            width = 6,
            height = 600,
            shiny::navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "GO/KEGG Plot",
              sidebar = bslib::accordion(
                bslib::accordion_panel(
                  title = 'Parameter',
                  shiny::uiOutput(ns("term2gene_ui")),
                  shiny::uiOutput(ns("term2name_ui"))
                )
              ),
              shiny::mainPanel(
                shiny::plotOutput(ns("plot_result"))
              )
            )
          ),
          # Right panel for table
          shiny::column(
            width = 6,
            height = 600,
            shiny::navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Results Table",
              shiny::mainPanel(
                DT::DTOutput(ns("result_table"))
              )
            )
          )
        )
      )
    )
  )
}

#' GO and KEGG Server Logic
#'
#' Implements the logic for GO and KEGG analysis.
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import clusterProfiler
#' @export
#' @name GO_and_KEGG_server
#'

GO_and_KEGG_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Reactive to read uploaded background file
    background_data <- shiny::reactive({
      shiny::req(input$file)
      readxl::excel_sheets(input$file$datapath)
    })
    # Update TERM2GENE and TERM2NAME choices dynamically
    shiny::observeEvent(background_data(), {
      shiny::updateSelectInput(session, "term2gene", choices = background_data())
      shiny::updateSelectInput(session, "term2name", choices = background_data())
    })
    # UI for TERM2GENE
    output$term2gene_ui <- shiny::renderUI({
      shiny::req(input$file)
      shiny::selectInput(ns("term2gene"), "Select TERM2GENE from Background", choices = NULL)
    })
    # UI for TERM2NAME
    output$term2name_ui <- shiny::renderUI({
      shiny::req(input$file)
      shiny::selectInput(ns("term2name"), "Select TERM2NAME from Background", choices = NULL)
    })
    # Reactive for TERM2GENE and TERM2NAME
    t2g.go <- shiny::reactive({
      shiny::req(input$file, input$term2gene)
      readxl::read_excel(input$file$datapath, sheet = input$term2gene)
    })
    t2n.go <- shiny::reactive({
      shiny::req(input$file, input$term2name)
      readxl::read_excel(input$file$datapath, sheet = input$term2name)
    })
    # Placeholder for `glist`
    glist <- shiny::reactive({
      shiny::req(input$gene_list)
      gene_data <- readxl::read_excel(input$gene_list$datapath)
      gene_data[[1]]  # Assuming the first column contains gene IDs
    })
    # Perform enrichment analysis when the "Run" button is clicked
    enrichment_results <- shiny::eventReactive(input$run, {
      shiny::req(glist(), t2g.go(), t2n.go())
      clusterProfiler::enricher(
        gene = glist(),
        TERM2GENE = t2g.go(),
        TERM2NAME = t2n.go(),
        pvalueCutoff = 1,
        qvalueCutoff = 1
      )
    })
    # Render plot for enrichment results
    output$plot_result <- shiny::renderPlot({
      shiny::req(enrichment_results())
      clusterProfiler::dotplot(enrichment_results())  # Replace with the actual plot you want to generate
    })
    # Render DT table for enrichment results
    output$result_table <- DT::renderDT({
      shiny::req(enrichment_results())
      base::as.data.frame(enrichment_results()) %>%
        DT::datatable(
          options = list(pageLength = 5, scrollX = TRUE),
          rownames = FALSE
        )
    })
  })
}
