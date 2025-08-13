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
  nav_panel(
    title = 'GO and KEGG analyze',
    icon = icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = icon("upload"),
          fileInput(
            inputId = ns('file'),
            label = 'Background (.xlsx)',
            multiple = FALSE,
            accept = '.xlsx'
          ),
          fileInput(
            inputId = ns('gene_list'),
            label = 'Gene list (.xlsx)',
            multiple = FALSE,
            accept = '.xlsx'
          ),
          radioButtons(
            inputId = ns("choice"),
            label = "Select a function:",
            choices = c("GO" = "choice_go",
                        "KEGG" = "choice_KEGG"),
            selected = "GO"
          ),
          actionButton(ns("run"), "Run")
        )
      ),
      page_fluid(
        fluidRow(
          # Left panel for plot
          column(
            width = 6,
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "GO/KEGG Plot",
              sidebar = accordion(
                accordion_panel(
                  title = 'Parameter',
                  uiOutput(ns("term2gene_ui")),
                  uiOutput(ns("term2name_ui"))
                )
              ),
              mainPanel(
                plotOutput(ns("plot_result"))
              )
            )
          ),
          # Right panel for table
          column(
            width = 6,
            height = 600,
            navset_card_tab(
              height = 600,
              full_screen = TRUE,
              title = "Results Table",
              mainPanel(
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
#'
GO_and_KEGG_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive to read uploaded background file
    background_data <- reactive({
      req(input$file)
      readxl::excel_sheets(input$file$datapath)
    })

    # Update TERM2GENE and TERM2NAME choices dynamically
    observeEvent(background_data(), {
      updateSelectInput(session, "term2gene", choices = background_data())
      updateSelectInput(session, "term2name", choices = background_data())
    })

    # UI for TERM2GENE
    output$term2gene_ui <- renderUI({
      req(input$file)
      selectInput(ns("term2gene"), "Select TERM2GENE from Background", choices = NULL)
    })

    # UI for TERM2NAME
    output$term2name_ui <- renderUI({
      req(input$file)
      selectInput(ns("term2name"), "Select TERM2NAME from Background", choices = NULL)
    })

    # Reactive for TERM2GENE and TERM2NAME
    t2g.go <- reactive({
      req(input$file, input$term2gene)
      readxl::read_excel(input$file$datapath, sheet = input$term2gene)
    })

    t2n.go <- reactive({
      req(input$file, input$term2name)
      readxl::read_excel(input$file$datapath, sheet = input$term2name)
    })

    # Placeholder for `glist`
    glist <- reactive({
      req(input$gene_list)
      gene_data <- readxl::read_excel(input$gene_list$datapath)
      gene_data[[1]]  # Assuming the first column contains gene IDs
    })

    # Perform enrichment analysis when the "Run" button is clicked
    enrichment_results <- eventReactive(input$run, {
      req(glist(), t2g.go(), t2n.go())
      enricher(
        gene = glist(),
        TERM2GENE = t2g.go(),
        TERM2NAME = t2n.go(),
        pvalueCutoff = 1,
        qvalueCutoff = 1
      )
    })

    # Render plot for enrichment results
    output$plot_result <- renderPlot({
      req(enrichment_results())
      dotplot(enrichment_results())  # Replace with the actual plot you want to generate
    })

    # Render DT table for enrichment results
    output$result_table <- DT::renderDT({
      req(enrichment_results())
      as.data.frame(enrichment_results()) %>%
        DT::datatable(
          options = list(pageLength = 5, scrollX = TRUE),
          rownames = FALSE
        )
    })
  })
}
