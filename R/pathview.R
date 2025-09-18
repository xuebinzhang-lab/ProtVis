#' Pathview Pathway Visualization Module UI
#' @import shiny bslib data.table pathview clusterProfiler
#' @noRd
pathview_ui <- function(id) {
  ns <- NS(id)

  # Theme
  my_theme <- bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#0d6efd",
    secondary = "#6c757d",
    success = "#198754",
    base_font = font_google("Roboto")
  )

  fluidPage(
    theme = my_theme,
    titlePanel("Pathway Visualization"),

    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        fileInput(ns("file"), "Upload pathview.csv (2 columns: KO, logFC)", accept = ".csv"),
        fileInput(ns("pathway"), "Upload pathway_file.csv (3 columns: pathway, ko, Description)", accept = ".csv"),
        actionButton(ns("enrich"), "Run KEGG Enrichment", class = "btn-primary"),
        uiOutput(ns("pathway_select")),
        actionButton(ns("run"), "Draw Selected Pathway", class = "btn-success"),
        class = "bg-light"
      ),

      card(
        card_header("Working Directory:"),
        verbatimTextOutput(ns("wd_text")),
        card_header("Pathway Plot Preview"),
        card_body(
          imageOutput(ns("plot_ui"), height = "auto"),
          class = "p-0"
        ),
        card_header("KO-logFC Table Preview"),
        card_body(
          tableOutput(ns("preview")),
          class = "p-3"
        ),
        full_screen = TRUE
      )
    )
  )
}


#' Pathview Pathway Visualization Module Server
#' @noRd
pathview_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns   # ← ADD THIS LINE

    output$wd_text <- renderText(getwd())
    geneList_react <- reactiveVal()
    desc_df <- reactiveVal()

    observeEvent(input$enrich, {
      req(input$file, input$pathway)

      # Read KO-logFC data
      gene_df <- fread(input$file$datapath)
      gene_df[[1]] <- sub(".*:", "", gene_df[[1]])
      geneList <- gene_df[[2]]
      names(geneList) <- gene_df[[1]]
      geneList_react(geneList)

      # Read pathway_file.csv
      pf <- fread(input$pathway$datapath)
      pf[, pathway := sub("path:","",pathway)]
      pf[, ko := sub("ko:","",ko)]

      term2gene <- unique(pf[, .(pathway, ko)])
      desc <- unique(pf[, .(pathway, Description)])
      desc_df(desc)

      # Run enrichment
      eg <- enricher(
        gene = names(geneList),
        TERM2GENE = term2gene,
        pvalueCutoff = 0.05
      )

      if (!is.null(eg) && nrow(eg@result) > 0) {
        result <- merge(
          eg@result %>% select(-Description),
          desc,
          by.x = "ID",
          by.y = "pathway",
          all.x = TRUE
        )

        output$pathway_select <- renderUI({
          selectInput(
            ns("selected_pathway"),   # now ns is available
            "Select KEGG Pathway",
            choices = setNames(result$ID, result$Description),
            multiple = FALSE
          )
        })
      } else {
        showNotification("No significant enriched pathways found", type = "error")
      }

      output$preview <- renderTable({
        head(data.frame(KO = names(geneList), logFC = geneList))
      })
    })

    observeEvent(input$run, {
      req(geneList_react(), input$selected_pathway)

      pid <- sub("^(map|ko)", "", input$selected_pathway)
      geneList <- geneList_react()

      pathview(
        gene.data = geneList,
        pathway.id = pid,
        species = "ko",
        out.suffix = "KOmap",
        kegg.native = TRUE
      )

      img_file <- normalizePath(file.path(getwd(), paste0("ko", pid, ".KOmap.png")))

      output$plot_ui <- renderImage({
        list(src = img_file, contentType = "image/png",
             width = "100%", alt = paste("Pathway:", pid))
      }, deleteFile = FALSE)
    })
  })
}

