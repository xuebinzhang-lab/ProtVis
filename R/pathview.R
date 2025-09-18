#' Pathview Pathway Visualization Module UI
#' @import shiny bslib data.table pathview clusterProfiler
#' @noRd
pathview_ui <- function(id) {
  ns <- NS(id)

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
    titlePanel("Pathview Pathway Visualization (Offline)"),

    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        fileInput(ns("file"), "Upload pathview.csv (2 columns: KO, logFC)", accept = ".csv"),
        fileInput(ns("pathway"), "Upload pathway_file.csv (3 columns: pathway, ko, Description)", accept = ".csv"),
        selectInput(
          ns("tax_group"),
          "Select Taxonomic Group",
          choices = c("Plant", "Animals", "Bacteria", "Fungi", "Eukaryotes", "Hsa"),
          selected = "Plant"
        ),
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
    ns <- session$ns

    output$wd_text <- renderText(getwd())
    geneList_react <- reactiveVal()
    desc_df <- reactiveVal()

    # ---------- Enrichment ----------
    observeEvent(input$enrich, {
      req(input$file, input$pathway)

      # Read KO-logFC
      gene_df <- fread(input$file$datapath)
      colnames(gene_df)[1:2] <- c("KO", "logFC")
      gene_df$KO <- toupper(sub(".*:", "", gene_df$KO))

      # Read pathway_file.csv
      pf <- fread(input$pathway$datapath)
      colnames(pf)[1:3] <- c("pathway","ko","Description")
      pf$pathway <- sub("path:","",pf$pathway)
      pf$ko <- sub("ko:","",pf$ko)

      # Background KO list by tax group
      bg_list <- list(
        Plant = ProtVisDatabase::Plant_KEGG_Background,
        Animals = ProtVisDatabase::Animals_KEGG_Background,
        Bacteria = ProtVisDatabase::Bacteria_KEGG_Background,
        Fungi = ProtVisDatabase::Fungi_KEGG_Background,
        Eukaryotes = ProtVisDatabase::Eukaryotes_KEGG_Background,
        Hsa = ProtVisDatabase::hsa_KEGG_Background
      )
      selected_bg <- bg_list[[input$tax_group]]

      # Filter by background KO list
      pf <- pf %>% dplyr::filter(ko %in% selected_bg$V1)
      gene_df <- gene_df %>% dplyr::filter(KO %in% selected_bg$V1)

      # Keep only valid Kxxxxx IDs
      valid_idx <- grepl("^K\\d{5}$", gene_df$KO)
      gene_df <- gene_df[valid_idx, ]

      if (nrow(gene_df) == 0) {
        showNotification("No valid KO IDs (Kxxxxx) found for selected group!", type = "error")
        return()
      }

      geneList <- gene_df$logFC
      names(geneList) <- gene_df$KO
      geneList_react(geneList)

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
          eg@result %>% dplyr::select(-Description),
          desc,
          by.x = "ID",
          by.y = "pathway",
          all.x = TRUE
        )

        output$pathway_select <- renderUI({
          selectInput(
            ns("selected_pathway"),
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

    # ---------- Pathview PNG Drawing ----------
    observeEvent(input$run, {
      req(geneList_react(), input$selected_pathway)

      pid <- sub("^(map|ko)", "", input$selected_pathway)
      geneList <- geneList_react()

      if (exists("bods", envir = .GlobalEnv)) rm(bods, envir = .GlobalEnv)

      tryCatch({
        pathview(
          gene.data = geneList,
          pathway.id = pid,
          species = "ko",
          gene.idtype = "KEGG",
          kegg.native = TRUE,
          out.format = "png",
          out.suffix = "KOmap",
          node.sum = "mean"
        )

        png_file <- normalizePath(file.path(getwd(), paste0("ko", pid, ".KOmap.png")))

        if (!file.exists(png_file)) {
          showNotification("No PNG file generated. Check KO IDs or network.", type = "error")
          return()
        }

        output$plot_ui <- renderImage({
          list(src = png_file, contentType = "image/png",
               width = "100%", alt = paste("Pathway:", pid))
        }, deleteFile = FALSE)

      }, error = function(e) {
        showNotification(
          paste("Pathview error:", e$message),
          type = "error"
        )
      })
    })
  })
}
