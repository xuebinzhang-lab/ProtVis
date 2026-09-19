#' Pathview Pathway Visualization Module UI
#' @import shiny
#' @import bslib
#' @name pathview_ui
#' @export
#'
pathview_ui <- function(id) {
  ns <- shiny::NS(id)
  my_theme <- bslib::bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#0d6efd",
    secondary = "#6c757d",
    success = "#198754",
    base_font = bslib::font_google("Roboto")
  )
  shiny::fluidPage(
    theme = my_theme,
    shiny::titlePanel("Pathway Visualization"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::fileInput(ns("file"), "Upload pathview.csv (2 columns: KO, logFC)", accept = ".csv"),
        shiny::fileInput(ns("pathway"), "Upload pathway_file.csv (3 columns: pathway, ko, Description)", accept = ".csv"),
        shiny::selectInput(
          ns("tax_group"),
          "Select Taxonomic Group",
          choices = c("Plant", "Animals", "Bacteria", "Fungi", "Eukaryotes", "Hsa"),
          selected = "Plant"
        ),
        shiny::actionButton(ns("enrich"), "Run KEGG Enrichment", class = "btn-primary"),
        shiny::uiOutput(ns("pathway_select")),
        shiny::actionButton(ns("run"), "Draw Selected Pathway", class = "btn-success"),
        class = "bg-light"
      ),
      bslib::card(
        bslib::card_header("Working Directory:"),
        shiny::verbatimTextOutput(ns("wd_text")),
        bslib::card_header("Pathway Plot Preview"),
        bslib::card_body(
          shiny::imageOutput(ns("plot_ui"), height = "auto"),
          class = "p-0"
        ),
        bslib::card_header("KO-logFC Table Preview"),
        bslib::card_body(
          shiny::tableOutput(ns("preview")),
          class = "p-3"
        ),
        full_screen = TRUE
      )
    )
  )
}

#' Pathview Pathway Visualization Module Server
#' @name pathview_server
#' @import shiny
#' @importFrom data.table fread
#' @importFrom dplyr filter select
#' @importFrom clusterProfiler enricher
#' @importFrom utils head
#' @importFrom pathview pathview
#' @export
#'
utils::globalVariables(c("ko", "KO", "pathway", "Description"))

pathview_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (!base::exists("bods", envir = .GlobalEnv)) {
      base::assign("bods",
                   base::data.frame(
                     package = c(
                       "org.Ag.eg.db","org.At.tair.db","org.Bt.eg.db","org.Ce.eg.db","org.Cf.eg.db",
                       "org.Dm.eg.db","org.Dr.eg.db","org.EcK12.eg.db","org.EcSakai.eg.db","org.Gg.eg.db",
                       "org.Hs.eg.db","org.Mm.eg.db","org.Mmu.eg.db","org.Pf.plasmo.db","org.Pt.eg.db",
                       "org.Rn.eg.db","org.Sc.sgd.db","org.Ss.eg.db","org.Xl.eg.db"),
               species = c(
                 "Anopheles","Arabidopsis","Bovine","Worm","Canine",
                 "Fly","Zebrafish","E coli strain K12","E coli strain Sakai","Chicken",
                 "Human","Mouse","Rhesus","Malaria","Chimp",
                 "Rat","Yeast","Pig","Xenopus"
               ),
               kegg = c(
                 "aga","ath","bta","cel","cfa",
                 "dme","dre","eco","ecs","gga",
                 "hsa","mmu","mcc","pfa","ptr",
                 "rno","sce","ssc","xla"
               ),
               code = c(
                 "eg","tair","eg","eg","eg",
                 "eg","eg","eg","eg","eg",
                 "eg","eg","eg","orf","eg",
                 "eg","orf","eg","eg"
               ),
               stringsAsFactors = FALSE
             ),
             envir = .GlobalEnv
      )
    }
    output$wd_text <- shiny::renderText(getwd())
    geneList_react <- shiny::reactiveVal()
    desc_df <- shiny::reactiveVal()
    # ---------- Enrichment ----------
    shiny::observeEvent(input$enrich, {
      shiny::req(input$file, input$pathway)
      # Read KO-logFC
      gene_df <- data.table::fread(input$file$datapath)
      colnames(gene_df)[1:2] <- c("KO", "logFC")
      gene_df$KO <- base::toupper(base::sub(".*:", "", gene_df$KO))
      # Read pathway_file.csv
      pf <- data.table::fread(input$pathway$datapath)
      colnames(pf)[1:3] <- c("pathway","ko","Description")
      pf$pathway <- base::sub("path:","",pf$pathway)
      pf$ko <- base::sub("ko:","",pf$ko)
      # Background KO list by tax group
      bg_list <- base::list(
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
      valid_idx <- base::grepl("^K\\d{5}$", gene_df$KO)
      gene_df <- gene_df[valid_idx, ]
      if (nrow(gene_df) == 0) {
        shiny::showNotification("No valid KO IDs (Kxxxxx) found for selected group!", type = "error")
        return()
      }
      geneList <- gene_df$logFC
      base::names(geneList) <- gene_df$KO
      geneList_react(geneList)
      term2gene <- base::unique(pf[, .(pathway, ko)])
      desc <- base::unique(pf[, .(pathway, Description)])
      desc_df(desc)
      # Run enrichment
      eg <- clusterProfiler::enricher(
        gene = base::names(geneList),
        TERM2GENE = term2gene,
        pvalueCutoff = 0.05
      )
      if (!base::is.null(eg) && base::nrow(eg@result) > 0) {
        result <- base::merge(
          eg@result %>% dplyr::select(-Description),
          desc,
          by.x = "ID",
          by.y = "pathway",
          all.x = TRUE
        )
        output$pathway_select <- shiny::renderUI({
          shiny::selectInput(
            ns("selected_pathway"),
            "Select KEGG Pathway",
            choices = stats::setNames(result$ID, result$Description),
            multiple = FALSE
          )
        })
      } else {
        shiny::showNotification("No significant enriched pathways found", type = "error")
      }
      output$preview <- shiny::renderTable({
        utils::head(data.frame(KO = names(geneList), logFC = geneList))
      })
    })
    # ---------- Pathview PNG Drawing ----------
    shiny::observeEvent(input$run, {
      shiny::req(geneList_react(), input$selected_pathway)
      pid <- base::sub("^(map|ko)", "", input$selected_pathway)
      geneList <- geneList_react()
      # KO ID sanity check
      names(geneList) <- base::toupper(base::sub(".*:", "", names(geneList)))
      valid_idx <- base::grepl("^K\\d{5}$", names(geneList))
      geneList <- geneList[valid_idx]
      if (base::length(geneList) == 0) {
        shiny::showNotification("No valid KO IDs (Kxxxxx) found!", type = "error")
        return()
      }
      tryCatch({
        pathview::pathview(
          gene.data = geneList,
          pathway.id = pid,
          species = "ko",
          gene.idtype = "KEGG",
          kegg.native = TRUE,
          out.format = "png",
          out.suffix = "KOmap",
          node.sum = "mean"
        )
        png_file <- base::normalizePath(base::file.path(base::getwd(), base::paste0("ko", pid, ".KOmap.png")))
        if (!base::file.exists(png_file)) {
          shiny::showNotification("No PNG file generated. Check KO IDs or network.", type = "error")
          return()
        }
        output$plot_ui <- shiny::renderImage({
          base::list(src = png_file, contentType = "image/png",
               width = "100%", alt = base::paste("Pathway:", pid))
        }, deleteFile = FALSE)
      }, error = function(e) {
        shiny::showNotification(
          base::paste("Pathview error:", e$message),
          type = "error"
        )
      })
    })
  })
}
