#' GSEA module UI
#' @import shiny
#' @import bslib
#' @importFrom DT dataTableOutput
#' @name gsea_ui
#' @noRd
#' @export
#'
gsea_ui <- function(id) {
  ns <- NS(id)
  bslib::page_sidebar(
    title = "GSEA Analysis",
    sidebar = bslib::sidebar(
      shiny::fileInput(ns("expr_file"), "Upload Expression Matrix", accept = ".txt"),
      shiny::fileInput(ns("group_file"), "Upload Group Info", accept = ".txt"),
      shiny::fileInput(ns("ko_file"), "Upload KO Pathway Info", accept = ".txt"),
      shiny::numericInput(ns("minGSSize"), "minGSSize", value = 1, min = 1, step = 1),
      shiny::numericInput(ns("maxGSSize"), "maxGSSize", value = 5000, min = 10, step = 10),
      shiny::numericInput(ns("plot_top_x"), "Show Top X Pathways", value = 20, min = 1, step = 1),
      shiny::checkboxInput(ns("sig_only"), "Show only significant (p.adjust < 0.05)", value = FALSE),
      shiny::radioButtons(ns("sort_by"), "Dotplot Sort By", choices = c("NES","p.adjust"), inline = TRUE),
      shiny::selectInput(ns("pathway"), "Select a Pathway", choices = NULL),
      shiny::actionButton(ns("run"), "Run Analysis", class = "btn-primary w-100 mt-2"),
      shiny::hr(),
      shiny::downloadButton(ns("download_csv"), "Download Results CSV", class = "btn-success w-100"),
      shiny::downloadButton(ns("download_dotplot"), "Download Dotplot PDF", class = "btn-success w-100 mt-1"),
      shiny::downloadButton(ns("download_curve"), "Download Curve PDF", class = "btn-success w-100 mt-1")
    ),
    bslib::card(
      bslib::card_header("GSEA Results Table"),
      bslib::card_body(DT::dataTableOutput(ns("gsea_table")))
    ),
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Top Pathways Dotplot"),
        bslib::card_body(shiny::plotOutput(ns("dotplot"), height = "600px"))
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Single Pathway Enrichment Curve"),
        bslib::card_body(shiny::plotOutput(ns("gsea_plot"), height = "600px"))
      )
    )
  )
}

#' GSEA module server
#' @import shiny
#' @importFrom data.table fread rbindlist data.table
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom clusterProfiler GSEA dotplot
#' @importFrom GseaVis gseaNb
#' @importFrom utils write.csv
#' @importFrom grDevices pdf dev.off
#' @noRd
#' @name gsea_server
#' @export
#'
gsea_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    gsea_res_val <- shiny::reactiveVal()
    top_df_val <- shiny::reactiveVal()
    shiny::observeEvent(input$run, {
      shiny::req(input$expr_file, input$group_file, input$ko_file)
      expr <- data.table::fread(input$expr_file$datapath)
      group <- data.table::fread(input$group_file$datapath)
      ko <- data.table::fread(input$ko_file$datapath)
      mat <- base::as.matrix(expr[, -1])
      base::rownames(mat) <- expr$ID
      group_df <- base::data.frame(
        row.names = group$Sample,
        condition = group$Group
      )
      dds <- DESeq2::DESeqDataSetFromMatrix(countData = mat, colData = group_df, design = ~ condition)
      dds <- DESeq2::DESeq(dds)
      res <- DESeq2::results(dds)
      res <- res[!is.na(res$log2FoldChange), ]
      gene_ranks <- res$log2FoldChange
      base::names(gene_ranks) <- base::rownames(res)
      gene_ranks <- base::sort(gene_ranks, decreasing = TRUE)
      term2gene <- data.table::rbindlist(base::lapply(1:base::nrow(ko), function(i) {
        genes <- base::unlist(base::strsplit(ko[i, 3][[1]], ";"))
        data.table::data.table(term = ko[i, 1][[1]], gene = genes)
      }))
      gsea_res <- clusterProfiler::GSEA(
        geneList = gene_ranks,
        TERM2GENE = term2gene,
        pvalueCutoff = 1,
        minGSSize = input$minGSSize,
        maxGSSize = input$maxGSSize
      )
      gsea_res_val(gsea_res)
      df <- gsea_res@result
      if (input$sig_only) {
        df <- df[df$p.adjust < 0.05, ]
      }
      if (input$sort_by == "NES") {
        df <- df[order(-abs(df$NES)), ]
      } else {
        df <- df[order(df$p.adjust), ]
      }
      top_df <- df[1:min(nrow(df), input$plot_top_x), ]
      top_df_val(top_df)
      if (nrow(top_df) > 0) {
        shiny::updateSelectInput(session, "pathway", choices = top_df$Description)
      }
      output$gsea_table <- shiny::renderDataTable({ top_df })
      output$dotplot <- shiny::renderPlot({
        if (nrow(top_df) > 0) {
          sub_res <- gsea_res
          sub_res@result <- top_df
          clusterProfiler::dotplot(sub_res, showCategory = nrow(top_df))
        }
      })
      output$gsea_plot <- shiny::renderPlot({
        shiny::req(input$pathway)
        GseaVis::gseaNb(object = gsea_res, geneSetID = input$pathway, subPlot = 3)
      })
    })
    output$download_csv <- shiny::downloadHandler(
      filename = function() base::paste0("GSEA_result_", base::Sys.Date(), ".csv"),
      content = function(file) {
        res <- gsea_res_val()
        if (!base::is.null(res)) utils::write.csv(res@result, file, row.names = FALSE)
      }
    )
    output$download_dotplot <- shiny::downloadHandler(
      filename = function() base::paste0("dotplot_", base::Sys.Date(), ".pdf"),
      content = function(file) {
        gsea_res <- gsea_res_val(); top_df <- top_df_val()
        if (!base::is.null(gsea_res) && !base::is.null(top_df)) {
          sub_res <- gsea_res; sub_res@result <- top_df
          grDevices::pdf(file, width = 8, height = 6)
          print(clusterProfiler::dotplot(sub_res, showCategory = base::nrow(top_df)))
          grDevices::dev.off()
        }
      }
    )
    output$download_curve <- shiny::downloadHandler(
      filename = function() base::paste0("gsea_curve_", base::Sys.Date(), ".pdf"),
      content = function(file) {
        gsea_res <- gsea_res_val()
        if (!base::is.null(gsea_res) && !base::is.null(input$pathway)) {
          grDevices::pdf(file, width = 8, height = 6)
          print(GseaVis::gseaNb(object = gsea_res, geneSetID = input$pathway, subPlot = 3))
          grDevices::dev.off()
        }
      }
    )
  })
}
