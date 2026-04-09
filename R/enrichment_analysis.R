#' @importFrom circlize colorRamp2 circos.clear circos.genomicInitialize circos.trackPlotRegion
#'   get.cell.meta.data circos.text get.all.sector.index circos.axis circos.genomicTrack
#'   circos.genomicRect circos.genomicText
#' @importFrom ComplexHeatmap draw Legend
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grid unit gpar grid.lines
#' @importFrom utils head
#' @importFrom grDevices pdf dev.off
#' @importFrom pryr f
#' @title plot_go_circos
#' @name plot_go_circos
#' @export
plot_go_circos <- function(go_data, top_n = 15, output_pdf = NULL) {
  data <- go_data[order(go_data$pvalue), ]
  datasig <- data[data$pvalue < 0.05, , drop = FALSE]
  data <- utils::head(datasig, top_n)
  if (base::nrow(data) == 0) {
    message("No significant GO terms to plot.")
    return(NULL)
  }
  BgGene <- base::as.numeric(base::sapply(base::strsplit(data$BgRatio, "/"), `[`, 1))
  Gene <- base::as.numeric(base::sapply(base::strsplit(data$GeneRatio, "/"), `[`, 1))
  ratio <- Gene / BgGene
  logpvalue <- -base::log10(data$pvalue)
  logpvalue.col <- RColorBrewer::brewer.pal(n = 8, name = "Reds")
  f <- circlize::colorRamp2(
    breaks = c(0, 2, 4, 6, 8, 10, 15, 20),
    colors = logpvalue.col
  )
  BgGene.col <- pryr::f(base::pmin(logpvalue, 20))
  df_circos <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = base::max(BgGene)
  )
  base::rownames(df_circos) <- df_circos$GO
  bed2 <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = BgGene,
    label = BgGene,
    col = BgGene.col
  )
  bed3 <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = Gene,
    label = Gene
  )
  bed4 <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = base::max(BgGene),
    ratio = ratio / base::max(ratio) * 9.5,
    col = "#00AFBB"
  )
  if (!is.null(output_pdf)) {
    grDevices::pdf(output_pdf, width = 10, height = 6)
  }
  circlize::circos.clear()
  circlize::circos.genomicInitialize(df_circos, plotType = "none")
  circlize::circos.trackPlotRegion(
    ylim = c(0, 1),
    panel.fun = function(x, y) {
      sector.index <- circlize::get.cell.meta.data("sector.index")
      xlim <- circlize::get.cell.meta.data("xlim")
      ylim <- circlize::get.cell.meta.data("ylim")
      desc <- data[data$ID == sector.index, ]$Description
      desc <- paste(base::strwrap(desc, width = 20), collapse = "\n")
      circlize::circos.text(
        base::mean(xlim), base::mean(ylim),
        desc, cex = 0.6,
        facing = "bending.inside", niceFacing = TRUE
      )
    },
    track.height = 0.12,
    bg.border = NA,
    bg.col = "grey95"
  )
  for (si in circlize::get.all.sector.index()) {
    circlize::circos.axis(
      h = "top",
      labels.cex = 0.5,
      sector.index = si,
      track.index = 1,
      major.at = base::seq(0, base::max(BgGene), by = 100),
      labels.facing = "clockwise"
    )
  }
  circlize::circos.genomicTrack(
    bed2,
    ylim = c(0, 1),
    track.height = 0.1,
    bg.border = "white",
    panel.fun = function(region, value, ...) {
      circlize::circos.genomicRect(region, value,
                                   ytop = 1, ybottom = 0,
                                   col = value$col, border = NA, ...)
      circlize::circos.genomicText(region, value,
                                   y = 0.4, labels = value$label,
                                   adj = 0, cex = 0.6, ...)
    }
  )
  circlize::circos.genomicTrack(
    bed3,
    ylim = c(0, 1),
    track.height = 0.1,
    bg.border = "white",
    panel.fun = function(region, value, ...) {
      circlize::circos.genomicRect(region, value,
                                   ytop = 1, ybottom = 0,
                                   col = "#BA55D3", border = NA, ...)
      circlize::circos.genomicText(region, value,
                                   y = 0.4, labels = value$label,
                                   adj = 0, cex = 0.6, ...)
    }
  )
  circlize::circos.genomicTrack(
    bed4,
    ylim = c(0, 10),
    track.height = 0.35,
    bg.border = "white",
    bg.col = "grey90",
    panel.fun = function(region, value, ...) {
      cell.xlim <- circlize::get.cell.meta.data("cell.xlim")
      cell.ylim <- circlize::get.cell.meta.data("cell.ylim")
      for (j in 1:9) {
        y <- cell.ylim[1] + (cell.ylim[2] - cell.ylim[1]) / 10 * j
        grid::grid.lines(cell.xlim, c(y, y), gp = grid::gpar(col = "#FFFFFF", lwd = 0.3))
      }
      circlize::circos.genomicRect(region, value,
                                   ytop = value$ratio, ybottom = 0,
                                   col = value$col, border = NA, ...)
    }
  )
  circlize::circos.clear()
  circle_size <- grid::unit(1, "snpc")
  ComplexHeatmap::draw(ComplexHeatmap::Legend(
    labels = c("Number of Genes", "Number of Select", "Rich Factor(0-1)"),
    type = "points",
    pch = c(15, 15, 17),
    legend_gp = grid::gpar(col = c("pink", "#BA55D3", "#00AFBB")),
    title = "",
    nrow = 3,
    size = grid::unit(3, "mm")
  ), x = circle_size * 0.83, y = circle_size * 0.5, just = "center")
  ComplexHeatmap::draw(ComplexHeatmap::Legend(
    labels = c("(0,2]", "(2,4]", "(4,6]", "(6,8]", "(8,10]", "(10,15]", "(15,20]", ">=20"),
    type = "points",
    pch = 16,
    legend_gp = grid::gpar(col = logpvalue.col),
    title = "-log10(Pvalue)",
    title_position = "topcenter",
    grid_height = grid::unit(5, "mm"),
    grid_width = grid::unit(5, "mm"),
    size = grid::unit(3, "mm")
  ), x = circle_size * 1.4, y = circle_size * 0.5, just = "left")

  if (!is.null(output_pdf)) grDevices::dev.off()
  message("GO Circos plot finished!")
}

#' Enrichment Analysis Module UI
#' This function creates the user interface for the enrichment analysis module.
#' It includes file uploads, parameter settings, and visualization panels for GO and KEGG enrichment analysis.
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing the enrichment analysis interface
#' @name enrichment_analysis_ui
#' @export
#'
enrichment_analysis_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        shiny::uiOutput(ns("load_status_panel")),
        shiny::uiOutput(ns("compare_select_ui")),
        shiny::div(style = "margin-bottom: 15px;",
                   shiny::fileInput(ns("enrichment_analysis_file"), "Upload Enrichment Analysis File (Created by Toolkits > Background Make)",
                      accept = c(".csv", ".xlsx"),
                      buttonLabel = "Browse..."),
                   shiny::actionButton(ns("check_file"), "Check File",
                         class = "btn btn-success fw-bold mb-2")
        ),
        shiny::hr(),
        tags$small("The genelist requires an ID column.(.xlsx or .csv)",
                   style = "color: #6c757d"),
        # Input mode toggle switch
        shinyWidgets::switchInput(
          inputId = ns("input_mode"),
          label = "Input Manually",
          value = TRUE,
          onLabel = "Upload",
          offLabel = "Paste",
          width = "100%"
        ),
        # Conditional panel: File upload mode
        shiny::conditionalPanel(
          condition = base::paste0("input['", ns("input_mode"), "'] == true"),
          tags$small('Upload Genelist', style = "color: #6c757d"),
          shiny::fileInput(
            inputId = ns('genelist_file'),
            label = NULL,
            multiple = FALSE,
            accept = c('.csv','.xlsx')
          )
        ),
        # Conditional panel: Manual input mode
        shiny::conditionalPanel(
          condition = base::paste0("input['", ns("input_mode"), "'] == false"),
          shiny::div(
            tags$small("Paste Genelist", style = "color: #6c757d"),
            shiny::textAreaInput(
              inputId = ns("paste_data"),
              label = NULL,
              placeholder = "Copy and paste Excel data here.",
              rows = 5
            ),
            shiny::actionButton(ns("apply_paste"), "Apply paste data",
                         class = "btn btn-light fw-bold")
          )
        ),
        bslib::accordion(
          bslib::accordion_panel(
            title = "Enrichment analysis",
            icon = enrichment_bubble_icon,
            shiny::selectInput(
              inputId = ns("species"),
              label = "Select taxonomic group:",
              choices = c("Plant" = "Plant",
                          "Animals" = "Animals",
                          "Bacteria" = "Bacteria",
                          "Fungi" = "Fungi",
                          "Eukaryotes"  = "Eukaryotes",
                          "Hsa"  = "Hsa"),
              selected = "Plant"
            ),
            shiny::checkboxGroupInput(
              inputId = ns("choices"),
              label = "Please select the analysis content:",
              choices = c("GO" = "go_analysis",
                          "KEGG" = "kegg_analysis"),
              selected = c("go_analysis","kegg_analysis")
            ),
            shiny::actionButton(ns("run_enrichment_analysis"), "Analysis")
          )
        )
      ),
      bslib::page_fluid(
        bslib::card(
          bslib::card_header("File Check Result"),
          bslib::card_body(
            shiny::textOutput(ns("file_check_result"))
          )
        ),
        bslib::layout_column_wrap(
          width = 1/2,
          height = 600,
          # === GO enrichment card ===
          bslib::card(
            height = "800px",
            bslib::card_header("GO Enrichment Analysis"),
            bslib::card_body(
              shiny::tabsetPanel(
                id = ns("go_tabs"),
                type = "tabs",
                shiny::tabPanel("Visualization",
                                bslib::layout_sidebar(
                                sidebar = sidebar(
                                width = 250,
                                position = "left",
                                open = "closed",
                                shiny::selectInput(ns("go_plot_type"), "Select plot type:",
                                         choices = c("Bar plot" = "bar",
                                                     "Dot plot" = "dot",
                                                     "Circle plot" = "circle"),
                                         selected = "bar"),
                                shiny::sliderInput(ns("go_top_n"), "Top N terms:",
                                         min = 5, max = 20, value = 10),
                             colourpicker::colourInput(ns("go_color"), "Select color:", value = "#2c7bb6"),
                             shiny::numericInput(ns("go_width"), "Plot width (inch)", value = 8, min = 4, max = 20),
                             shiny::numericInput(ns("go_height"), "Plot height (inch)", value = 6, min = 4, max = 20),
                             shiny::downloadButton(ns("download_go_plot"), "Download Plot (PDF)"),
                             shiny::downloadButton(ns("download_go_table"), "Download Table (CSV)")
                           ),
                           bslib::card_body(
                             shiny::plotOutput(ns("go_plot"))
                           )
                         )
                ),
                shiny::tabPanel("Result Table", DT::DTOutput(ns("go_res_table")))
              )
            )
          ),
          # === KEGG enrichment card ===
          bslib::card(
            height = "800px",
            bslib::card_header("KEGG Enrichment Analysis"),
            bslib::card_body(
              shiny::tabsetPanel(
                id = ns("kegg_tabs"),
                type = "tabs",
                shiny::tabPanel("Visualization",
                                bslib::layout_sidebar(
                                sidebar = bslib::sidebar(
                                width = 250,
                                position = "left",
                                open = "closed",
                                shiny::selectInput(ns("kegg_plot_type"), "Select plot type:",
                                         choices = c("Bar plot" = "bar",
                                                     "Dot plot" = "dot",
                                                     "Circle plot" = "circle"),
                                         selected = "bar"),
                                shiny::sliderInput(ns("kegg_top_n"), "Top N pathways:",
                                         min = 5, max = 20, value = 10),
                                colourpicker::colourInput(ns("kegg_color"), "Select color:", value = "#d7191c"),
                                shiny::numericInput(ns("kegg_width"), "Plot width (inch)", value = 8, min = 4, max = 20),
                                shiny::numericInput(ns("kegg_height"), "Plot height (inch)", value = 6, min = 4, max = 20),
                                shiny::downloadButton(ns("download_kegg_plot"), "Download Plot (PDF)"),
                                shiny::downloadButton(ns("download_kegg_table"), "Download Table (CSV)")
                           ),
                           bslib::card_body(
                             shiny::plotOutput(ns("kegg_plot"))
                           )
                         )
                ),
                shiny::tabPanel("Result Table", DT::DTOutput(ns("kegg_res_table")))
              )
            )
          )
        )
      )
    )
  )
}


# -------------------------------------------------------------------------

#' Enrichment Analysis Module Server
#'
#' This function provides the server-side logic for the enrichment analysis module.
#' It handles data loading, file validation, enrichment analysis execution, and result visualization.
#'
#' @param id The namespace identifier for the module
#' @param shared_state A reactive values list for sharing state between modules
#' @return A module server function that handles enrichment analysis operations
#' @name enrichment_analysis_server
#' @export
#'

utils::globalVariables(c("regulation", "V3", "Pathway_ID", "TERM", "GENE", "NAME"))
enrichment_analysis_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      compare_data = NULL,
      input_mode = TRUE,
      dep_results = base::list(),
      file_check_msg = NULL,
      background_data = NULL,
      go_res = NULL,
      kegg_res = NULL
    )
    template_df <- shiny::reactive({
      base::data.frame(ID = c(NA, NA, NA), stringsAsFactors = FALSE)
    })
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      rda_path <- base::file.path(shared_state$workdir, "Step7_DEP_result.rda")
      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        base::load(rda_path, envir = e)
        if (base::exists("dep_results2", envir = e)) {
          rv$dep_results <- e$dep_results2
          shiny::updateSelectInput(session, "dep_compare", choices = base::names(rv$dep_results))
        } else {
          rv$dep_results <- NULL
          shiny::showNotification("Step7_DEP_result.rda does not contain dep_results2.", type = "warning")
        }
        rv$load_success <- TRUE
        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      }
    })
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    output$compare_select_ui <- shiny::renderUI({
      shiny::req(rv$load_success)
      shiny::selectInput(ns("dep_compare"),
                  label = "Select DEP comparison",
                  choices = names(rv$dep_results),
                  selected = names(rv$dep_results)[1])
    })
    shiny::observeEvent(input$dep_compare, {
      shiny::req(rv$dep_results)
      rv$compare_data <- rv$dep_results[[input$dep_compare]]
    })
    genelist <- shiny::reactive({
      if (!is.null(rv$compare_data)) {
        genes <- rv$compare_data %>%
          dplyr::filter(regulation != "Not significant") %>%
          dplyr::pull(ID)
        return(unique(genes))
      }
      if (!is.null(input$genelist_file)) {
        ext <- tools::file_ext(input$genelist_file$name)
        if (ext == "csv") {
          df <- utils::read.csv(input$genelist_file$datapath)
        } else if (ext == "xlsx") {
          df <- readxl::read_excel(input$genelist_file$datapath)
        }
        if ("ID" %in% colnames(df)) {
          return(unique(df$ID))
        }
      }
      if (!is.null(input$paste_data) && base::nchar(input$paste_data) > 0) {
        df <- utils::read.table(text = input$paste_data, header = TRUE, sep = "\t")
        if ("ID" %in% colnames(df)) {
          return(unique(df$ID))
        }
      }
      return(NULL)
    })
    shiny::observeEvent(input$check_file, {
      shiny::req(input$enrichment_analysis_file)
      file <- input$enrichment_analysis_file$datapath
      sheets <- readxl::excel_sheets(file)
      if (!all(c("GO_background", "KEGG_background") %in% sheets)) {
        rv$file_check_msg <- "❌ Missing required sheets: GO_background or KEGG_background"
        return()
      }
      GO_background <- readxl::read_excel(file, sheet = "GO_background")
      KEGG_background <- readxl::read_excel(file, sheet = "KEGG_background")
      rv$background_data <- base::list(GO_background = GO_background,
                                 KEGG_background = KEGG_background)
      rv$file_check_msg <- "✅ Background file valid."
    })
    output$file_check_result <- shiny::renderText({
      rv$file_check_msg
    })
    selected_kegg_background <- shiny::reactive({
      shiny::req(input$species)
      background_data <- switch(input$species,
                                "Plant"      = ProtVisDatabase::Plant_KEGG_Background,
                                "Animals"    = ProtVisDatabase::Animals_KEGG_Background,
                                "Bacteria"   = ProtVisDatabase::Bacteria_KEGG_Background,
                                "Fungi"      = ProtVisDatabase::Fungi_KEGG_Background,
                                "Eukaryotes" = ProtVisDatabase::Eukaryotes_KEGG_Background,
                                "Hsa"        = ProtVisDatabase::hsa_KEGG_Background,
                                NULL
      )
      shiny::req(background_data)
      map_id <- background_data %>%
        tidyr::separate(
          col = V3,
          into = c("Pathway_ID", "Pathway_Name"),
          sep = " ",
          extra = "merge"
        ) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_trim)) %>%
        dplyr::pull(Pathway_ID) %>%
        base::unique() %>%
        base::paste0("map", .)
      shiny::req(rv$background_data)
      filtered_bg <- rv$background_data$KEGG_background %>%
        dplyr::filter(TERM %in% map_id)
      return(filtered_bg)
    })
    shiny::observeEvent(input$run_enrichment_analysis, {
      shiny::req(genelist(), rv$background_data)
      if ("go_analysis" %in% input$choices) {
        t2g.go <- rv$background_data$GO_background %>% dplyr::select(TERM,GENE)
        t2n.go <- rv$background_data$GO_background %>% dplyr::select(TERM,NAME)
        rv$go_res <- clusterProfiler::enricher(
          gene = genelist(),
          TERM2GENE = t2g.go,
          TERM2NAME = t2n.go,
          pvalueCutoff = 1,
          qvalueCutoff = 1
        )
      }
      if ("kegg_analysis" %in% input$choices) {
        filtered_bg <- selected_kegg_background()
        t2g.kegg <- filtered_bg %>% dplyr::select(TERM,GENE)
        t2n.kegg <- filtered_bg %>% dplyr::select(TERM,NAME)
        rv$kegg_res <- clusterProfiler::enricher(
          gene = genelist(),
          TERM2GENE = t2g.kegg,
          TERM2NAME = t2n.kegg,
          pvalueCutoff = 1,
          qvalueCutoff = 1
        )
      }
      shiny::showNotification("✅ Enrichment analysis completed.", type = "message")
    })
    output$go_plot <- shiny::renderPlot({
      shiny::req(rv$go_res)
      if (input$go_plot_type == "bar") {
        graphics::barplot(rv$go_res, showCategory = input$go_top_n, fill = input$go_color)
      } else if (input$go_plot_type == "dot") {
        clusterProfiler::dotplot(rv$go_res, showCategory = input$go_top_n) +
          ggplot2::scale_color_manual(values = input$go_color)
      } else {
        plot_go_circos(rv$go_res, top_n = input$go_top_n)
      }
    })
    output$kegg_plot <- shiny::renderPlot({
      shiny::req(rv$kegg_res)
      if (input$kegg_plot_type == "bar") {
        graphics::barplot(rv$kegg_res, showCategory = input$kegg_top_n, fill = input$kegg_color)
      } else if (input$kegg_plot_type == "dot") {
        clusterProfiler::dotplot(rv$kegg_res, showCategory = input$kegg_top_n) +
          ggplot2::scale_color_manual(values = input$kegg_color)
      } else {
        plot_go_circos(rv$kegg_res, top_n = input$kegg_top_n)
      }
    })
    output$go_res_table <- DT::renderDT({
      shiny::req(rv$go_res)
      base::as.data.frame(rv$go_res@result)
    }, options = base::list(pageLength = 10, scrollX = TRUE))

    output$kegg_res_table <- renderDT({
      shiny::req(rv$kegg_res)
      base::as.data.frame(rv$kegg_res@result)
    }, options = base::list(pageLength = 10, scrollX = TRUE))
    # GO plot
    output$download_go_plot <- shiny::downloadHandler(
      filename = function() { base::paste0("GO_enrichment_plot_", base::Sys.Date(), ".pdf") },
      content = function(file) {
        shiny::req(rv$go_res)
        if (input$go_plot_type %in% c("bar", "dot")) {
          grDevices::pdf(file, width = input$go_width, height = input$go_height)
          if (input$go_plot_type == "dot") {
            print(clusterProfiler::dotplot(rv$go_res, showCategory = input$go_top_n) +
                    ggplot2::scale_color_manual(values = input$go_color))
          } else {
            print(graphics::barplot(rv$go_res, showCategory = input$go_top_n, fill = input$go_color))
          }
          grDevices::dev.off()
        } else {
          plot_go_circos(rv$go_res, top_n = input$go_top_n, output_pdf = file)
        }
      }
    )
    # GO table
    output$download_go_table <- shiny::downloadHandler(
      filename = function() { base::paste0("GO_enrichment_table_", base::Sys.Date(), ".csv") },
      content = function(file) {
        shiny::req(rv$go_res)
        utils::write.csv(base::as.data.frame(rv$go_res@result), file, row.names = FALSE)
      }
    )
    # KEGG plot
    output$download_kegg_plot <- shiny::downloadHandler(
      filename = function() { base::paste0("KEGG_enrichment_plot_", base::Sys.Date(), ".pdf") },
      content = function(file) {
        shiny::req(rv$kegg_res)
        if (input$kegg_plot_type %in% c("bar", "dot")) {
          grDevices::pdf(file, width = input$kegg_width, height = input$kegg_height)
          if (input$kegg_plot_type == "dot") {
            print(clusterProfiler::dotplot(rv$kegg_res, showCategory = input$kegg_top_n) +
                    ggplot2::scale_color_manual(values = input$kegg_color))
          } else {
            print(graphics::barplot(rv$kegg_res, showCategory = input$kegg_top_n, fill = input$kegg_color))
          }
          grDevices::dev.off()
        } else {
          plot_go_circos(rv$kegg_res, top_n = input$kegg_top_n, output_pdf = file)
        }
      }
    )
    # KEGG table
    output$download_kegg_table <- shiny::downloadHandler(
      filename = function() { base::paste0("KEGG_enrichment_table_", base::Sys.Date(), ".csv") },
      content = function(file) {
        shiny::req(rv$kegg_res)
        utils::write.csv(base::as.data.frame(rv$kegg_res@result), file, row.names = FALSE)
      }
    )
  })
}
