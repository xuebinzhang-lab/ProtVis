#' WGCNA Module for ProtVis
#'
#' Weighted gene co-expression network analysis module adapted for ProtVis.
#' This module integrates data filtering, soft-threshold selection, module
#' detection, module-trait association, interested module exploration,
#' hub-gene extraction, and Cytoscape export.
#'
#' @name wgcna_module
NULL

#' WGCNA UI Module
#'
#' @param id A unique module id.
#'
#' @return A UI definition for the WGCNA module.
#' @import shiny
#' @import bslib
#' @export
wgcna_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),
    bslib::page_fillable(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 340,
          open = TRUE,

          bslib::accordion(
            id = ns("wgcna_accordion"),
            multiple = TRUE,
            open = c("Data Input"),

            bslib::accordion_panel(
              title = "Data Input",
              shiny::fileInput(
                ns("ExpMat"),
                "Upload expression matrix",
                accept = c(".txt", ".tsv", ".csv")
              ),
              shiny::p(
                "Supported formats: tab-delimited txt/tsv or csv. First column should be feature ID and the remaining columns should be samples.",
                style = "color:#6c757d; font-size:12px;"
              ),
              shiny::radioButtons(
                ns("format"),
                "Format",
                choices = c(
                  "count",
                  "expected count",
                  "normalized count",
                  "peak area (metabolomics)",
                  "protein abundance"
                ),
                selected = "count"
              ),
              shiny::radioButtons(
                ns("networktype"),
                "Network type",
                choices = c("unsigned", "signed"),
                selected = "unsigned"
              ),
              shiny::selectInput(
                ns("method1"),
                "Normalization method",
                choices = c("vst", "raw", "logarithm"),
                selected = "vst"
              ),
              shiny::actionButton(
                ns("action1"),
                "Run Data Cleaning",
                class = "btn btn-primary w-100"
              )
            ),

            bslib::accordion_panel(
              title = "Filtering",
              shiny::h5("First filter"),
              shiny::textInput(ns("SamPer"), "Sample percentage", value = "0.9"),
              shiny::textInput(ns("RCcut"), "Expression cutoff", value = "10"),
              shiny::p(
                "Remove low-abundance features detected below the cutoff in most samples.",
                style = "color:#6c757d; font-size:12px;"
              ),
              shiny::hr(),
              shiny::h5("Second filter"),
              shiny::radioButtons(
                ns("CutMethod"),
                "Filter method",
                choices = c("MAD", "Var"),
                selected = "MAD"
              ),
              shiny::textInput(ns("remain"), "Reserved feature number", value = "8000")
            ),

            bslib::accordion_panel(
              title = "Soft Threshold",
              shiny::sliderInput(
                ns("CutoffR"),
                shiny::HTML("R<sup>2</sup> cutoff"),
                min = 0,
                max = 1,
                value = 0.8
              ),
              shiny::radioButtons(
                ns("PowerTorF"),
                "Power type",
                choices = c("Recommended", "Customized"),
                selected = "Recommended"
              ),
              shiny::sliderInput(ns("PowerSelect"), "Final power", min = 1, max = 33, value = 6),
              shiny::actionButton(
                ns("Startsft"),
                "Run SFT",
                class = "btn btn-primary w-100"
              ),
              shiny::br(),
              shiny::br(),
              shiny::actionButton(
                ns("Startcheck"),
                "Check Scale-Free Topology",
                class = "btn btn-outline-primary w-100"
              )
            ),

            bslib::accordion_panel(
              title = "Module Detection",
              shiny::sliderInput(ns("minMsize"), "Minimum module size", min = 5, max = 200, value = 30),
              shiny::sliderInput(ns("mch"), "Module cut tree height", min = 0, max = 1, value = 0.25),
              shiny::textInput(ns("blocksize"), "Max block size", value = "5000"),
              shiny::actionButton(
                ns("Startnet"),
                "Run Module Detection",
                class = "btn btn-primary w-100"
              )
            ),

            bslib::accordion_panel(
              title = "Trait Association",
              shiny::fileInput(
                ns("traitData"),
                "Upload trait data",
                accept = c(".txt", ".tsv", ".csv")
              ),
              colourpicker::colourInput(ns("colormin"), "Minimum", value = "purple"),
              colourpicker::colourInput(ns("colormid"), "Middle", value = "white"),
              colourpicker::colourInput(ns("colormax"), "Maximum", value = "yellow"),
              shiny::textInput(ns("xangle"), "X-axis label angle", value = "0"),
              shiny::actionButton(
                ns("starttrait"),
                "Run Module-Trait Analysis",
                class = "btn btn-primary w-100"
              )
            ),

            bslib::accordion_panel(
              title = "Interested Module",
              shiny::selectInput(ns("strait"), "Select trait", choices = NULL),
              shiny::selectInput(ns("smodule"), "Select module", choices = NULL),
              shiny::actionButton(
                ns("InterMode"),
                "Run Interested Module Analysis",
                class = "btn btn-primary w-100"
              )
            ),

            bslib::accordion_panel(
              title = "Hub Gene",
              shiny::selectInput(ns("hubtrait"), "Select trait", choices = NULL),
              shiny::selectInput(ns("hubmodule"), "Select module", choices = NULL),
              shiny::sliderInput(
                ns("kMEcut"),
                "Absolute kME cutoff",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.01
              ),
              shiny::sliderInput(
                ns("GScut"),
                "Absolute GS cutoff",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.01
              ),
              shiny::textInput(ns("threshold"), "Weight threshold for Cytoscape", value = "0.02"),
              shiny::actionButton(
                ns("starthub"),
                "Run Hub Gene Analysis",
                class = "btn btn-primary w-100"
              ),
              shiny::br(),
              shiny::br(),
              shiny::actionButton(
                ns("threadd"),
                "Generate Cytoscape Tables",
                class = "btn btn-outline-primary w-100"
              )
            )
          )
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header("WGCNA Analysis"),
          bslib::navset_card_tab(
            id = ns("wgcna_tabs"),

            bslib::nav_panel(
              "Input Summary",
              shiny::uiOutput(ns("Inputcheck")),
              shiny::uiOutput(ns("filter1")),
              DT::DTOutput(ns("Inputbl"))
            ),

            bslib::nav_panel(
              "Sample Clustering",
              shiny::plotOutput(ns("clustPlot"), height = "500px"),
              shiny::downloadButton(ns("downfig1"), "Download")
            ),

            bslib::nav_panel(
              "SFT",
              shiny::uiOutput(ns("powerout")),
              shiny::plotOutput(ns("sftplot"), height = "500px"),
              DT::DTOutput(ns("sfttbl")),
              shiny::plotOutput(ns("sfttest"), height = "500px"),
              shiny::downloadButton(ns("downfig2"), "Download SFT Plot"),
              shiny::downloadButton(ns("downfig3"), "Download Scale-Free Plot")
            ),

            bslib::nav_panel(
              "Module Detection",
              shiny::plotOutput(ns("cluster"), height = "500px"),
              shiny::tableOutput(ns("m2num")),
              shiny::plotOutput(ns("eah"), height = "500px"),
              DT::DTOutput(ns("g2m")),
              shiny::downloadButton(ns("downfig4"), "Download Dendrogram"),
              shiny::downloadButton(ns("downfig5"), "Download Eigengene Heatmap"),
              shiny::downloadButton(ns("downtbl2"), "Download Gene-to-Module")
            ),

            bslib::nav_panel(
              "Module-Trait",
              shiny::plotOutput(ns("mtplot"), height = "600px"),
              DT::DTOutput(ns("traitmat")),
              DT::DTOutput(ns("traitp")),
              DT::DTOutput(ns("KME")),
              shiny::downloadButton(ns("downfig6"), "Download Module-Trait Heatmap"),
              shiny::downloadButton(ns("downtbl3"), "Download KME Table")
            ),

            bslib::nav_panel(
              "Interested Module",
              shiny::plotOutput(ns("GSCon"), height = "500px"),
              shiny::plotOutput(ns("heatmap"), height = "500px"),
              shiny::plotOutput(ns("GSMM.all"), height = "500px"),
              shiny::downloadButton(ns("downfig7"), "Download GS-Connectivity"),
              shiny::downloadButton(ns("downfig8"), "Download Heatmap"),
              shiny::downloadButton(ns("downfig10"), "Download GS vs MM")
            ),

            bslib::nav_panel(
              "Hub Gene & Cytoscape",
              DT::DTOutput(ns("cthub")),
              DT::DTOutput(ns("kMEhub")),
              DT::DTOutput(ns("edgeFile")),
              DT::DTOutput(ns("nodeFile")),
              shiny::downloadButton(ns("downtbl4"), "Download Hub Gene Table"),
              shiny::downloadButton(ns("downtbl5"), "Download Edge File"),
              shiny::downloadButton(ns("downtbl6"), "Download Node File")
            )
          )
        )
      )
    )
  )
}

#' WGCNA Server Module
#'
#' @param id A unique module id.
#' @param rv Optional shared reactiveValues object from ProtVis.
#'
#' @return No return value. This function is called for its side effects.
#' @import shiny
#' @import bslib
#' @export
wgcna_server <- function(id, rv = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    testInteger <- function(x) {
      test <- base::all.equal(x, base::as.integer(x), check.attributes = FALSE)
      identical(test, TRUE)
    }

    require_shinywgcna <- function() {
      if (base::requireNamespace("ShinyWGCNA", quietly = TRUE)) {
        return(TRUE)
      }

      shiny::showNotification(
        'The optional ShinyWGCNA package is required for the WGCNA module. Install it with remotes::install_github("ShawnWx2019/WGCNAShinyFun", ref = "master").',
        type = "error",
        duration = NULL
      )
      FALSE
    }

    read_input_table <- function(path) {
      ext <- base::tolower(tools::file_ext(path))
      if (ext %in% c("csv")) {
        utils::read.csv(
          path,
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else {
        utils::read.delim(
          path,
          sep = "\t",
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      }
    }

    exp.ds <- shiny::reactiveValues()
    downloads <- shiny::reactiveValues(
      width2 = 10, height2 = 10,
      width3 = 10, height3 = 10,
      width4 = 10, height4 = 10,
      width5 = 10, height5 = 10,
      width6 = 10, height6 = 10,
      width7 = 10, height7 = 10,
      width8 = 10, height8 = 10,
      width10 = 10, height10 = 10
    )

    data <- shiny::reactive({
      shiny::req(input$ExpMat)
      read_input_table(input$ExpMat$datapath)
    })

    data_check <- shiny::reactive({
      shiny::req(data())
      if (isTRUE(testInteger(data()[, 2]))) "count" else "non-count"
    })

    fmt_select <- shiny::reactive({
      shiny::req(data())
      if (isTRUE(testInteger(data()[, 2]))) {
        "count"
      } else {
        "normalized count, peak area (metabolomics), protein abundance or expected count"
      }
    })

    output$Inputcheck <- shiny::renderUI({
      shiny::req(data())
      if (base::length(base::which(base::is.na(data()))) == 0) {
        shiny::HTML(
          base::paste0(
            "<b>Input file looks valid.</b><br/>",
            "Detected matrix type: <b>", data_check(), "</b><br/>",
            "Recommended format selection: <b>", fmt_select(), "</b>"
          )
        )
      } else {
        shiny::HTML(
          "<b>Input matrix contains NA values.</b><br/>Please remove blank values or rows and upload again."
        )
      }
    })

    shiny::observe({
      if (input$format %in% c("count", "expected count")) {
        shiny::updateSelectInput(session, "method1", choices = c("vst"))
        shiny::updateTextInput(session, "RCcut", value = "10")
      } else {
        shiny::updateSelectInput(session, "method1", choices = c("raw", "logarithm"))
        shiny::updateTextInput(session, "RCcut", value = "1")
      }
    })

    sampP <- shiny::reactive(base::as.numeric(input$SamPer))
    rccutoff <- shiny::reactive(base::as.numeric(input$RCcut))
    GNC <- shiny::reactive(base::as.numeric(input$remain))
    cutmethod <- shiny::reactive(input$CutMethod)
    networktype <- shiny::reactive(base::as.character(input$networktype))
    fmt <- shiny::reactive(input$format)
    mtd <- shiny::reactive(input$method1)

    shiny::observeEvent(input$action1, {
      if (!require_shinywgcna()) {
        return(NULL)
      }

      shiny::req(data())

      if (base::length(base::which(base::is.na(data()))) != 0) {
        shiny::showNotification("Input matrix contains NA values.", type = "error")
        return(NULL)
      }

      exp.ds$table <- base::data.frame()
      exp.ds$table2 <- base::data.frame()
      exp.ds$param <- base::list()

      shiny::withProgress(message = "Data cleaning", value = 0, {
        shiny::incProgress(0.5, detail = "Running first filter")

        exp.ds$table <- ShinyWGCNA::getdatExpr(
          rawdata = data(),
          RcCutoff = rccutoff(),
          samplePerc = sampP(),
          datatype = fmt(),
          method = mtd()
        )

        shiny::incProgress(0.5, detail = "Running second filter")

        exp.ds$GNC_check <- GNC() - base::nrow(exp.ds$table)

        if (exp.ds$GNC_check > 0) {
          exp.ds$GNC <- base::nrow(exp.ds$table)
          exp.ds$gnccheck <- "Requested reserved feature number exceeds the remaining features after first filtering. All remaining features were kept."
        } else {
          exp.ds$GNC <- GNC()
          exp.ds$gnccheck <- "Filtering finished successfully."
        }

        exp.ds$table2 <- ShinyWGCNA::getdatExpr2(
          datExpr = exp.ds$table,
          GeneNumCut = 1 - exp.ds$GNC / base::nrow(exp.ds$table),
          cutmethod = cutmethod()
        )

        exp.ds$param <- ShinyWGCNA::getsampleTree(exp.ds$table2)
      })

      output$filter1 <- shiny::renderUI({
        shiny::HTML(
          base::paste0(
            "<b>First filter cutoff:</b> ", rccutoff(), "<br/>",
            "<b>Sample percentage:</b> ", 100 * sampP(), "%<br/>",
            "<b>Remaining after first filter:</b> ", base::nrow(exp.ds$table), "<br/>",
            "<b>Second filter method:</b> ", cutmethod(), "<br/>",
            "<b>Reserved features:</b> ", exp.ds$GNC, "<br/>",
            "<b>Remaining matrix columns:</b> ", base::ncol(exp.ds$table2), "<br/>",
            "<b>Note:</b> ", exp.ds$gnccheck
          )
        )
      })

      shiny::showNotification("WGCNA data cleaning finished.", type = "message")
    })

    output$Inputbl <- DT::renderDT({
      shiny::req(exp.ds$table2)
      DT::datatable(
        base::as.data.frame(base::t(exp.ds$table2)),
        options = base::list(scrollX = TRUE)
      )
    })

    output$clustPlot <- shiny::renderPlot({
      shiny::req(exp.ds$param$sampleTree)
      graphics::plot(
        exp.ds$param$sampleTree,
        main = "Sample clustering to detect outlier",
        sub = "",
        xlab = ""
      )
    })

    rscut <- shiny::reactive(base::as.numeric(input$CutoffR))

    shiny::observeEvent(input$Startsft, {
      shiny::req(exp.ds$table2)

      shiny::withProgress(message = "SFT selection", value = 0, {
        shiny::incProgress(1, detail = "Selecting soft threshold")
        exp.ds$sft <- ShinyWGCNA::getpower(
          datExpr = exp.ds$table2,
          rscut = rscut(),
          type = networktype()
        )
      })

      output$powerout <- shiny::renderUI({
        shiny::HTML(
          base::paste0(
            "<b>Recommended power:</b> ", exp.ds$sft$power,
            "<br/>If this does not satisfy your scale-free criterion, switch to customized power."
          )
        )
      })

      shiny::showNotification("SFT analysis finished.", type = "message")
    })

    output$sftplot <- shiny::renderPlot({
      shiny::req(exp.ds$sft$plot)
      exp.ds$sft$plot
    })

    output$sfttbl <- DT::renderDT({
      shiny::req(exp.ds$sft$sft)
      DT::datatable(
        base::as.data.frame(exp.ds$sft$sft),
        options = base::list(scrollX = TRUE)
      )
    })

    shiny::observeEvent(input$Startcheck, {
      shiny::req(exp.ds$table2, exp.ds$sft)

      power_use <- if (input$PowerTorF == "Recommended") {
        exp.ds$sft$power
      } else {
        base::as.numeric(input$PowerSelect)
      }

      exp.ds$power <- power_use

      shiny::withProgress(message = "Checking scale-free topology", value = 0, {
        shiny::incProgress(1, detail = "Testing selected power")
        exp.ds$cksft <- ShinyWGCNA::powertest(
          power.test = power_use,
          datExpr = exp.ds$table2,
          nGenes = exp.ds$param$nGenes,
          type = networktype()
        )
      })

      shiny::showNotification("Scale-free topology check finished.", type = "message")
    })

    output$sfttest <- shiny::renderPlot({
      shiny::req(exp.ds$cksft)
      exp.ds$cksft
    })

    shiny::observeEvent(input$Startnet, {
      shiny::req(exp.ds$table2, exp.ds$power)

      shiny::withProgress(message = "Module detection", value = 0, {
        shiny::incProgress(1, detail = "Detecting modules")
        exp.ds$netout <- ShinyWGCNA::getnetwork(
          datExpr = exp.ds$table2,
          power = exp.ds$power,
          minModuleSize = base::as.numeric(input$minMsize),
          mergeCutHeight = base::as.numeric(input$mch),
          maxBlocksize = base::as.numeric(input$blocksize)
        )
      })

      exp.ds$nSamples <- base::nrow(exp.ds$table2)
      exp.ds$net <- exp.ds$netout$net
      exp.ds$moduleLabels <- exp.ds$netout$moduleLabels
      exp.ds$moduleColors <- exp.ds$netout$moduleColors
      exp.ds$MEs_col <- exp.ds$netout$MEs_col
      exp.ds$MEs <- exp.ds$netout$MEs
      exp.ds$Gene2module <- exp.ds$netout$Gene2module

      shiny::showNotification("Module detection finished.", type = "message")
    })

    output$cluster <- shiny::renderPlot({
      shiny::req(exp.ds$net, exp.ds$moduleColors)
      WGCNA::plotDendroAndColors(
        exp.ds$net$dendrograms[[1]],
        exp.ds$moduleColors[exp.ds$net$blockGenes[[1]]],
        "Module colors",
        dendroLabels = FALSE,
        hang = 0.03,
        addGuide = TRUE,
        guideHang = 0.05
      )
    })

    output$m2num <- shiny::renderTable({
      shiny::req(exp.ds$moduleColors)
      base::table(exp.ds$moduleColors)
    })

    output$eah <- shiny::renderPlot({
      shiny::req(exp.ds$MEs_col)
      WGCNA::plotEigengeneNetworks(
        exp.ds$MEs_col,
        "Eigengene adjacency heatmap",
        marDendro = c(3, 3, 2, 4),
        marHeatmap = c(3, 4, 2, 2),
        plotDendrograms = TRUE,
        xLabelsAngle = 90
      )
    })

    output$g2m <- DT::renderDT({
      shiny::req(exp.ds$Gene2module)
      DT::datatable(exp.ds$Gene2module, options = base::list(scrollX = TRUE))
    })

    phen <- shiny::reactive({
      shiny::req(input$traitData)
      read_input_table(input$traitData$datapath)
    })

    shiny::observeEvent(input$starttrait, {
      shiny::req(phen(), exp.ds$table2, exp.ds$moduleColors, exp.ds$MEs_col)

      if (base::ncol(phen()) == 2) {
        x <- phen()
        Tcol <- base::as.character(base::unique(x[, 2]))
        b <- base::vector("list", base::length(Tcol))

        for (i in base::seq_along(Tcol)) {
          b[[i]] <- base::data.frame(
            row.names = x[, 1],
            levels = base::ifelse(x[, 2] == Tcol[i], 1, 0)
          )
        }

        cmat <- dplyr::bind_cols(b)
        cmat <- base::data.frame(row.names = x[, 1], cmat)
        base::colnames(cmat) <- Tcol
        exp.ds$phen <- cmat
      } else {
        exp.ds$phen <- base::data.frame(
          row.names = phen()[, 1],
          phen()[, -1, drop = FALSE]
        )
      }

      exp.ds$phen <- exp.ds$phen[
        base::match(base::rownames(exp.ds$table2), base::rownames(exp.ds$phen)),
        ,
        drop = FALSE
      ]

      shiny::withProgress(message = "Module-trait analysis", value = 0, {
        shiny::incProgress(0.5, detail = "Calculating module-trait relationships")
        exp.ds$traitout <- ShinyWGCNA::getMt(
          phenotype = exp.ds$phen,
          nSamples = exp.ds$nSamples,
          moduleColors = exp.ds$moduleColors,
          datExpr = exp.ds$table2
        )

        shiny::incProgress(0.5, detail = "Calculating KME")
        exp.ds$KME <- ShinyWGCNA::getKME(
          datExpr = exp.ds$table2,
          moduleColors = exp.ds$moduleColors,
          MEs_col = exp.ds$MEs_col
        )
      })

      exp.ds$modTraitCor <- exp.ds$traitout$modTraitCor
      exp.ds$modTraitP <- exp.ds$traitout$modTraitP
      exp.ds$textMatrix <- exp.ds$traitout$textMatrix
      exp.ds$mod_color <- base::gsub("^..", "", base::rownames(exp.ds$modTraitCor))
      exp.ds$mod_color_anno <- stats::setNames(exp.ds$mod_color, base::rownames(exp.ds$modTraitCor))

      exp.ds$Left_anno <- ComplexHeatmap::rowAnnotation(
        Module = base::rownames(exp.ds$modTraitCor),
        col = base::list(Module = exp.ds$mod_color_anno),
        show_legend = FALSE,
        show_annotation_name = FALSE
      )

      shiny::showNotification("Module-trait analysis finished.", type = "message")
    })

    output$mtplot <- shiny::renderPlot({
      shiny::req(exp.ds$modTraitCor, exp.ds$textMatrix, exp.ds$Left_anno)

      hm <- ComplexHeatmap::Heatmap(
        matrix = exp.ds$modTraitCor,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        left_annotation = exp.ds$Left_anno,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid::grid.text(
            base::sprintf(exp.ds$textMatrix[i, j]),
            x,
            y,
            gp = grid::gpar(fontsize = 12)
          )
        },
        row_names_side = "left",
        column_names_rot = base::as.numeric(input$xangle),
        heatmap_legend_param = base::list(
          at = c(-1, -0.5, 0, 0.5, 1),
          labels = c("-1", "-0.5", "0", "0.5", "1"),
          title = ""
        ),
        rect_gp = grid::gpar(col = "black", lwd = 1.2),
        column_title = "Module-trait relationships",
        col = circlize::colorRamp2(
          c(-1, 0, 1),
          c(input$colormin, input$colormid, input$colormax)
        )
      )

      ComplexHeatmap::draw(hm)
    })

    output$traitmat <- DT::renderDT({
      shiny::req(exp.ds$modTraitCor)
      DT::datatable(
        base::as.data.frame(exp.ds$modTraitCor),
        options = base::list(scrollX = TRUE)
      )
    })

    output$traitp <- DT::renderDT({
      shiny::req(exp.ds$modTraitP)
      DT::datatable(
        base::as.data.frame(exp.ds$modTraitP),
        options = base::list(scrollX = TRUE)
      )
    })

    output$KME <- DT::renderDT({
      shiny::req(exp.ds$KME)
      DT::datatable(
        base::as.data.frame(exp.ds$KME),
        options = base::list(scrollX = TRUE)
      )
    })

    shiny::observe({
      if (!base::is.null(exp.ds$modTraitP)) {
        shiny::updateSelectInput(
          session,
          "smodule",
          choices = base::gsub("^..", "", base::rownames(exp.ds$modTraitP))
        )
        shiny::updateSelectInput(
          session,
          "hubmodule",
          choices = base::gsub("^..", "", base::rownames(exp.ds$modTraitP))
        )
        shiny::updateSelectInput(
          session,
          "strait",
          choices = base::colnames(exp.ds$modTraitP)
        )
        shiny::updateSelectInput(
          session,
          "hubtrait",
          choices = base::colnames(exp.ds$modTraitP)
        )
      }
    })

    shiny::observeEvent(input$InterMode, {
      shiny::req(exp.ds$phen, exp.ds$table2, exp.ds$MEs_col)

      exp.ds$GSout <- ShinyWGCNA::getMM(
        datExpr = exp.ds$table2,
        MEs_col = exp.ds$MEs_col,
        nSamples = exp.ds$nSamples,
        corType = "pearson"
      )

      exp.ds$MM <- exp.ds$GSout$MM
      exp.ds$MMP <- exp.ds$GSout$MMP
      exp.ds$sml <- base::as.character(input$smodule)
      exp.ds$st <- base::as.character(input$strait)

      exp.ds$Heatmap <- ShinyWGCNA::moduleheatmap(
        datExpr = exp.ds$table2,
        MEs = exp.ds$MEs_col,
        which.module = exp.ds$sml,
        moduleColors = exp.ds$moduleColors
      )

      shiny::showNotification("Interested module analysis finished.", type = "message")
    })

    output$GSCon <- shiny::renderPlot({
      shiny::req(exp.ds$st, exp.ds$sml, exp.ds$MM)
      ShinyWGCNA::getverboseplot(
        datExpr = exp.ds$table2,
        module = exp.ds$sml,
        pheno = exp.ds$st,
        MEs = exp.ds$MEs_col,
        traitData = exp.ds$phen,
        moduleColors = exp.ds$moduleColors,
        geneModuleMembership = exp.ds$MM,
        nSamples = exp.ds$nSamples
      )
    })

    output$heatmap <- shiny::renderPlot({
      shiny::req(exp.ds$Heatmap)
      exp.ds$Heatmap
    })

    output$GSMM.all <- shiny::renderPlot({
      shiny::req(exp.ds$st, exp.ds$sml, exp.ds$MM)
      ShinyWGCNA::MMvsGSall(
        which.trait = exp.ds$st,
        traitData = exp.ds$phen,
        datExpr = exp.ds$table2,
        moduleColors = exp.ds$moduleColors,
        geneModuleMembership = exp.ds$MM,
        MEs = exp.ds$MEs_col,
        nSamples = exp.ds$nSamples
      )
    })

    shiny::observeEvent(input$starthub, {
      shiny::req(exp.ds$table2, exp.ds$KME, exp.ds$phen, exp.ds$Gene2module)

      exp.ds$hubml <- base::as.character(input$hubmodule)
      exp.ds$hubt <- base::as.character(input$hubtrait)
      exp.ds$kMEcut <- base::as.numeric(input$kMEcut)
      exp.ds$GScut <- base::as.numeric(input$GScut)

      exp.ds$hub.all <- ShinyWGCNA::hubgenes(
        datExpr = exp.ds$table2,
        mdl = exp.ds$hubml,
        power = exp.ds$power,
        trt = exp.ds$hubt,
        KME = exp.ds$KME,
        GS.cut = exp.ds$GScut,
        kME.cut = exp.ds$kMEcut,
        datTrait = exp.ds$phen,
        g2m = exp.ds$Gene2module,
        type = networktype()
      )

      shiny::showNotification("Hub gene analysis finished.", type = "message")
    })

    shiny::observeEvent(input$threadd, {
      shiny::req(exp.ds$hubml, exp.ds$table2, exp.ds$moduleColors)

      exp.ds$threshold_use <- base::as.numeric(input$threshold)

      exp.ds$cyt <- ShinyWGCNA::cytoscapeout(
        datExpr = exp.ds$table2,
        power = exp.ds$power,
        module = exp.ds$hubml,
        moduleColors = exp.ds$moduleColors,
        threshold = exp.ds$threshold_use,
        type = networktype()
      )

      shiny::showNotification("Cytoscape tables generated.", type = "message")
    })

    output$cthub <- DT::renderDT({
      shiny::req(exp.ds$hub.all$hub1)
      DT::datatable(exp.ds$hub.all$hub1, options = base::list(scrollX = TRUE))
    })

    output$kMEhub <- DT::renderDT({
      shiny::req(exp.ds$hub.all$hub3)
      DT::datatable(exp.ds$hub.all$hub3, options = base::list(scrollX = TRUE))
    })

    output$edgeFile <- DT::renderDT({
      shiny::req(exp.ds$cyt[[1]])
      DT::datatable(exp.ds$cyt[[1]], options = base::list(scrollX = TRUE))
    })

    output$nodeFile <- DT::renderDT({
      shiny::req(exp.ds$cyt[[2]])
      DT::datatable(exp.ds$cyt[[2]], options = base::list(scrollX = TRUE))
    })

    output$downfig1 <- shiny::downloadHandler(
      filename = function() {
        "01.SampleCluster.nwk"
      },
      content = function(file) {
        ape::write.tree(phy = exp.ds$param$tree, file = file)
      }
    )

    output$downfig2 <- shiny::downloadHandler(
      filename = function() {
        "02.SftResult.pdf"
      },
      content = function(file) {
        ggplot2::ggsave(
          plot = exp.ds$sft$plot,
          filename = file,
          width = 10,
          height = 10
        )
      }
    )

    output$downfig3 <- shiny::downloadHandler(
      filename = function() {
        "03.CheckSft.pdf"
      },
      content = function(file) {
        ggplot2::ggsave(
          plot = exp.ds$cksft,
          filename = file,
          width = 10,
          height = 10
        )
      }
    )

    output$downfig4 <- shiny::downloadHandler(
      filename = function() {
        "04.ClusterDendrogram.pdf"
      },
      content = function(file) {
        grDevices::pdf(file = file, width = 10, height = 10)
        WGCNA::plotDendroAndColors(
          exp.ds$net$dendrograms[[1]],
          exp.ds$moduleColors[exp.ds$net$blockGenes[[1]]],
          "Module colors",
          dendroLabels = FALSE,
          hang = 0.03,
          addGuide = TRUE,
          guideHang = 0.05
        )
        grDevices::dev.off()
      }
    )

    output$downfig5 <- shiny::downloadHandler(
      filename = function() {
        "05.EigengeneAdjacencyHeatmap.pdf"
      },
      content = function(file) {
        grDevices::pdf(file = file, width = 10, height = 10)
        WGCNA::plotEigengeneNetworks(
          exp.ds$MEs_col,
          "Eigengene adjacency heatmap",
          marDendro = c(3, 3, 2, 4),
          marHeatmap = c(3, 4, 2, 2),
          plotDendrograms = TRUE,
          xLabelsAngle = 90
        )
        grDevices::dev.off()
      }
    )

    output$downfig6 <- shiny::downloadHandler(
      filename = function() {
        "06.ModuleTrait.pdf"
      },
      content = function(file) {
        grDevices::pdf(file = file, width = 10, height = 10)

        hm <- ComplexHeatmap::Heatmap(
          matrix = exp.ds$modTraitCor,
          cluster_rows = FALSE,
          cluster_columns = FALSE,
          left_annotation = exp.ds$Left_anno,
          cell_fun = function(j, i, x, y, width, height, fill) {
            grid::grid.text(
              base::sprintf(exp.ds$textMatrix[i, j]),
              x,
              y,
              gp = grid::gpar(fontsize = 12)
            )
          },
          row_names_side = "left",
          column_names_rot = base::as.numeric(input$xangle),
          rect_gp = grid::gpar(col = "black", lwd = 1.2),
          column_title = "Module-trait relationships",
          col = circlize::colorRamp2(
            c(-1, 0, 1),
            c(input$colormin, input$colormid, input$colormax)
          )
        )

        ComplexHeatmap::draw(hm)
        grDevices::dev.off()
      }
    )

    output$downfig7 <- shiny::downloadHandler(
      filename = function() {
        base::paste0("07.GS_", exp.ds$sml, "_", exp.ds$st, ".pdf")
      },
      content = function(file) {
        grDevices::pdf(file = file, width = 10, height = 10)
        base::print(
          ShinyWGCNA::getverboseplot(
            datExpr = exp.ds$table2,
            module = exp.ds$sml,
            pheno = exp.ds$st,
            MEs = exp.ds$MEs_col,
            traitData = exp.ds$phen,
            moduleColors = exp.ds$moduleColors,
            geneModuleMembership = exp.ds$MM,
            nSamples = exp.ds$nSamples
          )
        )
        grDevices::dev.off()
      }
    )

    output$downfig8 <- shiny::downloadHandler(
      filename = function() {
        base::paste0("08.", exp.ds$sml, "_heatmap.pdf")
      },
      content = function(file) {
        grDevices::pdf(file = file, width = 10, height = 10)
        base::print(exp.ds$Heatmap)
        grDevices::dev.off()
      }
    )

    output$downfig10 <- shiny::downloadHandler(
      filename = function() {
        "09.GSvsMM.all.pdf"
      },
      content = function(file) {
        grDevices::pdf(file = file, width = 10, height = 10)
        base::print(
          ShinyWGCNA::MMvsGSall(
            which.trait = exp.ds$st,
            traitData = exp.ds$phen,
            nSamples = exp.ds$nSamples,
            datExpr = exp.ds$table2,
            moduleColors = exp.ds$moduleColors,
            geneModuleMembership = exp.ds$MM,
            MEs = exp.ds$MEs_col
          )
        )
        grDevices::dev.off()
      }
    )

    output$downtbl2 <- shiny::downloadHandler(
      filename = function() {
        "01.Gene2Module.tsv"
      },
      content = function(file) {
        utils::write.table(
          exp.ds$Gene2module,
          file = file,
          sep = "\t",
          row.names = FALSE,
          quote = FALSE
        )
      }
    )

    output$downtbl3 <- shiny::downloadHandler(
      filename = function() {
        "02.KMEofAllGenes.tsv"
      },
      content = function(file) {
        utils::write.table(
          exp.ds$KME,
          file = file,
          sep = "\t",
          row.names = TRUE,
          quote = FALSE
        )
      }
    )

    output$downtbl4 <- shiny::downloadHandler(
      filename = function() {
        base::paste0("03.", exp.ds$hubml, "_", exp.ds$hubt, "_hubgene.tsv")
      },
      content = function(file) {
        utils::write.table(
          exp.ds$hub.all$hub3,
          file = file,
          sep = "\t",
          row.names = FALSE,
          quote = FALSE
        )
      }
    )

    output$downtbl5 <- shiny::downloadHandler(
      filename = function() {
        base::paste0("04.", exp.ds$hubml, ".edge.tsv")
      },
      content = function(file) {
        utils::write.table(
          exp.ds$cyt[[1]],
          file = file,
          sep = "\t",
          row.names = FALSE,
          quote = FALSE
        )
      }
    )

    output$downtbl6 <- shiny::downloadHandler(
      filename = function() {
        base::paste0("05.", exp.ds$hubml, ".node.tsv")
      },
      content = function(file) {
        utils::write.table(
          exp.ds$cyt[[2]],
          file = file,
          sep = "\t",
          row.names = FALSE,
          quote = FALSE
        )
      }
    )
  })
}
