
#' @importFrom circlize colorRamp2 circos.clear circos.genomicInitialize circos.trackPlotRegion
#'   get.cell.meta.data circos.text get.all.sector.index circos.axis circos.genomicTrack
#'   circos.genomicRect circos.genomicText
#' @importFrom ComplexHeatmap draw Legend
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grid unit gpar grid.lines
#' @importFrom utils head write.csv read.csv read.table
#' @importFrom grDevices pdf dev.off
#' @importFrom shiny NS moduleServer reactiveValues reactive req observeEvent renderUI
#'   uiOutput fileInput actionButton textAreaInput conditionalPanel selectInput
#'   checkboxGroupInput numericInput sliderInput downloadButton plotOutput renderPlot
#'   renderText showNotification textOutput tagList span div hr
#' @import bslib
#' @importFrom shinyWidgets switchInput
#' @importFrom colourpicker colourInput
#' @importFrom dplyr filter pull select mutate across everything
#' @importFrom tidyr separate
#' @importFrom stringr str_trim
#' @importFrom readxl read_excel excel_sheets
#' @importFrom tools file_ext
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom clusterProfiler enricher
#' @importFrom ggplot2 ggplot aes geom_col geom_point coord_flip theme_bw labs
#'   theme element_text scale_size_continuous
#' @importFrom graphics plot text par
#' @title plot_go_circos
#' @name plot_go_circos
#' @export
plot_go_circos <- function(go_data, top_n = 15, output_pdf = NULL) {
  if (base::is.null(go_data) || !base::is.data.frame(go_data) || base::nrow(go_data) == 0) {
    message("No enrichment data available to plot.")
    return(NULL)
  }

  required_cols <- c("ID", "Description", "pvalue", "BgRatio", "GeneRatio")
  if (!base::all(required_cols %in% base::colnames(go_data))) {
    message("Input data does not contain required columns.")
    return(NULL)
  }

  data <- go_data[base::order(go_data$pvalue), , drop = FALSE]
  datasig <- data[data$pvalue < 0.05, , drop = FALSE]
  data <- utils::head(datasig, top_n)

  if (base::nrow(data) == 0) {
    message("No significant GO/KEGG terms to plot.")
    return(NULL)
  }

  BgGene <- base::as.numeric(base::sapply(base::strsplit(base::as.character(data$BgRatio), "/"), `[`, 1))
  Gene <- base::as.numeric(base::sapply(base::strsplit(base::as.character(data$GeneRatio), "/"), `[`, 1))
  ratio <- Gene / BgGene
  logpvalue <- -base::log10(data$pvalue)

  logpvalue.col <- RColorBrewer::brewer.pal(n = 8, name = "Reds")

  circlize::colorRamp2(
    breaks = c(0, 2, 4, 6, 8, 10, 15, 20),
    colors = logpvalue.col
  )

  BgGene.col <- logpvalue.col[
    base::cut(
      base::pmin(logpvalue, 20),
      breaks = c(0, 2, 4, 6, 8, 10, 15, 20, Inf),
      include.lowest = TRUE,
      labels = FALSE
    )
  ]

  df_circos <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = base::max(BgGene),
    stringsAsFactors = FALSE
  )
  base::rownames(df_circos) <- df_circos$GO

  bed2 <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = BgGene,
    label = BgGene,
    col = BgGene.col,
    stringsAsFactors = FALSE
  )

  bed3 <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = Gene,
    label = Gene,
    stringsAsFactors = FALSE
  )

  bed4 <- base::data.frame(
    GO = data$ID,
    start = 1,
    end = base::max(BgGene),
    ratio = ratio / base::max(ratio) * 9.5,
    col = "#00AFBB",
    stringsAsFactors = FALSE
  )

  if (!base::is.null(output_pdf)) {
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
      desc <- data[data$ID == sector.index, "Description"]
      desc <- base::paste(base::strwrap(desc, width = 20), collapse = "\n")
      circlize::circos.text(
        base::mean(xlim), base::mean(ylim),
        desc,
        cex = 0.6,
        facing = "bending.inside",
        niceFacing = TRUE
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
    track.height = 0.10,
    bg.border = "white",
    panel.fun = function(region, value, ...) {
      circlize::circos.genomicRect(
        region, value,
        ytop = 1, ybottom = 0,
        col = value$col, border = NA, ...
      )
      circlize::circos.genomicText(
        region, value,
        y = 0.4,
        labels = value$label,
        adj = 0,
        cex = 0.6,
        ...
      )
    }
  )

  circlize::circos.genomicTrack(
    bed3,
    ylim = c(0, 1),
    track.height = 0.10,
    bg.border = "white",
    panel.fun = function(region, value, ...) {
      circlize::circos.genomicRect(
        region, value,
        ytop = 1, ybottom = 0,
        col = "#BA55D3", border = NA, ...
      )
      circlize::circos.genomicText(
        region, value,
        y = 0.4,
        labels = value$label,
        adj = 0,
        cex = 0.6,
        ...
      )
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
        grid::grid.lines(
          cell.xlim,
          c(y, y),
          gp = grid::gpar(col = "#FFFFFF", lwd = 0.3)
        )
      }
      circlize::circos.genomicRect(
        region, value,
        ytop = value$ratio,
        ybottom = 0,
        col = value$col,
        border = NA,
        ...
      )
    }
  )

  circlize::circos.clear()

  circle_size <- grid::unit(1, "snpc")

  ComplexHeatmap::draw(
    ComplexHeatmap::Legend(
      labels = c("Number of Genes", "Number of Select", "Rich Factor(0-1)"),
      type = "points",
      pch = c(15, 15, 17),
      legend_gp = grid::gpar(col = c("pink", "#BA55D3", "#00AFBB")),
      title = "",
      nrow = 3,
      size = grid::unit(3, "mm")
    ),
    x = circle_size * 0.83,
    y = circle_size * 0.5,
    just = "center"
  )

  ComplexHeatmap::draw(
    ComplexHeatmap::Legend(
      labels = c("(0,2]", "(2,4]", "(4,6]", "(6,8]", "(8,10]", "(10,15]", "(15,20]", ">=20"),
      type = "points",
      pch = 16,
      legend_gp = grid::gpar(col = logpvalue.col),
      title = "-log10(Pvalue)",
      title_position = "topcenter",
      grid_height = grid::unit(5, "mm"),
      grid_width = grid::unit(5, "mm"),
      size = grid::unit(3, "mm")
    ),
    x = circle_size * 1.4,
    y = circle_size * 0.5,
    just = "left"
  )

  if (!base::is.null(output_pdf)) {
    grDevices::dev.off()
  }

  message("GO/KEGG Circos plot finished!")
}


#' @title plot_enrichment_bar
#' @name plot_enrichment_bar
#' @keywords internal
plot_enrichment_bar <- function(enrich_df, top_n = 10, fill_color = "#2c7bb6", title = NULL) {
  if (base::is.null(enrich_df) || !base::is.data.frame(enrich_df) || base::nrow(enrich_df) == 0) {
    return(NULL)
  }

  df <- enrich_df
  if ("p.adjust" %in% base::colnames(df)) {
    df <- df[base::order(df$p.adjust, df$pvalue), , drop = FALSE]
  } else {
    df <- df[base::order(df$pvalue), , drop = FALSE]
  }

  df <- utils::head(df, top_n)
  df$Description <- base::factor(df$Description, levels = base::rev(df$Description))

  ggplot2::ggplot(df, ggplot2::aes(x = Description, y = Count)) +
    ggplot2::geom_col(fill = fill_color) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = title,
      x = NULL,
      y = "Count"
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}


#' @title plot_enrichment_dot
#' @name plot_enrichment_dot
#' @keywords internal
plot_enrichment_dot <- function(enrich_df, top_n = 10, point_color = "#2c7bb6", title = NULL) {
  if (base::is.null(enrich_df) || !base::is.data.frame(enrich_df) || base::nrow(enrich_df) == 0) {
    return(NULL)
  }

  df <- enrich_df
  if ("p.adjust" %in% base::colnames(df)) {
    df <- df[base::order(df$p.adjust, df$pvalue), , drop = FALSE]
  } else {
    df <- df[base::order(df$pvalue), , drop = FALSE]
  }

  df <- utils::head(df, top_n)

  df$GeneRatio_num <- base::vapply(
    base::strsplit(base::as.character(df$GeneRatio), "/"),
    function(x) {
      base::as.numeric(x[1]) / base::as.numeric(x[2])
    },
    numeric(1)
  )

  df$Description <- base::factor(df$Description, levels = base::rev(df$Description))

  ggplot2::ggplot(df, ggplot2::aes(x = GeneRatio_num, y = Description, size = Count)) +
    ggplot2::geom_point(color = point_color) +
    ggplot2::theme_bw() +
    ggplot2::scale_size_continuous(range = c(3, 8)) +
    ggplot2::labs(
      title = title,
      x = "GeneRatio",
      y = NULL,
      size = "Count"
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      plot.title = ggplot2::element_text(hjust = 0.5)
    )
}


#' Enrichment Analysis Module UI
#'
#' This function creates the user interface for the enrichment analysis module.
#'
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing the enrichment analysis interface
#' @import shiny
#' @import bslib
#' @importFrom shinyWidgets switchInput
#' @importFrom colourpicker colourInput
#' @name enrichment_analysis_ui
#' @export
options(shiny.maxRequestSize = 100*1024^2)
enrichment_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,

        shiny::actionButton(
          ns("load_data"),
          "LOAD DATA",
          class = "btn btn-light fw-bold"
        ),

        shiny::uiOutput(ns("load_status_panel")),
        shiny::uiOutput(ns("compare_select_ui")),

        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::fileInput(
            ns("enrichment_analysis_file"),
            "Upload Enrichment Analysis File (.xlsx)",
            accept = c(".xlsx"),
            buttonLabel = "Browse..."
          ),
          shiny::actionButton(
            ns("check_file"),
            "Check File",
            class = "btn btn-success fw-bold mb-2"
          )
        ),

        shiny::hr(),

        shiny::tags$small(
          "The genelist requires an ID column (.xlsx or .csv).",
          style = "color: #6c757d"
        ),

        shinyWidgets::switchInput(
          inputId = ns("input_mode"),
          label = "Input Manually",
          value = TRUE,
          onLabel = "Upload",
          offLabel = "Paste",
          width = "100%"
        ),

        shiny::conditionalPanel(
          condition = base::paste0("input['", ns("input_mode"), "'] == true"),
          shiny::tags$small("Upload Genelist", style = "color: #6c757d"),
          shiny::fileInput(
            inputId = ns("genelist_file"),
            label = NULL,
            multiple = FALSE,
            accept = c(".csv", ".xlsx")
          )
        ),

        shiny::conditionalPanel(
          condition = base::paste0("input['", ns("input_mode"), "'] == false"),
          shiny::div(
            shiny::tags$small("Paste Genelist", style = "color: #6c757d"),
            shiny::textAreaInput(
              inputId = ns("paste_data"),
              label = NULL,
              placeholder = "Copy and paste Excel data here.",
              rows = 5
            ),
            shiny::actionButton(
              ns("apply_paste"),
              "Apply paste data",
              class = "btn btn-light fw-bold"
            )
          )
        ),

        bslib::accordion(
          bslib::accordion_panel(
            title = "Enrichment analysis",
            icon = enrichment_bubble_icon,

            shiny::selectInput(
              inputId = ns("species"),
              label = "Select taxonomic group:",
              choices = c(
                "Plant" = "Plant",
                "Animals" = "Animals",
                "Bacteria" = "Bacteria",
                "Fungi" = "Fungi",
                "Eukaryotes" = "Eukaryotes",
                "Hsa" = "Hsa"
              ),
              selected = "Plant"
            ),

            shiny::checkboxGroupInput(
              inputId = ns("choices"),
              label = "Please select the analysis content:",
              choices = c(
                "GO" = "go_analysis",
                "KEGG" = "kegg_analysis"
              ),
              selected = c("go_analysis", "kegg_analysis")
            ),

            shiny::actionButton(
              ns("run_enrichment_analysis"),
              "Analysis"
            )
          )
        )
      ),

      bslib::card(
        bslib::card_header("File Check Result"),
        bslib::card_body(
          shiny::textOutput(ns("file_check_result"))
        )
      ),

      bslib::layout_column_wrap(
        width = 1 / 2,
        height = 600,

        bslib::card(
          height = "800px",
          bslib::card_header("GO Enrichment Analysis"),
          bslib::card_body(
            shiny::tabsetPanel(
              id = ns("go_tabs"),
              type = "tabs",

              shiny::tabPanel(
                "Visualization",
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    width = 250,
                    position = "left",
                    open = "open",

                    shiny::selectInput(
                      ns("go_plot_type"),
                      "Select plot type:",
                      choices = c(
                        "Bar plot" = "bar",
                        "Dot plot" = "dot",
                        "Circle plot" = "circle"
                      ),
                      selected = "bar"
                    ),

                    shiny::sliderInput(
                      ns("go_top_n"),
                      "Top N terms:",
                      min = 5,
                      max = 20,
                      value = 10
                    ),

                    colourpicker::colourInput(
                      ns("go_color"),
                      "Select color:",
                      value = "#2c7bb6"
                    ),

                    shiny::numericInput(
                      ns("go_width"),
                      "Plot width (inch)",
                      value = 8,
                      min = 4,
                      max = 20
                    ),

                    shiny::numericInput(
                      ns("go_height"),
                      "Plot height (inch)",
                      value = 6,
                      min = 4,
                      max = 20
                    ),

                    shiny::downloadButton(
                      ns("download_go_plot"),
                      "Download Plot (PDF)"
                    ),

                    shiny::downloadButton(
                      ns("download_go_table"),
                      "Download Table (CSV)"
                    )
                  ),

                  bslib::card_body(
                    shiny::plotOutput(ns("go_plot"))
                  )
                )
              ),

              shiny::tabPanel(
                "Result Table",
                DT::DTOutput(ns("go_res_table"))
              )
            )
          )
        ),

        bslib::card(
          height = "800px",
          bslib::card_header("KEGG Enrichment Analysis"),
          bslib::card_body(
            shiny::tabsetPanel(
              id = ns("kegg_tabs"),
              type = "tabs",

              shiny::tabPanel(
                "Visualization",
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    width = 250,
                    position = "left",
                    open = "open",

                    shiny::selectInput(
                      ns("kegg_plot_type"),
                      "Select plot type:",
                      choices = c(
                        "Bar plot" = "bar",
                        "Dot plot" = "dot",
                        "Circle plot" = "circle"
                      ),
                      selected = "bar"
                    ),

                    shiny::sliderInput(
                      ns("kegg_top_n"),
                      "Top N pathways:",
                      min = 5,
                      max = 20,
                      value = 10
                    ),

                    colourpicker::colourInput(
                      ns("kegg_color"),
                      "Select color:",
                      value = "#d7191c"
                    ),

                    shiny::numericInput(
                      ns("kegg_width"),
                      "Plot width (inch)",
                      value = 8,
                      min = 4,
                      max = 20
                    ),

                    shiny::numericInput(
                      ns("kegg_height"),
                      "Plot height (inch)",
                      value = 6,
                      min = 4,
                      max = 20
                    ),

                    shiny::downloadButton(
                      ns("download_kegg_plot"),
                      "Download Plot (PDF)"
                    ),

                    shiny::downloadButton(
                      ns("download_kegg_table"),
                      "Download Table (CSV)"
                    )
                  ),

                  bslib::card_body(
                    shiny::plotOutput(ns("kegg_plot"))
                  )
                )
              ),

              shiny::tabPanel(
                "Result Table",
                DT::DTOutput(ns("kegg_res_table"))
              )
            )
          )
        )
      )
    )
  )
}


utils::globalVariables(c("regulation", "V3", "Pathway_ID", "TERM", "GENE", "NAME"))

# Store the complete, validated enrichment workbook in the project object.
# The two sheets remain separate so a saved ProtVis_dataset can be reopened
# and used for GO/KEGG analysis without re-uploading the workbook.
.protvis_add_enrichment_background <- function(dataset, background, file_name) {
  dataset <- as_protvis_dataset(dataset)
  required <- c("GO_background", "KEGG_background")
  if (!is.list(background) || !all(required %in% names(background)) ||
      !all(vapply(background[required], is.data.frame, logical(1)))) {
    stop("background must contain GO_background and KEGG_background data.frames.",
         call. = FALSE)
  }

  dataset <- .protvis_new_analysis_dataset(
    dataset,
    "enrichment_background",
    parameters = list(
      file_name = as.character(file_name),
      worksheets = required
    )
  )
  dataset$other_files$enrichment_background <- list(
    file_name = as.character(file_name),
    uploaded_at = as.character(Sys.time()),
    sheets = background[required]
  )
  dataset <- .protvis_append_process(
    dataset,
    "enrichment_background",
    status = "success",
    parameters = dataset$process_info$parameters$enrichment_background,
    message = paste0("Stored enrichment background workbook: ", file_name)
  )
  validate_protvis_dataset(dataset)
  dataset
}


#' Enrichment Analysis Module Server
#'
#' This function provides the server-side logic for the enrichment analysis module.
#'
#' @param id The namespace identifier for the module
#' @param shared_state A reactive values list for sharing state between modules
#' @return A module server function
#' @import shiny
#' @name enrichment_analysis_server
#' @export
enrichment_analysis_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- shiny::reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      compare_data = NULL,
      dep_results = base::list(),
      file_check_msg = NULL,
      background_data = NULL,
      go_res = NULL,
      kegg_res = NULL,
      pasted_genelist = NULL
    )

    get_result_df <- function(enrich_obj) {
      if (base::is.null(enrich_obj)) {
        return(NULL)
      }

      res <- tryCatch(
        enrich_obj@result,
        error = function(e) NULL
      )

      if (!base::is.null(res) && base::nrow(res) > 0) {
        return(base::as.data.frame(res))
      }

      NULL
    }

    normalize_dep_results <- function(dep_obj) {
      if (base::is.null(dep_obj) || !base::is.list(dep_obj) ||
          base::length(dep_obj) == 0L) {
        return(NULL)
      }
      dep_obj <- dep_obj[!vapply(dep_obj, is.null, logical(1))]
      if (base::length(dep_obj) == 0L) return(NULL)
      dep_obj
    }

    shiny::observeEvent(input$load_data, {
      dep_obj <- normalize_dep_results(shared_state$dep_results)
      source_label <- "current DEP results"

      # ProtVis_dataset is the canonical source for saved analyses.
      if (base::is.null(dep_obj) && inherits(shared_state$dataset, "ProtVis_dataset")) {
        stored <- shared_state$dataset@analysis_results$differential_analysis
        if (base::is.list(stored) && base::is.list(stored$comparisons)) {
          dep_obj <- normalize_dep_results(stored$comparisons)
          if (!base::is.null(dep_obj)) source_label <- "ProtVis_dataset"
        }
        if (base::is.null(dep_obj) && base::is.list(stored) && base::is.data.frame(stored$table)) {
          tab <- stored$table
          id_col <- if ("protein_id" %in% names(tab)) "protein_id" else if ("ID" %in% names(tab)) "ID" else NULL
          if (!base::is.null(id_col)) {
            logfc_col <- if ("log2FC" %in% names(tab)) "log2FC" else if ("logFC" %in% names(tab)) "logFC" else NULL
            p_col <- if ("p_value" %in% names(tab)) "p_value" else if ("P.Value" %in% names(tab)) "P.Value" else NULL
            if (!base::is.null(logfc_col) && !base::is.null(p_col)) {
              tab$ID <- base::as.character(tab[[id_col]])
              tab$logFC <- base::as.numeric(tab[[logfc_col]])
              tab$P.Value <- base::as.numeric(tab[[p_col]])
              significant <- if ("significant" %in% names(tab)) {
                !is.na(tab$significant) & base::as.logical(tab$significant)
              } else {
                rep(TRUE, base::nrow(tab))
              }
              tab$regulation <- ifelse(
                significant & tab$logFC > 0, "Upregulated",
                ifelse(significant & tab$logFC < 0, "Downregulated", "Not significant")
              )
              comparison <- base::paste(stored$group1 %||% "Group1", "vs", stored$group2 %||% "Group2")
              dep_obj <- base::list()
              dep_obj[[comparison]] <- tab
              source_label <- "ProtVis_dataset"
            }
          }
        }
      }

      # Legacy compatibility: load Step7 only when it actually exists.
      if (base::is.null(dep_obj) && !base::is.null(shared_state$workdir)) {
        rda_path <- base::file.path(shared_state$workdir, "Step7_DEP_result.rda")
        if (base::file.exists(rda_path)) {
          e <- base::new.env()
          loaded <- tryCatch({ base::load(rda_path, envir = e); TRUE }, error = function(e) FALSE)
          if (isTRUE(loaded) && base::exists("dep_results2", envir = e, inherits = FALSE)) {
            dep_obj <- normalize_dep_results(base::get("dep_results2", envir = e, inherits = FALSE))
            source_label <- "Step7_DEP_result.rda"
          }
        }
      }

      if (!base::is.null(dep_obj)) {
        rv$dep_results <- dep_obj
        rv$load_success <- TRUE

        dep_names <- base::names(rv$dep_results)

        if (!base::is.null(dep_names) && base::length(dep_names) > 0) {
          rv$compare_data <- rv$dep_results[[dep_names[1]]]
        } else {
          rv$compare_data <- NULL
        }

        shiny::showNotification(base::paste("Data loaded successfully from", source_label, "."), type = "message")
      } else {
        rv$dep_results <- base::list()
        rv$compare_data <- NULL
        rv$load_success <- FALSE

        shiny::showNotification(
          "No DEP results are available. Run DEP analysis first or load a ProtVis_dataset containing differential analysis results.",
          type = "warning"
        )
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (base::isTRUE(rv$load_success)) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })

    output$compare_select_ui <- shiny::renderUI({
      if (!base::isTRUE(rv$load_success) || base::length(rv$dep_results) == 0) {
        return(NULL)
      }

      choices <- base::names(rv$dep_results)

      if (base::is.null(choices) || base::length(choices) == 0) {
        choices <- base::paste0("Comparison_", base::seq_along(rv$dep_results))
      }

      shiny::selectInput(
        ns("dep_compare"),
        label = "Select DEP comparison",
        choices = choices,
        selected = choices[1]
      )
    })

    shiny::observeEvent(input$dep_compare, {
      shiny::req(rv$dep_results)

      dep_names <- base::names(rv$dep_results)

      if (base::is.null(dep_names) || base::length(dep_names) == 0) {
        return()
      }

      if (!base::is.null(input$dep_compare) && input$dep_compare %in% dep_names) {
        rv$compare_data <- rv$dep_results[[input$dep_compare]]
      }
    }, ignoreNULL = FALSE)

    shiny::observeEvent(input$apply_paste, {
      if (base::is.null(input$paste_data) || base::nchar(base::trimws(input$paste_data)) == 0) {
        rv$pasted_genelist <- NULL
        shiny::showNotification("Paste data is empty.", type = "warning")
        return()
      }

      df <- tryCatch(
        utils::read.table(
          text = input$paste_data,
          header = TRUE,
          sep = "\t",
          stringsAsFactors = FALSE,
          check.names = FALSE,
          quote = "",
          comment.char = ""
        ),
        error = function(e) NULL
      )

      if (base::is.null(df) || !"ID" %in% base::colnames(df)) {
        rv$pasted_genelist <- NULL
        shiny::showNotification("Paste data must contain an ID column.", type = "error")
        return()
      }

      ids <- base::as.character(df$ID)
      ids <- ids[!base::is.na(ids) & base::nzchar(ids)]

      if (base::length(ids) == 0) {
        rv$pasted_genelist <- NULL
        shiny::showNotification("No valid IDs found in pasted data.", type = "error")
        return()
      }

      rv$pasted_genelist <- base::unique(ids)
      shiny::showNotification("Paste data applied successfully.", type = "message")
    })

    genelist <- shiny::reactive({
      if (!base::is.null(rv$compare_data) && base::is.data.frame(rv$compare_data)) {
        if (base::all(c("regulation", "ID") %in% base::colnames(rv$compare_data))) {
          genes <- rv$compare_data %>%
            dplyr::filter(regulation != "Not significant") %>%
            dplyr::pull(ID)

          genes <- base::unique(base::as.character(genes))
          genes <- genes[!base::is.na(genes) & base::nzchar(genes)]

          if (base::length(genes) > 0) {
            return(genes)
          }
        }
      }

      if (!base::is.null(input$genelist_file)) {
        ext <- base::tolower(tools::file_ext(input$genelist_file$name))

        df <- tryCatch(
          {
            if (ext == "csv") {
              utils::read.csv(
                input$genelist_file$datapath,
                stringsAsFactors = FALSE,
                check.names = FALSE
              )
            } else if (ext == "xlsx") {
              readxl::read_excel(input$genelist_file$datapath)
            } else {
              NULL
            }
          },
          error = function(e) NULL
        )

        if (!base::is.null(df) && "ID" %in% base::colnames(df)) {
          genes <- base::unique(base::as.character(df$ID))
          genes <- genes[!base::is.na(genes) & base::nzchar(genes)]

          if (base::length(genes) > 0) {
            return(genes)
          }
        }
      }

      if (!base::is.null(rv$pasted_genelist) && base::length(rv$pasted_genelist) > 0) {
        return(rv$pasted_genelist)
      }

      NULL
    })

    shiny::observeEvent(input$check_file, {
      shiny::req(input$enrichment_analysis_file)

      ext <- base::tolower(tools::file_ext(input$enrichment_analysis_file$name))
      if (ext != "xlsx") {
        rv$file_check_msg <- "❌ Background file must be an .xlsx file."
        rv$background_data <- NULL
        return()
      }

      file <- input$enrichment_analysis_file$datapath

      sheets <- tryCatch(
        readxl::excel_sheets(file),
        error = function(e) NULL
      )

      if (base::is.null(sheets)) {
        rv$file_check_msg <- "❌ Failed to read the Excel file."
        rv$background_data <- NULL
        return()
      }

      required_sheets <- c("GO_background", "KEGG_background")
      if (!base::all(required_sheets %in% sheets)) {
        rv$file_check_msg <- "❌ Missing required sheets: GO_background and/or KEGG_background."
        rv$background_data <- NULL
        return()
      }

      GO_background <- tryCatch(
        readxl::read_excel(file, sheet = "GO_background"),
        error = function(e) NULL
      )
      KEGG_background <- tryCatch(
        readxl::read_excel(file, sheet = "KEGG_background"),
        error = function(e) NULL
      )

      if (base::is.null(GO_background) || base::is.null(KEGG_background)) {
        rv$file_check_msg <- "❌ Failed to read GO_background or KEGG_background."
        rv$background_data <- NULL
        return()
      }

      GO_background <- base::as.data.frame(GO_background, stringsAsFactors = FALSE)
      KEGG_background <- base::as.data.frame(KEGG_background, stringsAsFactors = FALSE)

      if (!base::all(c("TERM", "GENE", "NAME") %in% base::colnames(GO_background))) {
        rv$file_check_msg <- "❌ GO_background must contain TERM, GENE, and NAME columns."
        rv$background_data <- NULL
        return()
      }

      if (!base::all(c("TERM", "GENE", "NAME") %in% base::colnames(KEGG_background))) {
        rv$file_check_msg <- "❌ KEGG_background must contain TERM, GENE, and NAME columns."
        rv$background_data <- NULL
        return()
      }

      rv$background_data <- base::list(
        GO_background = GO_background,
        KEGG_background = KEGG_background
      )

      # Preserve a complete copy of both validated worksheets in the active
      # ProtVis_dataset.  If the workbook is selected before Project init, it
      # is retained and attached as soon as the project object is created.
      shared_state$pending_enrichment_background <- rv$background_data
      shared_state$pending_enrichment_background_name <- input$enrichment_analysis_file$name
      if (inherits(shared_state$dataset, "ProtVis_dataset")) {
        saved <- tryCatch({
          dataset <- .protvis_add_enrichment_background(
            shared_state$dataset,
            rv$background_data,
            input$enrichment_analysis_file$name
          )
          if (!base::is.null(shared_state$workdir) &&
              base::dir.exists(shared_state$workdir)) {
            dataset <- protvis_auto_export_dataset(
              dataset, directory = shared_state$workdir
            )
          }
          .protvis_ui_sync_state(dataset, shared_state)
          TRUE
        }, error = function(e) {
          rv$file_check_msg <- paste("❌ Background valid but could not be saved:", e$message)
          FALSE
        })
        if (!saved) return()
        rv$file_check_msg <- "✅ Background file valid and saved to ProtVis_dataset."
      } else {
        rv$file_check_msg <- paste(
          "✅ Background file valid. It will be added to ProtVis_dataset when Project init is completed."
        )
      }
    })

    output$file_check_result <- shiny::renderText({
      rv$file_check_msg
    })

    selected_kegg_background <- shiny::reactive({
      shiny::req(input$species)
      shiny::req(rv$background_data)

      background_data <- switch(
        input$species,
        "Plant" = ProtVisDatabase::Plant_KEGG_Background,
        "Animals" = ProtVisDatabase::Animals_KEGG_Background,
        "Bacteria" = ProtVisDatabase::Bacteria_KEGG_Background,
        "Fungi" = ProtVisDatabase::Fungi_KEGG_Background,
        "Eukaryotes" = ProtVisDatabase::Eukaryotes_KEGG_Background,
        "Hsa" = ProtVisDatabase::hsa_KEGG_Background,
        NULL
      )

      shiny::req(background_data)

      bg_df <- tryCatch(
        base::as.data.frame(background_data, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      shiny::req(bg_df)

      if (!"V3" %in% base::colnames(bg_df)) {
        return(rv$background_data$KEGG_background)
      }

      map_id <- bg_df %>%
        tidyr::separate(
          col = V3,
          into = c("Pathway_ID", "Pathway_Name"),
          sep = " ",
          extra = "merge",
          fill = "right"
        ) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_trim)) %>%
        dplyr::pull(Pathway_ID)

      map_id <- base::unique(map_id)
      map_id <- map_id[!base::is.na(map_id) & base::nzchar(map_id)]
      map_id <- base::paste0("map", map_id)

      filtered_bg <- rv$background_data$KEGG_background %>%
        dplyr::filter(TERM %in% map_id)

      if (base::nrow(filtered_bg) == 0) {
        return(rv$background_data$KEGG_background)
      }

      filtered_bg
    })

    shiny::observeEvent(input$run_enrichment_analysis, {
      if (base::is.null(genelist()) || base::length(genelist()) == 0) {
        shiny::showNotification("No valid genelist found.", type = "error")
        return()
      }

      if (base::is.null(rv$background_data)) {
        shiny::showNotification("Please check and load the background file first.", type = "error")
        return()
      }

      rv$go_res <- NULL
      rv$kegg_res <- NULL

      if ("go_analysis" %in% input$choices) {
        t2g.go <- rv$background_data$GO_background %>%
          dplyr::select(TERM, GENE)

        t2n.go <- rv$background_data$GO_background %>%
          dplyr::select(TERM, NAME)

        rv$go_res <- tryCatch(
          clusterProfiler::enricher(
            gene = genelist(),
            TERM2GENE = t2g.go,
            TERM2NAME = t2n.go,
            pvalueCutoff = 1,
            qvalueCutoff = 1
          ),
          error = function(e) NULL
        )
      }

      if ("kegg_analysis" %in% input$choices) {
        filtered_bg <- selected_kegg_background()

        t2g.kegg <- filtered_bg %>%
          dplyr::select(TERM, GENE)

        t2n.kegg <- filtered_bg %>%
          dplyr::select(TERM, NAME)

        rv$kegg_res <- tryCatch(
          clusterProfiler::enricher(
            gene = genelist(),
            TERM2GENE = t2g.kegg,
            TERM2NAME = t2n.kegg,
            pvalueCutoff = 1,
            qvalueCutoff = 1
          ),
          error = function(e) NULL
        )
      }

      shiny::showNotification("Enrichment analysis completed.", type = "message")
    })

    output$go_plot <- shiny::renderPlot({
      go_df <- get_result_df(rv$go_res)

      if (base::is.null(go_df) || base::nrow(go_df) == 0) {
        return(invisible(NULL))
      }

      if (input$go_plot_type == "bar") {
        p <- plot_enrichment_bar(
          enrich_df = go_df,
          top_n = input$go_top_n,
          fill_color = input$go_color,
          title = "GO Enrichment"
        )
        print(p)
      } else if (input$go_plot_type == "dot") {
        p <- plot_enrichment_dot(
          enrich_df = go_df,
          top_n = input$go_top_n,
          point_color = input$go_color,
          title = "GO Enrichment"
        )
        print(p)
      } else {
        plot_go_circos(
          go_data = go_df,
          top_n = input$go_top_n
        )
      }
    })

    output$kegg_plot <- shiny::renderPlot({
      kegg_df <- get_result_df(rv$kegg_res)

      if (base::is.null(kegg_df) || base::nrow(kegg_df) == 0) {
        return(invisible(NULL))
      }

      if (input$kegg_plot_type == "bar") {
        p <- plot_enrichment_bar(
          enrich_df = kegg_df,
          top_n = input$kegg_top_n,
          fill_color = input$kegg_color,
          title = "KEGG Enrichment"
        )
        print(p)
      } else if (input$kegg_plot_type == "dot") {
        p <- plot_enrichment_dot(
          enrich_df = kegg_df,
          top_n = input$kegg_top_n,
          point_color = input$kegg_color,
          title = "KEGG Enrichment"
        )
        print(p)
      } else {
        plot_go_circos(
          go_data = kegg_df,
          top_n = input$kegg_top_n
        )
      }
    })

    output$go_res_table <- DT::renderDT({
      go_df <- get_result_df(rv$go_res)

      if (base::is.null(go_df) || base::nrow(go_df) == 0) {
        return(NULL)
      }

      DT::datatable(
        go_df,
        options = base::list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$kegg_res_table <- DT::renderDT({
      kegg_df <- get_result_df(rv$kegg_res)

      if (base::is.null(kegg_df) || base::nrow(kegg_df) == 0) {
        return(NULL)
      }

      DT::datatable(
        kegg_df,
        options = base::list(pageLength = 10, scrollX = TRUE)
      )
    })

    output$download_go_plot <- shiny::downloadHandler(
      filename = function() {
        base::paste0("GO_enrichment_plot_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        go_df <- get_result_df(rv$go_res)
        shiny::req(go_df)

        if (input$go_plot_type == "circle") {
          plot_go_circos(
            go_data = go_df,
            top_n = input$go_top_n,
            output_pdf = file
          )
          return()
        }

        grDevices::pdf(file, width = input$go_width, height = input$go_height)

        if (input$go_plot_type == "bar") {
          p <- plot_enrichment_bar(
            enrich_df = go_df,
            top_n = input$go_top_n,
            fill_color = input$go_color,
            title = "GO Enrichment"
          )
          print(p)
        } else if (input$go_plot_type == "dot") {
          p <- plot_enrichment_dot(
            enrich_df = go_df,
            top_n = input$go_top_n,
            point_color = input$go_color,
            title = "GO Enrichment"
          )
          print(p)
        }

        grDevices::dev.off()
      }
    )

    output$download_go_table <- shiny::downloadHandler(
      filename = function() {
        base::paste0("GO_enrichment_table_", base::Sys.Date(), ".csv")
      },
      content = function(file) {
        go_df <- get_result_df(rv$go_res)
        shiny::req(go_df)
        utils::write.csv(go_df, file, row.names = FALSE)
      }
    )

    output$download_kegg_plot <- shiny::downloadHandler(
      filename = function() {
        base::paste0("KEGG_enrichment_plot_", base::Sys.Date(), ".pdf")
      },
      content = function(file) {
        kegg_df <- get_result_df(rv$kegg_res)
        shiny::req(kegg_df)

        if (input$kegg_plot_type == "circle") {
          plot_go_circos(
            go_data = kegg_df,
            top_n = input$kegg_top_n,
            output_pdf = file
          )
          return()
        }

        grDevices::pdf(file, width = input$kegg_width, height = input$kegg_height)

        if (input$kegg_plot_type == "bar") {
          p <- plot_enrichment_bar(
            enrich_df = kegg_df,
            top_n = input$kegg_top_n,
            fill_color = input$kegg_color,
            title = "KEGG Enrichment"
          )
          print(p)
        } else if (input$kegg_plot_type == "dot") {
          p <- plot_enrichment_dot(
            enrich_df = kegg_df,
            top_n = input$kegg_top_n,
            point_color = input$kegg_color,
            title = "KEGG Enrichment"
          )
          print(p)
        }

        grDevices::dev.off()
      }
    )

    output$download_kegg_table <- shiny::downloadHandler(
      filename = function() {
        base::paste0("KEGG_enrichment_table_", base::Sys.Date(), ".csv")
      },
      content = function(file) {
        kegg_df <- get_result_df(rv$kegg_res)
        shiny::req(kegg_df)
        utils::write.csv(kegg_df, file, row.names = FALSE)
      }
    )
  })
}
