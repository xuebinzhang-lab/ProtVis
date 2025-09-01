options(shiny.maxRequestSize = 800*1024^2)

# plot_go_circos ----------------------------------------------------------

# =========================
# GO Circos Plot Function
# =========================
plot_go_circos <- function(go_data, top_n = 15, output_pdf = NULL) {

  # 按 pvalue 排序并筛选显著项
  data <- go_data[order(go_data$pvalue), ]
  datasig <- data[data$pvalue < 0.05, , drop = FALSE]
  data <- head(datasig, top_n)

  if (nrow(data) == 0) {
    message("No significant GO terms to plot.")
    return(NULL)
  }

  # 计算基因数
  BgGene <- as.numeric(sapply(strsplit(data$BgRatio, "/"), `[`, 1))
  Gene <- as.numeric(sapply(strsplit(data$GeneRatio, "/"), `[`, 1))

  # 富集因子和 -log10(pvalue)
  ratio <- Gene / BgGene
  logpvalue <- -log10(data$pvalue)

  # 颜色映射
  logpvalue.col <- RColorBrewer::brewer.pal(n = 8, name = "Reds")
  f <- circlize::colorRamp2(
    breaks = c(0, 2, 4, 6, 8, 10, 15, 20),
    colors = logpvalue.col
  )
  BgGene.col <- f(pmin(logpvalue, 20))

  # circos 数据
  df_circos <- data.frame(
    GO = data$ID,
    start = 1,
    end = max(BgGene)
  )
  rownames(df_circos) <- df_circos$GO

  bed2 <- data.frame(
    GO = data$ID,
    start = 1,
    end = BgGene,
    label = BgGene,
    col = BgGene.col
  )

  bed3 <- data.frame(
    GO = data$ID,
    start = 1,
    end = Gene,
    label = Gene
  )

  bed4 <- data.frame(
    GO = data$ID,
    start = 1,
    end = max(BgGene),
    ratio = ratio / max(ratio) * 9.5,   # 标准化到 0-10
    col = "#00AFBB"
  )

  # PDF 输出可选
  if (!is.null(output_pdf)) {
    grDevices::pdf(output_pdf, width = 10, height = 6)
  }

  # 清除旧 circos 图
  circlize::circos.clear()
  circlize::circos.genomicInitialize(df_circos, plotType = "none")

  # 轨道 1: GO term 标签
  circlize::circos.trackPlotRegion(
    ylim = c(0, 1),
    panel.fun = function(x, y) {
      sector.index <- circlize::get.cell.meta.data("sector.index")
      xlim <- circlize::get.cell.meta.data("xlim")
      ylim <- circlize::get.cell.meta.data("ylim")
      desc <- data[data$ID == sector.index, ]$Description
      desc <- paste(base::strwrap(desc, width = 20), collapse = "\n")
      circlize::circos.text(
        mean(xlim), mean(ylim),
        desc, cex = 0.6,
        facing = "bending.inside", niceFacing = TRUE
      )
    },
    track.height = 0.12,
    bg.border = NA,
    bg.col = "grey95"
  )

  # 添加轴标签
  for (si in circlize::get.all.sector.index()) {
    circlize::circos.axis(
      h = "top",
      labels.cex = 0.5,
      sector.index = si,
      track.index = 1,
      major.at = seq(0, max(BgGene), by = 100),
      labels.facing = "clockwise"
    )
  }

  # 轨道 2: 背景基因数
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

  # 轨道 3: 差异基因数
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

  # 轨道 4: 富集因子
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

  # 绘制图例
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
#'
#' This function creates the user interface for the enrichment analysis module.
#' It includes file uploads, parameter settings, and visualization panels for GO and KEGG enrichment analysis.
#'
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing the enrichment analysis interface
#' @export
#'
enrichment_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        uiOutput(ns("load_status_panel")),
        uiOutput(ns("compare_select_ui")),
        div(style = "margin-bottom: 15px;",
            fileInput(ns("enrichment_analysis_file"), "Upload Enrichment Analysis File (Created by Toolkits > Background Make)",
                      accept = c(".csv", ".xlsx"),
                      buttonLabel = "Browse..."),
            actionButton(ns("check_file"), "Check File",
                         class = "btn btn-success fw-bold mb-2")
        ),
        hr(),
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
        conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == true"),
          tags$small('Upload Genelist', style = "color: #6c757d"),
          fileInput(
            inputId = ns('genelist_file'),
            label = NULL,
            multiple = FALSE,
            accept = c('.csv','.xlsx')
          )
        ),

        # Conditional panel: Manual input mode
        conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == false"),
          div(
            # tags$small("Edit Genelist", style = "color: #6c757d"),
            # rhandsontable::rHandsontableOutput(ns("hot_compare")),
            # br(),
            tags$small("Paste Genelist", style = "color: #6c757d"),
            textAreaInput(
              inputId = ns("paste_data"),
              label = NULL,
              placeholder = "Copy and paste Excel data here.",
              rows = 5
            ),
            actionButton(ns("apply_paste"), "Apply paste data",
                         class = "btn btn-light fw-bold")
          )
        ),
        accordion(
          accordion_panel(
            title = "Enrichment analysis",
            icon = enrichment_bubble_icon,
            checkboxGroupInput(
              inputId = ns("choices"),
              label = "Please select the analysis content:",
              choices = c("GO" = "go_analysis",
                          "KEGG" = "kegg_analysis"),
              selected = c("go_analysis","kegg_analysis")
            ),
            actionButton(ns("run_enrichment_analysis"), "Analysis")
          )
        )
      ),
      page_fluid(
        card(
          card_header("File Check Result"),
          card_body(
            textOutput(ns("file_check_result"))
          )
        ),

        layout_column_wrap(
          width = 1/2,
          height = 600,

          # === GO enrichment card ===
          card(
            height = "800px",
            card_header("GO Enrichment Analysis"),
            card_body(
              tabsetPanel(
                id = ns("go_tabs"),
                type = "tabs",
                tabPanel("Visualization",
                         layout_sidebar(
                           sidebar = sidebar(
                             width = 250,
                             position = "left",
                             open = "closed",
                             selectInput(ns("go_plot_type"), "Select plot type:",
                                         choices = c("Bar plot" = "bar",
                                                     "Dot plot" = "dot",
                                                     "Circle plot" = "circle"),
                                         selected = "bar"),
                             sliderInput(ns("go_top_n"), "Top N terms:",
                                         min = 5, max = 20, value = 10),
                             colourpicker::colourInput(ns("go_color"), "Select color:", value = "#2c7bb6"),
                             numericInput(ns("go_width"), "Plot width (inch)", value = 8, min = 4, max = 20),
                             numericInput(ns("go_height"), "Plot height (inch)", value = 6, min = 4, max = 20),
                             downloadButton(ns("download_go_plot"), "Download Plot (PDF)"),
                             downloadButton(ns("download_go_table"), "Download Table (CSV)")
                           ),
                           card_body(
                             plotOutput(ns("go_plot"))
                           )
                         )
                ),
                tabPanel("Result Table", DTOutput(ns("go_res_table")))
              )
            )
          ),

          # === KEGG enrichment card ===
          card(
            height = "800px",
            card_header("KEGG Enrichment Analysis"),
            card_body(
              tabsetPanel(
                id = ns("kegg_tabs"),
                type = "tabs",
                tabPanel("Visualization",
                         layout_sidebar(
                           sidebar = sidebar(
                             width = 250,
                             position = "left",
                             open = "closed",
                             selectInput(ns("kegg_plot_type"), "Select plot type:",
                                         choices = c("Bar plot" = "bar",
                                                     "Dot plot" = "dot",
                                                     "Circle plot" = "circle"),
                                         selected = "bar"),
                             sliderInput(ns("kegg_top_n"), "Top N pathways:",
                                         min = 5, max = 20, value = 10),
                             colourpicker::colourInput(ns("kegg_color"), "Select color:", value = "#d7191c"),
                             numericInput(ns("kegg_width"), "Plot width (inch)", value = 8, min = 4, max = 20),
                             numericInput(ns("kegg_height"), "Plot height (inch)", value = 6, min = 4, max = 20),
                             downloadButton(ns("download_kegg_plot"), "Download Plot (PDF)"),
                             downloadButton(ns("download_kegg_table"), "Download Table (CSV)")
                           ),
                           card_body(
                             plotOutput(ns("kegg_plot"))
                           )
                         )
                ),
                tabPanel("Result Table", DTOutput(ns("kegg_res_table")))
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
#' @export
#'

enrichment_analysis_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      compare_data = NULL,
      input_mode = TRUE,
      dep_results = list(),
      file_check_msg = NULL,
      background_data = NULL,
      go_res = NULL,
      kegg_res = NULL
    )

    # 内置空表
    template_df <- reactive({
      data.frame(
        ID = c(NA, NA, NA),
        stringsAsFactors = FALSE
      )
    })

    # ============ 数据加载 (完全保留原有内容) ============
    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step7_DEP_result.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        if (exists("dep_results2", envir = e)) {
          rv$dep_results <- e$dep_results2
          updateSelectInput(session, "dep_compare", choices = names(rv$dep_results))
        } else {
          rv$dep_results <- NULL
          showNotification("Step7_DEP_result.rda does not contain dep_results2.",
                           type = "warning")
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
      }
    })

    # 显示加载状态
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # 下拉菜单 UI
    output$compare_select_ui <- renderUI({
      req(rv$load_success)
      selectInput(ns("dep_compare"),
                  label = "Select DEP comparison",
                  choices = names(rv$dep_results),
                  selected = names(rv$dep_results)[1])
    })

    observeEvent(input$dep_compare, {
      req(rv$dep_results)
      rv$compare_data <- rv$dep_results[[input$dep_compare]]
    })

    # ============ 基因列表处理 ============
    genelist <- reactive({
      # 从 DEP 结果获取
      if (!is.null(rv$compare_data)) {
        genes <- rv$compare_data %>%
          dplyr::filter(regulation != "Not significant") %>%
          dplyr::pull(ID)
        return(unique(genes))
      }

      # 上传基因列表
      if (!is.null(input$genelist_file)) {
        ext <- tools::file_ext(input$genelist_file$name)
        if (ext == "csv") {
          df <- read.csv(input$genelist_file$datapath)
        } else if (ext == "xlsx") {
          df <- readxl::read_excel(input$genelist_file$datapath)
        }
        if ("ID" %in% colnames(df)) {
          return(unique(df$ID))
        }
      }

      # 粘贴模式
      if (!is.null(input$paste_data) && nchar(input$paste_data) > 0) {
        df <- read.table(text = input$paste_data, header = TRUE, sep = "\t")
        if ("ID" %in% colnames(df)) {
          return(unique(df$ID))
        }
      }

      return(NULL)
    })

    # ============ 背景文件检查 ============
    observeEvent(input$check_file, {
      req(input$enrichment_analysis_file)
      file <- input$enrichment_analysis_file$datapath
      sheets <- readxl::excel_sheets(file)

      if (!all(c("GO_background", "KEGG_background") %in% sheets)) {
        rv$file_check_msg <- "❌ Missing required sheets: GO_background or KEGG_background"
        return()
      }

      GO_background <- readxl::read_excel(file, sheet = "GO_background")
      KEGG_background <- readxl::read_excel(file, sheet = "KEGG_background")

      rv$background_data <- list(GO_background = GO_background,
                                 KEGG_background = KEGG_background)
      rv$file_check_msg <- "✅ Background file valid."
    })

    output$file_check_result <- renderText({
      rv$file_check_msg
    })

    # ============ 富集分析 ============
    observeEvent(input$run_enrichment_analysis, {
      req(genelist(), rv$background_data)

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
        t2g.kegg <- rv$background_data$KEGG_background %>% dplyr::select(TERM,GENE)
        t2n.kegg <- rv$background_data$KEGG_background %>% dplyr::select(TERM,NAME)

        rv$kegg_res <- clusterProfiler::enricher(
          gene = genelist(),
          TERM2GENE = t2g.kegg,
          TERM2NAME = t2n.kegg,
          pvalueCutoff = 1,
          qvalueCutoff = 1
        )
      }

      showNotification("✅ Enrichment analysis completed.", type = "message")
    })

    # ============ 可视化 ============
    output$go_plot <- renderPlot({
      req(rv$go_res)
      if (input$go_plot_type == "bar") {
        barplot(rv$go_res, showCategory = input$go_top_n, fill = input$go_color)
      } else if (input$go_plot_type == "dot") {
        clusterProfiler::dotplot(rv$go_res, showCategory = input$go_top_n) +
          ggplot2::scale_color_manual(values = input$go_color)
      } else {
        plot_go_circos(rv$go_res, top_n = input$go_top_n)
      }
    })


    output$kegg_plot <- renderPlot({
      req(rv$kegg_res)
      if (input$kegg_plot_type == "bar") {
        barplot(rv$kegg_res, showCategory = input$go_top_n, fill = input$go_color)
      } else if (input$kegg_plot_type == "dot") {
        clusterProfiler::dotplot(rv$kegg_res, showCategory = input$go_top_n) +
          ggplot2::scale_color_manual(values = input$go_color)
      } else {
        plot_go_circos(rv$kegg_res, top_n = input$go_top_n)
      }
    })

    # ============ 表格输出 ============
    output$go_res_table <- renderDT({
      req(rv$go_res)
      as.data.frame(rv$go_res@result)
    }, options = list(pageLength = 10, scrollX = TRUE))

    output$kegg_res_table <- renderDT({
      req(rv$kegg_res)
      as.data.frame(rv$kegg_res@result)
    }, options = list(pageLength = 10, scrollX = TRUE))

    # === GO plot 下载 ===
    output$download_go_plot <- downloadHandler(
      filename = function() {
        paste0("GO_enrichment_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(rv$go_res)
        pdf(file, width = input$go_width, height = input$go_height)
        if (input$go_plot_type == "dot") {
          print(clusterProfiler::dotplot(rv$go_res, showCategory = input$go_top_n) +
                  ggplot2::scale_color_manual(values = input$go_color))
        } else {
          print(barplot(rv$go_res, showCategory = input$go_top_n, fill = input$go_color))
        }
        dev.off()
      }
    )
    # === GO plot 下载 ===
    output$download_go_plot <- downloadHandler(
      filename = function() {
        paste0("GO_enrichment_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(rv$go_res)
        plot_go_circos(rv$go_res, top_n = input$go_top_n, output_pdf = file)
      }
    )

    # === GO table 下载 ===
    output$download_go_table <- downloadHandler(
      filename = function() {
        paste0("GO_enrichment_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$go_res)
        write.csv(as.data.frame(rv$go_res@result), file, row.names = FALSE)
      }
    )

    # === KEGG plot 下载 ===
    # output$download_kegg_plot <- downloadHandler(
    #   filename = function() {
    #     paste0("KEGG_enrichment_plot_", Sys.Date(), ".pdf")
    #   },
    #   content = function(file) {
    #     req(rv$kegg_res)
    #     pdf(file, width = input$kegg_width, height = input$kegg_height)
    #     if (input$kegg_plot_type == "dot") {
    #       print(clusterProfiler::dotplot(rv$kegg_res, showCategory = input$kegg_top_n) +
    #               ggplot2::scale_color_manual(values = input$kegg_color))
    #     } else {
    #       print(barplot(rv$kegg_res, showCategory = input$kegg_top_n, fill = input$kegg_color))
    #     }
    #     dev.off()
    #   }
    # )
    # === KEGG plot 下载（支持 circlize 圈图） ===
    output$download_kegg_plot <- downloadHandler(
      filename = function() {
        paste0("KEGG_enrichment_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(rv$kegg_res)
        if (input$kegg_plot_type %in% c("bar", "dot")) {
          pdf(file, width = input$kegg_width, height = input$kegg_height)
          if (input$kegg_plot_type == "dot") {
            print(clusterProfiler::dotplot(rv$kegg_res, showCategory = input$kegg_top_n) +
                    ggplot2::scale_color_manual(values = input$kegg_color))
          } else {
            print(barplot(rv$kegg_res, showCategory = input$kegg_top_n, fill = input$kegg_color))
          }
          dev.off()
        } else if (input$kegg_plot_type == "circle") {
          # 使用 GO circos 绘图函数绘制 KEGG
          plot_go_circos(rv$kegg_res, top_n = input$kegg_top_n, output_pdf = file)
        }
      }
    )


    # === KEGG table 下载 ===
    output$download_kegg_table <- downloadHandler(
      filename = function() {
        paste0("KEGG_enrichment_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(rv$kegg_res)
        write.csv(as.data.frame(rv$kegg_res@result), file, row.names = FALSE)
      }
    )


  })
}


