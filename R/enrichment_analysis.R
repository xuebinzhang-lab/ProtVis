options(shiny.maxRequestSize = 500*1024^2)
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
                                         choices = c("Bar plot" = "bar", "Dot plot" = "dot"),
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
                                         choices = c("Bar plot" = "bar", "Dot plot" = "dot"),
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

# enrichment_analysis_server <- function(id, shared_state) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     rv <- reactiveValues(
#       sample_info = NULL,
#       load_success = FALSE,
#       normalized_matrix = NULL,
#       compare_data = NULL,
#       input_mode = TRUE,
#       dep_results = list(),
#       file_check_msg = NULL
#     )
#
#     # 内置空表
#     template_df <- reactive({
#       data.frame(
#         ID = c(NA, NA, NA),
#         stringsAsFactors = FALSE
#       )
#     })
#
#     # 数据加载
#     observeEvent(input$load_data, {
#       req(shared_state$workdir)
#       rda_path <- file.path(shared_state$workdir, "Step7_DEP_result.rda")
#       if (file.exists(rda_path)) {
#         e <- new.env()
#         load(rda_path, envir = e)
#         if (exists("dep_results2", envir = e)) {
#           rv$dep_results <- e$dep_results2
#           updateSelectInput(session, "dep_compare", choices = names(rv$dep_results))
#         } else {
#           rv$dep_results <- NULL
#           showNotification("Step7_DEP_result.rda does not contain dep_results2.",
#                            type = "warning")
#         }
#         rv$load_success <- TRUE
#         showNotification("✅ Data loaded successfully.", type = "message")
#       }
#     })
#
#     # 显示加载状态
#     output$load_status_panel <- renderUI({
#       if (rv$load_success) {
#         span("✅ Data loaded", style = "color: green;")
#       } else {
#         span("❌ Data not loaded", style = "color: red;")
#       }
#     })
#
#     # 下拉菜单 UI
#     output$compare_select_ui <- renderUI({
#       req(rv$load_success)
#       selectInput(ns("dep_compare"),
#                   label = "Select DEP comparison",
#                   choices = names(rv$dep_results),
#                   selected = names(rv$dep_results)[1])
#     })
#
#     observeEvent(input$dep_compare, {
#       req(rv$dep_results)
#       rv$compare_data <- rv$dep_results[[input$dep_compare]]
#     })
#
#     # 检查上传的富集背景文件
#     observeEvent(input$check_file, {
#       req(input$enrichment_analysis_file)
#
#       file <- input$enrichment_analysis_file$datapath
#       ext <- tools::file_ext(file)
#
#       if (ext != "xlsx") {
#         rv$file_check_msg <- "❌ Please upload a .xlsx file (not .csv)"
#       } else {
#         sheets <- readxl::excel_sheets(file)
#         required_sheets <- c("GO_background", "KEGG_background")
#
#         if (!all(required_sheets %in% sheets)) {
#           rv$file_check_msg <- paste0("❌ Missing required sheets. Found: ",
#                                       paste(sheets, collapse = ", "))
#         } else {
#           # 检查每个 sheet 是否包含 GENE TERM NAME
#           check_results <- lapply(required_sheets, function(sh) {
#             df <- readxl::read_excel(file, sheet = sh, n_max = 1)
#             required_cols <- c("GENE", "TERM", "NAME")
#             if (!all(required_cols %in% colnames(df))) {
#               return(paste0("❌ Sheet ", sh, " missing required columns"))
#             }
#             return(paste0("✅ Sheet ", sh, " is valid"))
#           })
#
#           rv$file_check_msg <- paste(check_results, collapse = "\n")
#         }
#       }
#     })
#
#     # 输出检查结果
#     output$file_check_result <- renderText({
#       rv$file_check_msg
#     })
#     # 运行富集分析
#     observeEvent(input$run_enrichment_analysis, {
#
#     })
#
#   })
# }


# -------------------------------------------------------------------------

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
      if (input$go_plot_type == "dot") {
        clusterProfiler::dotplot(rv$go_res, showCategory = input$go_top_n) +
          ggplot2::scale_color_manual(values = input$go_color)
      } else {
        barplot(rv$go_res, showCategory = input$go_top_n, fill = input$go_color)
      }
    })

    output$kegg_plot <- renderPlot({
      req(rv$kegg_res)
      if (input$kegg_plot_type == "dot") {
        clusterProfiler::dotplot(rv$kegg_res, showCategory = input$kegg_top_n) +
          ggplot2::scale_color_manual(values = input$kegg_color)
      } else {
        barplot(rv$kegg_res, showCategory = input$kegg_top_n, fill = input$kegg_color)
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
    output$download_kegg_plot <- downloadHandler(
      filename = function() {
        paste0("KEGG_enrichment_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        req(rv$kegg_res)
        pdf(file, width = input$kegg_width, height = input$kegg_height)
        if (input$kegg_plot_type == "dot") {
          print(clusterProfiler::dotplot(rv$kegg_res, showCategory = input$kegg_top_n) +
                  ggplot2::scale_color_manual(values = input$kegg_color))
        } else {
          print(barplot(rv$kegg_res, showCategory = input$kegg_top_n, fill = input$kegg_color))
        }
        dev.off()
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


