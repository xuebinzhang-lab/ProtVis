protein_fun_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(style = "margin-bottom: 15px;",
            actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold")
        ),
        uiOutput(ns("load_status_panel")),
        # 添加选中的蛋白信息显示
        uiOutput(ns("selected_protein_info")),
        fileInput(ns("fasta_file"), "Upload FASTA File",
                  accept = c(".fa", ".fasta", ".fasta.gz"),
                  buttonLabel = "Browse...")
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 1500,

          card(
            height = "800px",
            card_header("DEP"),
            card_body(
              uiOutput(ns("comparison_select_ui")),
              plotlyOutput(ns("volcano_plot"), height = "600px")
            )
          ),

          card(
            height = "800px",
            card_header("Protein sequence"),
            card_body(
              # 显示蛋白序列
              verbatimTextOutput(ns("protein_sequence")),
              # 下载序列按钮
              downloadButton(ns("download_sequence"), "Download FASTA")
            )
          ),

          card(
            height = "800px",
            card_header("Domain"),
            card_body(
              # 显示结构域信息
              plotOutput(ns("domain_plot"), height = "300px"),
              # 结构域表格
              DT::dataTableOutput(ns("domain_table"))
            )
          ),

          card(
            height = "800px",
            card_header("Protein 3D Structure"),
            card_body(
              # 显示3D结构或相关信息
              uiOutput(ns("structure_display")),
              # 外部数据库链接
              uiOutput(ns("external_links"))
            )
          )
        )
      )
    )
  )
}

protein_fun_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(
      compare_data = NULL,
      dep_results = NULL,
      load_success = FALSE,
      selected_protein = NULL,
      protein_sequences = NULL,
      domain_data = NULL
    )

    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step7_DEP_result.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        if (exists("compare_data2", envir = e)) rv$compare_data <- e$compare_data2
        if (exists("dep_results2", envir = e)) {
          rv$dep_results <- e$dep_results2
        } else {
          rv$dep_results <- NULL
          showNotification("Step7_DEP_result.rda does not exist. Expression matrix cannot be loaded.", type = "warning")
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")

        # 尝试加载蛋白序列和结构域数据
        load_additional_data()
      } else {
        rv$load_success <- FALSE
        showNotification("Step7_DEP_result.rda not found.", type = "error")
      }
    })

    # 加载额外的蛋白数据
    load_additional_data <- function() {
      # 加载蛋白序列数据
      fasta_path <- file.path(shared_state$workdir, "protein_sequences.fasta")
      if (file.exists(fasta_path)) {
        tryCatch({
          if (requireNamespace("Biostrings", quietly = TRUE)) {
            rv$protein_sequences <- Biostrings::readAAStringSet(fasta_path)
          }
        }, error = function(e) {
          message("Could not load protein sequences: ", e$message)
        })
      }

      # 加载结构域数据
      domain_path <- file.path(shared_state$workdir, "domain_data.rda")
      if (file.exists(domain_path)) {
        tryCatch({
          e <- new.env()
          load(domain_path, envir = e)
          if (exists("domain_data", envir = e)) {
            rv$domain_data <- e$domain_data
          }
        }, error = function(e) {
          message("Could not load domain data: ", e$message)
        })
      }
    }

    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        tagList(
          span("✅ Data loaded", style = "color: green;"),
          br(),
          if (!is.null(rv$protein_sequences)) {
            span("✅ Sequences available", style = "color: green;")
          } else {
            span("⚠️ No sequence data", style = "color: orange;")
          }
        )
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # 动态生成比较组选择下拉框
    output$comparison_select_ui <- renderUI({
      req(rv$dep_results)
      comparison_choices <- names(rv$dep_results)
      if (length(comparison_choices) > 0) {
        selectInput(
          ns("comparison_group"),
          "Please select a comparison group:",
          choices = comparison_choices,
          selected = comparison_choices[1]
        )
      } else {
        p("No comparison groups available in the loaded data.")
      }
    })

    # 生成交互式火山图
    output$volcano_plot <- renderPlotly({
      req(input$comparison_group, rv$dep_results)

      dep_data <- rv$dep_results[[input$comparison_group]]

      if (!is.null(dep_data)) {
        if (!is.data.frame(dep_data)) {
          dep_data <- data.frame(dep_data)
        }

        required_cols <- c("logFC", "P.Value", "regulation")
        if (all(required_cols %in% colnames(dep_data))) {

          # 确保有ID列，如果没有则创建行号作为ID
          if (!"ID" %in% colnames(dep_data)) {
            if (!is.null(rownames(dep_data)) && all(rownames(dep_data) != "")) {
              dep_data$ID <- rownames(dep_data)
            } else {
              dep_data$ID <- as.character(1:nrow(dep_data))
            }
          }

          # 添加自定义数据字段用于点击事件
          dep_data$point_index <- 1:nrow(dep_data)

          p <- ggplot(dep_data, aes(x = logFC, y = -log10(P.Value),
                                    color = regulation,
                                    customdata = point_index,  # 添加自定义数据
                                    text = paste("Protein:", ID,
                                                 "<br>logFC:", round(logFC, 3),
                                                 "<br>p-value:", format.pval(P.Value, digits = 3),
                                                 "<br>Regulation:", regulation))) +
            geom_point(alpha = 0.8, size = 2) +
            scale_color_manual(values = c("Upregulated" = "red",
                                          "Downregulated" = "blue",
                                          "Not significant" = "grey")) +
            theme_bw() +
            labs(x = "Log2 Fold Change",
                 y = "-Log10(p-value)",
                 color = "") +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "top") +
            geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
            geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black")

          plotly::ggplotly(p, tooltip = "text", source = "volcano") %>%
            layout(legend = list(orientation = "h", x = 0, y = 1.1))

        } else {
          plotly::plot_ly() %>%
            add_annotations(text = "Required columns (logFC, P.Value, regulation) not found in data",
                            xref = "paper", yref = "paper",
                            x = 0.5, y = 0.5, xanchor = "center", yanchor = "center",
                            showarrow = FALSE)
        }
      }
    })

    # 修复火山图点击事件
    observeEvent(event_data("plotly_click", source = "volcano"), {
      click_data <- event_data("plotly_click", source = "volcano")
      req(click_data, input$comparison_group, rv$dep_results)

      dep_data <- rv$dep_results[[input$comparison_group]]
      if (!is.data.frame(dep_data)) {
        dep_data <- data.frame(dep_data)
      }

      # 方法1：使用customdata（推荐）
      if (!is.null(click_data$customdata)) {
        point_index <- as.numeric(click_data$customdata)
      } else {
        # 方法2：使用pointNumber（备用）
        point_index <- click_data$pointNumber + 1
      }

      # 确保有ID列
      if (!"ID" %in% colnames(dep_data)) {
        if (!is.null(rownames(dep_data)) && all(rownames(dep_data) != "")) {
          dep_data$ID <- rownames(dep_data)
        } else {
          dep_data$ID <- as.character(1:nrow(dep_data))
        }
      }

      # 确保索引有效
      if (point_index <= nrow(dep_data) && point_index > 0) {
        protein_id <- dep_data$ID[point_index]

        rv$selected_protein <- list(
          id = protein_id,
          data = dep_data[point_index, , drop = FALSE]  # 使用drop=FALSE保持数据框结构
        )

        showNotification(paste("Selected protein:", protein_id))
      } else {
        showNotification("Invalid point selection", type = "warning")
      }
    })

    # 显示选中的蛋白信息
    output$selected_protein_info <- renderUI({
      req(rv$selected_protein)

      protein_data <- rv$selected_protein$data
      tagList(
        h4("Selected Protein"),
        p(strong("ID:"), rv$selected_protein$id),
        p(strong("logFC:"), round(protein_data$logFC, 3)),
        p(strong("P.Value:"), format.pval(protein_data$P.Value, digits = 3)),
        p(strong("Regulation:"), protein_data$regulation),
        hr()
      )
    })

    # 显示蛋白序列
    output$protein_sequence <- renderPrint({
      req(rv$selected_protein)

      protein_id <- rv$selected_protein$id

      if (!is.null(rv$protein_sequences)) {
        # 从序列数据中查找
        if (inherits(rv$protein_sequences, "AAStringSet")) {
          sequence <- as.character(rv$protein_sequences[protein_id])
        } else if (is.character(rv$protein_sequences)) {
          sequence <- rv$protein_sequences[protein_id]
        } else {
          sequence <- NULL
        }

        if (!is.null(sequence) && !is.na(sequence)) {
          cat(">", protein_id, "\n", sep = "")
          # 格式化序列显示
          seq_length <- nchar(sequence)
          for (i in seq(1, seq_length, by = 60)) {
            cat(substr(sequence, i, min(i+59, seq_length)), "\n")
          }
        } else {
          cat("Sequence not found for:", protein_id, "\n")
          cat("Please check if sequence data is available.")
        }
      } else {
        cat("No sequence data loaded.\n")
        cat("Please provide protein sequence data.")
      }
    })

    # 下载序列
    output$download_sequence <- downloadHandler(
      filename = function() {
        paste0(rv$selected_protein$id, ".fasta")
      },
      content = function(file) {
        req(rv$selected_protein, rv$protein_sequences)

        protein_id <- rv$selected_protein$id
        if (inherits(rv$protein_sequences, "AAStringSet")) {
          sequence <- as.character(rv$protein_sequences[protein_id])
        } else {
          sequence <- rv$protein_sequences[protein_id]
        }

        fasta_content <- paste0(">", protein_id, "\n", sequence)
        writeLines(fasta_content, file)
      }
    )

    # 显示结构域图
    output$domain_plot <- renderPlot({
      req(rv$selected_protein)

      protein_id <- rv$selected_protein$id

      # 创建示例结构域图
      par(mar = c(4, 2, 2, 1))
      plot(1, type = "n", xlim = c(0, 500), ylim = c(0, 3),
           xlab = "Amino Acid Position", ylab = "", yaxt = "n",
           main = paste("Domain Architecture -", protein_id))

      # 示例结构域
      domains <- data.frame(
        name = c("Kinase", "Regulatory", "Catalytic"),
        start = c(50, 200, 350),
        end = c(150, 300, 450),
        color = c("red", "blue", "green")
      )

      for (i in 1:nrow(domains)) {
        rect(domains$start[i], 1, domains$end[i], 2,
             col = domains$color[i], border = "black")
        text(mean(c(domains$start[i], domains$end[i])), 1.5,
             domains$name[i], cex = 0.8)
      }
    })

    # 显示结构域表格
    output$domain_table <- renderDT({
      req(rv$selected_protein)

      protein_id <- rv$selected_protein$id

      # 示例结构域数据
      domain_example <- data.frame(
        Domain = c("Kinase domain", "Regulatory domain", "Catalytic domain"),
        Start = c(50, 200, 350),
        End = c(150, 300, 450),
        Length = c(101, 101, 101),
        E.value = c("1e-50", "1e-30", "1e-40")
      )

      datatable(domain_example, options = list(pageLength = 5))
    })

    # 显示3D结构信息
    output$structure_display <- renderUI({
      req(rv$selected_protein)

      protein_id <- rv$selected_protein$id

      tagList(
        h4("3D Structure Information"),
        p("Protein ID:", protein_id),
        p("To view 3D structure, please visit:"),
        tags$ul(
          tags$li(tags$a(href = paste0("https://www.rcsb.org/search?request=%7B%22query%22%3A%7B%22parameters%22%3A%7B%22value%22%3A%22", protein_id, "%22%7D%7D%7D"),
                         "RCSB PDB", target = "_blank")),
          tags$li(tags$a(href = paste0("https://alphafold.ebi.ac.uk/entry/", protein_id),
                         "AlphaFold DB", target = "_blank")),
          tags$li(tags$a(href = paste0("https://www.uniprot.org/uniprotkb?query=", protein_id),
                         "UniProt", target = "_blank"))
        ),
        plotOutput(ns("structure_placeholder"), height = "200px")
      )
    })

    # 3D结构占位图
    output$structure_placeholder <- renderPlot({
      par(mar = c(0,0,0,0))
      plot(1, type = "n", xlim = c(0,1), ylim = c(0,1), axes = FALSE, xlab = "", ylab = "")
      text(0.5, 0.5, "3D Structure Visualization\n(External database links provided above)",
           cex = 1.2, col = "gray")
      rect(0.2, 0.3, 0.8, 0.7, border = "gray", lty = 2)
    })

    # 外部数据库链接
    output$external_links <- renderUI({
      req(rv$selected_protein)

      protein_id <- rv$selected_protein$id

      tagList(
        h5("External Database Links"),
        tags$ul(
          tags$li(tags$a(href = paste0("https://www.ncbi.nlm.nih.gov/protein/", protein_id),
                         "NCBI Protein", target = "_blank")),
          tags$li(tags$a(href = paste0("https://www.ebi.ac.uk/interpro/entry/InterPro/#table%7Cquery%7C", protein_id),
                         "InterPro", target = "_blank")),
          tags$li(tags$a(href = paste0("https://www.genome.jp/dbget-bin/www_bget?", protein_id),
                         "KEGG", target = "_blank"))
        )
      )
    })
  })
}
