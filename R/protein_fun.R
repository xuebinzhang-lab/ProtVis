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
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 1500,

          card(
            height = "800px",
            card_header("DEP"),
            card_body(
              # 将selectInput移到server端动态生成
              uiOutput(ns("comparison_select_ui")),
              plotlyOutput(ns("volcano_plot"), height = "600px")
            )
          ),

          card(
            height = "800px",
            card_header("Protein sequence"),
            card_body(
            )
          ),

          card(
            height = "800px",
            card_header("Domain"),
            card_body(
            )
          ),

          card(
            height = "800px",
            card_header("Protein 3D Structure"),
            card_body(
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
      load_success = FALSE
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
      } else {
        rv$load_success <- FALSE
        showNotification("Step7_DEP_result.rda not found.", type = "error")
      }
    })
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })
    # 动态生成比较组选择下拉框
    output$comparison_select_ui <- renderUI({
      req(rv$dep_results)
      # 获取dep_results的名称
      comparison_choices <- names(rv$dep_results)
      if (length(comparison_choices) > 0) {
        selectInput(
          ns("comparison_group"),
          "Please select a comparison group:",
          choices = comparison_choices,
          selected = comparison_choices[1]  # 默认选择第一个
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

          p <- ggplot(dep_data, aes(x = logFC, y = -log10(P.Value),
                                    color = regulation,
                                    text = paste("Protein:", rownames(dep_data),
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
                 color = ""
                 # title = paste("Volcano Plot -", input$comparison_group)
                 ) +
            theme(plot.title = element_text(hjust = 0.5),
                  legend.position = "top") +
            geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
            geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black")

          plotly::ggplotly(p, tooltip = "text") %>%
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



  })
}
