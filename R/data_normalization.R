sample_subtract <- function(data) {
  # Median subtraction (ignoring NA)
  data_median_subtracted <- apply(data, 2, function(x) x - median(x, na.rm = TRUE))
  return(data_median_subtracted)
}


data_normalization_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(style = "margin-bottom: 15px;",
            actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold")
        ),
        uiOutput(ns("load_status_panel")),
        hr(),
        div(style = "margin-top: 15px;",
            actionButton(ns("run_normalization"), "Run Normalization", class = "btn btn-primary")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 600,

          card(
            height = "800px",
            card_header("Original Data"),
            card_body(
              DT::DTOutput(ns("originalData"))
            )
          ),

          card(
            height = "800px",
            card_header("Original Data visualize"),
            card_body(
              plotOutput(ns("originalPlot"))
            )
          ),

          card(
            height = "800px",
            card_header("Normalized Data"),
            card_body(
              DT::DTOutput(ns("dataNormalization"))
            )
          ),

          card(
            height = "800px",
            card_header("Normalized Data Visualization"),
            card_body(
              plotOutput(ns("dataNormalizationPlot"))
            )
          )
        )
      )
    )
  )
}



data_normalization_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE,
      normalized_matrix = NULL
    )

    # 加载数据
    # Load processed data when "LOAD DATA" button is clicked
    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step5_data_imputation.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        if (exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (exists("imputed_df", envir = e)) {
          rv$expression_matrix <- e$imputed_df
        } else {
          rv$expression_matrix <- NULL
          showNotification("Step5_data_imputation.rda does not exist. Expression matrix cannot be loaded.", type = "warning")
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        showNotification("Step5_data_imputation.rda not found.", type = "error")
      }
    })

    # Display load status in the UI
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # 状态显示
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # 原始数据表预览
    output$originalData <- DT::renderDT({
      req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = list(scrollX = TRUE))
    })

    # 原始图片预览
    output$originalPlot <- renderPlot({
      req(rv$sample_info)
      req(rv$expression_matrix)
      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix

      expmat_before <- expression_matrix
      expmat_before.long <-
        expmat_before %>%
        tibble::rownames_to_column("ID") %>%
        pivot_longer(!ID,names_to = "sample_id",values_to = "intensity") %>%
        left_join(sample_info)
      ggplot(data = expmat_before.long,mapping = aes(x = sample_id,y = intensity,fill = group)) +
        xlab("") +
        ylab("Relative intensity") +
        geom_boxplot(outlier.size = 0.1,linewidth = 0.5,staplewidth = 0.5,fatten = 0.5)+
        coord_flip()+
        theme_bw()
    })

    # 运行归一化
    observeEvent(input$run_normalization, {
      req(rv$expression_matrix)
      req(rv$sample_info)
      sample_info <- rv$sample_info
      normalized_data <- sample_subtract(rv$expression_matrix)
      rv$normalized_matrix <- as.data.frame(normalized_data)

      save(sample_info,normalized_data, file = file.path(shared_state$workdir, "Step6_data_normalization.rda"))
      showNotification("Normalization completed", type = "message")
    })

    # 归一化数据表预览
    output$dataNormalization <- DT::renderDT({
      req(rv$normalized_matrix)
      DT::datatable(rv$normalized_matrix, options = list(scrollX = TRUE))
    })

    # 归一化图片预览
    output$dataNormalizationPlot <- renderPlot({
      req(rv$sample_info)
      req(rv$normalized_matrix)
      sample_info <- rv$sample_info
      normalized_matrix <- rv$normalized_matrix

      expmat_before <- normalized_matrix
      expmat_before.long <-
        expmat_before %>%
        tibble::rownames_to_column("ID") %>%
        pivot_longer(!ID,names_to = "sample_id",values_to = "intensity") %>%
        left_join(sample_info)
      ggplot(data = expmat_before.long,mapping = aes(x = sample_id,y = intensity,fill = group)) +
        xlab("") +
        ylab("Relative intensity") +
        geom_boxplot(outlier.size = 0.1,linewidth = 0.5,staplewidth = 0.5,fatten = 0.5)+
        coord_flip()+
        theme_bw()
    })

  })
}

