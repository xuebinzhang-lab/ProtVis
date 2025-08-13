#' Data Imputation UI Module
#'
#' This function creates the UI for the data imputation module.
#' It includes controls for loading data, visualizing missing values,
#' selecting imputation methods, and viewing imputed results.
#'
#' @param id Module ID used to namespace the UI elements.
#'
#' @return A Shiny UI element containing sidebar controls and main panel tabs.
#' @export
data_imputation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        actionButton(ns("visualize_missing_values"), "Visualize missing values", class = "btn btn-light fw-bold"),
        uiOutput(ns("load_status_panel")),
        accordion(
          accordion_panel(
            title = "Imputation Settings",
            icon = bs_icon("tools"),
            selectInput(ns("choice_method"), "Method",
                        choices = c("kNN", "RF", "Mean", "Median", "Zero", "Minimum"),
                        selected = "Mean"),
            numericInput(ns("knn_k"), "K for kNN", value = 5, min = 2, max = 20),
            numericInput(ns("minprob_q"), "q for MinProb", value = 0.01, min = 0, max = 0.05, step = 0.005),
            actionButton(ns("run_impute"), "Run Imputation", class = "btn btn-light fw-bold")
          ),
          accordion_panel(
            title = "Download",
            icon = bs_icon("download"),
            numericInput(ns("img_height"), "Height (inches):", value = 5, step = 1),
            numericInput(ns("img_width"), "Width (inches):", value = 5, step = 1),
            downloadButton(ns("downloadOriginalPlot"), "Download Original Plot"),
            downloadButton(ns("downloadImputedPlot"), "Download Imputed Plot"),
          )

        )
      ),
      page_fluid(
        tabsetPanel(
          id = ns("tabs"),
          tabPanel(
            title = "Sample Info",
            DT::DTOutput(ns("sample_info"))
          ),
          tabPanel(
            title = "Expression Matrix",
            DT::DTOutput(ns("expression_matrix"))
          ),
          tabPanel(
            title = "Visualize missing values",
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
                  card_header("Imputed Data"),
                  card_body(
                    DT::DTOutput(ns("imputedData"))
                  )
                ),

                card(
                  height = "800px",
                  card_header("Imputed Data visualize"),
                  card_body(
                    plotOutput(ns("imputedPlot"))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' Data Imputation Server Module
#'
#' This function defines the server logic for the data imputation module.
#' It handles:
#' - Loading data from the working directory
#' - Displaying sample information and expression matrix
#' - Visualizing missing values in original data
#' - Performing data imputation based on the selected method
#' - Displaying imputed data and visualizations
#'
#' @param id Module ID used to namespace the server elements.
#' @param shared_state A reactive list containing shared application state,
#'        including the working directory.
#'
#' @return No direct return value; generates Shiny server-side outputs.
#' @export
data_imputation_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE
    )

    # Load processed data when "LOAD DATA" button is clicked
    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step4_data_transformed.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        if (exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (exists("transformed", envir = e)) {
          rv$expression_matrix <- e$transformed
        } else {
          rv$expression_matrix <- NULL
          showNotification("⚠️ Step4_data_transformed.rda does not exist. Expression matrix cannot be loaded.", type = "warning")
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        showNotification("❌ Step4_select_protein_id.rda not found.", type = "error")
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

    # Display sample information table
    output$sample_info <- DT::renderDT({
      req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE, pageLength = 10))
    })

    # Display expression matrix table
    output$expression_matrix <- DT::renderDT({
      req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = list(scrollX = TRUE, pageLength = 10))
    })

    # Visualize missing values in the original data
    observeEvent(input$visualize_missing_values, {
      output$originalData <- DT::renderDT({
        req(rv$expression_matrix)
        DT::datatable(rv$expression_matrix, options = list(pageLength = 10))
      })

      output$originalPlot <- renderPlot({
        req(rv$expression_matrix)
        visdat::vis_dat(data.frame(rv$expression_matrix)) +
          scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )
      })
    })

    # Perform imputation when "Run Imputation" button is clicked
    imputed_data <- eventReactive(input$run_impute, {
      req(rv$expression_matrix)
      df <- as.data.frame(rv$expression_matrix)
      method <- input$choice_method

      if (method == "kNN") {
        return(as.data.frame(VIM::kNN(df, k = input$knn_k)))
      } else if (method == "RF") {
        return(as.data.frame(missForest::missForest(df)$ximp))
      } else if (method == "Mean") {
        return(df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                                  ~ifelse(is.na(.), mean(., na.rm = TRUE), .))))
      } else if (method == "Median") {
        return(df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                                  ~ifelse(is.na(.), median(., na.rm = TRUE), .))))
      } else if (method == "Zero") {
        return(df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                                  ~ifelse(is.na(.), 0, .))))
      } else if (method == "Minimum") {
        return(df %>% dplyr::mutate(dplyr::across(dplyr::everything(),
                                                  ~ifelse(is.na(.), min(., na.rm = TRUE), .))))
      }
    })

    observeEvent(input$run_impute, {
      req(imputed_data(), rv$sample_info)

      # 先赋值到普通变量
      sample_info <- rv$sample_info
      imputed_df <- imputed_data()

      # 保存到 Step6_data_imputation.rda
      save(sample_info, imputed_df,
           file = file.path(shared_state$workdir, "Step5_data_imputation.rda")
      )

      showNotification("✅ Step5_data_imputation.rda 已保存", type = "message")
    })


    # Display imputed data table
    output$imputedData <- DT::renderDT({
      req(imputed_data())
      DT::datatable(imputed_data(), options = list(pageLength = 10))
    })

    # Display imputed data visualization
    output$imputedPlot <- renderPlot({
      req(imputed_data())
      visdat::vis_dat(data.frame(imputed_data())) +
        scale_fill_manual(
          values = c(
            "character" = "skyblue",
            "factor" = "lightgreen",
            "numeric" = "#E0F3F8",
            "logical" = "lightyellow",
            "NA" = "#BEBEBE"
          )
        )
    })

    # 下载原始数据可视化图
    output$downloadOriginalPlot <- downloadHandler(
      filename = function() {
        paste0("original_data_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        g <- visdat::vis_dat(data.frame(rv$expression_matrix)) +
          scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )
        ggsave(file, plot = g, width = input$img_width, height = input$img_height, units = "in")
      }
    )

    # 下载插补后数据可视化图
    output$downloadImputedPlot <- downloadHandler(
      filename = function() {
        paste0("imputed_data_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        g <- visdat::vis_dat(data.frame(imputed_data())) +
          scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )
        ggsave(file, plot = g, width = input$img_width, height = input$img_height, units = "in")
      }
    )

  })
}
