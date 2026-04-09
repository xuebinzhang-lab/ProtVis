#' Data Imputation UI Module
#' This function creates the UI for the data imputation module.
#' It includes controls for loading data, visualizing missing values,
#' selecting imputation methods, and viewing imputed results.
#' @param id Module ID used to namespace the UI elements.
#' @return A Shiny UI element containing sidebar controls and main panel tabs.
#' @name data_imputation_ui
#' @export
data_imputation_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shinyjs::useShinyjs(),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        shiny::actionButton(ns("visualize_missing_values"), "Visualize missing values", class = "btn btn-light fw-bold"),
        shiny::uiOutput(ns("load_status_panel")),
        bslib::accordion(
          bslib::accordion_panel(
            title = "Imputation Settings",
            icon = imputation_settings_icon,
            shiny::selectInput(ns("choice_method"), "Method",
                        choices = c("kNN", "RF", "Mean", "Median", "Zero", "Minimum"),
                        selected = "Mean"),
            shiny::numericInput(ns("minprob_q"), "q for MinProb", value = 0.01, min = 0, max = 0.05, step = 0.005),
            shiny::actionButton(ns("run_impute"), "Run Imputation", class = "btn btn-light fw-bold")
          ),
          bslib::accordion_panel(
            title = "Download",
            icon = bs_icon("download"),
            shiny::numericInput(ns("img_height"), "Height (inches):", value = 5, step = 1),
            shiny::numericInput(ns("img_width"), "Width (inches):", value = 5, step = 1),
            shiny::downloadButton(ns("downloadOriginalPlot"), "Download Original Plot"),
            shiny::downloadButton(ns("downloadImputedPlot"), "Download Imputed Plot"),
          )
        )
      ),
      bslib::page_fluid(
        shiny::tabsetPanel(
          id = ns("tabs"),
          shiny::tabPanel(
            title = "Sample Info",
            DT::DTOutput(ns("sample_info"))
          ),
          shiny::tabPanel(
            title = "Expression Matrix",
            DT::DTOutput(ns("expression_matrix"))
          ),
          shiny::tabPanel(
            title = "Visualize missing values",
            bslib::page_fluid(
              bslib::layout_column_wrap(
                width = 1/2,
                height = 600,
                bslib::card(
                  height = "800px",
                  bslib::card_header("Original Data"),
                  bslib::card_body(
                    DT::DTOutput(ns("originalData"))
                  )
                ),
                bslib::card(
                  height = "800px",
                  bslib::card_header("Original Data visualize"),
                  bslib::card_body(
                    shiny::plotOutput(ns("originalPlot"))
                  )
                ),

                bslib::card(
                  height = "800px",
                  bslib::card_header("Imputed Data"),
                  bslib::card_body(
                    DT::DTOutput(ns("imputedData"))
                  )
                ),
                bslib::card(
                  height = "800px",
                  bslib::card_header("Imputed Data visualize"),
                  bslib::card_body(
                    shiny::plotOutput(ns("imputedPlot"))
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
#' @return No direct return value; generates Shiny server-side outputs.
#' @name data_imputation_server
#' @export
#'
data_imputation_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      load_success = FALSE
    )

    # Load processed data when "LOAD DATA" button is clicked
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      rda_path <- base::file.path(shared_state$workdir, "Step4_data_transformed.rda")
      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        base::load(rda_path, envir = e)
        if (base::exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (base::exists("transformed", envir = e)) {
          rv$expression_matrix <- e$transformed
        } else {
          rv$expression_matrix <- NULL
          shiny::showNotification("⚠️ Step4_data_transformed.rda does not exist. Expression matrix cannot be loaded.", type = "warning")
        }
        rv$load_success <- TRUE
        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("❌ Step4_select_protein_id.rda not found.", type = "error")
      }
    })
    # Display load status in the UI
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    # Display sample information table
    output$sample_info <- DT::renderDT({
      shiny::req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE, pageLength = 10))
    })
    # Display expression matrix table
    output$expression_matrix <- DT::renderDT({
      shiny::req(rv$expression_matrix)
      DT::datatable(rv$expression_matrix, options = list(scrollX = TRUE, pageLength = 10))
    })
    # Visualize missing values in the original data
    shiny::observeEvent(input$visualize_missing_values, {
      output$originalData <- DT::renderDT({
        shiny::req(rv$expression_matrix)
        DT::datatable(rv$expression_matrix, options = list(pageLength = 10))
      })
      output$originalPlot <- renderPlot({
        shiny::req(rv$expression_matrix)
        visdat::vis_dat(data.frame(rv$expression_matrix)) +
          ggplot2::scale_fill_manual(
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
      shiny::req(rv$expression_matrix)
      df <- base::as.data.frame(rv$expression_matrix)
      method <- input$choice_method
      set.seed(12345)
      if (method == "kNN") {
        return(impute::impute.knn(as.matrix(df))$data)
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
    shiny::observeEvent(input$run_impute, {
      shiny::req(imputed_data(), rv$sample_info)
      sample_info <- rv$sample_info
      imputed_df <- base::as.data.frame(imputed_data())
      base::save(sample_info, imputed_df,
           file = base::file.path(shared_state$workdir, "Step5_data_imputation.rda")
      )
      shiny::showNotification("✅ Step5_data_imputation.rda saved", type = "message")
    })
    # Display imputed data table
    output$imputedData <- DT::renderDT({
      shiny::req(imputed_data())
      DT::datatable(imputed_data(), options = list(pageLength = 10))
    })
    # Display imputed data visualization
    output$imputedPlot <- shiny::renderPlot({
      shiny::req(imputed_data())
      visdat::vis_dat(data.frame(imputed_data())) +
        ggplot2::scale_fill_manual(
          values = c(
            "character" = "skyblue",
            "factor" = "lightgreen",
            "numeric" = "#E0F3F8",
            "logical" = "lightyellow",
            "NA" = "#BEBEBE"
          )
        )
    })

    # Download the original data visualization.
    output$downloadOriginalPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("original_data_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        g <- visdat::vis_dat(data.frame(rv$expression_matrix)) +
          ggplot2::scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )
        ggplot2::ggsave(file, plot = g, width = input$img_width, height = input$img_height, units = "in")
      }
    )

    # Download the interpolated data visualization map.
    output$downloadImputedPlot <- shiny::downloadHandler(
      filename = function() {
        paste0("imputed_data_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        g <- visdat::vis_dat(data.frame(imputed_data())) +
          ggplot2::scale_fill_manual(
            values = c(
              "character" = "skyblue",
              "factor" = "lightgreen",
              "numeric" = "#E0F3F8",
              "logical" = "lightyellow",
              "NA" = "#BEBEBE"
            )
          )
        ggplot2::ggsave(file, plot = g, width = input$img_width, height = input$img_height, units = "in")
      }
    )
  })
}
