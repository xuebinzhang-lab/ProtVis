data_transformed_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
        uiOutput(ns("load_status_panel")),
        accordion(
          selectInput(
            inputId = ns("data_transformed"),
            label = "Data transformed",
            choices = c("None", "log10", "log2", "Standardization", "Z-Score",
                        "scale", "center", "scale-center"),
            selected = "None"
          )
        ),
        actionButton(ns("export_remove_noise_data"), "export data", class = "btn btn-light fw-bold"),
        uiOutput(ns("export_remove_noise_data_status_panel"))
      ),
      div(
        card(
          card_header("Preview the data processing process"),
          card_body(
            fill = TRUE,
            navset_tab(
              id = ns("Sample Info"),
              nav_panel("Sample Info", DT::dataTableOutput(ns("tbl_sample_info"))),
              nav_panel("Expression Matrix", DT::dataTableOutput(ns("tbl_expression_matrix"))),
              nav_panel("Data transformed", DT::dataTableOutput(ns("table_data_transformed")))
            )
          )
        )
      )
    )
  )
}

data_transformed_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      correct_noise_result = NULL,
      sample_info = NULL,
      load_success = FALSE,
      transformed = NULL
    )

    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step3_correct_noise.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        if (exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (exists("correct_noise_result", envir = e)) {
          rv$correct_noise_result <- e$correct_noise_result
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        showNotification("❌ Step3_correct_noise.rda not found.", type = "error")
      }
    })

    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    output$tbl_sample_info <- DT::renderDT({
      req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE))
    })

    output$tbl_expression_matrix <- DT::renderDT({
      req(rv$correct_noise_result)
      DT::datatable(rv$correct_noise_result, options = list(scrollX = TRUE))
    })

    observe({
      req(input$data_transformed, rv$correct_noise_result)
      df <- rv$correct_noise_result

      # 确保ID列是行名
      df_mat <- df %>% tibble::column_to_rownames("ID")

      rv$transformed <- switch(input$data_transformed,
                               "None" = df_mat,
                               "log10" = log10(df_mat + 1e-8),
                               "log2" = log2(df_mat + 1e-8),
                               "Standardization" = scale(df_mat, center = TRUE, scale = TRUE),
                               "Z-Score" = scale(df_mat, center = TRUE, scale = TRUE),
                               "scale" = scale(df_mat, center = FALSE, scale = TRUE),
                               "center" = scale(df_mat, center = TRUE, scale = FALSE),
                               "scale-center" = scale(df_mat, center = TRUE, scale = TRUE),
                               df_mat
      )
    })

    observeEvent(input$export_remove_noise_data, {
      req(shared_state$workdir)
      save_path <- file.path(shared_state$workdir, "Step4_data_transformed.rda")

      sample_info <- rv$sample_info
      correct_noise_result <- rv$correct_noise_result
      transformed <- rv$transformed

      save(sample_info, correct_noise_result, transformed, file = save_path)

      showNotification(paste0("✅ Exported to: ", save_path), type = "message")

      output$export_remove_noise_data_status_panel <- renderUI({
        span(paste0("✅ Data exported to: ", basename(save_path)), style = "color: green;")
      })
    })

    output$table_data_transformed <- DT::renderDT({
      req(rv$transformed)
      DT::datatable(rv$transformed, options = list(scrollX = TRUE))
    })

    return(rv)
  })
}


