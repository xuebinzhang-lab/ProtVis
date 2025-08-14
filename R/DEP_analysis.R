DEP_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        # Data loading panel
        div(style = "margin-bottom: 15px;",
            actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold")
        ),
        uiOutput(ns("load_status_panel")),
        hr(),
        tags$small("CompareGroup requires two columns (Group1 & Group2) with different content.",
                   style = "color: #6c757d"),
        # Input mode toggle switch
        shinyWidgets::switchInput(
          inputId = ns("input_mode"),
          label = "CompareGroup",
          value = TRUE,
          onLabel = "Upload",
          offLabel = "Paste",
          width = "100%"
        ),

        # Conditional panel: File upload mode
        conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == true"),
          tags$small('Upload CompareGroup', style = "color: #6c757d"),
          fileInput(
            inputId = ns('compare_file'),
            label = NULL,
            multiple = FALSE,
            accept = c('.csv','.xlsx')
          )
        ),

        # Conditional panel: Manual input mode
        conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == false"),
          div(
            tags$small("Edit CompareGroup", style = "color: #6c757d"),
            rhandsontable::rHandsontableOutput(ns("hot_compare")),
            br(),
            tags$small("Paste CompareGroup", style = "color: #6c757d"),
            textAreaInput(
              inputId = ns("paste_data"),
              label = NULL,
              placeholder = "Copy and paste Excel data here.",
              rows = 5
            ),
            actionButton(ns("apply_paste"), "Apply paste data",
                         class = "btn btn-light fw-bold")
          )
        )
      ),

      # Main display panel (right side) - SIMPLIFIED VERSION FIRST
      card(
        height = "600px",  # Fixed height for testing
        card_header("Data Preview"),
        navset_card_tab(
          full_screen = TRUE,
          nav_panel(
            "Sample Info",
            div(
              style = "height: 500px; overflow: auto;",
              DT::dataTableOutput(ns("sample_info"))
            )
          ),
          nav_panel(
            "Normalized Data",
            div(
              style = "height: 500px; overflow: auto;",
              DT::dataTableOutput(ns("normalized_data"))
            )
          ),
          nav_panel(
            "Group Comparison",
            div(
              style = "height: 500px; overflow: auto;",
              DT::dataTableOutput(ns("group_comparison"), height = "100%")
            )
          ),
          nav_panel(
            "DEP result",
            # DEP result-------------------------------------------------------------------------
            page_fluid(
              layout_column_wrap(
                width = 1/2,
                height = 600,
                card(
                  height = "800px",
                  card_header("DEP table"),
                  card_body(
                    plotOutput(ns("DEP_table"))
                  )
                ),
                card(
                  height = "800px",
                  card_header("Volcano plot"),
                  card_body(
                    plotOutput(ns("Volcano_plot"))
                  )
                ),
                card(
                  height = "800px",
                  card_header("Heatmap"),
                  card_body(
                    plotOutput(ns("Heatmap"))
                  )
                ),
                card(
                  height = "800px",
                  card_header("Bar of DEP"),
                  card_body(
                    plotOutput(ns("bar_of_DEP"))
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


DEP_analysis_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      # Original variables
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,

      # New variables
      compare_data = NULL,
      input_mode = TRUE
    )

    # Built-in empty table template
    template_df <- reactive({
      data.frame(
        Group1 = c(NA, NA, NA),
        Group2 = c(NA, NA, NA),
        stringsAsFactors = FALSE
      )
    })

    # Original data loading logic
    observeEvent(input$load_data, {
      req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step6_data_normalization.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)
        if (exists("sample_info", envir = e)) rv$sample_info <- e$sample_info
        if (exists("normalized_data", envir = e)) {
          rv$normalized_matrix <- e$normalized_data
        } else {
          rv$normalized_matrix <- NULL
          showNotification("Step6_data_normalization.rda does not exist. Expression matrix cannot be loaded.",
                           type = "warning")
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
        }
      })

    # Display loading status
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # Render editable hot table (new)
    output$hot_compare <- rhandsontable::renderRHandsontable({
      df <- if(!is.null(rv$compare_data)) rv$compare_data else template_df()
      rhandsontable::rhandsontable(df, stretchH = "all") %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # Handle file upload (new)
    observeEvent(input$compare_file, {
      req(input$compare_file)
      ext <- tools::file_ext(input$compare_file$name)

      df <- tryCatch({
        if(ext == "csv") {
          read.csv(input$compare_file$datapath)
        } else if(ext == "xlsx") {
          readxl::read_excel(input$compare_file$datapath)
        }
      }, error = function(e) {
        showNotification(paste("Failed to read file:", e$message), type = "error")
        NULL
      })

      if(!is.null(df)) {
        rv$compare_data <- df
        showNotification("Comparison group file loaded!", type = "message")
      }
    })

    # Handle paste data (new)
    observeEvent(input$apply_paste, {
      req(input$paste_data)
      tryCatch({
        df <- read.table(text = input$paste_data, sep = "\t", header = TRUE)
        rv$compare_data <- df
        showNotification("Pasted data applied!", type = "message")
      }, error = function(e) {
        showNotification("Invalid paste data format. Please check separators and headers", type = "error")
      })
    })

    # Sync hot table changes to data (new)
    observeEvent(input$hot_compare, {
      rv$compare_data <- rhandsontable::hot_to_r(input$hot_compare)
    })

    # Preview -----------------------------------------------------------------
    # Preview sample info
    output$sample_info <- DT::renderDataTable({
      req(rv$sample_info)
      DT::datatable(
        rv$sample_info,
        options = list(scrollX = TRUE, dom = 't'),
        rownames = FALSE
      )
    })
    # Preview normalized data
    output$normalized_data <- DT::renderDataTable({
      req(rv$normalized_matrix)
      DT::datatable(
        rv$normalized_matrix,
        options = list(scrollX = TRUE, dom = 't'),
        rownames = FALSE
      )
    })
    # Preview group comparison
    output$group_comparison <- DT::renderDataTable({
      req(rv$compare_data)
      DT::datatable(
        rv$compare_data,
        options = list(scrollX = TRUE, dom = 't'),
        rownames = FALSE
      )
    })

    # Return ------------------------------------------------------------------
    # Return comparison data for other modules
    return(
      reactive({
        list(
          compare_data = rv$compare_data,
          normalized_matrix = rv$normalized_matrix,
          sample_info = rv$sample_info
        )
      })
    )
  })
}
