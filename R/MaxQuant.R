#' MaxQuant User Interface Module
#'
#' This function creates the user interface for processing proteomics data
#' from MaxQuant output files. It includes options for filtering, selecting
#' protein identifiers, and previewing processing results.
#'
#' @param id Character. Module ID used for namespacing the UI elements.
#'
#' @return UI layout for the MaxQuant processing module.
#' @export

MaxQuant_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold"),
      uiOutput(ns("load_status_panel")),
      accordion(
        accordion_panel(
          title = "remove unreliable peptide",
          icon = bs_icon("Filter"),
          div(
            style = "font-size: 12px;",
            checkboxGroupInput(
              inputId = ns("selected_Method"),
              label = "Please Select the Removal Method:",
              choices = c("remove peptide Only identified by site" = "site",
                          "remove potential contaminant peptide" = "conpeptide",
                          "remove reverse peptide" = "peptide"),
              selected = c("site", "peptide", "conpeptide")
            ),
            actionButton(ns("run_filter_unreliable"), "Remove", class = "btn btn-light fw-bold"),
            actionButton(ns("report"), "Report", class = "btn btn-light fw-bold")
          )
        )
      )
    ),
    div(
      card(
        card_header("Preview the data processing process"),
        card_body(
          fill = TRUE,
          navset_tab(
            id = ns("Expression_Matrix"),
            nav_panel("Sample info",
                      uiOutput(ns("sample_info_ui"))
            ),
            nav_panel("Expression Matrix",
                      htmlOutput(ns("matrix_check")),
                      DT::dataTableOutput(ns("tbl_expression_matrix"))
            ),
            nav_panel("Filtered Unreliable Peptide",
                      DT::dataTableOutput(ns("tbl_unreliable_filtered"))
            ),
            nav_panel("Reporter",
                      DT::dataTableOutput(ns("result_df"))
            )
          )
        )
      )
    )
  )
}

#' MaxQuant Server Logic Module
#'
#' This function implements the server-side logic for processing proteomics data
#' from MaxQuant output. It supports loading data, filtering proteins based on
#' expression, removing unreliable peptides, selecting protein identifiers, and
#' saving intermediate results.
#'
#' @param id Character. Module ID used for namespacing server inputs/outputs.
#' @param shared_state A reactiveValues object containing shared state variables
#'   across modules, including `workdir`, `sample_info`, and `expression_matrix`.
#'
#' @return A list of reactive values containing processed results, including:
#'   - `protein_id_select`: A reactive expression with selected protein IDs.
#' @export

MaxQuant_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      sample_info = NULL,
      expression_matrix = NULL,
      expression_matrix_filtered = NULL,
      load_success = FALSE,
      filter_summary = NULL
    )

    filter_done <- reactiveVal(FALSE)

    # Load data
    observeEvent(input$load_data, {
      req(shared_state$workdir)

      rda_path <- file.path(shared_state$workdir, "Step1_project_init.rda")
      if (file.exists(rda_path)) {
        e <- new.env()
        load(rda_path, envir = e)

        if (exists("sample_info", envir = e)) {
          rv$sample_info <- e$sample_info
        }
        if (exists("expression_matrix", envir = e)) {
          rv$expression_matrix <- e$expression_matrix
          rv$expression_matrix_filtered <- e$expression_matrix
          filter_done(FALSE)
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        showNotification("❌ Step1_project_init.rda not found in working directory.", type = "error")
      }
    })

    # remove unreliable peptide
    observeEvent(input$run_filter_unreliable, {
      req(rv$expression_matrix)
      selected_filters <- input$selected_Method
      filtered <- rv$expression_matrix
      if ("site" %in% selected_filters && "Only identified by site" %in% colnames(filtered)) {
        filtered <- filtered %>% dplyr::filter(is.na(`Only identified by site`))
      }

      if ("peptide" %in% selected_filters && "Reverse" %in% colnames(filtered)) {
        filtered <- filtered %>% dplyr::filter(is.na(`Reverse`))
      }

      if ("conpeptide" %in% selected_filters && "Potential contaminant" %in% colnames(filtered)) {
        filtered <- filtered %>% dplyr::filter(is.na(`Potential contaminant`)) %>%
          dplyr::mutate(ID = stringr::str_split(`Protein IDs`,";",2,T)[,1]) %>%
          dplyr::select(ID,contains("Reporter"))
      }


      # Update results
      rv$expression_matrix_filtered <- filtered
      rv$unreliable_filtered <- filtered
      filter_done(TRUE)
      showNotification(paste("Unreliable peptides filtered, remaining rows:", nrow(filtered)), type = "message")

      # save data
      save_path <- file.path(shared_state$workdir, "Step2_remove_unreliable_peptide.rda")
      sample_info <- rv$sample_info
      expression_matrix <- rv$expression_matrix
      expression_matrix_filtered <- rv$expression_matrix_filtered
      save(sample_info, expression_matrix, expression_matrix_filtered, file = save_path)
      showNotification("✅ Saved to Step2_remove_unreliable_peptide.rda", type = "message")
    })
    # Display report results
    observeEvent(input$report, {
      req(rv$expression_matrix_filtered)
      req(rv$expression_matrix)
      filtered <- rv$expression_matrix
      n_oibs <- nrow(filtered %>% dplyr::filter(!is.na(`Only identified by site`)))
      n_r <- nrow(filtered %>% dplyr::filter(!is.na(`Reverse`)))
      n_pc <- nrow(filtered %>% dplyr::filter(!is.na(`Potential contaminant`)))
      rep_id <- (filtered$`Protein IDs` %>% stringr::str_split(";",2,T))[,1]
      removed_protein_group <- setdiff(sort(rep_id), sort(rv$expression_matrix_filtered$ID))
      result_df <- data.frame(
        Metric = c("Only identified by site", "Reverse", "Potential contaminant", "Removed protein groups count"),
        Count = c(n_oibs, n_r, n_pc, length(removed_protein_group)),
        stringsAsFactors = FALSE
      )

      rv$filter_summary <- result_df
    })
    output$result_df <- DT::renderDataTable({
      req(rv$filter_summary)
      DT::datatable(rv$filter_summary,
                    options = list(pageLength = 5, searching = FALSE))
    })


    # UI outputs
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    output$sample_info_ui <- renderUI({
      req(rv$load_success)
      DTOutput(ns("sample_info_table"))
    })

    output$sample_info_table <- renderDT({
      req(rv$sample_info)
      datatable(rv$sample_info, options = list(pageLength = input$rows_to_show))
    })

    output$matrix_check <- renderUI({
      if (!rv$load_success || is.null(rv$expression_matrix)) {
        HTML("<span style='color: red;'>Expression matrix not loaded.</span>")
      } else {
        HTML(paste("<span style='color: green;'>Matrix dimensions:",
                   nrow(rv$expression_matrix), "rows x", ncol(rv$expression_matrix), "columns</span>"))
      }
    })

    output$tbl_expression_matrix <- renderDT({
      req(rv$expression_matrix)
      datatable(rv$expression_matrix, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$tbl_filtered_expression <- renderDT({
      req(filter_done())
      datatable(rv$expression_matrix_filtered, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$tbl_unreliable_filtered <- renderDT({
      req(rv$unreliable_filtered)
      datatable(rv$unreliable_filtered, options = list(pageLength = 10, scrollX = TRUE))
    })

  })
}
