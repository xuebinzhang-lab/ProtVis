#' DEP Analysis UI Module
#'
#' Creates the user interface for the Differential Expression Protein (DEP) analysis module.
#' This module allows users to load data, specify comparison groups, and view analysis results.
#'
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing all UI elements for the DEP analysis module
#' @export
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

      # Main display panel (right side)
      card(
        height = "600px",
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
            uiOutput(ns("dynamic_dep_tabs"))
          )
        )
      )
    )
  )
}
#' DEP Analysis Server Module
#'
#' Server-side logic for the Differential Expression Protein (DEP) analysis module.
#' Handles data loading, comparison group specification, differential expression analysis,
#' and visualization of results.
#'
#' @param id The namespace identifier for the module
#' @param shared_state A reactive list containing shared state variables across modules
#' @return A reactive list containing comparison data, normalized matrix, sample info, and DEP results
#' @export
DEP_analysis_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      compare_data = NULL,
      input_mode = TRUE,
      dep_results = list()  # Store DEP results for each comparison
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

    # Render editable hot table
    output$hot_compare <- rhandsontable::renderRHandsontable({
      df <- if(!is.null(rv$compare_data)) rv$compare_data else template_df()
      rhandsontable::rhandsontable(df, stretchH = "all") %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # Handle file upload
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

    # Handle paste data
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

    # Sync hot table changes to data
    observeEvent(input$hot_compare, {
      rv$compare_data <- rhandsontable::hot_to_r(input$hot_compare)
    })

    # Generate dynamic DEP tabs based on comparison groups
    output$dynamic_dep_tabs <- renderUI({
      req(rv$compare_data)

      compare_data <- rv$compare_data[complete.cases(rv$compare_data), ]
      if(nrow(compare_data) == 0) {
        return(tags$p("No valid comparison groups found."))
      }

      # Create a tabset for each comparison
      tabs <- lapply(1:nrow(compare_data), function(i) {
        group1 <- compare_data[i, "Group1"]
        group2 <- compare_data[i, "Group2"]
        tab_name <- paste(group1, "vs", group2)

        nav_panel(
          tab_name,
          layout_column_wrap(
            width = 1/2,
            height = 600,
            card(
              height = "800px",
              card_header(paste("DEP table -", tab_name)),
              card_body(
                DT::dataTableOutput(ns(paste0("dep_table_", i)))
              )
            ),
            card(
              height = "800px",
              card_header(paste("Volcano plot -", tab_name)),
              card_body(
                plotOutput(ns(paste0("volcano_plot_", i)))
              )
            ),
            card(
              height = "800px",
              card_header(paste("Heatmap -", tab_name)),
              card_body(
                plotOutput(ns(paste0("heatmap_", i)))
              )
            ),
            card(
              height = "800px",
              card_header(paste("Bar of DEP -", tab_name)),
              card_body(
                plotOutput(ns(paste0("bar_dep_", i)))
              )
            )
          )
        )
      })

      navset_card_tab(full_screen = TRUE, !!!tabs)
    })

    # Perform DEP analysis and generate plots for each comparison
    observe({
      req(rv$compare_data, rv$normalized_matrix, rv$sample_info)
      compare_data <- rv$compare_data[complete.cases(rv$compare_data), ]

      if(nrow(compare_data) > 0) {
        lapply(1:nrow(compare_data), function(i) {
          group1 <- compare_data[i, "Group1"]
          group2 <- compare_data[i, "Group2"]

          # Get samples
          samples_group1 <- rv$sample_info %>%
            dplyr::filter(group == group1) %>%
            dplyr::pull(sample_id)
          samples_group2 <- rv$sample_info %>%
            dplyr::filter(group == group2) %>%
            dplyr::pull(sample_id)

          # Subset expression matrix
          exp_matrix <- rv$normalized_matrix %>%
            as.data.frame() %>%
            dplyr::select(all_of(c(samples_group1, samples_group2)))

          # limma analysis
          group_list <- rep(c(group1, group2),
                            c(length(samples_group1), length(samples_group2))) %>%
            factor(., levels = c(group1, group2), ordered = F)
          design <- model.matrix(~factor(group_list)+0)
          colnames(design) <- c(group1, group2)

          df.fit <- limma::lmFit(exp_matrix, design)
          contrast <- limma::makeContrasts(contrasts = paste(group1, group2, sep = " - "),
                                           levels = design)
          fit <- limma::contrasts.fit(df.fit, contrast)
          fit <- limma::eBayes(fit)
          result <- limma::topTable(fit, n = Inf, adjust = "fdr")

          # Add regulation
          new_result <- result %>%
            dplyr::mutate(
              regulation = case_when(
                logFC > 1 & P.Value <= 0.05 ~ "Upregulated",
                logFC < -1 & P.Value <= 0.05 ~ "Downregulated",
                TRUE ~ "Not significant"
              )
            ) %>%
            tibble::rownames_to_column("ID") %>%
            dplyr::mutate(FC = 2^logFC)

          rv$dep_results[[paste0(group1, "_vs_", group2)]] <- new_result

          # === 自动保存 ===
          tryCatch({
            save_path <- file.path(shared_state$workdir, "Step7_DEP_result.rda")
            compare_data2 <- rv$compare_data
            normalized_matrix2 <- rv$normalized_matrix
            sample_info2 <- rv$sample_info
            dep_results2 <- rv$dep_results
            save(
              compare_data2,
              normalized_matrix2,
              sample_info2,
              dep_results2,
              file = save_path
            )
            showNotification(paste("✅ DEP results saved to", save_path), type = "message")
          }, error = function(e) {
            showNotification(paste("❌ Failed to save results:", e$message), type = "error")
          })

          # Render DEP table
          output[[paste0("dep_table_", i)]] <- DT::renderDataTable({
            DT::datatable(
              new_result,
              options = list(scrollX = TRUE, pageLength = 10,
                             dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
              extensions = 'Buttons',
              rownames = FALSE
            )
          })

          # Volcano plot
          output[[paste0("volcano_plot_", i)]] <- renderPlot({
            ggplot(new_result, aes(x = logFC, y = -log10(P.Value), color = regulation)) +
              geom_point(alpha = 0.8, size = 3) +
              scale_color_manual(values = c("Upregulated" = "red",
                                            "Downregulated" = "blue",
                                            "Not significant" = "gray")) +
              theme_bw() +
              labs(x = "Log2 Fold Change", y = "-Log10(pvalue)", color = "") +
              theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
              geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
              geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black")
          })

          # Heatmap
          output[[paste0("heatmap_", i)]] <- renderPlot({
            sig_proteins <- new_result %>%
              filter(regulation %in% c("Upregulated", "Downregulated")) %>%
              pull(ID)
            if(length(sig_proteins) > 0) {
              heatmap_data <- exp_matrix[rownames(exp_matrix) %in% sig_proteins, ]
              pheatmap::pheatmap(
                heatmap_data, scale = "row",
                clustering_distance_rows = "euclidean",
                clustering_distance_cols = "euclidean",
                clustering_method = "complete",
                show_rownames = FALSE,
                main = paste("Heatmap:", group1, "vs", group2)
              )
            } else {
              ggplot() + annotate("text", x = 0.5, y = 0.5,
                                  label = "No significant proteins", size = 8) +
                theme_void()
            }
          })

          # Barplot
          output[[paste0("bar_dep_", i)]] <- renderPlot({
            dep_counts <- new_result %>%
              filter(regulation != "Not significant") %>%
              count(regulation)
            ggplot(dep_counts, aes(x = regulation, y = n, fill = regulation)) +
              geom_bar(stat = "identity") +
              scale_fill_manual(values = c("Upregulated" = "red",
                                           "Downregulated" = "blue")) +
              labs(title = paste("Number of DEPs:", group1, "vs", group2),
                   x = "Regulation", y = "Count") +
              theme_bw() + theme(legend.position = "none")
          })
        })
      }
    })

    # Preview sample info
    output$sample_info <- DT::renderDataTable({
      req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE, dom = 't'), rownames = FALSE)
    })

    # Preview normalized data
    output$normalized_data <- DT::renderDataTable({
      req(rv$normalized_matrix)
      DT::datatable(rv$normalized_matrix, options = list(scrollX = TRUE, dom = 't'), rownames = FALSE)
    })

    # Preview group comparison
    output$group_comparison <- DT::renderDataTable({
      req(rv$compare_data)
      DT::datatable(rv$compare_data, options = list(scrollX = TRUE, dom = 't'), rownames = FALSE)
    })

    # Return comparison data
    return(
      reactive({
        list(
          compare_data = rv$compare_data,
          normalized_matrix = rv$normalized_matrix,
          sample_info = rv$sample_info,
          dep_results = rv$dep_results
        )
      })
    )
  })
}
