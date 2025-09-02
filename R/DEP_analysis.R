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

    # helper: replace %||% behaviour
    coalesce_input <- function(x, default) {
      if (is.null(x) || length(x) == 0) default else x
    }

    rv <- reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      compare_data = NULL,
      dep_results = list()  # Store DEP results
    )

    # --- Template for compare_data ---
    template_df <- reactive({
      data.frame(
        Group1 = c(NA, NA, NA),
        Group2 = c(NA, NA, NA),
        stringsAsFactors = FALSE
      )
    })

    # --- Data loading ---
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
          showNotification("Step6_data_normalization.rda does not contain normalized data.", type = "warning")
        }
        rv$load_success <- TRUE
        showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        showNotification("Step6_data_normalization.rda not found.", type = "error")
      }
    })

    # --- Load status ---
    output$load_status_panel <- renderUI({
      if (rv$load_success) {
        span("✅ Data loaded", style = "color: green;")
      } else {
        span("❌ Data not loaded", style = "color: red;")
      }
    })

    # --- Editable hot table ---
    output$hot_compare <- rhandsontable::renderRHandsontable({
      df <- if(!is.null(rv$compare_data)) rv$compare_data else template_df()
      rhandsontable::rhandsontable(df, stretchH = "all") %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    # --- File upload ---
    observeEvent(input$compare_file, {
      req(input$compare_file)
      ext <- tools::file_ext(input$compare_file$name)
      df <- tryCatch({
        if(ext == "csv") {
          read.csv(input$compare_file$datapath)
        } else if(ext %in% c("xls", "xlsx")) {
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

    # --- Paste data ---
    observeEvent(input$apply_paste, {
      req(input$paste_data)
      tryCatch({
        df <- read.table(text = input$paste_data, sep = "\t", header = TRUE)
        rv$compare_data <- df
        showNotification("Pasted data applied!", type = "message")
      }, error = function(e) {
        showNotification("Invalid paste data format.", type = "error")
      })
    })

    # --- Sync hot table changes ---
    observeEvent(input$hot_compare, {
      rv$compare_data <- rhandsontable::hot_to_r(input$hot_compare)
    })

    # --- Dynamic tabs for DEP results ---
    output$dynamic_dep_tabs <- renderUI({
      req(rv$compare_data)
      compare_data <- rv$compare_data[complete.cases(rv$compare_data), ]
      if(nrow(compare_data) == 0) return(tags$p("No valid comparison groups found."))

      tabs <- lapply(1:nrow(compare_data), function(i) {
        group1 <- compare_data[i, "Group1"]
        group2 <- compare_data[i, "Group2"]
        tab_name <- paste(group1, "vs", group2)

        nav_panel(
          tab_name,
          layout_column_wrap(
            width = 1/2,
            height = 600,

            # DEP table
            card(
              height = "800px",
              card_header(paste("DEP table -", tab_name)),
              card_body(DT::dataTableOutput(ns(paste0("dep_table_", i))))
            ),

            # Volcano plot
            card(
              height = "800px",
              card_header(paste("Volcano plot -", tab_name)),
              card_body(
                layout_sidebar(
                  sidebar = sidebar(
                    id = ns(paste0("volcano_sidebar_", i)),
                    position = "left",
                    open = TRUE,
                    width = 250,
                    accordion(
                      accordion_panel(
                        title = "Parameter",
                        icon = correlation_icon,
                        numericInput(ns(paste0("volcano_logfc_", i)), "logFC threshold", value = 1.0, min = 0, max = 5, step = 0.1),
                        numericInput(ns(paste0("volcano_pval_", i)), "P-value threshold", value = 0.05, min = 0, max = 1, step = 0.01),
                        colourpicker::colourInput(ns(paste0("color_up_", i)), "Upregulated colour", value = "#d62728"),
                        colourpicker::colourInput(ns(paste0("color_down_", i)), "Downregulated colour", value = "#1f77b4"),
                        colourpicker::colourInput(ns(paste0("color_ns_", i)), "Not significant colour", value = "#7f7f7f")
                      ),
                      accordion_panel(
                        title = "Download",
                        icon = bs_icon("download"),
                        numericInput(ns(paste0("go_width_", i)), "Plot width (inch)", value = 8, min = 4, max = 20),
                        numericInput(ns(paste0("go_height_", i)), "Plot height (inch)", value = 6, min = 4, max = 20),
                        downloadButton(ns(paste0("download_volcano_", i)), "Download Plot")
                      )
                    )
                  ),
                  plotOutput(ns(paste0("volcano_plot_", i)))
                )
              )
            ),

            # Heatmap
            card(
              height = "800px",
              card_header(paste("Heatmap -", tab_name)),
              card_body(plotOutput(ns(paste0("heatmap_", i))))
            ),

            # Bar of DEP (with sidebar)
            card(
              height = "800px",
              card_header(paste("Bar of DEP -", tab_name)),
              card_body(
                layout_sidebar(
                  sidebar = sidebar(
                    id = ns(paste0("bar_sidebar_", i)),
                    position = "left",
                    open = TRUE,
                    width = 250,
                    accordion(
                      accordion_panel(
                        title = "Parameter",
                        icon = correlation_icon,
                        colourpicker::colourInput(ns(paste0("bar_color_up_", i)), "Upregulated colour", value = "#d62728"),
                        colourpicker::colourInput(ns(paste0("bar_color_down_", i)), "Downregulated colour", value = "#1f77b4")
                      ),
                      accordion_panel(
                        title = "Download",
                        icon = bs_icon("download"),
                        numericInput(ns(paste0("bar_width_", i)), "Plot width (inch)", value = 8, min = 4, max = 20),
                        numericInput(ns(paste0("bar_height_", i)), "Plot height (inch)", value = 6, min = 4, max = 20),
                        downloadButton(ns(paste0("download_bar_", i)), "Download Plot")
                      )
                    )
                  ),
                  plotOutput(ns(paste0("bar_dep_", i)))
                )
              )
            )
          )
        )
      })

      navset_card_tab(full_screen = TRUE, !!!tabs)
    })

    # --- DEP analysis and plots ---
    observe({
      req(rv$compare_data, rv$normalized_matrix, rv$sample_info)
      compare_data <- rv$compare_data[complete.cases(rv$compare_data), ]

      if(nrow(compare_data) > 0) {
        lapply(1:nrow(compare_data), function(i) {
          local({
            i_local <- i
            group1 <- compare_data[i_local, "Group1"]
            group2 <- compare_data[i_local, "Group2"]

            samples_group1 <- rv$sample_info %>% dplyr::filter(group == group1) %>% dplyr::pull(sample_id)
            samples_group2 <- rv$sample_info %>% dplyr::filter(group == group2) %>% dplyr::pull(sample_id)
            exp_matrix <- rv$normalized_matrix %>% as.data.frame() %>% dplyr::select(all_of(c(samples_group1, samples_group2)))

            # limma
            group_list <- rep(c(group1, group2), c(length(samples_group1), length(samples_group2))) %>% factor(levels = c(group1, group2))
            design <- model.matrix(~factor(group_list)+0)
            colnames(design) <- c(group1, group2)
            df.fit <- limma::lmFit(exp_matrix, design)
            contrast <- limma::makeContrasts(contrasts = paste(group1, group2, sep = " - "), levels = design)
            fit <- limma::contrasts.fit(df.fit, contrast) %>% limma::eBayes()
            result <- limma::topTable(fit, n = Inf, adjust = "fdr")

            new_result <- result %>%
              dplyr::mutate(regulation = case_when(
                logFC > 1 & P.Value <= 0.05 ~ "Upregulated",
                logFC < -1 & P.Value <= 0.05 ~ "Downregulated",
                TRUE ~ "Not significant"
              )) %>%
              tibble::rownames_to_column("ID") %>%
              dplyr::mutate(FC = 2^logFC)

            rv$dep_results[[paste0(group1, "_vs_", group2)]] <- new_result

            # DEP table
            output[[paste0("dep_table_", i_local)]] <- DT::renderDataTable({
              DT::datatable(new_result, options = list(scrollX = TRUE, pageLength = 10,
                                                       dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel')),
                            extensions = 'Buttons', rownames = FALSE)
            })

            # Volcano plot
            output[[paste0("volcano_plot_", i_local)]] <- renderPlot({
              req(rv$dep_results[[paste0(group1, "_vs_", group2)]])
              df <- rv$dep_results[[paste0(group1, "_vs_", group2)]]

              logfc_thresh <- coalesce_input(input[[paste0("volcano_logfc_", i_local)]], 1.0)
              pval_thresh <- coalesce_input(input[[paste0("volcano_pval_", i_local)]], 0.05)
              color_up <- coalesce_input(input[[paste0("color_up_", i_local)]], "#d62728")
              color_down <- coalesce_input(input[[paste0("color_down_", i_local)]], "#1f77b4")
              color_ns <- coalesce_input(input[[paste0("color_ns_", i_local)]], "#7f7f7f")

              df <- df %>% dplyr::mutate(
                regulation = case_when(
                  logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                  logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                  TRUE ~ "Not significant"
                )
              )

              ggplot(df, aes(x = logFC, y = -log10(P.Value), color = regulation)) +
                geom_point(alpha = 0.8, size = 3) +
                scale_color_manual(values = c("Upregulated" = color_up,
                                              "Downregulated" = color_down,
                                              "Not significant" = color_ns)) +
                theme_bw() +
                labs(x = "Log2 Fold Change", y = "-Log10(pvalue)", color = "") +
                theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
                geom_hline(yintercept = -log10(pval_thresh), linetype = "dashed", color = "black") +
                geom_vline(xintercept = c(-logfc_thresh, logfc_thresh), linetype = "dashed", color = "black")
            })

            # Volcano download
            output[[paste0("download_volcano_", i_local)]] <- downloadHandler(
              filename = function() {
                paste0("Volcano_", group1, "_vs_", group2, ".pdf")
              },
              content = function(file) {
                df <- rv$dep_results[[paste0(group1, "_vs_", group2)]]
                logfc_thresh <- coalesce_input(input[[paste0("volcano_logfc_", i_local)]], 1.0)
                pval_thresh <- coalesce_input(input[[paste0("volcano_pval_", i_local)]], 0.05)
                color_up <- coalesce_input(input[[paste0("color_up_", i_local)]], "#d62728")
                color_down <- coalesce_input(input[[paste0("color_down_", i_local)]], "#1f77b4")
                color_ns <- coalesce_input(input[[paste0("color_ns_", i_local)]], "#7f7f7f")

                df <- df %>% dplyr::mutate(
                  regulation = case_when(
                    logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                    logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                    TRUE ~ "Not significant"
                  )
                )

                g <- ggplot(df, aes(x = logFC, y = -log10(P.Value), color = regulation)) +
                  geom_point(alpha = 0.8, size = 3) +
                  scale_color_manual(values = c("Upregulated" = color_up,
                                                "Downregulated" = color_down,
                                                "Not significant" = color_ns)) +
                  theme_bw() +
                  labs(x = "Log2 Fold Change", y = "-Log10(pvalue)", color = "") +
                  theme(plot.title = element_text(hjust = 0.5), legend.position = "top") +
                  geom_hline(yintercept = -log10(pval_thresh), linetype = "dashed", color = "black") +
                  geom_vline(xintercept = c(-logfc_thresh, logfc_thresh), linetype = "dashed", color = "black")

                ggsave(
                  filename = file, plot = g,
                  width = coalesce_input(input[[paste0("go_width_", i_local)]], 8),
                  height = coalesce_input(input[[paste0("go_height_", i_local)]], 6),
                  units = "in"
                )
              }
            )

            # Heatmap
            output[[paste0("heatmap_", i_local)]] <- renderPlot({
              sig_proteins <- new_result %>% dplyr::filter(regulation %in% c("Upregulated", "Downregulated")) %>% dplyr::pull(ID)
              if(length(sig_proteins) > 0) {
                heatmap_data <- exp_matrix[rownames(exp_matrix) %in% sig_proteins, ]
                pheatmap::pheatmap(heatmap_data, scale = "row",
                                   clustering_distance_rows = "euclidean",
                                   clustering_distance_cols = "euclidean",
                                   clustering_method = "complete",
                                   show_rownames = FALSE,
                                   main = paste("Heatmap:", group1, "vs", group2))
              } else {
                ggplot() + annotate("text", x = 0.5, y = 0.5, label = "No significant proteins", size = 8) + theme_void()
              }
            })

            # Barplot (uses bar colors)
            # Bar plot
            output[[paste0("bar_dep_", i_local)]] <- renderPlot({
              req(rv$dep_results[[paste0(group1, "_vs_", group2)]])
              df <- rv$dep_results[[paste0(group1, "_vs_", group2)]]

              logfc_thresh <- coalesce_input(input[[paste0("volcano_logfc_", i_local)]], 1.0)
              pval_thresh  <- coalesce_input(input[[paste0("volcano_pval_", i_local)]], 0.05)
              color_up     <- coalesce_input(input[[paste0("bar_color_up_", i_local)]], "#d62728")
              color_down   <- coalesce_input(input[[paste0("bar_color_down_", i_local)]], "#1f77b4")

              df <- df %>% dplyr::mutate(
                regulation = case_when(
                  logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                  logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                  TRUE ~ "Not significant"
                )
              )

              dep_counts <- df %>%
                filter(regulation %in% c("Upregulated", "Downregulated")) %>%
                count(regulation)

              ggplot(dep_counts, aes(x = regulation, y = n, fill = regulation)) +
                geom_bar(stat = "identity") +
                scale_fill_manual(values = c("Upregulated" = color_up,
                                             "Downregulated" = color_down)) +
                labs(title = paste("Number of DEPs:", group1, "vs", group2),
                     x = "Regulation", y = "Count") +
                theme_bw() + theme(legend.position = "none")
            })

            # Bar download
            output[[paste0("download_bar_", i_local)]] <- downloadHandler(
              filename = function() {
                paste0("Bar_DEP_", group1, "_vs_", group2, ".pdf")
              },
              content = function(file) {
                df <- rv$dep_results[[paste0(group1, "_vs_", group2)]]

                logfc_thresh <- coalesce_input(input[[paste0("volcano_logfc_", i_local)]], 1.0)
                pval_thresh  <- coalesce_input(input[[paste0("volcano_pval_", i_local)]], 0.05)
                color_up     <- coalesce_input(input[[paste0("bar_color_up_", i_local)]], "#d62728")
                color_down   <- coalesce_input(input[[paste0("bar_color_down_", i_local)]], "#1f77b4")

                df <- df %>% dplyr::mutate(
                  regulation = case_when(
                    logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                    logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                    TRUE ~ "Not significant"
                  )
                )

                dep_counts <- df %>%
                  filter(regulation %in% c("Upregulated", "Downregulated")) %>%
                  count(regulation)

                g <- ggplot(dep_counts, aes(x = regulation, y = n, fill = regulation)) +
                  geom_bar(stat = "identity") +
                  scale_fill_manual(values = c("Upregulated" = color_up,
                                               "Downregulated" = color_down)) +
                  labs(title = paste("Number of DEPs:", group1, "vs", group2),
                       x = "Regulation", y = "Count") +
                  theme_bw() + theme(legend.position = "none")

                ggsave(file, g,
                       width  = coalesce_input(input[[paste0("bar_width_", i_local)]], 8),
                       height = coalesce_input(input[[paste0("bar_height_", i_local)]], 6))
              }
            )

          }) # end local
        }) # end lapply
      }
    })

    # --- Preview tables ---
    output$sample_info <- DT::renderDataTable({
      req(rv$sample_info)
      DT::datatable(rv$sample_info, options = list(scrollX = TRUE, dom = 't'), rownames = FALSE)
    })

    output$normalized_data <- DT::renderDataTable({
      req(rv$normalized_matrix)
      DT::datatable(rv$normalized_matrix, options = list(scrollX = TRUE, dom = 't'), rownames = FALSE)
    })

    output$group_comparison <- DT::renderDataTable({
      req(rv$compare_data)
      DT::datatable(rv$compare_data, options = list(scrollX = TRUE, dom = 't'), rownames = FALSE)
    })

    # --- Return state ---
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



