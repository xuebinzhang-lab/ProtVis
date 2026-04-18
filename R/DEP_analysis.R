#' DEP Analysis UI Module
#'
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing all UI elements for the DEP analysis module
#' @import shiny
#' @import bslib
#' @importFrom shinyWidgets switchInput progressBar
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom colourpicker colourInput
#' @importFrom bsicons bs_icon
#' @importFrom DT DTOutput
#' @name DEP_analysis_ui
#' @export
#'
DEP_analysis_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,

        shiny::div(
          style = "margin-bottom: 12px;",
          shiny::actionButton(
            ns("load_data"),
            "LOAD DATA",
            class = "btn btn-light fw-bold"
          )
        ),

        shiny::uiOutput(ns("load_status_panel")),

        shinyWidgets::progressBar(
          id = ns("load_progress"),
          value = 0,
          total = 100,
          display_pct = TRUE,
          striped = TRUE,
          status = "success",
          title = "Load progress"
        ),

        shiny::hr(),

        shiny::h5("Demo CompareGroup"),
        shiny::tags$small(
          "The example below shows the required two-column format: Group1 and Group2.",
          style = "color: #6c757d;"
        ),
        shiny::div(
          style = "margin-top: 8px; margin-bottom: 8px;",
          DT::DTOutput(ns("demo_compare_table"))
        ),
        shiny::actionButton(
          ns("use_demo_data"),
          "Use demo data",
          class = "btn btn-light fw-bold"
        ),

        shiny::hr(),

        shiny::tags$small(
          "CompareGroup requires two columns (Group1 & Group2) with different content.",
          style = "color: #6c757d"
        ),

        shinyWidgets::switchInput(
          inputId = ns("input_mode"),
          label = "CompareGroup",
          value = TRUE,
          onLabel = "Upload",
          offLabel = "Paste",
          width = "100%"
        ),

        shiny::conditionalPanel(
          condition = base::paste0("input['", ns("input_mode"), "'] == true"),
          shiny::tags$small("Upload CompareGroup", style = "color: #6c757d"),
          shiny::fileInput(
            inputId = ns("compare_file"),
            label = NULL,
            multiple = FALSE,
            accept = c(".csv", ".xlsx")
          )
        ),

        shiny::conditionalPanel(
          condition = base::paste0("input['", ns("input_mode"), "'] == false"),
          shiny::div(
            shiny::tags$small("Edit CompareGroup", style = "color: #6c757d"),
            rhandsontable::rHandsontableOutput(ns("hot_compare")),
            shiny::br(),
            shiny::tags$small("Paste CompareGroup", style = "color: #6c757d"),
            shiny::textAreaInput(
              inputId = ns("paste_data"),
              label = NULL,
              placeholder = "Copy and paste Excel data here.",
              rows = 5
            ),
            shiny::actionButton(
              ns("apply_paste"),
              "Apply paste data",
              class = "btn btn-light fw-bold")
          )
        ),

        shiny::hr(),

        shiny::actionButton(
          ns("run_dep"),
          "RUN DEP",
          class = "btn btn-light fw-bold"
        ),
        shiny::br(),
        shiny::br(),

        shiny::h5("DEP Analysis Progress"),
        shinyWidgets::progressBar(
          id = ns("dep_progress"),
          value = 0,
          total = 100,
          display_pct = TRUE,
          striped = TRUE,
          status = "warning",
          title = "DEP progress"
        )
      ),

      bslib::card(
        height = "600px",
        bslib::card_header("Data Preview"),
        bslib::navset_card_tab(
          full_screen = TRUE,

          bslib::nav_panel(
            "Sample Info",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              DT::DTOutput(ns("sample_info"))
            )
          ),

          bslib::nav_panel(
            "Normalized Data",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              DT::DTOutput(ns("normalized_data"))
            )
          ),

          bslib::nav_panel(
            "Group Comparison",
            shiny::div(
              style = "height: 500px; overflow: auto;",
              DT::DTOutput(ns("group_comparison"))
            )
          ),

          bslib::nav_panel(
            "DEP result",
            shiny::uiOutput(ns("dynamic_dep_tabs"))
          )
        )
      )
    )
  )
}

#' DEP Analysis Server Module
#'
#' @param id The namespace identifier for the module
#' @param shared_state A reactive list containing shared state variables across modules
#' @return A reactive list containing comparison data, normalized matrix, sample info, and DEP results
#' @import shiny
#' @importFrom dplyr filter pull select all_of mutate case_when count
#' @importFrom tibble rownames_to_column
#' @importFrom rhandsontable renderRHandsontable rhandsontable hot_table hot_to_r
#' @importFrom readxl read_excel
#' @importFrom tools file_ext
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom limma lmFit makeContrasts contrasts.fit eBayes topTable
#' @importFrom pheatmap pheatmap
#' @importFrom colourpicker colourInput
#' @importFrom ggplot2 ggsave
#' @importFrom shinyWidgets updateProgressBar
#' @name DEP_analysis_server
#' @export
#'
utils::globalVariables(c(
  "P.Value", "regulation", "logFC", "Cluster", "Var2", "Var1", "group", "sample_id"
))

DEP_analysis_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    coalesce_input <- function(x, default) {
      if (base::is.null(x) || base::length(x) == 0 || base::identical(x, "")) {
        default
      } else {
        x
      }
    }

    reset_dep_state <- function() {
      rv$dep_ready <- FALSE
      rv$dep_has_run <- FALSE
      rv$dep_results <- base::list()
      shinyWidgets::updateProgressBar(
        session = session,
        id = "dep_progress",
        value = 0,
        total = 100
      )
    }

    rv <- shiny::reactiveValues(
      sample_info = NULL,
      load_success = FALSE,
      normalized_matrix = NULL,
      compare_data = NULL,
      dep_results = base::list(),
      dep_ready = FALSE,
      dep_has_run = FALSE
    )

    demo_compare_data <- shiny::reactive({
      base::data.frame(
        Group1 = c("B73_Root_VE", "B73_Root_V2"),
        Group2 = c("Y12_Root_VE", "Y12_Root_V2"),
        stringsAsFactors = FALSE
      )
    })

    template_df <- shiny::reactive({
      demo_compare_data()
    })

    shiny::observe({
      shinyWidgets::updateProgressBar(
        session = session,
        id = "load_progress",
        value = 0,
        total = 100
      )
      shinyWidgets::updateProgressBar(
        session = session,
        id = "dep_progress",
        value = 0,
        total = 100
      )
    })

    output$demo_compare_table <- DT::renderDT({
      DT::datatable(
        demo_compare_data(),
        options = base::list(
          dom = "t",
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    shiny::observeEvent(input$use_demo_data, {
      rv$compare_data <- demo_compare_data()
      reset_dep_state()
      shiny::showNotification("✅ Demo CompareGroup loaded.", type = "message")
    })

    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)

      shinyWidgets::updateProgressBar(
        session = session,
        id = "load_progress",
        value = 10,
        total = 100
      )

      rda_path <- base::file.path(shared_state$workdir, "Step6_data_normalization.rda")

      if (base::file.exists(rda_path)) {
        shinyWidgets::updateProgressBar(
          session = session,
          id = "load_progress",
          value = 35,
          total = 100
        )

        e <- base::new.env()
        base::load(rda_path, envir = e)

        shinyWidgets::updateProgressBar(
          session = session,
          id = "load_progress",
          value = 70,
          total = 100
        )

        if (base::exists("sample_info", envir = e)) {
          rv$sample_info <- e$sample_info
        } else {
          rv$sample_info <- NULL
        }

        if (base::exists("normalized_data", envir = e)) {
          rv$normalized_matrix <- e$normalized_data
        } else {
          rv$normalized_matrix <- NULL
          shiny::showNotification(
            "Step6_data_normalization.rda does not contain normalized data.",
            type = "warning"
          )
        }

        rv$load_success <- TRUE
        reset_dep_state()

        shinyWidgets::updateProgressBar(
          session = session,
          id = "load_progress",
          value = 100,
          total = 100
        )

        shiny::showNotification("✅ Data loaded successfully.", type = "message")
      } else {
        rv$load_success <- FALSE
        shinyWidgets::updateProgressBar(
          session = session,
          id = "load_progress",
          value = 0,
          total = 100
        )
        shiny::showNotification("Step6_data_normalization.rda not found.", type = "error")
      }
    })

    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::span("✅ Data loaded", style = "color: green;")
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })

    output$hot_compare <- rhandsontable::renderRHandsontable({
      df <- if (!base::is.null(rv$compare_data)) rv$compare_data else template_df()

      rhandsontable::rhandsontable(df, stretchH = "all") %>%
        rhandsontable::hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })

    shiny::observeEvent(input$compare_file, {
      shiny::req(input$compare_file)

      ext <- tools::file_ext(input$compare_file$name)

      df <- tryCatch({
        if (ext == "csv") {
          utils::read.csv(
            input$compare_file$datapath,
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        } else if (ext %in% c("xls", "xlsx")) {
          as.data.frame(readxl::read_excel(input$compare_file$datapath))
        } else {
          NULL
        }
      }, error = function(e) {
        shiny::showNotification(
          base::paste("Failed to read file:", e$message),
          type = "error"
        )
        NULL
      })

      if (!base::is.null(df)) {
        rv$compare_data <- df
        reset_dep_state()
        shiny::showNotification("✅ Comparison group file loaded.", type = "message")
      }
    })

    shiny::observeEvent(input$apply_paste, {
      shiny::req(input$paste_data)

      tryCatch({
        df <- utils::read.table(
          text = input$paste_data,
          sep = "\t",
          header = TRUE,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        rv$compare_data <- df
        reset_dep_state()
        shiny::showNotification("✅ Pasted data applied.", type = "message")
      }, error = function(e) {
        shiny::showNotification("Invalid paste data format.", type = "error")
      })
    })

    shiny::observeEvent(input$hot_compare, {
      rv$compare_data <- rhandsontable::hot_to_r(input$hot_compare)
      reset_dep_state()
    })

    output$dynamic_dep_tabs <- shiny::renderUI({
      if (!isTRUE(rv$load_success)) {
        return(
          shiny::div(
            style = "padding:20px; color:#6c757d;",
            "Please click LOAD DATA first."
          )
        )
      }

      if (base::is.null(rv$compare_data)) {
        return(
          shiny::div(
            style = "padding:20px; color:#6c757d;",
            "Please upload, paste, or use demo CompareGroup."
          )
        )
      }

      compare_data <- rv$compare_data

      if (!base::all(c("Group1", "Group2") %in% base::colnames(compare_data))) {
        return(
          shiny::div(
            style = "padding:20px; color:#dc3545;",
            "CompareGroup must contain two columns: Group1 and Group2."
          )
        )
      }

      compare_data <- compare_data[
        stats::complete.cases(compare_data[, c("Group1", "Group2"), drop = FALSE]),
        ,
        drop = FALSE
      ]

      if (base::nrow(compare_data) == 0) {
        return(
          shiny::div(
            style = "padding:20px; color:#6c757d;",
            "No valid comparison groups found."
          )
        )
      }

      if (!isTRUE(rv$dep_has_run)) {
        return(
          shiny::div(
            style = "padding:20px; color:#6c757d;",
            "Please click RUN DEP after the data and CompareGroup are ready."
          )
        )
      }

      if (!isTRUE(rv$dep_ready)) {
        return(
          shiny::div(
            style = "padding:20px; color:#6c757d;",
            "DEP analysis is running or no valid result is available yet."
          )
        )
      }

      tabs <- base::lapply(seq_len(base::nrow(compare_data)), function(i) {
        group1 <- compare_data[i, "Group1"]
        group2 <- compare_data[i, "Group2"]
        tab_name <- base::paste(group1, "vs", group2)

        bslib::nav_panel(
          tab_name,
          bslib::layout_column_wrap(
            width = 1/2,
            height = 600,

            bslib::card(
              height = "800px",
              bslib::card_header(base::paste("DEP table -", tab_name)),
              bslib::card_body(DT::DTOutput(ns(base::paste0("dep_table_", i))))
            ),

            bslib::card(
              height = "800px",
              bslib::card_header(base::paste("Volcano plot -", tab_name)),
              bslib::card_body(
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    id = ns(base::paste0("volcano_sidebar_", i)),
                    position = "left",
                    open = TRUE,
                    width = 250,
                    bslib::accordion(
                      bslib::accordion_panel(
                        title = "Parameter",
                        icon = shiny::icon("correlation"),
                        shiny::numericInput(
                          ns(base::paste0("volcano_logfc_", i)),
                          "logFC threshold",
                          value = 0.27,
                          min = 0,
                          max = 5,
                          step = 0.1
                        ),
                        shiny::numericInput(
                          ns(base::paste0("volcano_pval_", i)),
                          "P-value threshold",
                          value = 0.05,
                          min = 0,
                          max = 1,
                          step = 0.01
                        ),
                        colourpicker::colourInput(
                          ns(base::paste0("color_up_", i)),
                          "Upregulated colour",
                          value = "#d62728"
                        ),
                        colourpicker::colourInput(
                          ns(base::paste0("color_down_", i)),
                          "Downregulated colour",
                          value = "#1f77b4"
                        ),
                        colourpicker::colourInput(
                          ns(base::paste0("color_ns_", i)),
                          "Not significant colour",
                          value = "#7f7f7f"
                        )
                      ),
                      bslib::accordion_panel(
                        title = "Download",
                        icon = bsicons::bs_icon("download"),
                        shiny::numericInput(
                          ns(base::paste0("go_width_", i)),
                          "Plot width (inch)",
                          value = 8,
                          min = 4,
                          max = 20
                        ),
                        shiny::numericInput(
                          ns(base::paste0("go_height_", i)),
                          "Plot height (inch)",
                          value = 6,
                          min = 4,
                          max = 20
                        ),
                        shiny::downloadButton(
                          ns(base::paste0("download_volcano_", i)),
                          "Download Plot"
                        )
                      )
                    )
                  ),
                  shiny::plotOutput(ns(base::paste0("volcano_plot_", i)))
                )
              )
            ),

            bslib::card(
              height = "800px",
              bslib::card_header(base::paste("Heatmap -", tab_name)),
              bslib::card_body(shiny::plotOutput(ns(base::paste0("heatmap_", i))))
            ),

            bslib::card(
              height = "800px",
              bslib::card_header(base::paste("Bar of DEP -", tab_name)),
              bslib::card_body(
                bslib::layout_sidebar(
                  sidebar = bslib::sidebar(
                    id = ns(base::paste0("bar_sidebar_", i)),
                    position = "left",
                    open = TRUE,
                    width = 250,
                    bslib::accordion(
                      bslib::accordion_panel(
                        title = "Parameter",
                        icon = shiny::icon("correlation"),
                        colourpicker::colourInput(
                          ns(base::paste0("bar_color_up_", i)),
                          "Upregulated colour",
                          value = "#d62728"
                        ),
                        colourpicker::colourInput(
                          ns(base::paste0("bar_color_down_", i)),
                          "Downregulated colour",
                          value = "#1f77b4"
                        )
                      ),
                      bslib::accordion_panel(
                        title = "Download",
                        icon = bsicons::bs_icon("download"),
                        shiny::numericInput(
                          ns(base::paste0("bar_width_", i)),
                          "Plot width (inch)",
                          value = 8,
                          min = 4,
                          max = 20
                        ),
                        shiny::numericInput(
                          ns(base::paste0("bar_height_", i)),
                          "Plot height (inch)",
                          value = 6,
                          min = 4,
                          max = 20
                        ),
                        shiny::downloadButton(
                          ns(base::paste0("download_bar_", i)),
                          "Download Plot"
                        )
                      )
                    )
                  ),
                  shiny::plotOutput(ns(base::paste0("bar_dep_", i)))
                )
              )
            )
          )
        )
      })

      bslib::navset_card_tab(full_screen = TRUE, !!!tabs)
    })

    shiny::observeEvent(input$run_dep, {
      shiny::req(rv$load_success, rv$compare_data, rv$normalized_matrix, rv$sample_info)

      if (!base::all(c("Group1", "Group2") %in% base::colnames(rv$compare_data))) {
        shiny::showNotification(
          "CompareGroup must contain Group1 and Group2 columns.",
          type = "error"
        )
        return(NULL)
      }

      compare_data <- rv$compare_data
      compare_data <- compare_data[
        stats::complete.cases(compare_data[, c("Group1", "Group2"), drop = FALSE]),
        ,
        drop = FALSE
      ]

      if (base::nrow(compare_data) == 0) {
        shiny::showNotification("No valid comparison groups found.", type = "error")
        return(NULL)
      }

      rv$dep_ready <- FALSE
      rv$dep_has_run <- FALSE
      rv$dep_results <- base::list()

      shinyWidgets::updateProgressBar(
        session = session,
        id = "dep_progress",
        value = 5,
        total = 100
      )

      total_n <- base::nrow(compare_data)
      valid_result_n <- 0

      for (i in seq_len(total_n)) {
        group1 <- as.character(compare_data[i, "Group1"])
        group2 <- as.character(compare_data[i, "Group2"])

        progress_value <- max(5, base::round((i - 1) / total_n * 100))
        shinyWidgets::updateProgressBar(
          session = session,
          id = "dep_progress",
          value = progress_value,
          total = 100
        )

        if (base::is.null(rv$sample_info) ||
            !base::all(c("group", "sample_id") %in% base::colnames(rv$sample_info))) {
          next
        }

        samples_group1 <- rv$sample_info %>%
          dplyr::filter(group == group1) %>%
          dplyr::pull(sample_id)

        samples_group2 <- rv$sample_info %>%
          dplyr::filter(group == group2) %>%
          dplyr::pull(sample_id)

        req_cols <- c(samples_group1, samples_group2)

        if (base::length(samples_group1) == 0 || base::length(samples_group2) == 0) {
          next
        }

        if (!base::all(req_cols %in% base::colnames(rv$normalized_matrix))) {
          next
        }

        exp_matrix <- rv$normalized_matrix %>%
          as.data.frame() %>%
          dplyr::select(dplyr::all_of(req_cols))

        if (base::ncol(exp_matrix) < 2 || base::nrow(exp_matrix) == 0) {
          next
        }

        local({
          i_local <- i
          g1 <- group1
          g2 <- group2
          exp_mat_local <- exp_matrix

          group_list <- base::rep(
            c(g1, g2),
            c(base::length(samples_group1), base::length(samples_group2))
          ) %>% factor(levels = c(g1, g2))

          design <- stats::model.matrix(~ factor(group_list) + 0)
          base::colnames(design) <- c(g1, g2)

          df.fit <- limma::lmFit(exp_mat_local, design)
          contrast <- limma::makeContrasts(
            contrasts = base::paste(g1, g2, sep = " - "),
            levels = design
          )
          fit <- limma::contrasts.fit(df.fit, contrast) %>% limma::eBayes()
          result <- limma::topTable(fit, n = Inf, adjust = "fdr")

          logfc_thresh <- coalesce_input(input[[base::paste0("volcano_logfc_", i_local)]], 1.0)
          pval_thresh <- coalesce_input(input[[base::paste0("volcano_pval_", i_local)]], 0.05)

          new_result <- result %>%
            dplyr::mutate(
              regulation = dplyr::case_when(
                logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                TRUE ~ "Not significant"
              )
            ) %>%
            tibble::rownames_to_column("ID") %>%
            dplyr::mutate(FC = 2 ^ logFC)

          rv$dep_results[[base::paste0(g1, "_vs_", g2)]] <- new_result

          output[[base::paste0("dep_table_", i_local)]] <- DT::renderDT({
            DT::datatable(
              new_result,
              options = base::list(
                scrollX = TRUE,
                pageLength = 10,
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel")
              ),
              extensions = "Buttons",
              rownames = FALSE
            )
          })

          output[[base::paste0("volcano_plot_", i_local)]] <- shiny::renderPlot({
            df <- rv$dep_results[[base::paste0(g1, "_vs_", g2)]]
            shiny::req(df)

            logfc_thresh <- coalesce_input(input[[base::paste0("volcano_logfc_", i_local)]], 0.5)
            pval_thresh <- coalesce_input(input[[base::paste0("volcano_pval_", i_local)]], 0.05)
            color_up <- coalesce_input(input[[base::paste0("color_up_", i_local)]], "#d62728")
            color_down <- coalesce_input(input[[base::paste0("color_down_", i_local)]], "#1f77b4")
            color_ns <- coalesce_input(input[[base::paste0("color_ns_", i_local)]], "#7f7f7f")

            df <- df %>%
              dplyr::mutate(
                regulation = dplyr::case_when(
                  logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                  logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                  TRUE ~ "Not significant"
                )
              )

            ggplot2::ggplot(df, ggplot2::aes(x = logFC, y = -base::log10(P.Value), color = regulation)) +
              ggplot2::geom_point(alpha = 0.8, size = 3) +
              ggplot2::scale_color_manual(values = c(
                "Upregulated" = color_up,
                "Downregulated" = color_down,
                "Not significant" = color_ns
              )) +
              ggplot2::theme_bw() +
              ggplot2::labs(x = "Log2 Fold Change", y = "-Log10(pvalue)", color = "") +
              ggplot2::theme(
                plot.title = ggplot2::element_text(hjust = 0.5),
                legend.position = "top"
              ) +
              ggplot2::geom_hline(
                yintercept = -base::log10(pval_thresh),
                linetype = "dashed",
                color = "black"
              ) +
              ggplot2::geom_vline(
                xintercept = c(-logfc_thresh, logfc_thresh),
                linetype = "dashed",
                color = "black"
              )
          })

          output[[base::paste0("download_volcano_", i_local)]] <- shiny::downloadHandler(
            filename = function() {
              base::paste0("Volcano_", g1, "_vs_", g2, ".pdf")
            },
            content = function(file) {
              df <- rv$dep_results[[base::paste0(g1, "_vs_", g2)]]

              logfc_thresh <- coalesce_input(input[[base::paste0("volcano_logfc_", i_local)]], 1.0)
              pval_thresh <- coalesce_input(input[[base::paste0("volcano_pval_", i_local)]], 0.05)
              color_up <- coalesce_input(input[[base::paste0("color_up_", i_local)]], "#d62728")
              color_down <- coalesce_input(input[[base::paste0("color_down_", i_local)]], "#1f77b4")
              color_ns <- coalesce_input(input[[base::paste0("color_ns_", i_local)]], "#7f7f7f")

              df <- df %>%
                dplyr::mutate(
                  regulation = dplyr::case_when(
                    logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                    logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                    TRUE ~ "Not significant"
                  )
                )

              g <- ggplot2::ggplot(df, ggplot2::aes(x = logFC, y = -base::log10(P.Value), color = regulation)) +
                ggplot2::geom_point(alpha = 0.8, size = 3) +
                ggplot2::scale_color_manual(values = c(
                  "Upregulated" = color_up,
                  "Downregulated" = color_down,
                  "Not significant" = color_ns
                )) +
                ggplot2::theme_bw() +
                ggplot2::labs(x = "Log2 Fold Change", y = "-Log10(pvalue)", color = "") +
                ggplot2::theme(
                  plot.title = ggplot2::element_text(hjust = 0.5),
                  legend.position = "top"
                ) +
                ggplot2::geom_hline(
                  yintercept = -base::log10(pval_thresh),
                  linetype = "dashed",
                  color = "black"
                ) +
                ggplot2::geom_vline(
                  xintercept = c(-logfc_thresh, logfc_thresh),
                  linetype = "dashed",
                  color = "black"
                )

              ggplot2::ggsave(
                file,
                g,
                width = coalesce_input(input[[base::paste0("go_width_", i_local)]], 8),
                height = coalesce_input(input[[base::paste0("go_height_", i_local)]], 6)
              )
            }
          )

          output[[base::paste0("heatmap_", i_local)]] <- shiny::renderPlot({
            df <- rv$dep_results[[base::paste0(g1, "_vs_", g2)]]
            shiny::req(df)

            sig_proteins <- df %>%
              dplyr::filter(regulation %in% c("Upregulated", "Downregulated")) %>%
              dplyr::pull(ID)

            if (base::length(sig_proteins) > 0) {
              heatmap_data <- exp_mat_local[
                base::rownames(exp_mat_local) %in% sig_proteins,
                ,
                drop = FALSE
              ]

              if (base::nrow(heatmap_data) > 1) {
                pheatmap::pheatmap(
                  heatmap_data,
                  scale = "row",
                  clustering_distance_rows = "euclidean",
                  clustering_distance_cols = "euclidean",
                  clustering_method = "complete",
                  show_rownames = FALSE,
                  main = base::paste("Heatmap:", g1, "vs", g2)
                )
              } else {
                ggplot2::ggplot() +
                  ggplot2::annotate(
                    "text",
                    x = 0.5,
                    y = 0.5,
                    label = "Not enough significant proteins for heatmap",
                    size = 6
                  ) +
                  ggplot2::theme_void()
              }
            } else {
              ggplot2::ggplot() +
                ggplot2::annotate(
                  "text",
                  x = 0.5,
                  y = 0.5,
                  label = "No significant proteins",
                  size = 8
                ) +
                ggplot2::theme_void()
            }
          })

          output[[base::paste0("bar_dep_", i_local)]] <- shiny::renderPlot({
            df <- rv$dep_results[[base::paste0(g1, "_vs_", g2)]]
            shiny::req(df)

            logfc_thresh <- coalesce_input(input[[base::paste0("volcano_logfc_", i_local)]], 1.0)
            pval_thresh <- coalesce_input(input[[base::paste0("volcano_pval_", i_local)]], 0.05)
            bar_color_up <- coalesce_input(input[[base::paste0("bar_color_up_", i_local)]], "#d62728")
            bar_color_down <- coalesce_input(input[[base::paste0("bar_color_down_", i_local)]], "#1f77b4")

            df <- df %>%
              dplyr::mutate(
                regulation = dplyr::case_when(
                  logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                  logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                  TRUE ~ "Not significant"
                )
              )

            dep_counts <- df %>%
              dplyr::filter(regulation %in% c("Upregulated", "Downregulated")) %>%
              dplyr::count(regulation)

            if (base::nrow(dep_counts) == 0) {
              ggplot2::ggplot() +
                ggplot2::annotate(
                  "text",
                  x = 0.5,
                  y = 0.5,
                  label = "No DEPs found",
                  size = 8
                ) +
                ggplot2::theme_void()
            } else {
              ggplot2::ggplot(dep_counts, ggplot2::aes(x = regulation, y = n, fill = regulation)) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::scale_fill_manual(values = c(
                  "Upregulated" = bar_color_up,
                  "Downregulated" = bar_color_down
                )) +
                ggplot2::labs(
                  title = base::paste("Number of DEPs:", g1, "vs", g2),
                  x = "Regulation",
                  y = "Count"
                ) +
                ggplot2::theme_bw() +
                ggplot2::theme(legend.position = "none")
            }
          })

          output[[base::paste0("download_bar_", i_local)]] <- shiny::downloadHandler(
            filename = function() {
              base::paste0("Bar_DEP_", g1, "_vs_", g2, ".pdf")
            },
            content = function(file) {
              df <- rv$dep_results[[base::paste0(g1, "_vs_", g2)]]

              logfc_thresh <- coalesce_input(input[[base::paste0("volcano_logfc_", i_local)]], 1.0)
              pval_thresh <- coalesce_input(input[[base::paste0("volcano_pval_", i_local)]], 0.05)
              bar_color_up <- coalesce_input(input[[base::paste0("bar_color_up_", i_local)]], "#d62728")
              bar_color_down <- coalesce_input(input[[base::paste0("bar_color_down_", i_local)]], "#1f77b4")

              df <- df %>%
                dplyr::mutate(
                  regulation = dplyr::case_when(
                    logFC > logfc_thresh & P.Value <= pval_thresh ~ "Upregulated",
                    logFC < -logfc_thresh & P.Value <= pval_thresh ~ "Downregulated",
                    TRUE ~ "Not significant"
                  )
                )

              dep_counts <- df %>%
                dplyr::filter(regulation %in% c("Upregulated", "Downregulated")) %>%
                dplyr::count(regulation)

              if (base::nrow(dep_counts) == 0) {
                g <- ggplot2::ggplot() +
                  ggplot2::annotate(
                    "text",
                    x = 0.5,
                    y = 0.5,
                    label = "No DEPs found",
                    size = 8
                  ) +
                  ggplot2::theme_void()
              } else {
                g <- ggplot2::ggplot(dep_counts, ggplot2::aes(x = regulation, y = n, fill = regulation)) +
                  ggplot2::geom_bar(stat = "identity") +
                  ggplot2::scale_fill_manual(values = c(
                    "Upregulated" = bar_color_up,
                    "Downregulated" = bar_color_down
                  )) +
                  ggplot2::labs(
                    title = base::paste("Number of DEPs:", g1, "vs", g2),
                    x = "Regulation",
                    y = "Count"
                  ) +
                  ggplot2::theme_bw() +
                  ggplot2::theme(legend.position = "none")
              }

              ggplot2::ggsave(
                file,
                g,
                width = coalesce_input(input[[base::paste0("bar_width_", i_local)]], 8),
                height = coalesce_input(input[[base::paste0("bar_height_", i_local)]], 6)
              )
            }
          )

          valid_result_n <<- valid_result_n + 1
        })
      }

      if (valid_result_n > 0) {
        rv$dep_ready <- TRUE
        rv$dep_has_run <- TRUE
        shinyWidgets::updateProgressBar(
          session = session,
          id = "dep_progress",
          value = 100,
          total = 100
        )
        shiny::showNotification("✅ DEP analysis completed.", type = "message")
      } else {
        rv$dep_ready <- FALSE
        rv$dep_has_run <- FALSE
        rv$dep_results <- base::list()
        shinyWidgets::updateProgressBar(
          session = session,
          id = "dep_progress",
          value = 0,
          total = 100
        )
        shiny::showNotification(
          "❌ No valid DEP result was generated. Please check CompareGroup and sample names.",
          type = "error"
        )
      }
    })

    output$sample_info <- DT::renderDT({
      if (base::is.null(rv$sample_info)) {
        return(NULL)
      }
      DT::datatable(
        rv$sample_info,
        options = base::list(scrollX = TRUE, dom = "t"),
        rownames = FALSE
      )
    })

    output$normalized_data <- DT::renderDT({
      if (base::is.null(rv$normalized_matrix)) {
        return(NULL)
      }
      DT::datatable(
        rv$normalized_matrix,
        options = base::list(scrollX = TRUE, dom = "t"),
        rownames = FALSE
      )
    })

    output$group_comparison <- DT::renderDT({
      if (base::is.null(rv$compare_data)) {
        return(NULL)
      }
      DT::datatable(
        rv$compare_data,
        options = base::list(scrollX = TRUE, dom = "t"),
        rownames = FALSE
      )
    })

    return(
      shiny::reactive({
        base::list(
          compare_data = rv$compare_data,
          normalized_matrix = rv$normalized_matrix,
          sample_info = rv$sample_info,
          dep_results = rv$dep_results
        )
      })
    )
  })
}
