#' Register a placeholder data source analysis server
#'
#' Shared implementation for data-source modules whose full processing pipeline
#' has not been implemented yet. The placeholder keeps the selected data source
#' from failing at runtime and explains the current feature status in all output
#' areas.
#'
#' @param id Module ID.
#' @param source_name Human-readable data source name.
#' @param shared_state Shared application reactive values.
#' @return None. Called for side effects in the Shiny session.
#' @keywords internal
register_placeholder_analysis_server <- function(id, source_name, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    status_data <- shiny::reactive({
      base::data.frame(
        Field = c("Data source", "Protein file", "Sample info", "Status"),
        Value = c(
          source_name,
          if (base::is.null(input$protein_file)) "Not uploaded" else input$protein_file$name,
          if (base::is.null(input$sample_info)) "Not uploaded" else input$sample_info$name,
          "The dedicated processing pipeline is not implemented yet. Upload controls are available, but downstream analysis outputs are placeholders."
        ),
        stringsAsFactors = FALSE
      )
    })

    output$group_select <- shiny::renderUI({
      shiny::tags$div(
        class = "alert alert-info",
        base::paste(
          source_name,
          "metadata parsing and group selection are pending implementation."
        )
      )
    })

    render_pending_plot <- function(plot_title) {
      shiny::renderPlot({
        graphics::plot.new()
        graphics::title(main = base::paste(source_name, plot_title))
        graphics::text(
          x = 0.5,
          y = 0.5,
          labels = "Processing pipeline pending implementation",
          cex = 1.1
        )
      })
    }

    output$umap_plot <- render_pending_plot("UMAP")
    output$heatmap_plot <- render_pending_plot("Heatmap")
    output$boxplot <- render_pending_plot("Boxplot")

    output$de_table <- DT::renderDT({
      DT::datatable(status_data(), options = base::list(dom = "t", paging = FALSE))
    })

    output$download_cleaned <- shiny::downloadHandler(
      filename = function() {
        base::paste0(base::tolower(source_name), "_status.csv")
      },
      content = function(file) {
        utils::write.csv(status_data(), file, row.names = FALSE)
      }
    )

    output$download_diff <- shiny::downloadHandler(
      filename = function() {
        base::paste0(base::tolower(source_name), "_de_status.csv")
      },
      content = function(file) {
        utils::write.csv(status_data(), file, row.names = FALSE)
      }
    )

    base::invisible(NULL)
  })
}

#' Shared data input interface styles
#'
#' @return A Shiny style tag for data-source modules.
#' @keywords internal
protvis_data_input_style <- function() {
  shiny::tags$style(shiny::HTML("\n    .pv-data-input-header { display: flex; justify-content: space-between; gap: 1rem; align-items: flex-end; margin: 1.25rem 0 1rem; padding: 1.25rem 1.5rem; border: 1px solid #d7e3ef; border-radius: 18px; background: linear-gradient(135deg, #ffffff 0%, #eef7ff 100%); box-shadow: 0 10px 30px rgba(15, 76, 117, 0.08); }\n    .pv-section-eyebrow { color: #0b84c6; font-size: 0.78rem; font-weight: 700; letter-spacing: 0.08em; text-transform: uppercase; }\n    .pv-page-title { margin: 0.25rem 0; color: #18324a; font-weight: 800; }\n    .pv-page-subtitle { margin: 0; max-width: 760px; color: #607080; }\n    .pv-source-pill { min-width: 210px; padding: 0.85rem 1rem; border-radius: 14px; background: #ffffff; border: 1px solid #cfe2f3; box-shadow: inset 0 0 0 1px rgba(255,255,255,0.6); }\n    .pv-source-label { display: block; color: #6c7a89; font-size: 0.78rem; margin-bottom: 0.2rem; }\n    .pv-mq-shell .sidebar { border-right: 0; }\n    .pv-sidebar-card { padding: 1rem; border: 1px solid #d9e7f2; border-radius: 16px; background: #ffffff; box-shadow: 0 8px 24px rgba(24, 50, 74, 0.08); }\n    .pv-load-button { width: 100%; border-radius: 12px; padding: 0.7rem 1rem; text-transform: uppercase; letter-spacing: 0.03em; }\n    .pv-status { display: flex; gap: 0.5rem; align-items: center; margin: 0.85rem 0 1rem; padding: 0.75rem; border-radius: 12px; font-weight: 700; }\n    .pv-status-ready { background: #eaf7ef; color: #177245; }\n    .pv-status-empty { background: #fff1f1; color: #c73535; }\n    .pv-filter-note { color: #5f6f7f; font-size: 0.82rem; margin-bottom: 0.75rem; }\n    .pv-action-row { display: grid; grid-template-columns: 1fr 1fr; gap: 0.5rem; }\n    .pv-preview-card { border: 1px solid #d7e3ef; border-radius: 18px; overflow: hidden; box-shadow: 0 12px 32px rgba(24, 50, 74, 0.08); }\n    .pv-card-header { display: flex; justify-content: space-between; gap: 1rem; align-items: center; background: #ffffff; }\n    .pv-card-title { margin: 0; font-weight: 800; color: #18324a; }\n    .pv-card-subtitle { margin: 0.15rem 0 0; color: #6c7a89; font-size: 0.9rem; }\n    .pv-file-input .form-group { margin-bottom: 0.9rem; }\n    .pv-download-row { display: grid; grid-template-columns: 1fr; gap: 0.55rem; }\n    @media (max-width: 900px) { .pv-data-input-header, .pv-card-header { align-items: stretch; flex-direction: column; } .pv-source-pill { min-width: 0; } }\n  "))
}

#' Shared tabular data-source UI
#'
#' @param id Module ID.
#' @param source_name Data-source display name.
#' @param source_label Primary file upload label.
#' @param source_accept Accepted primary file extensions.
#' @return A Shiny UI tag list.
#' @keywords internal
tabular_data_source_ui <- function(id, source_name, source_label, source_accept) {
  ns <- shiny::NS(id)
  shiny::tagList(
    protvis_data_input_style(),
    bslib::layout_sidebar(
      class = "pv-mq-shell",
      sidebar = bslib::sidebar(
        width = 320,
        shiny::div(
          class = "pv-sidebar-card",
          bslib::accordion(
            open = "File upload",
            bslib::accordion_panel(
              title = "File upload",
              icon = bsicons::bs_icon("upload"),
              shiny::div(
                class = "pv-file-input",
                shiny::fileInput(ns("protein_file"), source_label, multiple = FALSE, accept = source_accept),
                shiny::actionButton(ns("load_data"), base::paste("Parse", source_name, "output"), class = "btn btn-primary fw-bold pv-load-button"),
                shiny::fileInput(ns("sample_info"), "Upload sample info (xlsx, xls, csv, txt or tsv)", multiple = FALSE, accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv"))
              )
            ),
            bslib::accordion_panel(
              title = "Preprocessing options",
              icon = bsicons::bs_icon("gear"),
              shiny::checkboxInput(ns("log2_transform"), "Log2 transformation", TRUE),
              shiny::checkboxInput(ns("knn_impute"), "KNN imputation for missing values", TRUE),
              shiny::numericInput(ns("knn_k"), "K for KNN imputation", value = 10, min = 1, max = 50, step = 1),
              shiny::checkboxInput(ns("normalize"), "Tissue-centered normalization", TRUE)
            ),
            bslib::accordion_panel(
              title = "Differential expression",
              icon = bsicons::bs_icon("filter-circle"),
              shiny::uiOutput(ns("group_select")),
              shiny::numericInput(ns("logFC_cutoff"), "Log2 fold-change threshold", value = 1, min = 0, step = 0.1),
              shiny::numericInput(ns("adj_pval"), "Adjusted P-value (FDR) threshold", value = 0.05, min = 0, max = 1, step = 0.01)
            ),
            bslib::accordion_panel(
              title = "Export results",
              icon = bsicons::bs_icon("download"),
              shiny::div(
                class = "pv-download-row",
                shiny::downloadButton(ns("download_cleaned"), "Download cleaned data", class = "btn btn-outline-primary"),
                shiny::downloadButton(ns("download_diff"), "Download sample info", class = "btn btn-outline-primary")
              )
            )
          )
        )
      ),
      bslib::card(
        class = "pv-preview-card",
        bslib::card_header(
          class = "pv-card-header",
          shiny::div(
            shiny::tags$h4(base::paste(source_name, "processing preview"), class = "pv-card-title"),
            shiny::tags$p("Review parsed expression data, sample metadata, and quality visualizations.", class = "pv-card-subtitle")
          )
        ),
        bslib::card_body(
          fill = TRUE,
          bslib::navset_tab(
            bslib::nav_panel("PCA", shiny::plotOutput(ns("umap_plot"))),
            bslib::nav_panel("Heatmap", shiny::plotOutput(ns("heatmap_plot"))),
            bslib::nav_panel("Boxplot", shiny::plotOutput(ns("boxplot"))),
            bslib::nav_panel("Expression Matrix", DT::DTOutput(ns("de_table")))
          )
        )
      )
    )
  )
}
