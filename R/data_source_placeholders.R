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
