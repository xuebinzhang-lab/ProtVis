#' Help UI Module
#'
#' Creates the Help navigation panel for ProtVis. This panel provides
#' workflow guidance, module descriptions, usage tips, and documentation
#' information for users.
#'
#' @return A `bslib::nav_panel()` object for the Help page.
#' @import shiny
#' @importFrom bslib nav_panel layout_columns card card_header card_body
#' @importFrom bsicons bs_icon
#' @name help_ui
#' @export
#'
help_ui <- function() {
  bslib::nav_panel(
    "Help",
    icon = bsicons::bs_icon("question-circle"),
    shiny::div(
      class = "protvis-page",
      shiny::div(
        class = "pv-data-input-header",
        shiny::div(
          shiny::span("Help center", class = "pv-section-eyebrow"),
          shiny::tags$h2("ProtVis workflow guide", class = "pv-page-title"),
          shiny::tags$p(
            "Follow the recommended workflow, learn each module purpose, and use the built-in examples where available to quickly verify plots and tables.",
            class = "pv-page-subtitle"
          )
        ),
        shiny::div(
          class = "pv-source-pill",
          shiny::span("Recommended start", class = "pv-source-label"),
          shiny::strong("Project init → Data input")
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::card(
          class = "pv-preview-card",
          bslib::card_header("1. Prepare data"),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Create a project and confirm the working directory."),
              shiny::tags$li("Import MaxQuant, Proteome Discoverer, Skyline, Mascot, OpenMS, or custom matrices."),
              shiny::tags$li("Use example data buttons in supported modules to test the workflow.")
            )
          )
        ),
        bslib::card(
          class = "pv-preview-card",
          bslib::card_header("2. Process and analyze"),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Run filtering, transformation, imputation, and normalization in order."),
              shiny::tags$li("Inspect every preview table before moving downstream."),
              shiny::tags$li("Use DEP, enrichment, GSEA, and pathway views for interpretation.")
            )
          )
        ),
        bslib::card(
          class = "pv-preview-card",
          bslib::card_header("3. Visualize and export"),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Explore expression profiles, multi-omics, PTM, protein structure, and toolkit plots."),
              shiny::tags$li("Download publication-ready figures and processed tables."),
              shiny::tags$li(shiny::strong("Contact: "), "Fei Liang")
            )
          )
        )
      ),
      shiny::br(),
      bslib::card(
        class = "pv-preview-card",
        bslib::card_header("Module quick reference"),
        bslib::card_body(
          DT::datatable(
            data.frame(
              Module = c("Project init", "Data input", "Pre-processing", "Downstream analysis", "Multi-omics", "Toolkits"),
              Purpose = c(
                "Define project metadata and output location.",
                "Import expression matrices and search-engine outputs.",
                "Clean, transform, impute, and normalize data.",
                "Run differential analysis and functional interpretation.",
                "Explore expression profiles, co-enrichment, Venn/UpSet, and cross-omics views.",
                "Generate utility plots and protein-centric visualizations."
              ),
              stringsAsFactors = FALSE
            ),
            rownames = FALSE,
            options = list(dom = "t", paging = FALSE)
          )
        )
      )
    )
  )
}
