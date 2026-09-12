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
      class = "protvis-page pv-help-page",
      shiny::tags$style(shiny::HTML("\n        .pv-help-page { max-width: 1500px; margin: 0 auto; }\n        .pv-help-page .pv-help-hero { padding: 28px 32px; border: 1px solid #d7e3ef; border-radius: 10px; background: #f8fbfe; }\n        .pv-help-page .pv-help-hero h2 { margin: 6px 0 8px; color: #18324a; font-weight: 700; }\n        .pv-help-page .pv-help-hero p { max-width: 850px; margin: 0; color: #52616b; line-height: 1.6; }\n        .pv-help-page .pv-help-step { height: 100%; border: 1px solid #d7e3ef; border-radius: 8px; background: #fff; }\n        .pv-help-page .pv-help-step .card-header { color: #18324a; font-weight: 700; background: #f8fbfe; border-bottom: 1px solid #d7e3ef; }\n        .pv-help-page .pv-help-step .card-body { color: #405568; line-height: 1.6; }\n        .pv-help-page .pv-help-step ul { margin: 0; padding-left: 1.2rem; }\n        .pv-help-page .pv-help-step li + li { margin-top: 7px; }\n        .pv-help-page .pv-help-index { display: inline-flex; width: 26px; height: 26px; align-items: center; justify-content: center; margin-right: 8px; border: 1px solid #a9c5dd; border-radius: 50%; color: #1f5a85; font-size: 13px; }\n        .pv-help-page .pv-help-note { padding: 14px 16px; border-left: 3px solid #4e86ad; background: #f8fbfe; color: #52616b; }\n        .pv-help-page .pv-help-faq .accordion-button { color: #18324a; font-weight: 600; }\n      ")),
      shiny::div(
        class = "pv-help-hero",
        shiny::div(
          shiny::span("Documentation", class = "pv-section-eyebrow"),
          shiny::tags$h2("ProtVis workflow guide", class = "pv-page-title"),
          shiny::tags$p(
            "A concise guide to preparing proteomics data, running reproducible analyses, and exporting figures and tables for downstream interpretation.",
            class = "pv-page-subtitle"
          ),
          shiny::div(
            class = "pv-help-note",
            shiny::strong("Recommended start: "),
            "Project init → Pre-processing → MaxQuant Output Preparation"
          )
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header(shiny::span(shiny::span("1", class = "pv-help-index"), "Prepare data")),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Create a project and confirm the working directory."),
              shiny::tags$li("Select a data source in Project init; MaxQuant users then open MaxQuant Output Preparation."),
              shiny::tags$li("Use built-in examples to verify the interface before loading a full dataset.")
            )
          )
        ),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header(shiny::span(shiny::span("2", class = "pv-help-index"), "Process and analyze")),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Run filtering, transformation, imputation, and normalization in order."),
              shiny::tags$li("Inspect preview tables and preserve intermediate ProtVis_dataset objects."),
              shiny::tags$li("Use DEP, enrichment, GSEA, and pathway analyses for interpretation.")
            )
          )
        ),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header(shiny::span(shiny::span("3", class = "pv-help-index"), "Visualize and export")),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Explore expression profiles, multi-omics, PTM, protein structure, and toolkit plots."),
              shiny::tags$li("Adjust plot parameters before exporting publication-ready figures and tables."),
              shiny::tags$li(shiny::strong("Contact: "), "Fei Liang (fyliangfei@163.com)")
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
              Module = c("Project init", "MaxQuant Output Preparation", "Pre-processing", "Downstream analysis", "Multi-omics", "Toolkits"),
              Purpose = c(
                "Define project metadata and output location.",
                "Prepare MaxQuant protein-group output for preprocessing.",
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
      ),
      bslib::accordion(
        class = "pv-help-faq",
        bslib::accordion_panel(
          "Common questions",
          shiny::tags$p(shiny::strong("Where should I start? "), "Use Project init to define the output directory and select a data source. For MaxQuant, continue with MaxQuant Output Preparation."),
          shiny::tags$p(shiny::strong("Can I work without uploading a file? "), "Yes. Modules with a Use Example Data button provide deterministic example data for testing."),
          shiny::tags$p(shiny::strong("How are results preserved? "), "Each analysis writes its result into the current ProtVis_dataset and can be exported with the associated tables or figures.")
        )
      )
    )
  )
}
