#' Help UI Module
#'
#' Creates the Help navigation panel for ProtVis. The guide follows the
#' source-dependent workflow exposed by the application.
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
      shiny::tags$style(shiny::HTML("
        .pv-help-page { max-width: 1500px; margin: 0 auto; padding: 18px 0 42px; }
        .pv-help-page .pv-help-hero { padding: 30px 34px; border: 1px solid #d7e3ef; border-radius: 22px; background: linear-gradient(135deg, #ffffff 0%, #eef8fd 100%); box-shadow: 0 12px 32px rgba(24, 50, 74, 0.07); }
        .pv-help-page .pv-help-hero h2 { margin: 7px 0 9px; color: #18324a; font-weight: 800; }
        .pv-help-page .pv-help-hero p { max-width: 920px; margin: 0; color: #52616b; line-height: 1.7; }
        .pv-help-page .pv-help-note { margin-top: 20px; padding: 14px 16px; border-left: 4px solid #1787c9; border-radius: 10px; background: #ffffff; color: #36566d; line-height: 1.6; }
        .pv-help-page .pv-help-step { height: 100%; border: 1px solid #d7e3ef; border-radius: 18px; background: #fff; }
        .pv-help-page .pv-help-step .card-header { color: #18324a; font-weight: 800; background: #f8fbfe; border-bottom: 1px solid #d7e3ef; }
        .pv-help-page .pv-help-step .card-body { color: #405568; line-height: 1.65; }
        .pv-help-page .pv-help-step ul { margin: 0; padding-left: 1.2rem; }
        .pv-help-page .pv-help-step li + li { margin-top: 8px; }
        .pv-help-page .pv-help-index { display: inline-flex; width: 27px; height: 27px; align-items: center; justify-content: center; margin-right: 8px; border: 1px solid #a9c5dd; border-radius: 50%; color: #1f5a85; font-size: 13px; }
        .pv-help-page .pv-help-section-title { margin: 30px 0 14px; color: #18324a; font-size: 1.35rem; font-weight: 800; }
        .pv-help-page .pv-help-table td:first-child { color: #18324a; font-weight: 750; }
        .pv-help-page .pv-help-table td { color: #526b7c; vertical-align: top; line-height: 1.5; }
        .pv-help-page .pv-help-faq .accordion-button { color: #18324a; font-weight: 700; }
        .pv-help-page .pv-help-faq p { color: #52616b; line-height: 1.65; }
      ")),
      shiny::div(
        class = "pv-help-hero",
        shiny::span("Documentation", class = "pv-section-eyebrow"),
        shiny::tags$h2("ProtVis workflow guide", class = "pv-page-title"),
        shiny::tags$p(
          "Follow the data-source route shown by ProtVis, keep each completed stage in ProtVis_dataset, and use the linked tables, figures, and checkpoints for downstream analysis.",
          class = "pv-page-subtitle"
        ),
        shiny::div(
          class = "pv-help-note",
          shiny::strong("Navigation rule: "),
          "Raw is the default source. Search is shown only for Raw, while MaxQuant Output Preparation is shown only for MaxQuant. Other sources use their own parser and continue directly to Pre-processing."
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header(shiny::span(shiny::span("1", class = "pv-help-index"), "Project init")),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Set an accessible working directory."),
              shiny::tags$li("Upload or select sample information and an expression matrix when required."),
              shiny::tags$li("For Raw/Sage, register the FASTA and mzML directory; the initial sample-only dataset can be saved before the search."),
              shiny::tags$li("Click Project init once to create the project checkpoint.")
            )
          )
        ),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header(shiny::span(shiny::span("2", class = "pv-help-index"), "Choose the route")),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Raw: confirm FASTA/mzML paths, set Sage parameters, and run Search."),
              shiny::tags$li("MaxQuant: open MaxQuant Output Preparation after selecting MaxQuant."),
              shiny::tags$li("DIA-NN, Spectronaut, FragPipe, Skyline, OpenMS, Proteome Discoverer, or a custom matrix: use the matching source parser."),
              shiny::tags$li("The source-specific entry does not change the common downstream dataset model.")
            )
          )
        ),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header(shiny::span(shiny::span("3", class = "pv-help-index"), "Process and analyze")),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Load the current ProtVis_dataset before each downstream module."),
              shiny::tags$li("Run Correct Noise, Data Transformation, Data Imputation, and Data Normalization in order."),
              shiny::tags$li("Continue with Overview, DEP, Enrichment, GSEA, and Pathview."),
              shiny::tags$li("Use Multi-omics, PTM, Protein Structure, and Toolkits for specialized exploration.")
            )
          )
        )
      ),
      shiny::tags$h3("Connected workflow", class = "pv-help-section-title"),
      bslib::card(
        class = "pv-preview-card",
        bslib::card_body(
          DT::datatable(
            data.frame(
              Step = c("Project init", "Search", "MaxQuant Output Preparation", "Correct Noise", "Transformation → Imputation → Normalization", "Downstream analysis", "Specialized analysis"),
              When = c("All projects", "Raw only", "MaxQuant only", "All completed ProtVis_dataset routes", "After the input/correction stage", "After normalized data are available", "Whenever the required identifiers/results are available"),
              Output = c("Initial ProtVis_dataset and checkpoint", "Sage log, PSM/LFQ results, and searchable ProtVis_dataset", "Prepared quantitative matrix in ProtVis_dataset", "Corrected matrix and canonical checkpoint", "One new ProtVis_dataset stage per operation", "Statistics, enrichment, pathways, tables, and figures", "Protein structure, PTM, multi-omics, PPI, and toolkit results"),
              stringsAsFactors = FALSE
            ),
            rownames = FALSE,
            class = "display compact pv-help-table",
            options = list(dom = "t", paging = FALSE, searching = FALSE, scrollX = TRUE)
          )
        )
      ),
      shiny::tags$h3("ProtVis_dataset, recovery, and safe execution", class = "pv-help-section-title"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header("What is retained?"),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("expression_data: the current protein-by-sample matrix."),
              shiny::tags$li("sample_info and variable_info: experimental and feature-level metadata."),
              shiny::tags$li("annotation and analysis_results: functional annotations and stage-specific outputs."),
              shiny::tags$li("process_info: parameters, status, timestamps, and analysis history."),
              shiny::tags$li("Exported tables, figures, and attached files remain associated with the project output directory.")
            )
          )
        ),
        bslib::card(
          class = "pv-help-step",
          bslib::card_header("How can I resume?"),
          bslib::card_body(
            shiny::tags$ul(
              shiny::tags$li("Sage writes its log and result files to the Sage_search directory."),
              shiny::tags$li("After an interruption, return to the same project and load the completed Sage checkpoint before Correct Noise."),
              shiny::tags$li("Completed stages are saved as ProtVis_dataset checkpoints and can be loaded by downstream modules."),
              shiny::tags$li("Run buttons are protected by immediate UI disabling, a server-side lock, and completion-only checkpoint writes.")
            )
          )
        )
      ),
      bslib::accordion(
        class = "pv-help-faq",
        bslib::accordion_panel(
          "Common questions",
          shiny::tags$p(shiny::strong("Why do I not see Search? "), "Search is intentionally visible only when Select data source is Raw and is intended for FASTA + mzML Sage searching."),
          shiny::tags$p(shiny::strong("Why do I not see MaxQuant Output Preparation? "), "That module is intentionally hidden unless Select data source is MaxQuant."),
          shiny::tags$p(shiny::strong("Can a failed step damage the previous dataset? "), "A completed stage is written only after successful processing. Existing checkpoints remain available for recovery."),
          shiny::tags$p(shiny::strong("Where can I get help with a result? "), "Check the module status, log, preview table, and the saved checkpoint in the project working directory.")
        )
      )
    )
  )
}
