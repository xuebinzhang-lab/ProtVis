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
      style = "
        max-width: 1200px;
        margin: 30px auto;
        padding: 10px 20px 40px 20px;
      ",

      shiny::div(
        style = "
          text-align: center;
          margin-bottom: 30px;
        ",
        shiny::h2(
          "Help & Documentation",
          style = "font-weight: 700; margin-bottom: 10px;"
        ),
        shiny::p(
          "ProtVis is a modular platform for proteomics data analysis, visualization, and interpretation.",
          style = "font-size: 16px; color: #555;"
        ),
        shiny::p(
          "A recommended workflow is: Project init → Data input → Pre-processing → Downstream analysis. Protein Structure is provided as a standalone module.",
          style = "font-size: 15px; color: #666; max-width: 900px; margin: 0 auto;"
        )
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          style = "height: 100%;",
          bslib::card_header(
            shiny::tags$strong("Recommended Workflow")
          ),
          bslib::card_body(
            shiny::tags$ol(
              style = "line-height: 1.8; margin-bottom: 0;",
              shiny::tags$li(
                shiny::tags$strong("Project init: "),
                "Define project metadata and initialize the analysis environment."
              ),
              shiny::tags$li(
                shiny::tags$strong("Data input: "),
                "Upload abundance tables, annotation files, and sample metadata."
              ),
              shiny::tags$li(
                shiny::tags$strong("Pre-processing: "),
                "Perform filtering, missing value handling, normalization, and quality assessment."
              ),
              shiny::tags$li(
                shiny::tags$strong("Downstream analysis: "),
                "Run DEP analysis, enrichment analysis, GSEA, pathway interpretation, and visualization."
              ),
              shiny::tags$li(
                shiny::tags$strong("Toolkits: "),
                "Use additional plotting and utility modules for flexible exploration."
              ),
              shiny::tags$li(
                shiny::tags$strong("Protein Structure: "),
                "Explore proteins in a dedicated structure-based visualization module."
              )
            )
          )
        ),

        bslib::card(
          style = "height: 100%;",
          bslib::card_header(
            shiny::tags$strong("Module Guide")
          ),
          bslib::card_body(
            shiny::tags$ul(
              style = "line-height: 1.8; margin-bottom: 0;",
              shiny::tags$li(
                shiny::tags$strong("Project init"),
                ": Set up project name, output path, and analysis information."
              ),
              shiny::tags$li(
                shiny::tags$strong("Protein Structure"),
                ": Perform protein structure exploration and visualization."
              ),
              shiny::tags$li(
                shiny::tags$strong("Data input"),
                ": Import proteomics data and experimental design tables."
              ),
              shiny::tags$li(
                shiny::tags$strong("Pre-processing"),
                ": Prepare clean and analysis-ready expression matrices."
              ),
              shiny::tags$li(
                shiny::tags$strong("Downstream analysis"),
                ": Conduct DEP, enrichment, GSEA, and pathway-level interpretation."
              ),
              shiny::tags$li(
                shiny::tags$strong("Toolkits"),
                ": Access supplementary visualization and utility functions."
              )
            )
          )
        )
      ),

      shiny::br(),

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          style = "height: 100%;",
          bslib::card_header(
            shiny::tags$strong("Tips for Use")
          ),
          bslib::card_body(
            shiny::tags$ul(
              style = "line-height: 1.8; margin-bottom: 0;",
              shiny::tags$li("Proceed through the main workflow step by step for the most stable analysis experience."),
              shiny::tags$li("Check sample annotation and group labels carefully before downstream analysis."),
              shiny::tags$li("Inspect intermediate results after each pre-processing step."),
              shiny::tags$li("Use export options to save publication-ready figures and result tables."),
              shiny::tags$li("Use Protein Structure independently when structure-based exploration is needed.")
            )
          )
        ),

        bslib::card(
          style = "height: 100%;",
          bslib::card_header(
            shiny::tags$strong("Documentation")
          ),
          bslib::card_body(
            shiny::p(
              "This page can be extended with:",
              style = "margin-bottom: 12px;"
            ),
            shiny::tags$ul(
              style = "line-height: 1.8; margin-bottom: 15px;",
              shiny::tags$li("Step-by-step tutorials"),
              shiny::tags$li("Example workflows"),
              shiny::tags$li("Frequently asked questions"),
              shiny::tags$li("Downloadable user manuals"),
              shiny::tags$li("Links to GitHub and project resources")
            ),
            shiny::p(
              shiny::strong("Contact: "),
              "Fei Liang & Xiao Wang"
            )
          )
        )
      )
    )
  )
}
