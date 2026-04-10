#' Post-Translational Modification (PTM) Module UI
#' @description
#' This function defines the user interface for the Post-Translational Modification (PTM)
#' analysis module. It provides a comprehensive sidebar with accordion panels for
#' data uploading, ion modification settings, peptide selection, and visualization
#' parameters. The main panel displays the resulting PTM status or visualization.
#' @param id A character string representing the Shiny module namespace ID.
#' @return A Shiny UI tag list containing a sidebar layout with accordion controls
#' and a main display card.
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @name PTM_ui
#' @export

PTM_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        bslib::accordion(
          bslib::accordion_panel(
            title = "Upload File",
            shiny::fileInput(ns("ptm_file"),
                             "Upload PTM Data",
                             accept = c(".csv", ".xlsx"),
                             multiple = FALSE)
          ),
          bslib::accordion_panel(
            title = "Ion Modifications",
            shiny::selectInput(ns("add_modification"),
                               label = "Select Ion(s) to Add",
                               choices = base::list(
                                 "Add H⁺ (Protonation)" = "add_H_plus",
                                 "Add H₂O (Hydration)" = "add_H2O",
                                 "Add NH₃ (Ammonia)" = "add_NH3",
                                 "Add CH₃ (Methylation)" = "add_methyl",
                                 "Add H₂ (Hydrogenation)" = "add_H2",
                                 "Add CO₂ (Decarboxylation)" = "add_CO2"
                               ),
                               multiple = TRUE
            ),
            shiny::selectInput(ns("remove_modification"),
                               label = "Select Ion(s) to Remove",
                               choices = base::list(
                                 "Remove H⁺ (Deprotonation)" = "remove_H_plus",
                                 "Remove H₂O (Dehydration)" = "remove_H2O",
                                 "Remove NH₃ (Deammoniation)" = "remove_NH3",
                                 "Remove CH₃ (Demethylation)" = "remove_methyl",
                                 "Remove H₂ (Dehydrogenation)" = "remove_H2",
                                 "Remove CO₂ (Decarboxylation)" = "remove_CO2"
                               ),
                               multiple = TRUE
            )
          ),
          bslib::accordion_panel(
            title = "Peptide Selection",
            shiny::selectInput(ns("Peptide"),
                               "Select Peptide",
                               choices = c(
                                 "GIVDQSQQAYQEAFEISK",
                                 "ACCPLEGVRPSPQQTEYR",
                                 "GLTMLDHEQVTPEDPGAQFLIR",
                                 "ACFLMAHNGWVMGDDPLR",
                                 "EILVGDVGQTVDDPYATFVK"),
                               selected = "ACCPLEGVRPSPQQTEYR")
          ),
          bslib::accordion_panel(
            title = "Modification Type",
            shiny::selectInput(ns("mod_type"),
                               "Select Modification Type",
                               choices = c("Phosphorylation", "Ubiquitination", "Acetylation", "Methylation", "Glycosylation"),
                               selected = "Phosphorylation")
          ),
          bslib::accordion_panel(
            title = "Color Selection for Ions",
            shiny::fluidRow(
              shiny::column(6,
                            tags$div(style = "font-weight: bold; text-align: left;", "b ion"),
                            colourpicker::colourInput(ns("color_up"),
                                                      "",
                                                      value = "blue")),
              shiny::column(6,
                            tags$div(style = "font-weight: bold; text-align: left;", "y ion"),
                            colourpicker::colourInput(ns("color_down"),
                                                      "",
                                                      value = "red"))
            )
          ),
          bslib::accordion_panel(
            title = "Visualization",
            shiny::actionButton(ns("visualize"),
                                "VISUALIZE PTM"),
            shiny::downloadButton(ns("download_ptm_plot"),
                                  "DOWNLOAD")
          )
        )
      ),

      main = bslib::card(
        height = "500px",
        bslib::card_header("Protein Post-translational Modifications (PTMs) Visualization"),
        bslib::card_body(
          shiny::textOutput(ns("status_message"))
        )
      )
    )
  )
}
