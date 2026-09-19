# Protein Workbench layout stabilization ---------------------------------
#
# The Workbench's original bslib::layout_sidebar() is a fillable layout. After
# Localization / Interaction were appended as a sibling UI block, the navbar
# page no longer supplied an unambiguous fill height to the original layout,
# which could collapse it to a thin horizontal strip. Keep the existing base
# Workbench and context extension untouched, but give the base fill layout an
# explicit viewport-derived shell and avoid rendering large empty context
# tables before a protein/sequence has been supplied.

.protvis_pw_context_empty_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    class = "pw-context-empty",
    shiny::div(
      class = "pw-context-empty-icon",
      bsicons::bs_icon("diagram-3")
    ),
    shiny::div(
      shiny::tags$strong("Localization and interaction context"),
      shiny::p(
        "Resolve a UniProt entry above to load curated localization, topology and interaction data. A protein sequence can also be used for Plant-mPLoc when appropriate.",
        class = "pw-note"
      )
    )
  )
}

#' Protein Workbench UI
#'
#' Stable layout wrapper for the Protein Workbench. This preserves every
#' existing Workbench tab and the additive Localization / Interaction context
#' while preventing the original fillable sidebar layout from collapsing.
#'
#' @param id Shiny module id.
#' @return Shiny UI.
#' @export
protein_workbench_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("\n      .pw-base-shell {\n        height: calc(100vh - 82px);\n        min-height: 760px;\n        width: 100%;\n        padding: 16px 16px 8px;\n        box-sizing: border-box;\n      }\n      .pw-base-shell > .bslib-sidebar-layout {\n        height: 100% !important;\n        min-height: 740px !important;\n      }\n      .pw-context-shell {\n        padding: 0 16px 28px;\n      }\n      .pw-context-empty {\n        display: flex;\n        align-items: center;\n        gap: 14px;\n        margin: 8px 0 18px;\n        padding: 18px 20px;\n        border: 1px dashed #bfd5e5;\n        border-radius: 16px;\n        background: #f8fbff;\n        color: #1f3447;\n      }\n      .pw-context-empty-icon {\n        width: 42px;\n        height: 42px;\n        flex: 0 0 42px;\n        display: grid;\n        place-items: center;\n        border-radius: 13px;\n        background: #e8f5fc;\n        color: #1787c9;\n      }\n      .pw-context-empty strong {\n        display: block;\n        margin-bottom: 3px;\n      }\n      .pw-context-empty p {\n        margin: 0;\n      }\n      @media (max-width: 900px) {\n        .pw-base-shell {\n          height: auto;\n          min-height: 900px;\n          padding: 10px;\n        }\n        .pw-base-shell > .bslib-sidebar-layout {\n          min-height: 880px !important;\n        }\n        .pw-context-shell { padding: 0 10px 20px; }\n      }\n    ")),
    shiny::div(
      class = "pw-base-shell",
      .protvis_pw_base_ui(id)
    ),
    shiny::div(
      class = "pw-context-shell",
      shiny::conditionalPanel(
        condition = base::sprintf(
          "(!input['%s'] || input['%s'] === '') && (!input['%s'] || input['%s'].trim() === '')",
          ns("accession"), ns("accession"), ns("sequence"), ns("sequence")
        ),
        .protvis_pw_context_empty_ui(id)
      ),
      shiny::conditionalPanel(
        condition = base::sprintf(
          "(input['%s'] && input['%s'] !== '') || (input['%s'] && input['%s'].trim() !== '')",
          ns("accession"), ns("accession"), ns("sequence"), ns("sequence")
        ),
        .protvis_pw_context_ui(id)
      )
    )
  )
}
