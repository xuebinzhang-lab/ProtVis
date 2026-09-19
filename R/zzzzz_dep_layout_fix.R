# Give the preserved legacy DEP layout a definite containing height when it is
# rendered inside the new workspace tab. This prevents bslib fill children from
# collapsing while leaving the legacy UI itself unchanged.
.protvis_dep_engines_ui_with_tabs <- DEP_analysis_ui

DEP_analysis_ui <- function(id) {
  shiny::tagList(
    shiny::tags$style(shiny::HTML("\n      .pv-dep-legacy-wrap {\n        height: max(900px, calc(100vh - 180px));\n        min-height: 900px;\n      }\n      @media (max-width: 900px) {\n        .pv-dep-legacy-wrap { height: 1100px; min-height: 1100px; }\n      }\n    ")),
    .protvis_dep_engines_ui_with_tabs(id)
  )
}
