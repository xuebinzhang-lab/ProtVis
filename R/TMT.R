# TMT module removed
#
# The standalone TMT Shiny module was intentionally removed. Its proteomics QC
# functionality now lives in the Downstream analysis > Overview module.
#
# This file is intentionally kept as a compatibility tombstone so branches and
# installed builds that still touch the historical R/TMT.R path can merge or run
# without a delete/modify conflict. The compatibility functions below are not
# exported and are not mounted by the current app.

TMT_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_panel(
    title = "Proteomics QC moved",
    icon = bsicons::bs_icon("info-circle"),
    bslib::card(
      bslib::card_header("TMT module removed"),
      bslib::card_body(
        shiny::p("The standalone TMT module has been removed."),
        shiny::p("Use Downstream analysis > Overview > Proteomics QC for sample totals, missingness, distributions, PCA, CV, and QC downloads."),
        shiny::uiOutput(ns("tmt_removed_notice"))
      )
    )
  )
}

TMT_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    output$tmt_removed_notice <- shiny::renderUI({
      shiny::tags$span(
        "This compatibility shim is intentionally not exported and contains no TMT analysis workflow.",
        style = "color: #6b7280;"
      )
    })
  })
}
