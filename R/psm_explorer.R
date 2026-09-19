# Standalone PSM Explorer built on the validated mzIdentML/MGF spectrum browser.

#' PSM Explorer UI.
#' @export
psm_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "px-3 pt-3",
      shiny::h2("PSM Explorer"),
      shiny::p(
        "Inspect the evidence chain Protein → Peptide → PSM → MS/MS spectrum. ",
        "Load mzIdentML + MGF files, search any PSM, and inspect matched b/y ions ",
        "and PTM-aware fragment coverage."
      )
    ),
    .protvis_vac14_ui(ns)
  )
}

#' PSM Explorer server.
#' @export
psm_explorer_server <- function(id, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    selected <- .protvis_vac14_server(input, output, session)

    shiny::observeEvent(selected(), {
      value <- selected()
      if (is.null(value) || is.null(shared_state) ||
          !inherits(shared_state$dataset, "ProtVis_dataset")) {
        return(invisible(NULL))
      }
      dataset <- shared_state$dataset
      summary <- list(
        protein = value$target$protein %||% NA_character_,
        peptide = value$target$sequence %||% NA_character_,
        modified_peptide = value$target$modified_sequence %||% NA_character_,
        spectrum = value$target$spectrum_label %||%
          value$target$spectrum_title %||% NA_character_,
        matched_ions = nrow(value$matched %||% data.frame()),
        viewed_at = as.character(Sys.time())
      )
      dataset$analysis_results$PSM_Explorer <- summary
      dataset <- .protvis_append_process(
        dataset, "PSM_Explorer", status = "success",
        parameters = summary,
        message = "A peptide-spectrum match was inspected in PSM Explorer."
      )
      shared_state$dataset <- dataset
      invisible(NULL)
    }, ignoreInit = TRUE)

    invisible(selected)
  })
}
