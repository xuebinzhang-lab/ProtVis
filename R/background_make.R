background_make_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        # File upload section
        div(style = "margin-bottom: 15px;",
            fileInput(ns("eggnog_output"), "Upload Eggnog Output File",
                      accept = c(".csv", ".xlsx"),
                      buttonLabel = "Browse...")
        )
      )
    )
  )
}
