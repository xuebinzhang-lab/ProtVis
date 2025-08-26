enrichment_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        div(style = "margin-bottom: 15px;",
            fileInput(ns("enrichment_analysis_file"), "Upload Enrichment analysis File",
                      accept = c(".csv", ".xlsx"),
                      buttonLabel = "Browse..."),
            actionButton(ns("check_file"), "Check File",
                         class = "btn btn-success fw-bold mb-2"),
            div(style = "border-top: 3px solid #ff0000; margin: 10px 0;"),
            actionButton(ns("extract"), "Make Background",
                         class = "btn btn-light fw-bold mb-3"),
            br(style = "line-height: 100px;"),
            downloadButton(ns("download_background"), "Download",
                           class = "btn btn-light fw-bold")
        )
      ),
      page_fluid(
        card(
          card_header("File Check Result"),
          card_body(
            textOutput(ns("file_check_result"))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          height = 600,
          card(
            height = "800px",
            card_header("GO Enrichment analysis"),
            card_body(
              dataTableOutput(ns("go_res_table"))
            )
          ),
          card(
            height = "800px",
            card_header("KEGG Enrichment analysis"),
            card_body(
              dataTableOutput(ns("kegg_res_table"))
            )
          )
        )
      )
    )
  )
}
