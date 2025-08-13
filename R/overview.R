overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        div(style = "margin-bottom: 15px;",
            actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold")
        ),
        uiOutput(ns("load_status_panel")),
        hr(),
        div(style = "margin-top: 15px;",
            actionButton(ns("run_normalization"), "Run Normalization", class = "btn btn-primary")
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1/2,
          height = 600,

          card(
            height = "800px",
            card_header("Original Data"),
            card_body(
              DT::DTOutput(ns("originalData"))
            )
          ),

          card(
            height = "800px",
            card_header("Original Data visualize"),
            card_body(
              plotOutput(ns("originalPlot"))
            )
          ),

          card(
            height = "800px",
            card_header("Normalized Data"),
            card_body(
              DT::DTOutput(ns("dataNormalization"))
            )
          ),

          card(
            height = "800px",
            card_header("Normalized Data Visualization"),
            card_body(
              plotOutput(ns("dataNormalizationPlot"))
            )
          )
        )
      )
    )
  )
}

