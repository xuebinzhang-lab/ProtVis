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
            card_header("Correlation"),
            card_body(
              DT::DTOutput(ns("cor_res"))
            )
          ),

          card(
            height = "800px",
            card_header("Heatmap"),
            card_body(
              plotOutput(ns("heatmapPlot"))
            )
          ),

          card(
            height = "800px",
            card_header("PCA before normalization"),
            card_body(
              DT::DTOutput(ns("pcaBeforeNormalization"))
            )
          ),

          card(
            height = "800px",
            card_header("PCA after normalization"),
            card_body(
              plotOutput(ns("pcaAfterNormalization"))
            )
          )
        )
      )
    )
  )
}

