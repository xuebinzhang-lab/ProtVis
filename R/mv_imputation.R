#' Missing value filling UI Module
#' @description Missing value filling UI Module
#' @param id A unique identifier for the Shiny namespace, Missing value filling.
#' @title mv_imputation_ui
#' @name mv_imputation_ui
#' @import shiny
#' @import bslib
#' @importFrom bsicons bs_icon
#' @export
#'
mv_imputation_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = 'Data transformation',
    icon = bsicons::bs_icon("play-circle"),
    bslib::layout_sidebar(
      sidebar = bslib::accordion(
        bslib::accordion_panel(
          title = "File Upload",
          icon = bsicons::bs_icon("upload"),
          shiny::fileInput(
            inputId = ns('file'),
            label = 'Upload Expression Matrix',
            multiple = FALSE,
            accept = '.csv'
          )
        ),
        bslib::accordion_panel(
          title = "Method of Data Interpolation",
          shiny::selectInput(ns("choice_method"), "Select a method:",
                      choices = c("kNN", "RF", "Mean", "Median", "Zero", "Minimum"),
                      selected = "Mean")
        ),
        bslib::accordion_panel(
          title = "Run",
          shiny::actionButton(ns("runButton"), "Run")
        ),
        bslib::accordion_panel(
          title = "Download",
          shiny::downloadButton(ns("downloadData"), "Download")
        )
      ),
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,
          height = 600,
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Original Data",
            shiny::mainPanel(
              DT::DTOutput(ns("originalData"))
            )
          ),
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Original Data visualize",
            shiny::mainPanel(
              shiny::plotOutput(ns("originalPlot"))
            )),
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Imputed Data",
            shiny::mainPanel(
              DT::DTOutput(ns("imputedData"))
            )),
          bslib::navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Imputed Data visualize",
            shiny::mainPanel(
              shiny::plotOutput(ns("imputedPlot"))
            )
          )
        )
      )
    )
  )
}

#' Missing value filling Server Module
#' @description Server logic for Missing value filling
#' @title mv_imputation_server
#' @param id The module ID.
#' @name mv_imputation_server
#' @import shiny
#' @importFrom utils read.csv write.csv
#' @importFrom tibble column_to_rownames
#' @importFrom dplyr mutate across everything
#' @importFrom DT renderDT datatable
#' @importFrom stats median
#' @importFrom visdat vis_dat
#' @importFrom ggplot2 scale_fill_manual
#' @export

mv_imputation_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    data <- shiny::reactive({
      shiny::req(input$file)
      utils::read.csv(input$file$datapath) %>%
        tibble::column_to_rownames("Accession")
    })
    imputed_data <- shiny::eventReactive(input$runButton, {
      shiny::req(data())
      method <- input$choice_method
      df <- base::as.data.frame(data())
      if (method == "kNN") {
        return(base::as.data.frame(VIM::kNN(df, k = 5)))
      } else if (method == "RF") {
        return(base::as.data.frame(missForest::missForest(df)$ximp))
      } else if (method == "Mean") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(base::is.na(.), base::mean(., na.rm = TRUE), .))))
      } else if (method == "Median") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(base::is.na(.), stats::median(., na.rm = TRUE), .))))
      } else if (method == "Zero") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(base::is.na(.), 0, .))))
      } else if (method == "Minimum") {
        return(df %>%
                 dplyr::mutate(dplyr::across(dplyr::everything(),
                                             ~ ifelse(base::is.na(.), base::min(., na.rm = TRUE), .))))
      }
    })
    output$originalData <- DT::renderDT({
      shiny::req(data())
      DT::datatable(data(), options = base::list(pageLength = 10, scrollX = TRUE))
    })
    output$originalPlot <- shiny::renderPlot({
      shiny::req(data())
      visdat::vis_dat(base::as.data.frame(data())) +
        ggplot2::scale_fill_manual(
          values = c(
            "character" = "skyblue",
            "factor" = "lightgreen",
            "numeric" = "#E0F3F8",
            "logical" = "lightyellow",
            "NA" = "#BEBEBE"
          )
        )
    })
    output$imputedData <- DT::renderDT({
      shiny::req(imputed_data())
      DT::datatable(imputed_data(), options = base::list(pageLength = 10, scrollX = TRUE))
    })
    output$imputedPlot <- shiny::renderPlot({
      shiny::req(imputed_data())
      visdat::vis_dat(base::as.data.frame(imputed_data())) +
        ggplot2::scale_fill_manual(
          values = c(
            "character" = "skyblue",
            "factor" = "lightgreen",
            "numeric" = "#E0F3F8",
            "logical" = "lightyellow",
            "NA" = "#BEBEBE"
          )
        )
    })
    output$downloadData <- shiny::downloadHandler(
      filename = function() {
        base::paste0("imputed_data_", base::Sys.Date(), ".csv")
      },
      content = function(file) {
        utils::write.csv(imputed_data(), file, row.names = TRUE)
      }
    )
  })
}
