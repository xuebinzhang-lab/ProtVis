#' Boxplot Module UI
#'
#' Creates the user interface for the Boxplot module.
#' Includes file upload, group selection, color and style options,
#' theme selection, and download options. All parameters are placed
#' in a sidebar using bslib accordion.
#'
#' @param id Module namespace ID
#' @return UI elements for the Boxplot module
#' @name boxplot_module_ui
#' @export
boxplot_module_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::accordion(
      id = ns("settings_panel"),
      open = NULL,
      bslib::accordion_panel(
        title = "Boxplot Parameters",
        shiny::fileInput(ns("file"), "Upload Excel file", accept = c(".xlsx")),
        shiny::uiOutput(ns("group_select_ui")),
        shiny::textInput(ns("x_label"), "X-axis label", value = "Group"),
        shiny::textInput(ns("y_label"), "Y-axis label", value = "Value"),
        shiny::hr(),
        shiny::h4("Colors & Style"),
        shiny::uiOutput(ns("color_ui")),
        shiny::numericInput(ns("box_width"), "Box width", value = 0.4, min = 0.1, max = 1, step = 0.05),
        shiny::numericInput(ns("point_size"), "Point size", value = 1, min = 0.1, max = 5, step = 0.1),
        colourpicker::colourInput(ns("line_color"), "Box line color", value = "black"),
        hr(),
        h4("Theme"),
        shiny::selectInput(ns("theme"), "Choose Theme",
                    choices = c("minimal", "classic", "light", "bw", "dark", "grey"),
                    selected = "grey"),
        shiny::hr(),
        shiny::h4("Download PDF"),
        shiny::numericInput(ns("plot_height"), "Height (inch)", value = 6),
        shiny::numericInput(ns("plot_width"), "Width (inch)", value = 8),
        shiny::downloadButton(ns("download_plot"), "Download Plot (PDF)")
      )
    ),
    shiny::mainPanel(
      shiny::plotOutput(ns("boxplot"), height = "600px")
    )
  )
}

#' Boxplot Module Server
#'
#' Server logic for the Boxplot module.
#' Handles file upload, data processing, long-format conversion,
#' dynamic color input generation, multi-group comparison,
#' plotting, and PDF download.
#'
#' @param id Module namespace ID
#' @return A list containing reactive plot object
#' @export
#' @name boxplot_module_server

boxplot_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Read Excel file
    df <- shiny::reactive({
      shiny::req(input$file)
      openxlsx::read.xlsx(input$file$datapath)
    })

    # Reactive UI: Group selection and comparisons
    output$group_select_ui <- shiny::renderUI({
      shiny::req(df())
      cols <- base::names(df())
      shiny::tagList(
        shiny::checkboxGroupInput(ns("selected_groups"), "Select groups", choices = cols, selected = cols),
        shiny::selectizeInput(ns("comparisons"), "Select comparisons",
                       choices = cols, multiple = TRUE, selected = cols[1:2],
                       options = list(plugins = list("remove_button")))
      )
    })

    # Reactive: Convert data to long format
    long_df <- shiny::reactive({
      req(df(), input$selected_groups)
      df() %>%
        dplyr::select(dplyr::all_of(input$selected_groups)) %>%
        tidyr::pivot_longer(cols = everything(), names_to = "Group", values_to = "Value")
    })

    # Reactive UI: Dynamic color selectors, auto-arranged in rows/columns
    output$color_ui <- shiny::renderUI({
      shiny::req(input$selected_groups)
      groups <- input$selected_groups
      n <- base::length(groups)
      ncol <- 3
      nrow <- base::ceiling(n / ncol)
      rows <- base::lapply(1:nrow, function(r) {
        cols_ui <- base::lapply(1:ncol, function(c) {
          idx <- (r - 1) * ncol + c
          if (idx <= n) {
            column(12/ncol, colourpicker::colourInput(ns(paste0("col_", groups[idx])), groups[idx],
                                                      value = RColorBrewer::brewer.pal(8, "Set2")[(idx-1) %% 8 + 1]))
          } else NULL
        })
        base::do.call(fluidRow, cols_ui)
      })
      base::do.call(tagList, rows)
    })

    # Reactive: Get color mapping for groups
    group_colors <- shiny::reactive({
      shiny::req(input$selected_groups)
      base::sapply(input$selected_groups, function(g) input[[paste0("col_", g)]], USE.NAMES = TRUE)
    })

    # Reactive: Generate boxplot with multi-group comparisons
    plot_box <- shiny::reactive({
      shiny::req(long_df(), group_colors(), input$comparisons)
      comparisons <- utils::combn(input$comparisons, 2, simplify = FALSE)  # All pairwise combinations

      p <- ggplot2::ggplot(long_df(), ggplot2::aes(x = Group, y = Value, fill = Group)) +
        ggplot2::geom_boxplot(
          width = input$box_width,
          size = 1,
          outlier.size = input$point_size,
          color = input$line_color
        ) +
        ggplot2::geom_jitter(width = input$box_width/4, alpha = 0.5, size = input$point_size, color = "black") +
        ggplot2::scale_fill_manual(values = group_colors()) +
        ggpubr::stat_compare_means(
          method = "t.test",
          comparisons = comparisons,
          label = "p.format",
          vjust = -0.3,
          size = 3
        ) +
        ggplot2::labs(x = input$x_label, y = input$y_label) +
        ggplot2::theme(plot.margin = ggplot2::margin(1,1,1,1,"cm"))

      # Apply selected theme
      p <- base::switch(input$theme,
                  minimal = p + ggplot2::theme_minimal(base_size = 14),
                  classic = p + ggplot2::theme_classic(base_size = 14),
                  light = p + ggplot2::theme_light(base_size = 14),
                  bw = p + ggplot2::theme_bw(base_size = 14),
                  dark = p + ggplot2::theme_dark(base_size = 14),
                  grey = p + ggplot2::theme_grey(base_size = 14))
      p
    })

    # Render plot
    output$boxplot <- shiny::renderPlot({
      plot_box()
    })

    # Download handler for PDF
    output$download_plot <- shiny::downloadHandler(
      filename = function() { base::paste0("boxplot_", base::Sys.Date(), ".pdf") },
      content = function(file) {
        ggplot2::ggsave(file, plot = plot_box(),
                        width = base::max(8, input$plot_width),
                        height = base::max(6, input$plot_height),
                        device = "pdf")
      }
    )

    # Return reactive plot object
    return(list(plot = plot_box))
  })
}
