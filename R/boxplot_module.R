#' Boxplot Module UI
#'
#' Creates the user interface for the Boxplot module.
#' Includes file upload, group selection, color and style options,
#' theme selection, and download options. All parameters are placed
#' in a sidebar using bslib accordion.
#'
#' @param id Module namespace ID
#' @return UI elements for the Boxplot module
#' @export
boxplot_module_ui <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    sidebar = accordion(
      id = ns("settings_panel"),
      open = NULL,
      accordion_panel(
        title = "Boxplot Parameters",
        fileInput(ns("file"), "Upload Excel file", accept = c(".xlsx")),

        uiOutput(ns("group_select_ui")),

        textInput(ns("x_label"), "X-axis label", value = "Group"),
        textInput(ns("y_label"), "Y-axis label", value = "Value"),

        hr(),
        h4("Colors & Style"),
        uiOutput(ns("color_ui")),
        numericInput(ns("box_width"), "Box width", value = 0.4, min = 0.1, max = 1, step = 0.05),
        numericInput(ns("point_size"), "Point size", value = 1, min = 0.1, max = 5, step = 0.1),
        colourInput(ns("line_color"), "Box line color", value = "black"),

        hr(),
        h4("Theme"),
        selectInput(ns("theme"), "Choose Theme",
                    choices = c("minimal", "classic", "light", "bw", "dark", "grey"),
                    selected = "grey"),

        hr(),
        h4("Download PDF"),
        numericInput(ns("plot_height"), "Height (inch)", value = 6),
        numericInput(ns("plot_width"), "Width (inch)", value = 8),
        downloadButton(ns("download_plot"), "Download Plot (PDF)")
      )
    ),
    mainPanel(
      plotOutput(ns("boxplot"), height = "600px")
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
boxplot_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive: Read Excel file
    df <- reactive({
      req(input$file)
      openxlsx::read.xlsx(input$file$datapath)
    })

    # Reactive UI: Group selection and comparisons
    output$group_select_ui <- renderUI({
      req(df())
      cols <- names(df())
      tagList(
        checkboxGroupInput(ns("selected_groups"), "Select groups", choices = cols, selected = cols),
        selectizeInput(ns("comparisons"), "Select comparisons",
                       choices = cols, multiple = TRUE, selected = cols[1:2],
                       options = list(plugins = list("remove_button")))
      )
    })

    # Reactive: Convert data to long format
    long_df <- reactive({
      req(df(), input$selected_groups)
      df() %>%
        dplyr::select(dplyr::all_of(input$selected_groups)) %>%
        pivot_longer(cols = everything(), names_to = "Group", values_to = "Value")
    })

    # Reactive UI: Dynamic color selectors, auto-arranged in rows/columns
    output$color_ui <- renderUI({
      req(input$selected_groups)
      groups <- input$selected_groups
      n <- length(groups)
      ncol <- 3
      nrow <- ceiling(n / ncol)
      rows <- lapply(1:nrow, function(r) {
        cols_ui <- lapply(1:ncol, function(c) {
          idx <- (r - 1) * ncol + c
          if (idx <= n) {
            column(12/ncol, colourInput(ns(paste0("col_", groups[idx])), groups[idx],
                                        value = RColorBrewer::brewer.pal(8, "Set2")[(idx-1) %% 8 + 1]))
          } else NULL
        })
        do.call(fluidRow, cols_ui)
      })
      do.call(tagList, rows)
    })

    # Reactive: Get color mapping for groups
    group_colors <- reactive({
      req(input$selected_groups)
      sapply(input$selected_groups, function(g) input[[paste0("col_", g)]], USE.NAMES = TRUE)
    })

    # Reactive: Generate boxplot with multi-group comparisons
    plot_box <- reactive({
      req(long_df(), group_colors(), input$comparisons)
      comparisons <- combn(input$comparisons, 2, simplify = FALSE)  # All pairwise combinations

      p <- ggplot(long_df(), aes(x = Group, y = Value, fill = Group)) +
        geom_boxplot(
          width = input$box_width,
          size = 1,
          outlier.size = input$point_size,
          color = input$line_color
        ) +
        geom_jitter(width = input$box_width/4, alpha = 0.5, size = input$point_size, color = "black") +
        scale_fill_manual(values = group_colors()) +
        stat_compare_means(
          method = "t.test",
          comparisons = comparisons,
          label = "p.format",
          vjust = -0.3,
          size = 3
        ) +
        labs(x = input$x_label, y = input$y_label) +
        theme(plot.margin = margin(1,1,1,1,"cm"))

      # Apply selected theme
      p <- switch(input$theme,
                  minimal = p + theme_minimal(base_size = 14),
                  classic = p + theme_classic(base_size = 14),
                  light = p + theme_light(base_size = 14),
                  bw = p + theme_bw(base_size = 14),
                  dark = p + theme_dark(base_size = 14),
                  grey = p + theme_grey(base_size = 14))
      p
    })

    # Render plot
    output$boxplot <- renderPlot({
      plot_box()
    })

    # Download handler for PDF
    output$download_plot <- downloadHandler(
      filename = function() { paste0("boxplot_", Sys.Date(), ".pdf") },
      content = function(file) {
        ggplot2::ggsave(file, plot = plot_box(),
                        width = max(8, input$plot_width),
                        height = max(6, input$plot_height),
                        device = "pdf")
      }
    )

    # Return reactive plot object
    return(list(plot = plot_box))
  })
}
