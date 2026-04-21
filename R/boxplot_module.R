#' Boxplot Module UI
#'
#' @param id Module namespace ID
#' @return Shiny UI for boxplot module
#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @importFrom DT DTOutput
#' @name boxplot_module_ui
#' @export
boxplot_module_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = "300px",
      style = "padding: 12px; overflow-y: auto;",

      bslib::accordion(
        id = ns("boxplot_accordion"),
        open = c("Data Input", "Plot Parameters"),

        bslib::accordion_panel(
          "Data Input",
          shiny::fileInput(
            ns("file"),
            "Upload Excel file",
            accept = c(".xlsx")
          ),
          shiny::actionButton(
            ns("load_demo"),
            "Load Demo Data",
            class = "btn btn-outline-secondary w-100"
          ),
          shiny::br(),
          shiny::br(),
          shiny::uiOutput(ns("group_select_ui")),
          shiny::actionButton(
            ns("run"),
            "Run",
            class = "btn btn-primary w-100"
          )
        ),

        bslib::accordion_panel(
          "Plot Parameters",
          shiny::textInput(ns("x_label"), "X-axis label", value = "Group"),
          shiny::textInput(ns("y_label"), "Y-axis label", value = "Value"),
          shiny::numericInput(
            ns("box_width"),
            "Box width",
            value = 0.5,
            min = 0.1,
            max = 1,
            step = 0.05
          ),
          shiny::numericInput(
            ns("point_size"),
            "Point size",
            value = 1.5,
            min = 0.1,
            max = 5,
            step = 0.1
          ),
          shiny::checkboxInput(
            ns("show_points"),
            "Show data points",
            value = TRUE
          ),
          colourpicker::colourInput(
            ns("line_color"),
            "Box line color",
            value = "#000000"
          ),
          shiny::uiOutput(ns("color_ui"))
        ),

        bslib::accordion_panel(
          "Statistics",
          shiny::selectInput(
            ns("stat_method"),
            "Statistical method",
            choices = c("t.test", "wilcox.test", "anova", "kruskal.test"),
            selected = "t.test"
          ),
          shiny::checkboxInput(
            ns("show_significance"),
            "Show significance labels on plot",
            value = TRUE
          )
        ),

        bslib::accordion_panel(
          "Theme",
          shiny::selectInput(
            ns("theme"),
            "Choose Theme",
            choices = c("minimal", "classic", "light", "bw", "dark", "grey"),
            selected = "grey"
          )
        ),

        bslib::accordion_panel(
          "Download",
          shiny::selectInput(
            ns("download_format"),
            "Plot format",
            choices = c("pdf", "png", "jpg", "svg"),
            selected = "pdf"
          ),
          shiny::numericInput(ns("plot_height"), "Height", value = 6, min = 1),
          shiny::numericInput(ns("plot_width"), "Width", value = 8, min = 1),
          shiny::numericInput(ns("plot_dpi"), "DPI (for PNG/JPG)", value = 300, min = 72, step = 50),
          shiny::downloadButton(ns("download_plot"), "Download Plot", class = "w-100"),
          shiny::br(),
          shiny::br(),
          shiny::downloadButton(ns("download_stats"), "Download Statistics", class = "w-100")
        )
      )
    ),

    shiny::div(
      style = "padding: 12px;",

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          full_screen = TRUE,
          height = 460,
          bslib::card_header("Uploaded Data Preview"),
          bslib::card_body(
            style = "overflow: auto;",
            shiny::uiOutput(ns("file_info")),
            shiny::uiOutput(ns("preview_ui"))
          )
        ),

        bslib::card(
          full_screen = TRUE,
          height = 460,
          bslib::card_header("Boxplot"),
          bslib::card_body(
            style = "overflow: auto;",
            shiny::uiOutput(ns("plot_ui"))
          )
        )
      ),

      shiny::br(),

      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Statistical Results"),
        bslib::card_body(
          style = "overflow-x: auto;",
          shiny::uiOutput(ns("stats_ui"))
        )
      )
    )
  )
}

#' Boxplot Module Server
#'
#' @param id Module namespace ID
#' @return A list containing reactive plot object and statistics
#' @export
#' @import shiny
#' @import bslib
#' @importFrom openxlsx read.xlsx
#' @importFrom openxlsx write.xlsx
#' @importFrom dplyr select all_of
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggpubr stat_compare_means compare_means
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter scale_fill_manual
#' @importFrom ggplot2 labs theme margin theme_minimal theme_classic
#' @importFrom ggplot2 theme_light theme_bw theme_dark theme_grey ggsave
#' @importFrom DT renderDT datatable
#' @importFrom shinyjs toggleState
#' @name boxplot_module_server
#' @export
boxplot_module_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      plot_obj = NULL,
      plot_ready = FALSE,
      stats_pairwise = NULL,
      stats_anova = NULL,
      stats_tukey = NULL,
      use_demo = FALSE,
      current_data_name = NULL
    )

    demo_data <- shiny::reactive({
      data.frame(
        Group_A = c(5.1, 5.4, 5.0, 5.3, 5.6, 5.2),
        Group_B = c(6.1, 6.5, 6.0, 6.3, 6.4, 6.2),
        Group_C = c(7.0, 7.2, 6.9, 7.4, 7.1, 7.3),
        check.names = FALSE
      )
    })

    raw_df <- shiny::reactive({
      if (isTRUE(rv$use_demo)) {
        rv$current_data_name <- "Demo data"
        return(demo_data())
      }

      shiny::req(input$file)

      dat <- openxlsx::read.xlsx(input$file$datapath)

      shiny::validate(
        shiny::need(is.data.frame(dat), "Uploaded file could not be read correctly."),
        shiny::need(ncol(dat) >= 2, "The Excel file must contain at least two columns.")
      )

      rv$current_data_name <- input$file$name
      dat
    })

    shiny::observeEvent(input$load_demo, {
      rv$use_demo <- TRUE
      rv$plot_ready <- FALSE
      rv$plot_obj <- NULL
      rv$stats_pairwise <- NULL
      rv$stats_anova <- NULL
      rv$stats_tukey <- NULL
    })

    shiny::observeEvent(input$file, {
      if (!is.null(input$file)) {
        rv$use_demo <- FALSE
        rv$plot_ready <- FALSE
        rv$plot_obj <- NULL
        rv$stats_pairwise <- NULL
        rv$stats_anova <- NULL
        rv$stats_tukey <- NULL
      }
    })

    output$file_info <- shiny::renderUI({
      if (is.null(input$file) && !isTRUE(rv$use_demo)) {
        shiny::div(
          style = paste(
            "height: 80px;",
            "display: flex;",
            "align-items: center;",
            "justify-content: center;",
            "color: #666;",
            "font-size: 15px;",
            "border: 1px dashed #ccc;",
            "border-radius: 8px;",
            "background-color: #fafafa;"
          ),
          "No file uploaded yet. You can also click 'Load Demo Data'."
        )
      } else {
        dat <- raw_df()
        shiny::div(
          style = "font-size: 14px; margin-bottom: 10px;",
          shiny::strong("Data: "), rv$current_data_name,
          shiny::HTML("&nbsp;&nbsp;&nbsp;"),
          shiny::strong("Rows: "), nrow(dat),
          shiny::HTML("&nbsp;&nbsp;&nbsp;"),
          shiny::strong("Columns: "), ncol(dat)
        )
      }
    })

    output$preview_ui <- shiny::renderUI({
      if (is.null(input$file) && !isTRUE(rv$use_demo)) {
        shiny::div(
          style = paste(
            "height: 260px;",
            "display: flex;",
            "align-items: center;",
            "justify-content: center;",
            "color: #666;",
            "font-size: 15px;",
            "border: 1px dashed #ccc;",
            "border-radius: 8px;",
            "background-color: #fafafa;"
          ),
          "Please upload an Excel file or click 'Load Demo Data' to preview the data."
        )
      } else {
        DT::DTOutput(session$ns("data_preview"))
      }
    })

    output$data_preview <- DT::renderDT({
      dat <- raw_df()
      DT::datatable(
        dat,
        options = list(
          pageLength = 5,
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })

    output$group_select_ui <- shiny::renderUI({
      if (is.null(input$file) && !isTRUE(rv$use_demo)) {
        return(
          shiny::div(
            style = "color: #888; font-size: 14px;",
            "Group selection will be available after file upload or demo data loading."
          )
        )
      }

      cols <- base::names(raw_df())

      shiny::tagList(
        shiny::checkboxGroupInput(
          session$ns("selected_groups"),
          "Select groups",
          choices = cols,
          selected = cols
        ),
        shiny::selectizeInput(
          session$ns("comparisons"),
          "Groups used for pairwise comparison",
          choices = cols,
          selected = utils::head(cols, 2),
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      )
    })

    long_df <- shiny::reactive({
      shiny::req(raw_df(), input$selected_groups)

      shiny::validate(
        shiny::need(length(input$selected_groups) >= 1, "Please select at least one group.")
      )

      dat <- raw_df() %>%
        dplyr::select(dplyr::all_of(input$selected_groups)) %>%
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          names_to = "Group",
          values_to = "Value"
        )

      dat <- dat[!is.na(dat$Value), , drop = FALSE]
      dat$Value <- suppressWarnings(as.numeric(dat$Value))
      dat <- dat[!is.na(dat$Value), , drop = FALSE]
      dat$Group <- factor(dat$Group, levels = input$selected_groups)

      shiny::validate(
        shiny::need(nrow(dat) > 0, "No valid numeric values available for plotting.")
      )

      dat
    })

    output$color_ui <- shiny::renderUI({
      if (is.null(input$selected_groups) || length(input$selected_groups) == 0) {
        return(
          shiny::div(
            style = "color: #888; font-size: 14px;",
            "Color options will appear after group selection."
          )
        )
      }

      groups <- input$selected_groups
      n <- length(groups)

      palette_base <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(8, "Set2")
      )(max(n, 3))

      ncol <- 3
      nrow <- ceiling(n / ncol)

      rows <- lapply(seq_len(nrow), function(r) {
        cols_ui <- lapply(seq_len(ncol), function(c) {
          idx <- (r - 1) * ncol + c
          if (idx <= n) {
            shiny::column(
              width = 12 / ncol,
              colourpicker::colourInput(
                inputId = session$ns(paste0("col_", groups[idx])),
                label = groups[idx],
                value = palette_base[idx]
              )
            )
          } else {
            NULL
          }
        })
        do.call(shiny::fluidRow, cols_ui)
      })

      do.call(shiny::tagList, rows)
    })

    group_colors <- shiny::reactive({
      shiny::req(input$selected_groups)

      stats::setNames(
        vapply(
          input$selected_groups,
          function(g) {
            val <- input[[paste0("col_", g)]]
            if (is.null(val) || !nzchar(val)) "#66C2A5" else val
          },
          FUN.VALUE = character(1)
        ),
        input$selected_groups
      )
    })

    comparison_list <- shiny::reactive({
      if (is.null(input$comparisons) || length(input$comparisons) < 2) {
        return(NULL)
      }
      utils::combn(input$comparisons, 2, simplify = FALSE)
    })

    make_pairwise_stats <- function(dat, method) {
      if (is.null(comparison_list())) {
        return(data.frame(
          Message = "Please select at least two groups for pairwise comparison."
        ))
      }

      res <- ggpubr::compare_means(
        Value ~ Group,
        data = dat,
        method = method,
        comparisons = comparison_list(),
        p.adjust.method = "BH"
      )

      res <- as.data.frame(res)

      if ("p" %in% colnames(res) && !"p.adj" %in% colnames(res)) {
        res$p.adj <- stats::p.adjust(res$p, method = "BH")
      }

      if ("p" %in% colnames(res) && !"p.format" %in% colnames(res)) {
        res$p.format <- format(res$p, scientific = TRUE, digits = 3)
      }

      if ("p.adj" %in% colnames(res) && !"p.adj.format" %in% colnames(res)) {
        res$p.adj.format <- format(res$p.adj, scientific = TRUE, digits = 3)
      }

      if ("p.adj" %in% colnames(res) && !"p.signif" %in% colnames(res)) {
        res$p.signif <- cut(
          res$p.adj,
          breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
          labels = c("****", "***", "**", "*", "ns")
        )
      }

      res
    }

    make_anova_stats <- function(dat) {
      fit <- stats::aov(Value ~ Group, data = dat)
      anova_tab <- summary(fit)[[1]]

      data.frame(
        Term = rownames(anova_tab),
        Df = anova_tab[, "Df"],
        Sum_Sq = anova_tab[, "Sum Sq"],
        Mean_Sq = anova_tab[, "Mean Sq"],
        F_value = anova_tab[, "F value"],
        P_value = anova_tab[, "Pr(>F)"],
        row.names = NULL,
        check.names = FALSE
      )
    }

    make_tukey_stats <- function(dat) {
      fit <- stats::aov(Value ~ Group, data = dat)
      tukey_obj <- stats::TukeyHSD(fit)

      tukey_df <- as.data.frame(tukey_obj$Group)
      tukey_df$Comparison <- rownames(tukey_df)
      rownames(tukey_df) <- NULL

      tukey_df <- tukey_df[, c("Comparison", "diff", "lwr", "upr", "p adj"), drop = FALSE]
      colnames(tukey_df) <- c("Comparison", "Difference", "Lower_CI", "Upper_CI", "P_adj")

      tukey_df$P_signif <- cut(
        tukey_df$P_adj,
        breaks = c(-Inf, 0.0001, 0.001, 0.01, 0.05, Inf),
        labels = c("****", "***", "**", "*", "ns")
      )

      tukey_df
    }

    make_kruskal_stats <- function(dat) {
      kt <- stats::kruskal.test(Value ~ Group, data = dat)

      data.frame(
        Method = "kruskal.test",
        Statistic = unname(kt$statistic),
        Df = unname(kt$parameter),
        P_value = kt$p.value,
        P_format = format(kt$p.value, scientific = TRUE, digits = 3),
        stringsAsFactors = FALSE
      )
    }

    make_plot <- function(dat) {
      p <- ggplot2::ggplot(
        dat,
        ggplot2::aes(x = Group, y = Value, fill = Group)
      ) +
        ggplot2::geom_boxplot(
          width = input$box_width,
          linewidth = 0.7,
          outlier.size = input$point_size,
          color = input$line_color,
          na.rm = TRUE
        ) +
        ggplot2::scale_fill_manual(values = group_colors()) +
        ggplot2::labs(
          x = input$x_label,
          y = input$y_label
        ) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(1, 1, 1, 1, "cm"),
          legend.position = "right"
        )

      if (isTRUE(input$show_points)) {
        p <- p + ggplot2::geom_jitter(
          width = input$box_width / 4,
          alpha = 0.6,
          size = input$point_size,
          color = "black",
          na.rm = TRUE
        )
      }

      if (isTRUE(input$show_significance)) {
        if (input$stat_method %in% c("t.test", "wilcox.test") && !is.null(comparison_list())) {
          p <- p + ggpubr::stat_compare_means(
            method = input$stat_method,
            comparisons = comparison_list(),
            label = "p.signif",
            size = 4
          )
        }

        if (input$stat_method == "anova") {
          p <- p + ggpubr::stat_compare_means(
            method = "anova",
            label = "p.format",
            label.y = max(dat$Value, na.rm = TRUE) * 1.08,
            size = 4
          )
        }

        if (input$stat_method == "kruskal.test") {
          p <- p + ggpubr::stat_compare_means(
            method = "kruskal.test",
            label = "p.format",
            label.y = max(dat$Value, na.rm = TRUE) * 1.08,
            size = 4
          )
        }
      }

      p <- switch(
        input$theme,
        minimal = p + ggplot2::theme_minimal(base_size = 14),
        classic = p + ggplot2::theme_classic(base_size = 14),
        light   = p + ggplot2::theme_light(base_size = 14),
        bw      = p + ggplot2::theme_bw(base_size = 14),
        dark    = p + ggplot2::theme_dark(base_size = 14),
        grey    = p + ggplot2::theme_grey(base_size = 14),
        p + ggplot2::theme_grey(base_size = 14)
      )

      p
    }

    output$plot_ui <- shiny::renderUI({
      if (!isTRUE(rv$plot_ready)) {
        shiny::div(
          style = paste(
            "height: 380px;",
            "display: flex;",
            "align-items: center;",
            "justify-content: center;",
            "color: #666;",
            "font-size: 16px;",
            "border: 1px dashed #ccc;",
            "border-radius: 10px;",
            "background-color: #fafafa;"
          ),
          "Please upload data or load demo data, set parameters, and click 'Run' to generate the boxplot."
        )
      } else {
        shiny::plotOutput(session$ns("boxplot"), height = "380px")
      }
    })

    output$stats_ui <- shiny::renderUI({
      if (!isTRUE(rv$plot_ready)) {
        shiny::div(
          style = paste(
            "min-height: 140px;",
            "display: flex;",
            "align-items: center;",
            "justify-content: center;",
            "color: #666;",
            "font-size: 15px;",
            "border: 1px dashed #ccc;",
            "border-radius: 10px;",
            "background-color: #fafafa;"
          ),
          "Statistical results will appear here after clicking 'Run'."
        )
      } else {
        shiny::tagList(
          if (!is.null(rv$stats_pairwise)) shiny::h5(style = "margin-top: 0; font-weight: 600;", "Pairwise / Overall Results"),
          if (!is.null(rv$stats_pairwise)) DT::DTOutput(session$ns("stats_pairwise_table")),
          if (!is.null(rv$stats_anova)) shiny::br(),
          if (!is.null(rv$stats_anova)) shiny::h5(style = "font-weight: 600;", "ANOVA Results"),
          if (!is.null(rv$stats_anova)) DT::DTOutput(session$ns("stats_anova_table")),
          if (!is.null(rv$stats_tukey)) shiny::br(),
          if (!is.null(rv$stats_tukey)) shiny::h5(style = "font-weight: 600;", "TukeyHSD Results"),
          if (!is.null(rv$stats_tukey)) DT::DTOutput(session$ns("stats_tukey_table"))
        )
      }
    })

    shiny::observe({
      shinyjs::toggleState("download_plot", condition = isTRUE(rv$plot_ready))
      shinyjs::toggleState("download_stats", condition = isTRUE(rv$plot_ready))
    })

    shiny::observeEvent(input$run, {
      dat <- long_df()

      rv$stats_pairwise <- NULL
      rv$stats_anova <- NULL
      rv$stats_tukey <- NULL

      if (input$stat_method %in% c("t.test", "wilcox.test")) {
        rv$stats_pairwise <- make_pairwise_stats(dat, input$stat_method)
      }

      if (input$stat_method == "anova") {
        rv$stats_anova <- make_anova_stats(dat)
        rv$stats_tukey <- make_tukey_stats(dat)
      }

      if (input$stat_method == "kruskal.test") {
        rv$stats_pairwise <- make_kruskal_stats(dat)
      }

      rv$plot_obj <- make_plot(dat)
      rv$plot_ready <- TRUE
    })

    output$boxplot <- shiny::renderPlot({
      shiny::req(rv$plot_ready, rv$plot_obj)
      rv$plot_obj
    })

    output$stats_pairwise_table <- DT::renderDT({
      shiny::req(rv$stats_pairwise)
      DT::datatable(
        rv$stats_pairwise,
        options = list(pageLength = 8, scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$stats_anova_table <- DT::renderDT({
      shiny::req(rv$stats_anova)
      DT::datatable(
        rv$stats_anova,
        options = list(pageLength = 8, scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$stats_tukey_table <- DT::renderDT({
      shiny::req(rv$stats_tukey)
      DT::datatable(
        rv$stats_tukey,
        options = list(pageLength = 8, scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        paste0("boxplot_", Sys.Date(), ".", input$download_format)
      },
      content = function(file) {
        shiny::req(rv$plot_ready, rv$plot_obj)

        fmt <- tolower(input$download_format)

        if (fmt %in% c("png", "jpg", "jpeg")) {
          ggplot2::ggsave(
            filename = file,
            plot = rv$plot_obj,
            width = max(1, input$plot_width),
            height = max(1, input$plot_height),
            dpi = input$plot_dpi
          )
        } else if (fmt %in% c("pdf", "svg")) {
          ggplot2::ggsave(
            filename = file,
            plot = rv$plot_obj,
            width = max(1, input$plot_width),
            height = max(1, input$plot_height)
          )
        }
      }
    )

    output$download_stats <- shiny::downloadHandler(
      filename = function() {
        paste0("boxplot_statistics_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        shiny::req(rv$plot_ready)

        sheets <- list()

        if (!is.null(rv$stats_pairwise)) {
          sheets[["Results"]] <- rv$stats_pairwise
        }
        if (!is.null(rv$stats_anova)) {
          sheets[["ANOVA_Results"]] <- rv$stats_anova
        }
        if (!is.null(rv$stats_tukey)) {
          sheets[["TukeyHSD_Results"]] <- rv$stats_tukey
        }

        if (length(sheets) == 0) {
          sheets[["Results"]] <- data.frame(Message = "No statistics available.")
        }

        openxlsx::write.xlsx(sheets, file, rowNames = FALSE)
      }
    )

    return(
      list(
        plot = shiny::reactive(rv$plot_obj),
        stats = shiny::reactive(rv$stats_pairwise),
        anova_stats = shiny::reactive(rv$stats_anova),
        tukey_stats = shiny::reactive(rv$stats_tukey),
        data = long_df
      )
    )
  })
}
