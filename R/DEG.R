#' @import shiny
#' @import bslib
#' @importFrom colourpicker colourInput
#' @importFrom bsicons bs_icon
#' @name DEG_ui
#' @title DEG Analysis UI
#' @description This function creates the user interface for DEG (Differential Expression Analysis) that includes file upload options, PCA plot settings, and volcano plot settings.
#' @param id A unique ID for the Shiny module, used to create input/output bindings.
#' @export
#'
DEG_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::fileInput(ns("data_count_file"), "Upload Gene Expression Data", accept = c(".xlsx")),
        shiny::helpText("Note: The first column name in expression file should be 'GeneID'"),
        shiny::fileInput(ns("group_file"), "Upload Group Information", accept = c(".xlsx")),
        shiny::helpText("Note: The first column in the group file should be 'Sample', and the second column should be 'Group'"),
        shiny::actionButton(ns("generate_plot"), "Run Analysis", class = "btn-primary"),
        shiny::hr(),
        bslib::accordion(
          bslib::accordion_panel(
            title = "PCA Settings",
            icon = pca_icon,
            shiny::selectInput(ns("pca_colby"), "Color by:",
                               choices = c("None" = "none"),
                               selected = "none"),
            shiny::selectInput(ns("pca_shapeby"), "Shape by:",
                               choices = c("None" = "none"),
                               selected = "none"),
            shiny::selectInput(ns("pca_pointsize"), "Point Size:",
                               choices = c("Small" = 2, "Medium" = 3, "Large" = 4),
                               selected = 3),
            shiny::uiOutput(ns("group_colors_ui")),
            colourpicker::colourInput(ns("pca_base_color"), "Base Color (when no grouping)", value = "#2E86AB"),
            shiny::checkboxInput(ns("pca_show_labels"), "Show Sample Labels", value = FALSE),
            shiny::checkboxInput(ns("pca_encircle"), "Encircle Groups", value = TRUE),
            shiny::checkboxInput(ns("pca_show_ellipse"), "Show Confidence Ellipse", value = TRUE),
            shiny::numericInput(ns("pca_ellipse_alpha"), "Ellipse Transparency",
                                value = 0.2, min = 0, max = 1, step = 0.1),
            shiny::numericInput(ns("pca_legend_size"), "Legend Text Size",
                                value = 12, min = 8, max = 20, step = 1),
            shiny::hr(),
            shiny::numericInput(ns("download_width_pca"), "Width of PCA Plot (inches)",
                                value = 8, min = 3, max = 20),
            shiny::numericInput(ns("download_height_pca"), "Height of PCA Plot (inches)",
                                value = 7, min = 3, max = 20),
            shiny::downloadButton(ns("download_pca"), "Download PCA Plot PDF", class = "btn-sm"),
            shiny::downloadButton(ns("download_pca_data"), "Download PCA Data", class = "btn-sm")
          ),
          bslib::accordion_panel(
            title = "Volcano Plot Settings",
            icon = volcano_icon,
            colourpicker::colourInput(ns("color_up"), "Color for Up-regulated", value = "salmon"),
            colourpicker::colourInput(ns("color_down"), "Color for Down-regulated", value = "lightblue"),
            colourpicker::colourInput(ns("color_not_sig"), "Color for Not Significant", value = "grey"),
            shiny::numericInput(ns("volcano_point_size"), "Point Size",
                                value = 2, min = 1, max = 5, step = 0.5),
            shiny::sliderInput(ns("volcano_alpha"), "Point Transparency",
                               min = 0.1, max = 1, value = 0.7, step = 0.1),
            shiny::checkboxInput(ns("volcano_show_grid"), "Show Grid", value = FALSE),
            shiny::hr(),
            shiny::numericInput(ns("download_width_voc"), "Width of Volcano Plot (inches)",
                                value = 8, min = 3, max = 20),
            shiny::numericInput(ns("download_height_voc"), "Height of Volcano Plot (inches)",
                                value = 7, min = 3, max = 20),
            shiny::downloadButton(ns("download_pdf"), "Download Volcano Plot PDF", class = "btn-sm"),
            shiny::downloadButton(ns("download_deg_data"), "Download DEG Data", class = "btn-sm")
          )
        )
      ),
      # 👇 👇 核心修改：直接删掉多余的 page_fluid()
      bslib::layout_column_wrap(
        width = 1/2,
        height = 750,
        bslib::card(
          height = "800px",
          bslib::card_header("PCA Analysis", icon = shiny::icon("chart-pie")),
          bslib::card_body(
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel("Plot",
                              shiny::plotOutput(ns("pca_plot"), height = "650px")
              ),
              shiny::tabPanel("PCA Data",
                              shiny::div(
                                style = "margin-bottom: 10px;",
                                shiny::downloadButton(ns("download_pca_table"), "Download as CSV",
                                                      class = "btn-sm btn-success", style = "float: right;")
                              ),
                              DT::DTOutput(ns("pca_data_table"), height = "600px")
              )
            )
          )
        ),
        bslib::card(
          height = "800px",
          bslib::card_header("Volcano Plot", icon = shiny::icon("fire")),
          bslib::card_body(
            shiny::tabsetPanel(
              type = "tabs",
              shiny::tabPanel("Plot",
                              shiny::plotOutput(ns("voc_plot"), height = "650px")
              ),
              shiny::tabPanel("DEG Results",
                              shiny::div(
                                style = "margin-bottom: 10px;",
                                shiny::downloadButton(ns("download_degs"), "Download as CSV",
                                                      class = "btn-sm btn-success", style = "float: right;")
                              ),
                              DT::DTOutput(ns("deg_table"), height = "600px")),
              shiny::tabPanel("Statistics",
                              bslib::card(
                                bslib::card_header("DEG Summary Statistics"),
                                shiny::tableOutput(ns("deg_stats"))),
                              bslib::card(
                                bslib::card_header("Top DEGs"),
                                DT::DTOutput(ns("top_degs_table"), height = "300px")
                              )
              )
            )
          )
        )
      )
    )
  )
}

#' @title DEG Analysis Server Logic
#' @description This function contains the server-side logic for performing DEG (Differential Expression Analysis), including PCA and volcano plot generation, and DEG result calculations.
#' @param id A unique ID for the Shiny module, used to create input/output bindings.
#' @import shiny
#' @importFrom readxl read_xlsx
#' @importFrom DESeq2 DESeqDataSetFromMatrix DESeq results
#' @importFrom dplyr mutate across everything case_when arrange desc filter select where
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom DT renderDT datatable formatStyle styleEqual
#' @importFrom grDevices pdf dev.off
#' @importFrom colourpicker updateColourInput
#' @name DEG_server
#' @export

utils::globalVariables(c("padj", "log2FoldChange", "regular",
                         "GeneID","baseMean","lfcSE","pvalue","Regulation"))

DEG_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # State management
    analysis_ready <- shiny::reactiveVal(FALSE)
    # Reactive loading data
    expression_data <- shiny::reactive({
      shiny::req(input$data_count_file)
      df <- readxl::read_xlsx(input$data_count_file$datapath)
      shiny::validate(
        shiny::need("GeneID" %in% base::colnames(df), "Error: Expression file must contain 'GeneID' column"),
        shiny::need(base::ncol(df) > 1, "Error: Expression file must contain sample columns")
      )
      return(df)
    })
    # Load grouping information
    group_data <- shiny::reactive({
      shiny::req(input$group_file)
      df <- readxl::read_xlsx(input$group_file$datapath)
      shiny::validate(
        shiny::need("Sample" %in% base::colnames(df), "Error: Group file must contain 'Sample' column"),
        shiny::need("Group" %in% base::colnames(df), "Error: Group file must contain 'Group' column")
      )
      return(df)
    })
    # Gets the column names of grouped data (used for PCA color and shape selection)
    group_columns <- shiny::reactive({
      shiny::req(group_data())
      cols <- base::colnames(group_data())
      # Exclude Sample column
      cols <- cols[cols != "Sample"]
      return(cols)
    })
    # Get different groups of the current grouping variable.
    selected_groups <- shiny::reactive({
      shiny::req(group_data(), input$pca_colby)
      if (input$pca_colby != "none") {
        groups <- base::unique(group_data()[[input$pca_colby]])
        return(base::sort(base::as.character(groups)))  # 确保是字符型并排序
      }
      return(NULL)
    })
    # Observe the change of packet data and update PCA setting options.
    shiny::observeEvent(group_data(), {
      cols <- group_columns()
      if(base::length(cols) > 0) {
        # Update color selection
        shiny::updateSelectInput(session, "pca_colby",
                          choices = c("None" = "none", cols),
                          selected = "Group")
        # Update shape selection
        shiny::updateSelectInput(session, "pca_shapeby",
                          choices = c("None" = "none", cols),
                          selected = "none")
      }
    })
    # Observe the change of grouping column selection
    shiny::observeEvent(input$pca_colby, {
      if (input$pca_colby != "none" && !is.null(group_data())) {
        # Clear the color input that may exist before.
        shiny::removeUI(
          selector = paste0("#", ns("group_colors_title")),
          immediate = TRUE
        )
      }
    })
    # Generate dynamic color selector
    output$group_colors_ui <- shiny::renderUI({
      groups <- selected_groups()
      if (is.null(groups) || input$pca_colby == "none") {
        return(NULL)  # If no grouping is selected or the grouping is "none", the color selector is not displayed.
      }
      # Generate a set of beautiful default colors
      default_colors <- c(
        "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
        "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
        "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
        "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA"
      )
      # Create color selectors for each group.
      color_pickers <- base::lapply(base::seq_along(groups), function(i) {
        group <- groups[i]
        default_color <- default_colors[(i-1) %% base::length(default_colors) + 1]
        # Create a unique ID for each group.
        group_id <- base::gsub("[^A-Za-z0-9]", "_", group)
        shiny::tagList(
          colourpicker::colourInput(
            ns(paste0("color_", group_id)),
            label = paste("Color for:", group),
            value = default_color
          )
        )
      })
      # Add a reset button
      reset_button <- shiny::actionButton(
        ns("reset_colors"),
        "Reset Colors to Default",
        icon = shiny::icon("refresh"),
        class = "btn-sm btn-outline-secondary"
      )
      shiny::tagList(
        shiny::h5("Customize Group Colors:", id = ns("group_colors_title")),
        shiny::br(),
        color_pickers,
        shiny::br(),
        reset_button
      )
    })
    # Handle color reset button
    shiny::observeEvent(input$reset_colors, {
      groups <- selected_groups()
      if (!is.null(groups)) {
        default_colors <- c(
          "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
          "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5",
          "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F",
          "#E5C494", "#B3B3B3", "#8DD3C7", "#FFFFB3", "#BEBADA"
        )
        for (i in base::seq_along(groups)) {
          group <- groups[i]
          default_color <- default_colors[(i-1) %% base::length(default_colors) + 1]
          group_id <- base::gsub("[^A-Za-z0-9]", "_", group)
          colourpicker::updateColourInput(
            session,
            base::paste0("color_", group_id),
            value = default_color
          )
        }
      }
    })
    # Gets the color selected by the user.
    get_group_colors <- shiny::reactive({
      groups <- selected_groups()
      if (is.null(groups) || input$pca_colby == "none") {
        return(NULL)
      }
      colors <- base::character(0)
      for (group in groups) {
        group_id <- base::gsub("[^A-Za-z0-9]", "_", group)
        color_input <- base::paste0("color_", group_id)
        if (!is.null(input[[color_input]])) {
          colors <- c(colors, input[[color_input]])
        } else {
          # If the color is not set, the default color is used.
          default_colors <- c(
            "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
            "#FFFF33", "#A65628", "#F781BF", "#999999", "#66C2A5"
          )
          default_color <- default_colors[(which(groups == group) - 1) %% base::length(default_colors) + 1]
          colors <- c(colors, default_color)
        }
      }
      base::names(colors) <- groups
      return(colors)
    })
    # Create sample information
    sample_info <- shiny::reactive({
      shiny::req(group_data())
      col_data <- group_data() %>%
        tibble::column_to_rownames("Sample")
      return(col_data)
    })
    # Operational analysis
    shiny::observeEvent(input$generate_plot, {
      # Verification data
      shiny::validate(
        shiny::need(!is.null(expression_data()), "Please upload expression data"),
        shiny::need(!is.null(sample_info()), "Please upload group information"),
        shiny::need(nrow(expression_data()) > 0, "Expression data is empty"),
        shiny::need(nrow(sample_info()) > 0, "Group information is empty")
      )
      # Check whether the sample names match.
      expr_samples <- base::colnames(expression_data())[-1]  # Exclude GeneID column
      group_samples <- base::rownames(sample_info())
      shiny::validate(
        shiny::need(all(expr_samples %in% group_samples),
                    base::paste("Error: Sample names in expression data do not match group data.\n",
                   "Expression samples:", base::paste(expr_samples, collapse = ", "), "\n",
                   "Group samples:", base::paste(group_samples, collapse = ", ")))
      )
      analysis_ready(TRUE)
    })
    # Perform PCA analysis
    pca_result <- shiny::reactive({
      shiny::req(analysis_ready(), expression_data(), sample_info())
      # Extract expression matrix
      expr_mat <- expression_data() %>%
        tibble::column_to_rownames("GeneID") %>%
        base::as.matrix()
      # Ensure that the sample order is consistent.
      expr_mat <- expr_mat[, base::rownames(sample_info()), drop = FALSE]
      # Run PCA
      pca <- PCAtools::pca(expr_mat, metadata = sample_info(), removeVar = 0.1)
      return(pca)
    })
    # Draw PCA diagram
    pca_plot_obj <- shiny::reactive({
      shiny::req(pca_result())
      # Get color and shape settings
      colby <- input$pca_colby
      shapeby <- input$pca_shapeby
      show_labels <- input$pca_show_labels
      encircle <- input$pca_encircle
      show_ellipse <- input$pca_show_ellipse
      ellipse_alpha <- input$pca_ellipse_alpha
      point_size <- base::as.numeric(input$pca_pointsize)
      base_color <- input$pca_base_color
      legend_size <- input$pca_legend_size
      # Basic PCA diagram setting
      pca_args <- base::list(
        pca_result(),
        x = "PC1",
        y = "PC2",
        legendPosition = "right",
        legendLabSize = legend_size,
        legendIconSize = 6,
        pointSize = point_size,
        title = "PCA Plot",
        subtitle = "Principal Component Analysis"
      )
      # Set color
      if (colby != "none") {
        pca_args$colby <- colby
        # Gets the user-defined color.
        group_colors <- get_group_colors()
        if (base::length(group_colors) > 0) {
          pca_args$colkey <- group_colors
        }
      } else {
        pca_args$colby <- NULL
        pca_args$colkey <- base_color
      }
      # Set the shape
      if (shapeby != "none") {
        pca_args$shape <- shapeby
      } else {
        pca_args$shape <- NULL
      }
      # Set sample label
      if (show_labels) {
        pca_args$lab <- base::rownames(pca_result()$metadata)
      } else {
        pca_args$lab <- NULL
      }
      # Set ellipse
      if (encircle && colby != "none" && show_ellipse) {
        pca_args$encircle <- TRUE
        pca_args$encircleFill <- TRUE
        pca_args$encircleAlpha <- ellipse_alpha
        pca_args$encircleLineSize <- 1
      } else {
        pca_args$encircle <- FALSE
      }
      # draw a graph
      pca_plot <- base::do.call(PCAtools::biplot, pca_args)
      return(pca_plot)
    })
    # Prepare PCA data table (show pca_result$rotated)
    pca_rotated_data <- shiny::reactive({
      shiny::req(pca_result())
      # Get the rotated coordinates.
      rotated_data <- base::as.data.frame(pca_result()$rotated)
      rotated_data <- rotated_data[, 1:base::min(10, base::ncol(rotated_data))]  # Only the top 10 principal components are displayed.
      # Add sample name
      rotated_data <- base::cbind(
        Sample = base::rownames(rotated_data),
        rotated_data
      )
      # Add grouping information
      if (!is.null(sample_info())) {
        rotated_data <- base::cbind(
          rotated_data,
          sample_info()
        )
      }
      return(rotated_data)
    })
    # Perform differential expression analysis.
    deseq_results <- shiny::eventReactive(input$generate_plot, {
      shiny::req(expression_data(), sample_info())
      shiny::withProgress(message = 'Running DESeq2 analysis...', value = 0.3, {
        # Prepare counting matrix
        count_mat <- expression_data() %>%
          tibble::column_to_rownames("GeneID") %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), ceiling)) %>%
          base::as.matrix()
        # Ensure that the sample order is consistent.
        count_mat <- count_mat[, base::rownames(sample_info()), drop = FALSE]
        # Create a DESeq2 object
        shiny::incProgress(0.2, detail = "Creating DESeq2 object...")
        dds <- DESeq2::DESeqDataSetFromMatrix(
          countData = count_mat,
          colData = sample_info(),
          design = ~ Group
        )
        # run DESeq2
        shiny::incProgress(0.3, detail = "Running DESeq2...")
        dds <- DESeq2::DESeq(dds)
        # Get results
        shiny::incProgress(0.2, detail = "Extracting results...")
        res <- DESeq2::results(dds, contrast = c("Group", "B73", "Y12"))
        # Collate results
        res_tbl <- res %>%
          base::as.data.frame() %>%
          tibble::rownames_to_column("GeneID") %>%
          dplyr::mutate(
            regular = dplyr::case_when(
              padj < 0.05 & log2FoldChange > 1 ~ "up",
              padj < 0.05 & log2FoldChange < -1 ~ "down",
              TRUE ~ "not sig"
            ),
            significant = base::ifelse(padj < 0.05 & base::abs(log2FoldChange) > 1, "yes", "no"),
            Regulation = dplyr::case_when(
              regular == "up" ~ "Up-regulated",
              regular == "down" ~ "Down-regulated",
              TRUE ~ "Not significant"
            )
          ) %>%
          dplyr::arrange(padj, dplyr::desc(base::abs(log2FoldChange)))
        return(res_tbl)
      })
    })
    # Get DEG statistics
    deg_stats <- shiny::reactive({
      shiny::req(deseq_results())
      res_tbl <- deseq_results()
      stats <- base::list(
        total_genes = base::nrow(res_tbl),
        up_regulated = base::sum(res_tbl$regular == "up", na.rm = TRUE),
        down_regulated = base::sum(res_tbl$regular == "down", na.rm = TRUE),
        significant = base::sum(res_tbl$regular %in% c("up", "down"), na.rm = TRUE),
        percent_sig = base::round(base::sum(res_tbl$regular %in% c("up", "down"), na.rm = TRUE) / base::nrow(res_tbl) * 100, 2)
      )
      return(stats)
    })
    # obtain top DEGs
    top_degs <- shiny::reactive({
      shiny::req(deseq_results())
      res_tbl <- deseq_results()
      # Obtaining significantly differentially expressed genes
      sig_genes <- res_tbl %>%
        dplyr::filter(regular %in% c("up", "down")) %>%
        dplyr::arrange(padj, dplyr::desc(base::abs(log2FoldChange))) %>%
        utils::head(20)
      return(sig_genes)
    })
    # Draw a volcano map
    voc_plot_obj <- shiny::reactive({
      shiny::req(deseq_results())
      res_tbl <- deseq_results()
      # Calculation statistics are used for subheadings
      stats <- deg_stats()
      # Create a volcano map
      p <- ggplot2::ggplot(res_tbl, ggplot2::aes(x = log2FoldChange, y = -log10(padj))) +
        ggplot2::geom_point(ggplot2::aes(color = regular),
                            size = input$volcano_point_size,
                            alpha = input$volcano_alpha) +
        ggplot2::scale_color_manual(
          values = c(
            "up" = input$color_up,
            "down" = input$color_down,
            "not sig" = input$color_not_sig
          ),
          name = "Expression"
        ) +
        ggplot2::geom_hline(
          yintercept = -log10(0.05),
          linetype = "dashed",
          color = "black",
          alpha = 0.5
        ) +
        ggplot2::geom_vline(
          xintercept = c(-1, 1),
          linetype = "dashed",
          color = "black",
          alpha = 0.5
        ) +
        ggplot2::labs(
          title = "Volcano Plot",
          subtitle = paste(
            "Up-regulated:", stats$up_regulated,
            "| Down-regulated:", stats$down_regulated,
            "| Total significant:", stats$significant,
            base::paste0("(", stats$percent_sig, "%)")
          ),
          x = "log2(Fold Change)",
          y = "-log10(Adjusted p-value)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 16, face = "bold"),
          plot.subtitle = ggplot2::element_text(size = 12, color = "gray50"),
          axis.title = ggplot2::element_text(size = 12),
          legend.position = "right",
          panel.grid = if(input$volcano_show_grid) ggplot2::element_line(color = "gray90") else ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.5)
        ) +
        ggplot2::coord_cartesian(ylim = c(0, base::max(-log10(res_tbl$padj[base::is.finite(-log10(res_tbl$padj))]), na.rm = TRUE) * 1.1))
      return(p)
    })
    # Rendering PCA diagram
    output$pca_plot <- shiny::renderPlot({
      shiny::req(pca_plot_obj())
      pca_plot_obj()
    })
    # Render a volcano map
    output$voc_plot <- shiny::renderPlot({
      shiny::req(voc_plot_obj())
      voc_plot_obj()
    })
    # Render PCA data table (show pca_result$rotated)
    output$pca_data_table <- DT::renderDT({
      shiny::req(pca_rotated_data())
      DT::datatable(
        pca_rotated_data(),
        extensions = c('Buttons', 'Scroller'),
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = 550,
          scroller = TRUE
        ),
        rownames = FALSE,
        class = 'display compact'
      )
    })
    # Render DEG result table (display res_tbl)
    output$deg_table <- DT::renderDT({
      shiny::req(deseq_results())
      res_tbl <- deseq_results() %>%
        dplyr::select(GeneID, baseMean, log2FoldChange, lfcSE, stat, pvalue, padj, Regulation) %>%
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), ~ base::round(., 4)),
          padj = base::format(padj, scientific = TRUE, digits = 3)
        )
      DT::datatable(
        res_tbl,
        extensions = c('Buttons', 'Scroller'),
        options = list(
          pageLength = 10,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE,
          scrollY = 550,
          scroller = TRUE
        ),
        rownames = FALSE,
        class = 'display compact'
      ) %>%
        DT::formatStyle(
          'Regulation',
          backgroundColor = DT::styleEqual(
            c('Up-regulated', 'Down-regulated', 'Not significant'),
            c('#FFCCCC', '#CCE5FF', '#F2F2F2')
          )
        )
    })
    # Render DEG statistics table
    output$deg_stats <- shiny::renderTable({
      shiny::req(deg_stats())
      stats <- deg_stats()
      base::data.frame(
        Statistic = c("Total Genes", "Up-regulated", "Down-regulated",
                      "Total Significant", "Percentage Significant"),
        Value = c(
          stats$total_genes,
          paste(stats$up_regulated, "genes"),
          paste(stats$down_regulated, "genes"),
          paste(stats$significant, "genes"),
          paste(stats$percent_sig, "%")
        )
      )
    }, align = 'lr')
    # Render Top DEGs table
    output$top_degs_table <- DT::renderDT({
      shiny::req(top_degs())
      top_genes <- top_degs() %>%
        dplyr::select(GeneID, log2FoldChange, padj, Regulation) %>%
        dplyr::mutate(
          log2FoldChange = base::round(log2FoldChange, 3),
          padj = base::format(padj, scientific = TRUE, digits = 3)
        )
      DT::datatable(
        top_genes,
        extensions = c('Buttons', 'Scroller'),
        options = list(
          pageLength = 5,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          scrollX = TRUE
        ),
        rownames = FALSE,
        class = 'display compact'
      ) %>%
        DT::formatStyle(
          'Regulation',
          backgroundColor = DT::styleEqual(
            c('Up-regulated', 'Down-regulated', 'Not significant'),
            c('#FFCCCC', '#CCE5FF', '#F2F2F2')
          )
        )
    })
    # Download PCA diagram
    output$download_pca <- shiny::downloadHandler(
      filename = function() {
        base::paste("PCA_plot_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        shiny::req(pca_plot_obj())
        grDevices::pdf(file, width = input$download_width_pca, height = input$download_height_pca)
        print(pca_plot_obj())
        grDevices::dev.off()
      }
    )
    # Download volcano map
    output$download_pdf <- shiny::downloadHandler(
      filename = function() {
        base::paste("volcano_plot_", base::Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        shiny::req(voc_plot_obj())
        grDevices::pdf(file, width = input$download_width_voc, height = input$download_height_voc)
        print(voc_plot_obj())
        grDevices::dev.off()
      }
    )
    output$download_pca_table <- shiny::downloadHandler(
      filename = function() {
        base::paste("pca_rotated_data_", base::Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(pca_rotated_data())
        utils::write.csv(pca_rotated_data(), file, row.names = FALSE)
      }
    )
    # Download PCA data (from the sidebar button)
    output$download_pca_data <- shiny::downloadHandler(
      filename = function() {
        base::paste("pca_rotated_data_", base::Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(pca_rotated_data())
        utils::write.csv(pca_rotated_data(), file, row.names = FALSE)
      }
    )
    # Download DEG data (res_tbl)-from the sidebar button
    output$download_deg_data <- shiny::downloadHandler(
      filename = function() {
        base::paste("deg_analysis_results_", base::Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(deseq_results())
        utils::write.csv(deseq_results(), file, row.names = FALSE)
      }
    )
    # Download DEG data (res_tbl)-from the button in the table
    output$download_degs <- shiny::downloadHandler(
      filename = function() {
        base::paste("deg_results_", base::Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        shiny::req(deseq_results())
        utils::write.csv(deseq_results(), file, row.names = FALSE)
      }
    )
  })
}
