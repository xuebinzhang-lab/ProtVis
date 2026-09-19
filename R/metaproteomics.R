utils::globalVariables(c(".data", "Sample", "Group", "ProteinID", "Intensity", "Taxon", "Function", "value"))

#' Built-in demo data for the metaproteomics module
#'
#' @return A named list containing abundance, taxonomy, and function tables.
#' @export
metaproteomics_demo_data <- function() {
  abundance <- data.frame(
    ProteinID = paste0("MP", sprintf("%03d", 1:12)),
    Control_1 = c(18, 30, 11, 24, 15, 19, 8, 12, 32, 21, 15, 10),
    Control_2 = c(20, 27, 12, 22, 17, 18, 9, 11, 30, 19, 16, 9),
    Control_3 = c(17, 29, 10, 25, 14, 20, 7, 13, 31, 20, 15, 11),
    Treatment_1 = c(29, 14, 22, 13, 26, 28, 19, 9, 16, 30, 25, 21),
    Treatment_2 = c(31, 16, 24, 12, 28, 26, 20, 10, 15, 32, 24, 19),
    Treatment_3 = c(30, 15, 23, 14, 27, 27, 18, 8, 17, 31, 26, 20),
    check.names = FALSE
  )

  taxonomy <- data.frame(
    ProteinID = abundance$ProteinID,
    Phylum = c(
      "Firmicutes", "Bacteroidota", "Proteobacteria", "Actinobacteriota",
      "Firmicutes", "Firmicutes", "Bacteroidota", "Proteobacteria",
      "Actinobacteriota", "Firmicutes", "Bacteroidota", "Proteobacteria"
    ),
    Genus = c(
      "Faecalibacterium", "Bacteroides", "Escherichia", "Bifidobacterium",
      "Roseburia", "Lactobacillus", "Prevotella", "Pseudomonas",
      "Collinsella", "Ruminococcus", "Alistipes", "Klebsiella"
    ),
    Species = c(
      "F. prausnitzii", "B. vulgatus", "E. coli", "B. longum",
      "R. intestinalis", "L. plantarum", "P. copri", "P. aeruginosa",
      "C. aerofaciens", "R. bromii", "A. putredinis", "K. pneumoniae"
    ),
    check.names = FALSE
  )

  function_table <- data.frame(
    ProteinID = abundance$ProteinID,
    KO = c("K01689", "K01810", "K00844", "K01190", "K01803", "K00174", "K00626", "K02003", "K00927", "K01834", "K01647", "K01915"),
    Pathway = c(
      "Butanoate metabolism", "Glycolysis / Gluconeogenesis", "Carbon metabolism",
      "Starch and sucrose metabolism", "Pyruvate metabolism", "Methane metabolism",
      "Amino sugar metabolism", "ABC transporters", "Purine metabolism",
      "Propanoate metabolism", "TCA cycle", "Fatty acid biosynthesis"
    ),
    COG = c("Energy production", "Carbohydrate transport", "Carbohydrate transport", "Carbohydrate transport", "Energy production", "Energy production", "Cell wall biogenesis", "Transport", "Nucleotide metabolism", "Energy production", "Energy production", "Lipid metabolism"),
    check.names = FALSE
  )

  list(abundance = abundance, taxonomy = taxonomy, "function" = function_table)
}

#' Metaproteomics UI Module
#'
#' @param id A unique module id.
#' @return A Shiny UI definition.
#' @import shiny
#' @import bslib
#' @export
metaproteomics_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 340,
        bslib::accordion(
          open = c("Data Input", "Analysis Settings"),
          bslib::accordion_panel(
            "Data Input",
            shiny::p("Upload abundance, taxonomy, and function annotation tables or run the built-in metaproteomics demo."),
            shiny::fileInput(ns("abundance_file"), "Abundance CSV", accept = ".csv"),
            shiny::fileInput(ns("taxonomy_file"), "Taxonomy annotation CSV", accept = ".csv"),
            shiny::fileInput(ns("function_file"), "Function annotation CSV", accept = ".csv"),
            shiny::actionButton(ns("use_demo"), "Use Built-in Demo", class = "btn btn-outline-primary w-100"),
            shiny::br(), shiny::br(),
            shiny::downloadButton(ns("download_demo_abundance"), "Demo Abundance CSV"),
            shiny::downloadButton(ns("download_demo_taxonomy"), "Demo Taxonomy CSV"),
            shiny::downloadButton(ns("download_demo_function"), "Demo Function CSV"),
            shiny::uiOutput(ns("data_status"))
          ),
          bslib::accordion_panel(
            "Analysis Settings",
            shiny::selectInput(ns("tax_level"), "Taxonomic Level", choices = c("Phylum", "Genus", "Species"), selected = "Genus"),
            shiny::selectInput(ns("function_level"), "Function Level", choices = c("Pathway", "KO", "COG"), selected = "Pathway"),
            shiny::checkboxInput(ns("relative_abundance"), "Use relative abundance", value = TRUE),
            shiny::numericInput(ns("top_n"), "Top categories", value = 10, min = 3, max = 30, step = 1),
            shiny::actionButton(ns("run_analysis"), "Run Metaproteomics Analysis", class = "btn btn-success w-100")
          ),
          bslib::accordion_panel(
            "Download",
            shiny::downloadButton(ns("download_merged"), "Download Merged Table")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Metaproteomics overview"),
        bslib::layout_column_wrap(
          width = 1 / 3,
          bslib::value_box("Proteins", shiny::textOutput(ns("n_proteins")), theme = "primary"),
          bslib::value_box("Taxa", shiny::textOutput(ns("n_taxa")), theme = "success"),
          bslib::value_box("Functions", shiny::textOutput(ns("n_functions")), theme = "warning")
        ),
        shiny::hr(),
        shiny::tabsetPanel(
          shiny::tabPanel("Merged table", DT::DTOutput(ns("merged_table"))),
          shiny::tabPanel("Taxonomy composition", shiny::plotOutput(ns("taxonomy_plot"), height = "520px")),
          shiny::tabPanel("Function composition", shiny::plotOutput(ns("function_plot"), height = "520px")),
          shiny::tabPanel("Taxon-function Sankey", plotly::plotlyOutput(ns("sankey_plot"), height = "560px")),
          shiny::tabPanel("Taxon-function heatmap", shiny::plotOutput(ns("heatmap_plot"), height = "560px"))
        )
      )
    )
  )
}

#' Metaproteomics Server Module
#'
#' @param id A unique module id.
#' @return No return value. Called for side effects.
#' @import shiny
#' @export
metaproteomics_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(data = metaproteomics_demo_data(), result = NULL)

    read_csv_file <- function(file) {
      shiny::req(file)
      utils::read.csv(file$datapath, check.names = FALSE, stringsAsFactors = FALSE)
    }

    observeEvent(input$use_demo, {
      rv$data <- metaproteomics_demo_data()
      rv$result <- NULL
      shiny::showNotification("Built-in metaproteomics demo data loaded.", type = "message")
    })

    observeEvent(list(input$abundance_file, input$taxonomy_file, input$function_file), {
      if (!is.null(input$abundance_file)) rv$data$abundance <- read_csv_file(input$abundance_file)
      if (!is.null(input$taxonomy_file)) rv$data$taxonomy <- read_csv_file(input$taxonomy_file)
      if (!is.null(input$function_file)) rv$data[["function"]] <- read_csv_file(input$function_file)
      rv$result <- NULL
    }, ignoreInit = TRUE)

    validate_inputs <- function(dat) {
      required_id <- "ProteinID"
      if (!required_id %in% names(dat$abundance)) stop("Abundance table must contain ProteinID.", call. = FALSE)
      if (!required_id %in% names(dat$taxonomy)) stop("Taxonomy table must contain ProteinID.", call. = FALSE)
      if (!required_id %in% names(dat[["function"]])) stop("Function table must contain ProteinID.", call. = FALSE)
      tax_cols <- c("Phylum", "Genus", "Species")
      fun_cols <- c("KO", "Pathway", "COG")
      if (!all(tax_cols %in% names(dat$taxonomy))) stop("Taxonomy table must contain Phylum, Genus, and Species.", call. = FALSE)
      if (!all(fun_cols %in% names(dat[["function"]]))) stop("Function table must contain KO, Pathway, and COG.", call. = FALSE)
      TRUE
    }

    make_result <- function(dat) {
      validate_inputs(dat)
      sample_cols <- setdiff(names(dat$abundance), "ProteinID")
      abundance_long <- tidyr::pivot_longer(
        dat$abundance,
        cols = tidyselect::all_of(sample_cols),
        names_to = "Sample",
        values_to = "Intensity"
      )
      abundance_long$Intensity <- suppressWarnings(as.numeric(abundance_long$Intensity))
      abundance_long$Group <- ifelse(grepl("treat|case|disease", abundance_long$Sample, ignore.case = TRUE), "Treatment", "Control")
      merged <- abundance_long |>
        dplyr::left_join(dat$taxonomy, by = "ProteinID") |>
        dplyr::left_join(dat[["function"]], by = "ProteinID") |>
        dplyr::filter(!is.na(Intensity))
      merged
    }

    observeEvent(input$run_analysis, {
      tryCatch({
        rv$result <- make_result(rv$data)
        shiny::showNotification("Metaproteomics analysis finished.", type = "message")
      }, error = function(e) {
        shiny::showNotification(e$message, type = "error", duration = NULL)
      })
    })

    result_data <- shiny::reactive({
      if (is.null(rv$result)) make_result(rv$data) else rv$result
    })

    output$data_status <- shiny::renderUI({
      dat <- rv$data
      shiny::tags$small(
        sprintf(
          "Loaded: %s proteins, %s taxonomy rows, %s function rows.",
          nrow(dat$abundance), nrow(dat$taxonomy), nrow(dat[["function"]])
        )
      )
    })

    output$n_proteins <- shiny::renderText(length(unique(result_data()$ProteinID)))
    output$n_taxa <- shiny::renderText(length(unique(result_data()[[input$tax_level]])))
    output$n_functions <- shiny::renderText(length(unique(result_data()[[input$function_level]])))

    output$merged_table <- DT::renderDT({
      DT::datatable(result_data(), options = list(pageLength = 10, scrollX = TRUE))
    })

    summarise_category <- function(df, category_col) {
      out <- df |>
        dplyr::group_by(Sample, Group, .data[[category_col]]) |>
        dplyr::summarise(Intensity = sum(Intensity, na.rm = TRUE), .groups = "drop")
      names(out)[names(out) == category_col] <- "Category"
      top_categories <- out |>
        dplyr::group_by(Category) |>
        dplyr::summarise(Total = sum(Intensity, na.rm = TRUE), .groups = "drop") |>
        dplyr::arrange(dplyr::desc(Total)) |>
        dplyr::slice_head(n = input$top_n) |>
        dplyr::pull(Category)
      out$Category <- ifelse(out$Category %in% top_categories, out$Category, "Other")
      out <- out |>
        dplyr::group_by(Sample, Group, Category) |>
        dplyr::summarise(Intensity = sum(Intensity, na.rm = TRUE), .groups = "drop")
      if (isTRUE(input$relative_abundance)) {
        out <- out |>
          dplyr::group_by(Sample) |>
          dplyr::mutate(Intensity = Intensity / sum(Intensity, na.rm = TRUE)) |>
          dplyr::ungroup()
      }
      out
    }

    make_bar_plot <- function(df, title, y_label) {
      ggplot2::ggplot(df, ggplot2::aes(x = Sample, y = Intensity, fill = Category)) +
        ggplot2::geom_col(width = 0.78, color = "white", linewidth = 0.15) +
        ggplot2::facet_grid(~Group, scales = "free_x", space = "free_x") +
        ggplot2::labs(title = title, x = NULL, y = y_label, fill = NULL) +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          panel.grid.major.x = ggplot2::element_blank(),
          legend.position = "right"
        )
    }

    output$taxonomy_plot <- shiny::renderPlot({
      df <- summarise_category(result_data(), input$tax_level)
      make_bar_plot(df, paste("Taxonomy composition by", input$tax_level), ifelse(input$relative_abundance, "Relative abundance", "Summed intensity"))
    })

    output$function_plot <- shiny::renderPlot({
      df <- summarise_category(result_data(), input$function_level)
      make_bar_plot(df, paste("Function composition by", input$function_level), ifelse(input$relative_abundance, "Relative abundance", "Summed intensity"))
    })

    output$sankey_plot <- plotly::renderPlotly({
      df <- result_data() |>
        dplyr::group_by(.data[[input$tax_level]], .data[[input$function_level]]) |>
        dplyr::summarise(value = sum(Intensity, na.rm = TRUE), .groups = "drop")
      names(df)[1:2] <- c("Taxon", "Function")
      nodes <- data.frame(name = unique(c(df$Taxon, df$Function)), stringsAsFactors = FALSE)
      df$source <- match(df$Taxon, nodes$name) - 1
      df$target <- match(df$Function, nodes$name) - 1
      plotly::plot_ly(
        type = "sankey",
        orientation = "h",
        node = list(label = nodes$name, pad = 14, thickness = 16),
        link = list(source = df$source, target = df$target, value = df$value)
      )
    })

    output$heatmap_plot <- shiny::renderPlot({
      df <- result_data() |>
        dplyr::group_by(.data[[input$tax_level]], .data[[input$function_level]]) |>
        dplyr::summarise(Intensity = sum(Intensity, na.rm = TRUE), .groups = "drop")
      names(df)[1:2] <- c("Taxon", "Function")
      ggplot2::ggplot(df, ggplot2::aes(x = Function, y = Taxon, fill = Intensity)) +
        ggplot2::geom_tile(color = "white") +
        ggplot2::scale_fill_gradient(low = "#eff6ff", high = "#1d4ed8") +
        ggplot2::labs(title = "Taxon-function abundance heatmap", x = input$function_level, y = input$tax_level, fill = "Intensity") +
        ggplot2::theme_minimal(base_size = 13) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    })

    output$download_merged <- shiny::downloadHandler(
      filename = function() paste0("metaproteomics_merged_", Sys.Date(), ".csv"),
      content = function(file) utils::write.csv(result_data(), file, row.names = FALSE)
    )

    output$download_demo_abundance <- shiny::downloadHandler(
      filename = function() "metaproteomics_demo_abundance.csv",
      content = function(file) utils::write.csv(metaproteomics_demo_data()$abundance, file, row.names = FALSE)
    )
    output$download_demo_taxonomy <- shiny::downloadHandler(
      filename = function() "metaproteomics_demo_taxonomy.csv",
      content = function(file) utils::write.csv(metaproteomics_demo_data()$taxonomy, file, row.names = FALSE)
    )
    output$download_demo_function <- shiny::downloadHandler(
      filename = function() "metaproteomics_demo_function.csv",
      content = function(file) utils::write.csv(metaproteomics_demo_data()[["function"]], file, row.names = FALSE)
    )
  })
}
