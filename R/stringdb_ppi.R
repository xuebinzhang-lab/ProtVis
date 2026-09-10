#' STRINGdb PPI Network UI
#'
#' @param id A unique module ID.
#'
#' @return A Shiny UI.
#' @export
#'
#' @import shiny
stringdb_ppi_ui <- function(id) {
  ns <- shiny::NS(id)

  common_species <- c(
    "Human | Homo sapiens | 9606" = "9606",
    "Mouse | Mus musculus | 10090" = "10090",
    "Rat | Rattus norvegicus | 10116" = "10116",
    "Arabidopsis | Arabidopsis thaliana | 3702" = "3702",
    "Rice | Oryza sativa | 4530" = "4530",
    "Maize | Zea mays | 4577" = "4577",
    "Soybean | Glycine max | 3847" = "3847",
    "Tomato | Solanum lycopersicum | 4081" = "4081",
    "Setaria viridis | 4556" = "4556",
    "Yeast | Saccharomyces cerevisiae | 4932" = "4932",
    "Fruit fly | Drosophila melanogaster | 7227" = "7227",
    "C. elegans | Caenorhabditis elegans | 6239" = "6239",
    "E. coli K12 | Escherichia coli K12 | 511145" = "511145",
    "Custom taxonomy ID" = "custom"
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .pv-stringdb-page {
        background: #F6F8FB;
        padding: 18px;
        border-radius: 18px;
      }

      .pv-stringdb-hero {
        background: linear-gradient(135deg, #FFFFFF 0%, #EEF5FF 100%);
        border: 1px solid #E5EAF2;
        border-radius: 18px;
        padding: 22px 26px;
        margin-bottom: 16px;
        box-shadow: 0 8px 26px rgba(15, 23, 42, 0.06);
      }

      .pv-stringdb-hero h2 {
        margin-top: 0;
        margin-bottom: 8px;
        font-weight: 750;
        color: #102A43;
        letter-spacing: -0.3px;
      }

      .pv-stringdb-hero p {
        margin-bottom: 0;
        color: #52616B;
        font-size: 14px;
        line-height: 1.65;
      }

      .pv-sidebar-card,
      .pv-main-card {
        background: #FFFFFF;
        border: 1px solid #E5EAF2;
        border-radius: 16px;
        padding: 16px;
        margin-bottom: 14px;
        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.045);
      }

      /* Keep the analysis controls compact while preserving a clear,
         stepwise scientific workflow. */
      .pv-sidebar-details {
        margin-bottom: 10px;
      }

      .pv-sidebar-details > summary {
        cursor: pointer;
        list-style: none;
        padding: 10px 14px;
        border: 1px solid #D7E0EA;
        border-radius: 6px;
        background: #F5F8FB;
        color: #102A43;
        font-size: 14px;
        font-weight: 700;
      }

      .pv-sidebar-details > summary::-webkit-details-marker {
        display: none;
      }

      .pv-sidebar-details > summary::after {
        content: '▸';
        float: right;
        color: #697586;
        transition: transform 0.15s ease;
      }

      .pv-sidebar-details[open] > summary::after {
        transform: rotate(90deg);
      }

      .pv-sidebar-details > .pv-sidebar-card {
        margin-top: 6px;
        margin-bottom: 0;
        border-radius: 6px;
        box-shadow: none;
      }

      .pv-sidebar-details > .pv-sidebar-card > h4 {
        display: none;
      }

      .pv-sidebar-card h4,
      .pv-main-card h4 {
        margin-top: 0;
        margin-bottom: 14px;
        font-size: 15px;
        font-weight: 700;
        color: #102A43;
        display: flex;
        align-items: center;
        gap: 8px;
      }

      .pv-step {
        display: inline-flex;
        width: 24px;
        height: 24px;
        align-items: center;
        justify-content: center;
        border-radius: 8px;
        background: #EAF2FF;
        color: #1D4ED8;
        font-size: 12px;
        font-weight: 700;
      }

      .pv-help-text {
        color: #697586;
        font-size: 12px;
        line-height: 1.55;
        margin-top: -4px;
        margin-bottom: 12px;
      }

      .pv-run-button .btn {
        border-radius: 10px;
        font-weight: 700;
        letter-spacing: 0.2px;
      }

      .pv-download-grid {
        display: grid;
        grid-template-columns: 1fr;
        gap: 8px;
      }

      .pv-download-grid .btn {
        border-radius: 10px;
      }

      .pv-summary-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 16px;
      }

      .pv-summary-card {
        background: #FFFFFF;
        border: 1px solid #E5EAF2;
        border-radius: 15px;
        padding: 14px 16px;
        box-shadow: 0 6px 18px rgba(15, 23, 42, 0.045);
      }

      .pv-summary-label {
        color: #697586;
        font-size: 12px;
        margin-bottom: 4px;
      }

      .pv-summary-value {
        color: #102A43;
        font-size: 22px;
        font-weight: 800;
        line-height: 1.15;
      }

      .pv-summary-sub {
        color: #7A8794;
        font-size: 11px;
        margin-top: 3px;
      }

      .pv-empty-state {
        background: #FFFFFF;
        border: 1px dashed #CBD5E1;
        border-radius: 16px;
        padding: 26px;
        text-align: center;
        color: #64748B;
        margin-bottom: 16px;
      }

      .pv-empty-state h4 {
        color: #102A43;
        margin-top: 0;
        margin-bottom: 8px;
      }

      .pv-plot-card {
        background: #FFFFFF;
        border: 1px solid #E5EAF2;
        border-radius: 18px;
        padding: 14px;
        box-shadow: 0 8px 26px rgba(15, 23, 42, 0.055);
      }

      .pv-tab-note {
        color: #64748B;
        font-size: 13px;
        margin-bottom: 12px;
      }

      .tab-content {
        padding-top: 14px;
      }

      .nav-tabs > li > a {
        border-radius: 12px 12px 0 0;
        font-weight: 600;
        color: #475569;
      }

      .form-control,
      .selectize-input {
        border-radius: 10px !important;
      }

      @media (max-width: 1200px) {
        .pv-summary-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 768px) {
        .pv-summary-grid {
          grid-template-columns: 1fr;
        }
      }
    ")),

    shiny::div(
      class = "pv-stringdb-page",

      shiny::fluidRow(
        shiny::column(
          width = 3,

          shiny::tags$details(
            class = "pv-sidebar-details",
            open = TRUE,
            shiny::tags$summary("1  Data Input"),
            shiny::div(
            class = "pv-sidebar-card",
            shiny::h4(
              shiny::span(class = "pv-step", "1"),
              "Data Input"
            ),
            shiny::div(
              class = "pv-help-text",
              "Use demo data, upload a CSV/TSV file, or paste a gene list with optional log2FC values."
            ),

            shiny::selectInput(
              inputId = ns("input_mode"),
              label = "Input mode",
              choices = c(
                "Demo data" = "demo",
                "Upload file" = "upload",
                "Paste genes" = "paste"
              ),
              selected = "demo"
            ),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'upload'", ns("input_mode")),
              shiny::fileInput(
                inputId = ns("gene_file"),
                label = "Upload CSV/TSV file",
                accept = c(".csv", ".tsv", ".txt")
              )
            ),

            shiny::conditionalPanel(
              condition = sprintf("input['%s'] == 'paste'", ns("input_mode")),
              shiny::textAreaInput(
                inputId = ns("gene_text"),
                label = "Paste gene list or table",
                value = paste(
                  "gene_symbol\tlogFC",
                  "RCOR3\t-2.0",
                  "RCOR1\t-1.5",
                  "KDM1A\t-1.4",
                  "RCOR2\t-1.9",
                  "MACROH2A2\t-1.8",
                  "H2AC7\t-0.6",
                  "HDAC2\t-0.5",
                  "MDC1\t-0.7",
                  "CENPC\t-0.4",
                  "UBTF\t-0.3",
                  "H2AX\t-0.6",
                  "CTCF\t-0.8",
                  "MIER3\t-0.9",
                  sep = "\n"
                ),
                rows = 9,
                resize = "vertical"
              )
            ),

            shiny::uiOutput(ns("column_controls"))
            )
          ),

          shiny::tags$details(
            class = "pv-sidebar-details",
            open = FALSE,
            shiny::tags$summary("2  STRING Settings"),
            shiny::div(
            class = "pv-sidebar-card",
            shiny::h4(
              shiny::span(class = "pv-step", "2"),
              "STRING Settings"
            ),
            shiny::div(
              class = "pv-help-text",
              "Select STRING version, species, and interaction confidence threshold."
            ),

            shiny::selectInput(
              inputId = ns("string_version"),
              label = "STRING version",
              choices = c("11.5", "12.0"),
              selected = "11.5"
            ),

            shiny::selectizeInput(
              inputId = ns("species_choice"),
              label = "Species",
              choices = common_species,
              selected = "9606",
              options = list(create = FALSE)
            ),

            shiny::uiOutput(ns("custom_species_ui")),

            shiny::numericInput(
              inputId = ns("score_threshold"),
              label = "Confidence score threshold",
              value = 400,
              min = 0,
              max = 1000,
              step = 50
            ),

            shiny::textInput(
              inputId = ns("cache_dir"),
              label = "STRING cache directory",
              value = "",
              placeholder = "Optional"
            )
            )
          ),

          shiny::tags$details(
            class = "pv-sidebar-details",
            open = FALSE,
            shiny::tags$summary("3  Network Options"),
            shiny::div(
            class = "pv-sidebar-card",
            shiny::h4(
              shiny::span(class = "pv-step", "3"),
              "Network Options"
            ),

            shiny::checkboxInput(
              inputId = ns("keep_input_only"),
              label = "Keep input proteins only",
              value = TRUE
            ),

            shiny::checkboxInput(
              inputId = ns("include_disconnected"),
              label = "Keep disconnected mapped proteins",
              value = TRUE
            ),

            shiny::selectInput(
              inputId = ns("layout"),
              label = "Network layout",
              choices = c(
                "Fruchterman-Reingold" = "fr",
                "Kamada-Kawai" = "kk",
                "Circle" = "circle",
                "Stress" = "stress"
              ),
              selected = "fr"
            ),

            shiny::checkboxInput(
              inputId = ns("size_by_degree"),
              label = "Node size by degree",
              value = TRUE
            ),

            shiny::numericInput(
              inputId = ns("node_size"),
              label = "Default node size",
              value = 6,
              min = 1,
              max = 20,
              step = 1
            ),

            shiny::numericInput(
              inputId = ns("label_size"),
              label = "Label size",
              value = 3,
              min = 1,
              max = 10,
              step = 0.5
            )
            )
          ),

          shiny::tags$details(
            class = "pv-sidebar-details",
            open = FALSE,
            shiny::tags$summary("4  Color & Export"),
            shiny::div(
            class = "pv-sidebar-card",
            shiny::h4(
              shiny::span(class = "pv-step", "4"),
              "Color & Export"
            ),

            shiny::numericInput(
              inputId = ns("fc_min"),
              label = "log2FC color min",
              value = -2,
              min = -20,
              max = 20,
              step = 0.5
            ),

            shiny::numericInput(
              inputId = ns("fc_max"),
              label = "log2FC color max",
              value = 2,
              min = -20,
              max = 20,
              step = 0.5
            ),

            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::numericInput(
                  inputId = ns("plot_width"),
                  label = "Width",
                  value = 8,
                  min = 4,
                  max = 20,
                  step = 1
                )
              ),
              shiny::column(
                width = 6,
                shiny::numericInput(
                  inputId = ns("plot_height"),
                  label = "Height",
                  value = 6,
                  min = 4,
                  max = 20,
                  step = 1
                )
              )
            ),

            shiny::selectInput(
              inputId = ns("download_format"),
              label = "Plot format",
              choices = c("PDF" = "pdf", "PNG" = "png"),
              selected = "pdf"
            ),

            shiny::div(
              class = "pv-run-button",
              shiny::actionButton(
                inputId = ns("run_ppi"),
                label = "Run STRINGdb PPI Analysis",
                class = "btn-primary",
                width = "100%"
              )
            ),

            shiny::br(),

            shiny::div(
              class = "pv-download-grid",
              shiny::downloadButton(
                outputId = ns("download_plot"),
                label = "Download Plot",
                width = "100%"
              ),
              shiny::downloadButton(
                outputId = ns("download_demo"),
                label = "Download Demo Data",
                width = "100%"
              )
            )
            )
          )
        ),

        shiny::column(
          width = 9,

          shiny::div(
            class = "pv-stringdb-hero",
            shiny::h2("STRINGdb PPI Network"),
            shiny::p(
              "Build protein-protein interaction networks from gene symbols, map genes to STRING protein IDs, visualize interaction confidence, and integrate log2 fold-change values for publication-ready network exploration."
            )
          ),

          shiny::uiOutput(ns("summary_box")),

          shiny::tabsetPanel(
            id = ns("ppi_tabs"),

            shiny::tabPanel(
              title = "Network",
              shiny::div(
                class = "pv-plot-card",
                shiny::plotOutput(ns("ppi_plot"), height = "650px")
              )
            ),

            shiny::tabPanel(
              title = "Input Preview",
              shiny::div(
                class = "pv-main-card",
                shiny::h4("Uploaded or Demo Input"),
                shiny::div(
                  class = "pv-tab-note",
                  "Preview the input gene table used for STRINGdb mapping."
                ),
                DT::DTOutput(ns("input_preview"))
              )
            ),

            shiny::tabPanel(
              title = "Mapping",
              shiny::div(
                class = "pv-main-card",
                shiny::h4("STRINGdb Mapping Table"),
                shiny::div(
                  class = "pv-tab-note",
                  "Mapped STRING IDs and the corresponding input gene information."
                ),
                DT::DTOutput(ns("mapping_table")),
                shiny::br(),
                shiny::downloadButton(
                  outputId = ns("download_mapping"),
                  label = "Download Mapping Table"
                )
              )
            ),

            shiny::tabPanel(
              title = "Nodes",
              shiny::div(
                class = "pv-main-card",
                shiny::h4("Network Node Table"),
                shiny::div(
                  class = "pv-tab-note",
                  "Node-level information including gene symbol, log2FC, and degree."
                ),
                DT::DTOutput(ns("node_table")),
                shiny::br(),
                shiny::downloadButton(
                  outputId = ns("download_nodes"),
                  label = "Download Node Table"
                )
              )
            ),

            shiny::tabPanel(
              title = "Edges",
              shiny::div(
                class = "pv-main-card",
                shiny::h4("Interaction Edge Table"),
                shiny::div(
                  class = "pv-tab-note",
                  "STRINGdb interaction records and combined confidence scores."
                ),
                DT::DTOutput(ns("edge_table")),
                shiny::br(),
                shiny::downloadButton(
                  outputId = ns("download_edges"),
                  label = "Download Edge Table"
                )
              )
            ),

            shiny::tabPanel(
              title = "Species Search",
              shiny::div(
                class = "pv-main-card",
                shiny::h4("Search Supported STRING Species"),
                shiny::div(
                  class = "pv-tab-note",
                  "Search species supported by STRINGdb and use the taxonomy ID for custom species selection."
                ),

                shiny::fluidRow(
                  shiny::column(
                    width = 8,
                    shiny::textInput(
                      inputId = ns("species_keyword"),
                      label = "Species keyword",
                      value = "Oryza"
                    )
                  ),
                  shiny::column(
                    width = 4,
                    shiny::br(),
                    shiny::actionButton(
                      inputId = ns("search_species"),
                      label = "Search Species",
                      class = "btn-info",
                      width = "100%"
                    )
                  )
                ),

                DT::DTOutput(ns("species_table"))
              )
            )
          )
        )
      )
    )
  )
}


#' STRINGdb PPI Network Server
#'
#' @param id A unique module ID.
#'
#' @return A Shiny server module.
#' @export
#'
#' @import shiny
stringdb_ppi_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    output$custom_species_ui <- shiny::renderUI({
      if (!identical(input$species_choice, "custom")) {
        return(NULL)
      }

      shiny::numericInput(
        inputId = session$ns("custom_species"),
        label = "Custom NCBI taxonomy ID",
        value = 9606,
        min = 1,
        step = 1
      )
    })

    raw_input_data <- shiny::reactive({
      mode <- input$input_mode

      if (identical(mode, "demo")) {
        return(stringdb_ppi_demo_data())
      }

      if (identical(mode, "upload")) {
        shiny::req(input$gene_file)
        return(
          stringdb_ppi_read_table(
            path = input$gene_file$datapath,
            filename = input$gene_file$name
          )
        )
      }

      if (identical(mode, "paste")) {
        shiny::req(input$gene_text)
        return(stringdb_ppi_read_paste(input$gene_text))
      }

      NULL
    })

    output$column_controls <- shiny::renderUI({
      dat <- raw_input_data()

      if (is.null(dat) || nrow(dat) == 0 || ncol(dat) == 0) {
        return(NULL)
      }

      cols <- names(dat)

      gene_default <- stringdb_ppi_guess_column(
        cols = cols,
        candidates = c(
          "gene_symbol",
          "gene",
          "symbol",
          "Gene",
          "Gene.Symbol",
          "protein",
          "Protein",
          "Protein.ID",
          "ProteinID"
        ),
        fallback = cols[1]
      )

      logfc_default <- stringdb_ppi_guess_column(
        cols = cols,
        candidates = c(
          "logFC",
          "log2FC",
          "log2FoldChange",
          "Log2FC",
          "fold_change",
          "FC"
        ),
        fallback = ""
      )

      shiny::tagList(
        shiny::selectInput(
          inputId = session$ns("gene_col"),
          label = "Gene column",
          choices = cols,
          selected = gene_default
        ),
        shiny::selectInput(
          inputId = session$ns("logfc_col"),
          label = "log2FC column",
          choices = c("None" = "", cols),
          selected = logfc_default
        )
      )
    })

    output$input_preview <- DT::renderDT({
      dat <- raw_input_data()

      DT::datatable(
        dat,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })

    ppi_result <- shiny::eventReactive(input$run_ppi, {
      shiny::req(input$gene_col)

      shiny::withProgress(
        message = "Building STRINGdb PPI network...",
        value = 0,
        {
          shiny::incProgress(0.10, detail = "Preparing input table")

          dat <- raw_input_data()

          prepared <- stringdb_ppi_prepare_input(
            dat = dat,
            gene_col = input$gene_col,
            logfc_col = input$logfc_col
          )

          species_id <- stringdb_ppi_get_species_id(
            species_choice = input$species_choice,
            custom_species = input$custom_species
          )

          cache_dir <- trimws(input$cache_dir)

          if (identical(cache_dir, "")) {
            cache_dir <- ""
          }

          shiny::incProgress(0.35, detail = "Mapping genes to STRING IDs")

          result <- stringdb_ppi_build_network(
            input_df = prepared,
            version = input$string_version,
            species = species_id,
            score_threshold = input$score_threshold,
            input_directory = cache_dir,
            keep_input_only = input$keep_input_only,
            include_disconnected = input$include_disconnected
          )

          shiny::incProgress(0.75, detail = "Generating network plot")

          plot_obj <- stringdb_ppi_make_plot(
            graph = result$graph,
            layout = input$layout,
            size_by_degree = input$size_by_degree,
            node_size = input$node_size,
            label_size = input$label_size,
            fc_min = input$fc_min,
            fc_max = input$fc_max
          )

          result$plot <- plot_obj

          shiny::incProgress(1, detail = "Done")

          result
        }
      )
    })

    output$summary_box <- shiny::renderUI({
      if (is.null(input$run_ppi) || input$run_ppi == 0) {
        return(
          shiny::div(
            class = "pv-empty-state",
            shiny::h4("Ready to build a STRINGdb PPI network"),
            shiny::p(
              "Select input data, species, confidence threshold, and layout settings, then click Run STRINGdb PPI Analysis."
            )
          )
        )
      }

      res <- ppi_result()

      shiny::div(
        class = "pv-summary-grid",

        shiny::div(
          class = "pv-summary-card",
          shiny::div(class = "pv-summary-label", "Input genes"),
          shiny::div(class = "pv-summary-value", res$n_input),
          shiny::div(class = "pv-summary-sub", "Original gene list")
        ),

        shiny::div(
          class = "pv-summary-card",
          shiny::div(class = "pv-summary-label", "Mapped proteins"),
          shiny::div(class = "pv-summary-value", res$n_mapped),
          shiny::div(class = "pv-summary-sub", "Mapped STRING IDs")
        ),

        shiny::div(
          class = "pv-summary-card",
          shiny::div(class = "pv-summary-label", "Network size"),
          shiny::div(
            class = "pv-summary-value",
            paste0(igraph::vcount(res$graph), " / ", igraph::ecount(res$graph))
          ),
          shiny::div(class = "pv-summary-sub", "Nodes / edges")
        ),

        shiny::div(
          class = "pv-summary-card",
          shiny::div(class = "pv-summary-label", "Database"),
          shiny::div(class = "pv-summary-value", paste0("v", res$version)),
          shiny::div(
            class = "pv-summary-sub",
            paste0("Species ID: ", res$species)
          )
        )
      )
    })

    output$ppi_plot <- shiny::renderPlot({
      shiny::req(input$run_ppi > 0)
      res <- ppi_result()
      print(res$plot)
    }, height = 650)

    output$mapping_table <- DT::renderDT({
      shiny::req(input$run_ppi > 0)
      res <- ppi_result()

      DT::datatable(
        res$mapping_table,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })

    output$node_table <- DT::renderDT({
      shiny::req(input$run_ppi > 0)
      res <- ppi_result()

      DT::datatable(
        res$node_table,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })

    output$edge_table <- DT::renderDT({
      shiny::req(input$run_ppi > 0)
      res <- ppi_result()

      DT::datatable(
        res$edge_table,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })

    species_search_result <- shiny::eventReactive(input$search_species, {
      shiny::req(input$species_keyword)

      out <- tryCatch(
        {
          STRINGdb::get_STRING_species(
            version = input$string_version,
            species_name = input$species_keyword
          )
        },
        error = function(e) {
          data.frame(
            error = conditionMessage(e),
            stringsAsFactors = FALSE
          )
        }
      )

      out
    })

    output$species_table <- DT::renderDT({
      dat <- species_search_result()

      DT::datatable(
        dat,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })

    output$download_plot <- shiny::downloadHandler(
      filename = function() {
        paste0("STRINGdb_PPI_network.", input$download_format)
      },
      content = function(file) {
        shiny::req(input$run_ppi > 0)

        res <- ppi_result()

        if (identical(input$download_format, "pdf")) {
          ggplot2::ggsave(
            filename = file,
            plot = res$plot,
            width = input$plot_width,
            height = input$plot_height,
            device = "pdf"
          )
        } else {
          ggplot2::ggsave(
            filename = file,
            plot = res$plot,
            width = input$plot_width,
            height = input$plot_height,
            dpi = 300,
            device = "png"
          )
        }
      }
    )

    output$download_demo <- shiny::downloadHandler(
      filename = function() {
        "STRINGdb_PPI_demo_data.csv"
      },
      content = function(file) {
        utils::write.csv(
          stringdb_ppi_demo_data(),
          file,
          row.names = FALSE
        )
      }
    )

    output$download_mapping <- shiny::downloadHandler(
      filename = function() {
        "STRINGdb_mapping_table.csv"
      },
      content = function(file) {
        shiny::req(input$run_ppi > 0)

        res <- ppi_result()

        utils::write.csv(
          res$mapping_table,
          file,
          row.names = FALSE
        )
      }
    )

    output$download_nodes <- shiny::downloadHandler(
      filename = function() {
        "STRINGdb_node_table.csv"
      },
      content = function(file) {
        shiny::req(input$run_ppi > 0)

        res <- ppi_result()

        utils::write.csv(
          res$node_table,
          file,
          row.names = FALSE
        )
      }
    )

    output$download_edges <- shiny::downloadHandler(
      filename = function() {
        "STRINGdb_edge_table.csv"
      },
      content = function(file) {
        shiny::req(input$run_ppi > 0)

        res <- ppi_result()

        utils::write.csv(
          res$edge_table,
          file,
          row.names = FALSE
        )
      }
    )
  })
}


stringdb_ppi_demo_data <- function() {
  data.frame(
    gene_symbol = c(
      "RCOR3",
      "RCOR1",
      "KDM1A",
      "RCOR2",
      "MACROH2A2",
      "H2AC7",
      "HDAC2",
      "MDC1",
      "CENPC",
      "UBTF",
      "H2AX",
      "CTCF",
      "MIER3"
    ),
    alias = c(
      "RCOR3",
      "CoREST",
      "LSD1",
      "RCOR2",
      "MACROH2A2",
      "H2AC7",
      "HDAC2",
      "MDC1",
      "CENPC",
      "UBTF",
      "H2AX",
      "CTCF",
      "MIER3"
    ),
    logFC = c(
      -2.0,
      -1.5,
      -1.4,
      -1.9,
      -1.8,
      -0.6,
      -0.5,
      -0.7,
      -0.4,
      -0.3,
      -0.6,
      -0.8,
      -0.9
    ),
    stringsAsFactors = FALSE
  )
}


stringdb_ppi_read_table <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))

  if (ext %in% c("csv")) {
    dat <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    dat <- utils::read.delim(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  dat
}


stringdb_ppi_read_paste <- function(text) {
  text <- trimws(text)

  if (identical(text, "")) {
    stop("No pasted gene content was provided.")
  }

  lines <- unlist(strsplit(text, "\n"))
  lines <- trimws(lines)
  lines <- lines[lines != ""]

  if (length(lines) == 0) {
    stop("No pasted gene content was provided.")
  }

  has_header <- grepl(
    pattern = "gene|symbol|logfc|log2fc|fold",
    x = tolower(lines[1])
  )

  dat <- tryCatch(
    {
      utils::read.table(
        text = text,
        header = has_header,
        sep = "",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },
    error = function(e) {
      NULL
    }
  )

  if (!is.null(dat) && nrow(dat) > 0 && ncol(dat) >= 1) {
    if (!has_header) {
      names(dat)[1] <- "gene_symbol"

      if (ncol(dat) >= 2) {
        names(dat)[2] <- "logFC"
      }
    }

    return(dat)
  }

  tokens <- unlist(strsplit(text, "[,;\\s]+"))
  tokens <- unique(trimws(tokens))
  tokens <- tokens[!is.na(tokens) & tokens != ""]

  data.frame(
    gene_symbol = tokens,
    stringsAsFactors = FALSE
  )
}


stringdb_ppi_guess_column <- function(cols, candidates, fallback = NULL) {
  idx <- match(tolower(candidates), tolower(cols))
  idx <- idx[!is.na(idx)]

  if (length(idx) > 0) {
    return(cols[idx[1]])
  }

  fallback
}


stringdb_ppi_prepare_input <- function(dat, gene_col, logfc_col = NULL) {
  if (is.null(dat) || nrow(dat) == 0) {
    stop("Input table is empty.")
  }

  if (!gene_col %in% names(dat)) {
    stop("Selected gene column was not found in the input table.")
  }

  gene_symbol <- trimws(as.character(dat[[gene_col]]))
  keep <- !is.na(gene_symbol) & gene_symbol != ""

  out <- data.frame(
    gene_symbol = gene_symbol[keep],
    stringsAsFactors = FALSE
  )

  if (!is.null(logfc_col) && !identical(logfc_col, "") && logfc_col %in% names(dat)) {
    out$logFC <- suppressWarnings(as.numeric(dat[[logfc_col]][keep]))
  } else {
    out$logFC <- NA_real_
  }

  out <- out[!duplicated(out$gene_symbol), , drop = FALSE]

  if (nrow(out) < 2) {
    stop("At least two valid genes are required.")
  }

  out
}


stringdb_ppi_get_species_id <- function(species_choice, custom_species = NULL) {
  if (identical(species_choice, "custom")) {
    species_id <- suppressWarnings(as.integer(custom_species))
  } else {
    species_id <- suppressWarnings(as.integer(species_choice))
  }

  if (is.na(species_id) || species_id <= 0) {
    stop("Invalid species taxonomy ID.")
  }

  species_id
}


stringdb_ppi_build_network <- function(input_df,
                                       version = "11.5",
                                       species = 9606,
                                       score_threshold = 400,
                                       input_directory = "",
                                       keep_input_only = TRUE,
                                       include_disconnected = TRUE) {
  if (!requireNamespace("STRINGdb", quietly = TRUE)) {
    stop("Package 'STRINGdb' is required.")
  }

  if (!requireNamespace("igraph", quietly = TRUE)) {
    stop("Package 'igraph' is required.")
  }

  n_input <- nrow(input_df)

  string_db <- STRINGdb::STRINGdb$new(
    version = version,
    species = species,
    score_threshold = score_threshold,
    input_directory = input_directory
  )

  mapped <- string_db$map(
    input_df,
    "gene_symbol",
    removeUnmappedRows = TRUE
  )

  if (is.null(mapped) || nrow(mapped) == 0) {
    stop("No genes were mapped to STRING IDs. Please check species and gene symbols.")
  }

  mapped <- mapped[!duplicated(mapped$STRING_id), , drop = FALSE]

  if (nrow(mapped) < 2) {
    stop("Fewer than two genes were mapped to STRING IDs.")
  }

  mapped_ids <- unique(mapped$STRING_id)

  ppi_edge <- string_db$get_interactions(mapped_ids)

  if (is.null(ppi_edge)) {
    ppi_edge <- data.frame()
  }

  if (nrow(ppi_edge) > 0 && keep_input_only) {
    ppi_edge <- ppi_edge[
      ppi_edge$from %in% mapped_ids & ppi_edge$to %in% mapped_ids,
      ,
      drop = FALSE
    ]
  }

  if (nrow(ppi_edge) > 0) {
    edge_df <- ppi_edge[, c("from", "to", "combined_score"), drop = FALSE]
    edge_df$combined_score <- suppressWarnings(as.numeric(edge_df$combined_score))
  } else {
    edge_df <- data.frame(
      from = character(),
      to = character(),
      combined_score = numeric(),
      stringsAsFactors = FALSE
    )
  }

  if (isTRUE(include_disconnected)) {
    vertex_df <- data.frame(
      name = mapped_ids,
      stringsAsFactors = FALSE
    )
  } else {
    connected_ids <- unique(c(edge_df$from, edge_df$to))

    vertex_df <- data.frame(
      name = connected_ids,
      stringsAsFactors = FALSE
    )
  }

  if (nrow(vertex_df) == 0) {
    stop("No connected proteins remained after filtering.")
  }

  if (nrow(edge_df) > 0) {
    g <- igraph::graph_from_data_frame(
      d = edge_df,
      directed = FALSE,
      vertices = vertex_df
    )

    g <- igraph::simplify(
      g,
      remove.multiple = TRUE,
      remove.loops = TRUE,
      edge.attr.comb = list(
        combined_score = "max",
        "ignore"
      )
    )
  } else {
    g <- igraph::make_empty_graph(
      n = nrow(vertex_df),
      directed = FALSE
    )

    igraph::V(g)$name <- vertex_df$name
  }

  id2symbol <- stats::setNames(mapped$gene_symbol, mapped$STRING_id)
  id2logfc <- stats::setNames(mapped$logFC, mapped$STRING_id)

  igraph::V(g)$symbol <- unname(id2symbol[igraph::V(g)$name])
  igraph::V(g)$symbol[is.na(igraph::V(g)$symbol)] <- igraph::V(g)$name

  igraph::V(g)$logFC <- suppressWarnings(as.numeric(id2logfc[igraph::V(g)$name]))
  igraph::V(g)$degree <- igraph::degree(g)

  if (igraph::ecount(g) > 0) {
    score_max <- max(igraph::E(g)$combined_score, na.rm = TRUE)

    if (is.finite(score_max) && score_max > 0) {
      igraph::E(g)$score_norm <- igraph::E(g)$combined_score / score_max
    } else {
      igraph::E(g)$score_norm <- 1
    }
  }

  node_table <- data.frame(
    STRING_id = igraph::V(g)$name,
    gene_symbol = igraph::V(g)$symbol,
    logFC = igraph::V(g)$logFC,
    degree = igraph::V(g)$degree,
    stringsAsFactors = FALSE
  )

  if (nrow(ppi_edge) > 0) {
    ppi_edge$from_symbol <- unname(id2symbol[ppi_edge$from])
    ppi_edge$to_symbol <- unname(id2symbol[ppi_edge$to])
  }

  list(
    graph = g,
    mapping_table = mapped,
    node_table = node_table,
    edge_table = ppi_edge,
    n_input = n_input,
    n_mapped = nrow(mapped),
    species = species,
    version = version
  )
}


stringdb_ppi_make_plot <- function(graph,
                                   layout = "fr",
                                   size_by_degree = TRUE,
                                   node_size = 6,
                                   label_size = 3,
                                   fc_min = -2,
                                   fc_max = 2) {
  if (!requireNamespace("ggraph", quietly = TRUE)) {
    stop("Package 'ggraph' is required.")
  }

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }

  if (identical(layout, "stress") && !requireNamespace("graphlayouts", quietly = TRUE)) {
    layout <- "fr"
  }

  if (is.na(fc_min) || is.na(fc_max) || fc_min >= fc_max) {
    fc_min <- -2
    fc_max <- 2
  }

  has_edges <- igraph::ecount(graph) > 0
  has_logfc <- any(!is.na(igraph::V(graph)$logFC))

  p <- ggraph::ggraph(graph, layout = layout)

  if (has_edges) {
    p <- p +
      ggraph::geom_edge_link(
        ggplot2::aes(edge_width = score_norm),
        colour = "grey70",
        alpha = 0.85
      ) +
      ggraph::scale_edge_width(
        range = c(0.2, 1.6),
        name = "STRINGdb\nconfidence"
      )
  }

  if (has_logfc && isTRUE(size_by_degree)) {
    p <- p +
      ggraph::geom_node_point(
        ggplot2::aes(colour = logFC, size = degree),
        alpha = 0.95
      ) +
      ggplot2::scale_size_continuous(
        range = c(4, 10),
        name = "Degree"
      ) +
      ggplot2::scale_colour_gradient2(
        low = "#2166AC",
        mid = "#F7F7F7",
        high = "#B2182B",
        midpoint = 0,
        limits = c(fc_min, fc_max),
        name = expression(log[2] * " fold-change")
      )
  }

  if (has_logfc && !isTRUE(size_by_degree)) {
    p <- p +
      ggraph::geom_node_point(
        ggplot2::aes(colour = logFC),
        size = node_size,
        alpha = 0.95
      ) +
      ggplot2::scale_colour_gradient2(
        low = "#2166AC",
        mid = "#F7F7F7",
        high = "#B2182B",
        midpoint = 0,
        limits = c(fc_min, fc_max),
        name = expression(log[2] * " fold-change")
      )
  }

  if (!has_logfc && isTRUE(size_by_degree)) {
    p <- p +
      ggraph::geom_node_point(
        ggplot2::aes(size = degree),
        colour = "#2C7FB8",
        alpha = 0.95
      ) +
      ggplot2::scale_size_continuous(
        range = c(4, 10),
        name = "Degree"
      )
  }

  if (!has_logfc && !isTRUE(size_by_degree)) {
    p <- p +
      ggraph::geom_node_point(
        colour = "#2C7FB8",
        size = node_size,
        alpha = 0.95
      )
  }

  p +
    ggraph::geom_node_text(
      ggplot2::aes(label = symbol),
      size = label_size,
      repel = TRUE
    ) +
    ggplot2::theme_void() +
    ggplot2::labs(
      title = "STRINGdb protein-protein interaction network"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 14
      )
    )
}
