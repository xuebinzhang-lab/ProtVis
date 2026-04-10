#' Protein Function Module UI
#' @description
#' This function defines the user interface for the protein analysis module.
#' It utilizes a \code{bslib} sidebar layout featuring a control panel and
#' a four-card grid displaying: Differential Expression Proteomics (DEP),
#' Protein Sequences, Functional Domains, and 3D Structures.
#' @param id A character string representing the Shiny module namespace ID.
#' @return A Shiny UI tag list.
#' @name protein_fun_ui
#' @importFrom shiny NS tagList div actionButton uiOutput verbatimTextOutput downloadButton plotOutput
#' @importFrom bslib layout_sidebar sidebar page_fluid layout_column_wrap card card_header card_body
#' @importFrom plotly plotlyOutput
#' @importFrom DT dataTableOutput
#' @export
#'
protein_fun_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 300,
        shiny::div(style = "margin-bottom: 15px;",
                   shiny::actionButton(ns("load_data"), "LOAD DATA", class = "btn btn-light fw-bold")
        ),
        shiny::uiOutput(ns("load_status_panel")),
        shiny::uiOutput(ns("selected_protein_info")),
        shiny::uiOutput(ns("sequence_extract_panel"))
      ),
      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,
          height = 1500,
          bslib::card(
            height = "800px",
            bslib::card_header("DEP"),
            bslib::card_body(
              shiny::uiOutput(ns("comparison_select_ui")),
              plotly::plotlyOutput(ns("volcano_plot"), height = "600px")
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Protein sequence"),
            bslib::card_body(
              shiny::verbatimTextOutput(ns("protein_sequence")),
              shiny::downloadButton(ns("download_sequence"), "Download FASTA")
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Domain"),
            bslib::card_body(
              shiny::plotOutput(ns("domain_plot"), height = "300px"),
              DT::dataTableOutput(ns("domain_table"))
            )
          ),
          bslib::card(
            height = "800px",
            bslib::card_header("Protein 3D Structure"),
            bslib::card_body(
              shiny::uiOutput(ns("structure_display")),
              shiny::uiOutput(ns("external_links"))
            )
          )
        )
      )
    )
  )
}



utils::globalVariables(c("logFC", "P.Value", "regulation"))
protein_fun_server <- function(id, shared_state) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- shiny::reactiveValues(
      compare_data = NULL,
      dep_results = NULL,
      load_success = FALSE,
      selected_protein = NULL,
      protein_sequences = NULL,
      domain_data = NULL,
      fasta_data = NULL,
      extracted_seqs = NULL,
      unmatched_ids = NULL,
      current_protein_id = NULL
    )
    output$sequence_extract_panel <- shiny::renderUI({
      shiny::tagList(
        shiny::h4("Protein Sequence Extraction"),
        shiny::fileInput(ns("fasta_file"), "Upload FASTA File",
                         accept = c(".fa", ".fasta", ".fasta.gz"),
                         buttonLabel = "Browse...",
                         width = "100%"),
        shiny::uiOutput(ns("current_protein_display")),
        shiny::actionButton(ns("extract_seqs"), "Extract Sequence",
                            class = "btn-primary btn-sm"),
        shiny::uiOutput(ns("extract_status"))
      )
    })
    output$current_protein_display <- shiny::renderUI({
      if (!is.null(rv$current_protein_id)) {
        shiny::tagList(
          shiny::div(style = "margin: 10px 0; padding: 8px; background: #f0f8ff; border-radius: 4px;",
                     shiny::strong("Current Protein ID:"),
                     shiny::br(),
                     shiny::tags$code(style = "color: #0066cc;", rv$current_protein_id)
          )
        )
      } else {
        shiny::div(style = "margin: 10px 0; padding: 8px; background: #fff3cd; border-radius: 4px;",
                   shiny::icon("info-circle"),
                   "Click on a point in the volcano plot to select a protein"
        )
      }
    })
    shiny::observeEvent(input$fasta_file, {
      shiny::req(input$fasta_file)
      tryCatch({
        if (endsWith(input$fasta_file$name, ".gz")) {
          con <- gzfile(input$fasta_file$datapath)
          rv$fasta_data <- Biostrings::readAAStringSet(con)
          close(con)
        } else {
          rv$fasta_data <- Biostrings::readAAStringSet(input$fasta_file$datapath)
        }
        shiny::showNotification("FASTA file loaded successfully!", type = "message")
      }, error = function(e) {
        shiny::showNotification(paste("Error loading FASTA:", e$message), type = "error")
        rv$fasta_data <- NULL
      })
    })
    shiny::observeEvent(input$extract_seqs, {
      shiny::req(rv$fasta_data, rv$current_protein_id)
      tryCatch({
        fasta_headers <- names(rv$fasta_data)
        matched_idx <- stringr::str_detect(fasta_headers, rv$current_protein_id)
        if (any(matched_idx)) {
          rv$extracted_seqs <- rv$fasta_data[matched_idx]
          shiny::showNotification(
            sprintf("Sequence extracted for: %s", rv$current_protein_id),
            type = "message"
          )
        } else {
          shiny::showNotification(
            sprintf("Protein ID '%s' not found in FASTA file", rv$current_protein_id),
            type = "warning"
          )
          rv$extracted_seqs <- NULL
        }
      }, error = function(e) {
        shiny::showNotification(paste("Extraction error:", e$message), type = "error")
      })
    })
    output$extract_status <- shiny::renderUI({
      if (!is.null(rv$extracted_seqs) && !is.null(rv$current_protein_id)) {
        shiny::tagList(
          shiny::div(style = "margin-top: 10px; padding: 8px; background: #d4edda; border-radius: 4px;",
                     shiny::span(shiny::icon("check"), "Sequence extracted successfully!",
                                 style = "color: #155724; font-weight: bold;"),
                     shiny::br(),
                     shiny::span(sprintf("Protein: %s", rv$current_protein_id)),
                     shiny::br(),
                     shiny::span(sprintf("Sequence length: %d aa", Biostrings::width(rv$extracted_seqs)))
          )
        )
      } else if (!is.null(rv$fasta_data)) {
        shiny::div(style = "margin-top: 10px; padding: 8px; background: #d1ecf1; border-radius: 4px;",
                   shiny::span(shiny::icon("info"), "FASTA loaded. Click 'Extract Sequence' to get current protein.",
                               style = "color: #0c5460;")
        )
      } else {
        shiny::div(style = "margin-top: 10px; padding: 8px; background: #fff3cd; border-radius: 4px;",
                   shiny::span(shiny::icon("exclamation-triangle"), "Please upload a FASTA file first.",
                               style = "color: #856404;")
        )
      }
    })
    shiny::observeEvent(input$load_data, {
      shiny::req(shared_state$workdir)
      rda_path <- file.path(shared_state$workdir, "Step7_DEP_result.rda")
      if (base::file.exists(rda_path)) {
        e <- base::new.env()
        load(rda_path, envir = e)
        if (base::exists("compare_data2", envir = e)) rv$compare_data <- e$compare_data2
        if (base::exists("dep_results2", envir = e)) {
          rv$dep_results <- e$dep_results2
        } else {
          rv$dep_results <- NULL
          shiny::showNotification("Step7_DEP_result.rda does not exist. Expression matrix cannot be loaded.", type = "warning")
        }
        rv$load_success <- TRUE
        shiny::showNotification("✅ Data loaded successfully.", type = "message")
        load_additional_data()
      } else {
        rv$load_success <- FALSE
        shiny::showNotification("Step7_DEP_result.rda not found.", type = "error")
      }
    })
    load_additional_data <- function() {
      fasta_path <- base::file.path(shared_state$workdir, "protein_sequences.fasta")
      if (base::file.exists(fasta_path)) {
        tryCatch({
          if (base::requireNamespace("Biostrings", quietly = TRUE)) {
            rv$protein_sequences <- Biostrings::readAAStringSet(fasta_path)
          }
        }, error = function(e) {
          message("Could not load protein sequences: ", e$message)
        })
      }
      domain_path <- base::file.path(shared_state$workdir, "domain_data.rda")
      if (base::file.exists(domain_path)) {
        tryCatch({
          e <- base::new.env()
          base::load(domain_path, envir = e)
          if (base::exists("domain_data", envir = e)) {
            rv$domain_data <- e$domain_data
          }
        }, error = function(e) {
          message("Could not load domain data: ", e$message)
        })
      }
    }
    output$load_status_panel <- shiny::renderUI({
      if (rv$load_success) {
        shiny::tagList(
          shiny::span("✅ Data loaded", style = "color: green;"),
          shiny::br(),
          if (!is.null(rv$protein_sequences)) {
            shiny::span("✅ Sequences available", style = "color: green;")
          } else {
            shiny::span("⚠️ No sequence data", style = "color: orange;")
          }
        )
      } else {
        shiny::span("❌ Data not loaded", style = "color: red;")
      }
    })
    output$comparison_select_ui <- shiny::renderUI({
      shiny::req(rv$dep_results)
      comparison_choices <- base::names(rv$dep_results)
      if (base::length(comparison_choices) > 0) {
        shiny::selectInput(
          ns("comparison_group"),
          "Please select a comparison group:",
          choices = comparison_choices,
          selected = comparison_choices[1]
        )
      } else {
        shiny::p("No comparison groups available in the loaded data.")
      }
    })
    output$volcano_plot <- plotly::renderPlotly({
      shiny::req(input$comparison_group, rv$dep_results)
      dep_data <- rv$dep_results[[input$comparison_group]]
      if (!base::is.null(dep_data)) {
        if (!is.data.frame(dep_data)) {
          dep_data <- data.frame(dep_data)
        }
        required_cols <- c("logFC", "P.Value", "regulation")
        if (base::all(required_cols %in% base::colnames(dep_data))) {
          if (!"ID" %in% base::colnames(dep_data)) {
            if (!base::is.null(base::rownames(dep_data)) && base::all(base::rownames(dep_data) != "")) {
              dep_data$ID <- base::rownames(dep_data)
            } else {
              dep_data$ID <- base::as.character(1:nrow(dep_data))
            }
          }
          dep_data$point_index <- 1:nrow(dep_data)
          p <- ggplot2::ggplot(dep_data, ggplot2::aes(x = logFC, y = -log10(P.Value),
                                                      color = regulation,
                                                      customdata = point_index,
                                                      text = base::paste("Protein:", ID,
                                                                   "<br>logFC:", base::round(logFC, 3),
                                                                   "<br>p-value:", base::format.pval(P.Value, digits = 3),
                                                                   "<br>Regulation:", regulation))) +
            ggplot2::geom_point(alpha = 0.8, size = 2) +
            ggplot2::scale_color_manual(values = c("Upregulated" = "red",
                                                   "Downregulated" = "blue",
                                                   "Not significant" = "grey")) +
            ggplot2::theme_bw() +
            ggplot2::labs(x = "Log2 Fold Change",
                          y = "-Log10(p-value)",
                          color = "") +
            ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                           legend.position = "top") +
            ggplot2::geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +
            ggplot2::geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black")

          plotly::ggplotly(p, tooltip = "text", source = "volcano") %>%
            plotly::layout(legend = base::list(orientation = "h", x = 0, y = 1.1))

        } else {
          plotly::plot_ly() %>%
            plotly::add_annotations(text = "Required columns (logFC, P.Value, regulation) not found in data",
                                    xref = "paper", yref = "paper",
                                    x = 0.5, y = 0.5, xanchor = "center", yanchor = "center",
                                    showarrow = FALSE)
        }
      }
    })
    shiny::observeEvent(plotly::event_data("plotly_click", source = "volcano"), {
      click_data <- plotly::event_data("plotly_click", source = "volcano")
      shiny::req(click_data, input$comparison_group, rv$dep_results)
      dep_data <- rv$dep_results[[input$comparison_group]]
      if (!base::is.data.frame(dep_data)) {
        dep_data <- base::data.frame(dep_data)
      }
      if (!base::is.null(click_data$customdata)) {
        point_index <- base::as.numeric(click_data$customdata)
      } else {
        point_index <- click_data$pointNumber + 1
      }
      if (!"ID" %in% base::colnames(dep_data)) {
        if (!is.null(base::rownames(dep_data)) && all(base::rownames(dep_data) != "")) {
          dep_data$ID <- base::rownames(dep_data)
        } else {
          dep_data$ID <- base::as.character(1:base::nrow(dep_data))
        }
      }
      if (point_index <= nrow(dep_data) && point_index > 0) {
        protein_id <- dep_data$ID[point_index]
        rv$selected_protein <- base::list(
          id = protein_id,
          data = dep_data[point_index, , drop = FALSE]
        )
        rv$current_protein_id <- protein_id
        shiny::showNotification(base::paste("Selected protein:", protein_id))
      } else {
        shiny::showNotification("Invalid point selection", type = "warning")
      }
    })
    output$selected_protein_info <- shiny::renderUI({
      shiny::req(rv$selected_protein)
      protein_data <- rv$selected_protein$data
      shiny::tagList(
        shiny::h4("Selected Protein"),
        shiny::p(shiny::strong("ID:"), rv$selected_protein$id),
        shiny::p(shiny::strong("logFC:"), round(protein_data$logFC, 3)),
        shiny::p(shiny::strong("P.Value:"), format.pval(protein_data$P.Value, digits = 3)),
        shiny::p(shiny::strong("Regulation:"), protein_data$regulation),
        shiny::hr()
      )
    })
    output$protein_sequence <- shiny::renderPrint({
      shiny::req(rv$selected_protein)
      protein_id <- rv$selected_protein$id
      if (!base::is.null(rv$extracted_seqs)) {
        if (inherits(rv$extracted_seqs, "AAStringSet")) {
          sequence <- as.character(rv$extracted_seqs[protein_id])
          if (!base::is.null(sequence) && !base::is.na(sequence)) {
            cat(">", protein_id, " (Extracted from FASTA)\n", sep = "")
            seq_length <- base::nchar(sequence)
            for (i in base::seq(1, seq_length, by = 60)) {
              base::cat(base::substr(sequence, i, min(i+59, seq_length)), "\n")
            }
            return()
          }
        }
      }
      if (!base::is.null(rv$protein_sequences)) {
        if (base::inherits(rv$protein_sequences, "AAStringSet")) {
          sequence <- base::as.character(rv$protein_sequences[protein_id])
        } else if (base::is.character(rv$protein_sequences)) {
          sequence <- rv$protein_sequences[protein_id]
        } else {
          sequence <- NULL
        }
        if (!base::is.null(sequence) && !base::is.na(sequence)) {
          cat(">", protein_id, " (Pre-loaded)\n", sep = "")
          seq_length <- base::nchar(sequence)
          for (i in base::seq(1, seq_length, by = 60)) {
            base::cat(base::substr(sequence, i, min(i+59, seq_length)), "\n")
          }
        } else {
          base::cat("Sequence not found for:", protein_id, "\n")
          base::cat("Please upload a FASTA file and click 'Extract Sequence'")
        }
      } else {
        base::cat(">Zm00001d025100_P003 pep chromosome:AGPv4:10:104005267:104026749:-1 gene:Zm00001d025100 \n transcript:Zm00001d025100_T003 gene_biotype:protein_coding transcript_biotype:\nprotein_coding description:Zm00001d025100\n")
        base::cat("MEHDAHAEASSHAVPPPEDATVDDWARDDAEPMSVESSATPPEVAAVDSGADTPPAPSASAAVAGEGVKEIQSSLQSLELKTNEDAH \nVVEDDVEETKRHLNVVFIGHVDAGKSTTGGQILFLSGQVDDRTIQKYEKEAKDKSRESWERLLKLVGPTLRQNTQDSLSWMHRYLFL \nLYVAYMLLMQGHKSYVPNMISGASQADIGVLVISARKGEFETGYERGGQTREHVLLAKTLGVAKLVVVINKMDEPTVKWSKERYDEIE \nAKMVPFLKSSGYNVKKDVQFLPISGLVGTNMKTRMDKSICSWWDGPCLFEVLDRIVVPLRDPKGSVRMPIIDKYKDMGTVAMGKIESG \nTIREGDSLLVMPNKSHVKVIGLNLDESKVRRAGPAENVRVKLSGVEEEDVMAGFVLSSVGKFIFRRK")
      }
    })
    output$download_sequence <- shiny::downloadHandler(
      filename = function() {
        base::paste0(rv$selected_protein$id, ".fasta")
      },
      content = function(file) {
        shiny::req(rv$selected_protein)
        protein_id <- rv$selected_protein$id
        sequence <- NULL
        if (!base::is.null(rv$extracted_seqs)) {
          if (base::inherits(rv$extracted_seqs, "AAStringSet")) {
            sequence <- as.character(rv$extracted_seqs[protein_id])
          }
        }
        if (base::is.null(sequence) && !base::is.null(rv$protein_sequences)) {
          if (base::inherits(rv$protein_sequences, "AAStringSet")) {
            sequence <- as.character(rv$protein_sequences[protein_id])
          } else {
            sequence <- rv$protein_sequences[protein_id]
          }
        }
        if (!is.null(sequence)) {
          fasta_content <- paste0(">", protein_id, "\n", sequence)
          writeLines(fasta_content, file)
        } else {
          shiny::showNotification("No sequence available for download", type = "warning")
        }
      }
    )
  })
}
