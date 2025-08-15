options(shiny.maxRequestSize = 500*1024^2)
#' Protein Extract UI Module
#'
#' Creates the user interface for protein extraction module that allows users to
#' upload FASTA files, input target protein IDs, and extract matching sequences.
#'
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing all UI elements
#' @export
protein_extract_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        # File upload section
        div(style = "margin-bottom: 15px;",
            fileInput(ns("fasta_file"), "Upload FASTA File",
                      accept = c(".fa", ".fasta", ".fasta.gz"),
                      buttonLabel = "Browse...")
        ),
        # Input mode toggle
        shinyWidgets::radioGroupButtons(
          inputId = ns("input_mode"),
          label = "Protein ID Input Method:",
          choices = c("Manual Input" = "manual", "File Upload" = "file"),
          selected = "manual",
          status = "primary"
        ),
        # Conditional panel for manual input
        conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == 'manual'"),
          div(
            style = "margin-top: 10px;",
            h5("Enter Protein IDs (one per line)"),
            textAreaInput(ns("protein_ids"),
                          label = NULL,
                          rows = 5,
                          placeholder = "Paste protein IDs here\nExample:\nP12345\nQ6GZX4")
          )
        ),
        # Conditional panel for file upload
        conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == 'file'"),
          div(
            style = "margin-top: 10px;",
            fileInput(ns("id_file"), "Upload Protein IDs",
                      accept = c(".txt", ".csv", ".tsv"),
                      buttonLabel = "Browse...")
          )
        ),
        # Action buttons
        div(style = "margin-top: 20px;",
            actionButton(ns("extract"), "Extract Proteins",
                         class = "btn btn-light fw-bold mb-3"),
            br(style = "line-height: 30px;"),
            downloadButton(ns("download_results"), "Download Results",
                           class = "btn btn-light fw-bold")
        )
      ),
      # Main display panel
      bslib::card(
        height = "600px",
        bslib::card_header("Protein Extraction Results"),
        bslib::navset_card_tab(
          full_screen = TRUE,
          bslib::nav_panel(
            "Matched Sequences",
            div(
              style = "height: 500px; overflow: auto;",
              verbatimTextOutput(ns("matched_summary"))
            )
          ),
          bslib::nav_panel(
            "Sequence Table",
            div(
              style = "height: 500px; overflow: auto;",
              DT::dataTableOutput(ns("sequence_table"))
            )
          ),
          bslib::nav_panel(
            "Unmatched IDs",
            div(
              style = "height: 500px; overflow: auto;",
              verbatimTextOutput(ns("unmatched_ids"))
            )
          )
        )
      )
    )
  )
}

#' Protein Extract Server Module
#'
#' Server-side logic for protein extraction module that handles FASTA file processing,
#' protein ID matching, and result generation.
#'
#' @param id The namespace identifier for the module
#' @return A reactive list containing matched sequences and summary statistics
#' @export
protein_extract_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values storage
    rv <- reactiveValues(
      fasta_data = NULL,
      protein_ids = NULL,
      matched_seqs = NULL,
      unmatched_ids = NULL
    )

    # Load FASTA file
    observeEvent(input$fasta_file, {
      req(input$fasta_file)

      tryCatch({
        # Handle gzipped files
        if (endsWith(input$fasta_file$name, ".gz")) {
          con <- gzfile(input$fasta_file$datapath)
          rv$fasta_data <- Biostrings::readAAStringSet(con)
          close(con)
        } else {
          rv$fasta_data <- Biostrings::readAAStringSet(input$fasta_file$datapath)
        }

        showNotification("FASTA file loaded successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading FASTA file:", e$message), type = "error")
        rv$fasta_data <- NULL
      })
    })

    # Get protein IDs based on input method
    observe({
      if (input$input_mode == "manual") {
        req(input$protein_ids)
        ids <- strsplit(input$protein_ids, "\n")[[1]]
        rv$protein_ids <- stringr::str_trim(ids[ids != ""])
      } else {
        req(input$id_file)
        tryCatch({
          ext <- tools::file_ext(input$id_file$name)
          if (ext %in% c("csv", "tsv")) {
            sep <- ifelse(ext == "csv", ",", "\t")
            df <- read.delim(input$id_file$datapath, sep = sep, header = FALSE)
            rv$protein_ids <- stringr::str_trim(unlist(df))
          } else {
            rv$protein_ids <- stringr::str_trim(readLines(input$id_file$datapath))
          }
        }, error = function(e) {
          showNotification(paste("Error reading ID file:", e$message), type = "error")
          rv$protein_ids <- NULL
        })
      }
    })

    # Extract protein sequences
    observeEvent(input$extract, {
      req(rv$fasta_data, rv$protein_ids)

      tryCatch({
        # Extract protein IDs from FASTA headers (assuming headers contain IDs)
        fasta_headers <- names(rv$fasta_data)

        # Create pattern to match any of the protein IDs
        pattern <- paste0("\\b(", paste(rv$protein_ids, collapse = "|"), ")\\b")

        # Find matches
        matched_idx <- stringr::str_detect(fasta_headers, pattern)
        rv$matched_seqs <- rv$fasta_data[matched_idx]

        # Find unmatched IDs
        found_ids <- stringr::str_extract(fasta_headers[matched_idx], pattern)
        rv$unmatched_ids <- setdiff(rv$protein_ids, found_ids)

        showNotification(
          sprintf("Matched %d out of %d proteins",
                  length(rv$matched_seqs),
                  length(rv$protein_ids)),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error during extraction:", e$message), type = "error")
        rv$matched_seqs <- NULL
        rv$unmatched_ids <- NULL
      })
    })

    # Display matched sequence summary
    output$matched_summary <- renderPrint({
      req(rv$matched_seqs)
      cat("=== Matched Protein Summary ===\n")
      cat(sprintf("Total proteins in FASTA: %d\n", length(rv$fasta_data)))
      cat(sprintf("Target proteins queried: %d\n", length(rv$protein_ids)))
      cat(sprintf("Successfully matched: %d (%.1f%%)\n",
                  length(rv$matched_seqs),
                  length(rv$matched_seqs)/length(rv$protein_ids)*100))
      cat("\n=== First 10 Matched Proteins ===\n")
      print(head(names(rv$matched_seqs), 10))
    })

    # Display sequence table
    output$sequence_table <- DT::renderDataTable({
      req(rv$matched_seqs)

      data.frame(
        Protein_ID = names(rv$matched_seqs),
        Length = Biostrings::width(rv$matched_seqs),
        Sequence = as.character(rv$matched_seqs),
        stringsAsFactors = FALSE
      ) %>%
        DT::datatable(
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          )
        )
    })

    # Display unmatched IDs
    output$unmatched_ids <- renderPrint({
      req(rv$unmatched_ids)
      cat("=== Unmatched Protein IDs ===\n")
      cat(sprintf("Total unmatched: %d\n\n", length(rv$unmatched_ids)))
      if (length(rv$unmatched_ids) <= 20) {
        cat(rv$unmatched_ids, sep = "\n")
      } else {
        cat(head(rv$unmatched_ids, 20), sep = "\n")
        cat(sprintf("\n... and %d more", length(rv$unmatched_ids) - 20))
      }
    })

    # Download handler
    output$download_results <- downloadHandler(
      filename = function() {
        paste0("protein_extract_results_", Sys.Date(), ".zip")
      },
      content = function(file) {
        req(rv$matched_seqs)

        # Create temp directory
        temp_dir <- tempdir()
        fasta_file <- file.path(temp_dir, "matched_sequences.fasta")
        summary_file <- file.path(temp_dir, "summary.txt")
        unmatched_file <- file.path(temp_dir, "unmatched_ids.txt")

        # Write outputs
        Biostrings::writeXStringSet(rv$matched_seqs, fasta_file)
        writeLines(
          c("=== Protein Extraction Summary ===",
            sprintf("FASTA file: %s", input$fasta_file$name),
            sprintf("Total proteins in FASTA: %d", length(rv$fasta_data)),
            sprintf("Target proteins queried: %d", length(rv$protein_ids)),
            sprintf("Successfully matched: %d", length(rv$matched_seqs)),
            sprintf("Unmatched IDs: %d", length(rv$unmatched_ids))),
          summary_file
        )
        writeLines(rv$unmatched_ids, unmatched_file)

        # Zip files
        zip(file, files = c(fasta_file, summary_file, unmatched_file), extras = "-j")
      }
    )

    # Return reactive values
    reactive({
      list(
        matched_sequences = rv$matched_seqs,
        unmatched_ids = rv$unmatched_ids,
        protein_ids = rv$protein_ids,
        fasta_data = rv$fasta_data
      )
    })
  })
}


