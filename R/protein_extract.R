#' Protein Extract UI Module
#' UI for protein sequence extraction from FASTA by protein IDs.
#'
#' @param id Module id
#'
#' @return Shiny UI
#' @import shiny
#' @import bslib
#' @importFrom shinyjs useShinyjs disabled enable disable
#' @importFrom shinyWidgets radioGroupButtons
#' @name protein_extract_ui
#' @export
protein_extract_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shinyjs::useShinyjs(),

    bslib::layout_sidebar(
      class = "pv-protein-extract",
      sidebar = bslib::sidebar(
        width = 340,
        open = TRUE,
        bg = "#f8f9fa",

        shiny::div(
          style = "padding-bottom: 8px;",
          shiny::h5("Protein Extraction", style = "font-weight: 700; margin-bottom: 6px;"),
          shiny::p(
            "Upload a FASTA file and extract target protein sequences by protein IDs.",
            style = "color: #6c757d; font-size: 13px; margin-bottom: 0;"
          )
        ),

        shiny::tags$hr(style = "margin: 10px 0 16px 0;"),

        shiny::fileInput(
          ns("fasta_file"),
          "Upload FASTA File",
          accept = c(".fa", ".faa", ".fasta", ".fa.gz", ".faa.gz", ".fasta.gz"),
          buttonLabel = "Browse..."
        ),

        shiny::div(
          style = "font-size: 12px; color: #6c757d; margin-top: -8px; margin-bottom: 12px;",
          "Supported formats: .fa, .faa, .fasta, .gz"
        ),

        shinyWidgets::radioGroupButtons(
          inputId = ns("input_mode"),
          label = "Protein ID Input Method",
          choices = c("Manual Input" = "manual", "File Upload" = "file"),
          selected = "manual",
          status = "primary",
          justified = TRUE,
          checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"))
        ),

        shiny::conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == 'manual'"),
          shiny::div(
            style = "margin-top: 12px;",
            shiny::tags$label(
              "Enter Protein IDs (one per line)",
              style = "font-weight: 600; margin-bottom: 6px;"
            ),
            shiny::textAreaInput(
              ns("protein_ids"),
              label = NULL,
              rows = 8,
              width = "100%",
              placeholder = paste(
                "Paste protein IDs here",
                "Example:",
                "Zm00001eb000010",
                "Zm00001eb000020",
                sep = "\n"
              )
            )
          )
        ),

        shiny::conditionalPanel(
          condition = paste0("input['", ns("input_mode"), "'] == 'file'"),
          shiny::div(
            style = "margin-top: 12px;",
            shiny::fileInput(
              ns("id_file"),
              "Upload Protein ID File",
              accept = c(".txt", ".csv", ".tsv"),
              buttonLabel = "Browse..."
            ),
            shiny::div(
              style = "font-size: 12px; color: #6c757d; margin-top: -8px;",
              "For .csv/.tsv files, all values will be read as candidate IDs."
            )
          )
        ),

        shiny::tags$hr(style = "margin: 16px 0;"),

        shiny::div(
          class = "d-grid gap-2",
          shiny::actionButton(
            ns("extract"),
            "Run Extraction",
            icon = shiny::icon("play"),
            class = "btn btn-primary"
          ),
          shinyjs::disabled(
            shiny::downloadButton(
              ns("download_results"),
              "Download Results",
              class = "btn btn-light"
            )
          )
        ),

        shiny::div(
          style = "margin-top: 16px;",
          bslib::card(
            full_screen = FALSE,
            bslib::card_header("Usage Notes"),
            bslib::card_body(
              shiny::tags$ul(
                style = "padding-left: 18px; margin-bottom: 0; font-size: 13px;",
                shiny::tags$li("Upload the protein FASTA file first."),
                shiny::tags$li("Enter IDs manually or upload an ID list."),
                shiny::tags$li("Click Run Extraction to generate results."),
                shiny::tags$li("Matched sequences can be viewed and downloaded.")
              )
            )
          )
        )
      ),

      shiny::div(
        style = "padding-top: 6px;",

        bslib::layout_column_wrap(
          width = 1/3,
          gap = "12px",

          bslib::value_box(
            title = "Total FASTA Entries",
            value = shiny::textOutput(ns("stat_total_fasta"), inline = TRUE),
            showcase = bsicons::bs_icon("database"),
            theme = "light"
          ),

          bslib::value_box(
            title = "Queried IDs",
            value = shiny::textOutput(ns("stat_total_query"), inline = TRUE),
            showcase = bsicons::bs_icon("list-check"),
            theme = "light"
          ),

          bslib::value_box(
            title = "Matched IDs",
            value = shiny::textOutput(ns("stat_total_matched"), inline = TRUE),
            showcase = bsicons::bs_icon("check-circle"),
            theme = "light"
          )
        ),

        shiny::br(),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header(
            shiny::div(
              style = "display:flex; justify-content:space-between; align-items:center;",
              shiny::span("Protein Extraction Results", style = "font-weight: 600;"),
              shiny::uiOutput(ns("run_status"))
            )
          ),

          bslib::navset_card_tab(
            id = ns("result_tabs"),

            bslib::nav_panel(
              "Matched Summary",
              shiny::div(
                style = "min-height: 460px; padding: 8px;",
                shiny::uiOutput(ns("matched_summary_ui"))
              )
            ),

            bslib::nav_panel(
              "Sequence Table",
              shiny::div(
                style = "min-height: 460px; padding: 8px;",
                DT::DTOutput(ns("sequence_table"))
              )
            ),

            bslib::nav_panel(
              "FASTA Viewer",
              shiny::div(
                style = "min-height: 460px; padding: 8px;",
                shiny::uiOutput(ns("fasta_viewer_ui"))
              )
            ),

            bslib::nav_panel(
              "Unmatched IDs",
              shiny::div(
                style = "min-height: 460px; padding: 8px;",
                shiny::uiOutput(ns("unmatched_ids_ui"))
              )
            )
          )
        )
      )
    ),

    shiny::tags$style(shiny::HTML("\n      .pv-protein-extract {\n        --pv-science-navy: #24445f;\n        --pv-science-border: #cbd5df;\n        --pv-science-muted: #64748b;\n      }\n      .pv-protein-extract .sidebar {\n        background: #f8fafc;\n        border-right: 1px solid var(--pv-science-border);\n      }\n      .pv-protein-extract .card,\n      .pv-protein-extract .bslib-value-box,\n      .pv-protein-extract .value-box {\n        border: 1px solid var(--pv-science-border);\n        border-radius: 6px;\n        box-shadow: none;\n      }\n      .pv-protein-extract .bslib-value-box,\n      .pv-protein-extract .value-box {\n        background: #ffffff !important;\n        color: var(--pv-science-navy) !important;\n      }\n      .pv-protein-extract .bslib-value-box .value-box-title,\n      .pv-protein-extract .value-box .value-box-title {\n        color: var(--pv-science-muted) !important;\n        font-size: 0.82rem;\n        font-weight: 600;\n      }\n      .pv-protein-extract .bslib-value-box .value-box-value,\n      .pv-protein-extract .value-box .value-box-value {\n        color: var(--pv-science-navy) !important;\n        font-weight: 700;\n      }\n      .pv-protein-extract .btn {\n        border-radius: 4px;\n        text-transform: none;\n        letter-spacing: 0;\n        box-shadow: none;\n      }\n      .pv-protein-extract .btn-primary {\n        background: var(--pv-science-navy);\n        border-color: var(--pv-science-navy);\n      }\n      .pv-protein-extract .btn-primary:hover,\n      .pv-protein-extract .btn-primary:focus {\n        background: #19364f;\n        border-color: #19364f;\n      }\n      .pv-protein-extract .btn-light {\n        background: #ffffff;\n        color: var(--pv-science-navy);\n        border: 1px solid var(--pv-science-border);\n      }\n      .pv-protein-extract .nav-tabs .nav-link {\n        color: var(--pv-science-muted);\n        border-radius: 4px 4px 0 0;\n      }\n      .pv-protein-extract .nav-tabs .nav-link.active {\n        color: var(--pv-science-navy);\n        font-weight: 600;\n      }\n    "))
  )
}

#' Protein Extract Server Module
#' Server for protein sequence extraction from FASTA by protein IDs.
#'
#' @param id Module id
#'
#' @return Reactive list
#' @import shiny
#' @importFrom shinyjs enable disable
#' @importFrom Biostrings readAAStringSet width writeXStringSet
#' @importFrom stringr str_trim str_detect str_extract fixed
#' @importFrom tools file_ext
#' @importFrom utils read.delim head zip
#' @importFrom DT renderDT datatable
#' @name protein_extract_server
#' @export
protein_extract_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    rv <- shiny::reactiveValues(
      fasta_data = NULL,
      protein_ids = NULL,
      matched_seqs = NULL,
      unmatched_ids = NULL,
      found_ids = NULL,
      has_run = FALSE
    )

    empty_state <- function(text = "Please upload input files and click 'Run Extraction'.") {
      bslib::card(
        style = "border: 1px dashed #ced4da; background: #fafbfc;",
        bslib::card_body(
          shiny::div(
            style = "min-height: 380px; display:flex; align-items:center; justify-content:center; color:#6c757d; font-size:15px;",
            text
          )
        )
      )
    }

    parse_protein_ids <- function() {
      if (is.null(input$input_mode)) {
        return(NULL)
      }

      if (input$input_mode == "manual") {
        txt <- input$protein_ids
        if (is.null(txt) || !nzchar(trimws(txt))) {
          return(NULL)
        }

        ids <- unlist(strsplit(txt, "\n", fixed = TRUE))
        ids <- stringr::str_trim(ids)
        ids <- ids[ids != ""]
        ids <- unique(ids)
        return(ids)
      }

      if (input$input_mode == "file") {
        if (is.null(input$id_file)) {
          return(NULL)
        }

        ext <- tolower(tools::file_ext(input$id_file$name))

        out <- tryCatch({
          if (ext %in% c("csv", "tsv")) {
            sep <- if (ext == "csv") "," else "\t"
            df <- utils::read.delim(
              input$id_file$datapath,
              sep = sep,
              header = FALSE,
              stringsAsFactors = FALSE,
              check.names = FALSE
            )
            vals <- unlist(df, use.names = FALSE)
          } else {
            vals <- readLines(input$id_file$datapath, warn = FALSE)
          }

          vals <- stringr::str_trim(vals)
          vals <- vals[vals != ""]
          vals <- unique(vals)
          vals
        }, error = function(e) {
          shiny::showNotification(
            paste("Error reading ID file:", e$message),
            type = "error"
          )
          NULL
        })

        return(out)
      }

      NULL
    }

    shiny::observe({
      rv$protein_ids <- parse_protein_ids()
    })

    shiny::observeEvent(input$fasta_file, {
      shiny::req(input$fasta_file)

      tryCatch({
        if (base::grepl("\\.gz$", input$fasta_file$name, ignore.case = TRUE)) {
          con <- gzfile(input$fasta_file$datapath, open = "rt")
          on.exit(close(con), add = TRUE)
          rv$fasta_data <- Biostrings::readAAStringSet(con)
        } else {
          rv$fasta_data <- Biostrings::readAAStringSet(input$fasta_file$datapath)
        }

        shiny::showNotification("FASTA file loaded successfully.", type = "message")
      }, error = function(e) {
        rv$fasta_data <- NULL
        shiny::showNotification(
          paste("Error loading FASTA file:", e$message),
          type = "error"
        )
      })
    })

    shiny::observeEvent(input$extract, {
      if (is.null(rv$fasta_data)) {
        shiny::showNotification("Please upload a FASTA file first.", type = "warning")
        return()
      }

      if (is.null(rv$protein_ids) || length(rv$protein_ids) == 0) {
        shiny::showNotification("Please provide at least one protein ID.", type = "warning")
        return()
      }

      tryCatch({
        headers <- names(rv$fasta_data)

        matched_idx <- vapply(
          headers,
          function(h) {
            any(vapply(
              rv$protein_ids,
              function(id) {
                stringr::str_detect(h, stringr::fixed(id))
              },
              logical(1)
            ))
          },
          logical(1)
        )

        rv$matched_seqs <- rv$fasta_data[matched_idx]

        found_ids <- unique(unlist(lapply(
          headers[matched_idx],
          function(h) {
            rv$protein_ids[vapply(
              rv$protein_ids,
              function(id) stringr::str_detect(h, stringr::fixed(id)),
              logical(1)
            )]
          }
        )))

        rv$found_ids <- found_ids
        rv$unmatched_ids <- setdiff(rv$protein_ids, found_ids)
        rv$has_run <- TRUE

        if (length(rv$matched_seqs) > 0) {
          shinyjs::enable("download_results")
        } else {
          shinyjs::disable("download_results")
        }

        shiny::showNotification(
          sprintf(
            "Extraction completed: %d matched / %d queried.",
            length(rv$matched_seqs),
            length(rv$protein_ids)
          ),
          type = "message"
        )
      }, error = function(e) {
        rv$matched_seqs <- NULL
        rv$unmatched_ids <- NULL
        rv$found_ids <- NULL
        rv$has_run <- TRUE
        shinyjs::disable("download_results")

        shiny::showNotification(
          paste("Error during extraction:", e$message),
          type = "error"
        )
      })
    })

    output$stat_total_fasta <- shiny::renderText({
      if (is.null(rv$fasta_data)) "0" else as.character(length(rv$fasta_data))
    })

    output$stat_total_query <- shiny::renderText({
      if (is.null(rv$protein_ids)) "0" else as.character(length(rv$protein_ids))
    })

    output$stat_total_matched <- shiny::renderText({
      if (is.null(rv$matched_seqs)) "0" else as.character(length(rv$matched_seqs))
    })

    output$run_status <- shiny::renderUI({
      if (!isTRUE(rv$has_run)) {
        shiny::tags$span(
          "Waiting for run",
          style = "display:inline-block; padding:4px 10px; background:#f1f3f5; border-radius:999px; color:#6c757d; font-size:12px;"
        )
      } else {
        shiny::tags$span(
          "Completed",
          style = "display:inline-block; padding:4px 10px; background:#e9f7ef; border-radius:999px; color:#198754; font-size:12px;"
        )
      }
    })

    output$matched_summary_ui <- shiny::renderUI({
      if (!isTRUE(rv$has_run)) {
        return(empty_state())
      }

      if (is.null(rv$matched_seqs) || length(rv$matched_seqs) == 0) {
        return(empty_state("No matched proteins were found."))
      }

      match_rate <- round(length(rv$matched_seqs) / length(rv$protein_ids) * 100, 2)

      shiny::tagList(
        bslib::layout_column_wrap(
          width = 1/3,
          gap = "12px",

          bslib::card(
            bslib::card_body(
              shiny::h4(length(rv$fasta_data), style = "margin:0; font-weight:700;"),
              shiny::div("Total FASTA Entries", style = "color:#6c757d;")
            )
          ),

          bslib::card(
            bslib::card_body(
              shiny::h4(length(rv$protein_ids), style = "margin:0; font-weight:700;"),
              shiny::div("Queried IDs", style = "color:#6c757d;")
            )
          ),

          bslib::card(
            bslib::card_body(
              shiny::h4(
                paste0(length(rv$matched_seqs), " (", match_rate, "%)"),
                style = "margin:0; font-weight:700; color:#198754;"
              ),
              shiny::div("Matched IDs", style = "color:#6c757d;")
            )
          )
        ),

        shiny::br(),

        bslib::card(
          bslib::card_header("First 10 Matched Headers"),
          bslib::card_body(
            shiny::tags$pre(
              style = "white-space: pre-wrap; word-break: break-word; margin-bottom: 0;",
              paste(utils::head(names(rv$matched_seqs), 10), collapse = "\n")
            )
          )
        )
      )
    })

    output$sequence_table <- DT::renderDT({
      if (!isTRUE(rv$has_run) || is.null(rv$matched_seqs) || length(rv$matched_seqs) == 0) {
        return(
          DT::datatable(
            data.frame(Message = "No matched sequence available."),
            rownames = FALSE,
            options = list(dom = "t", paging = FALSE)
          )
        )
      }

      df <- data.frame(
        Protein_ID = names(rv$matched_seqs),
        Length = Biostrings::width(rv$matched_seqs),
        Sequence = as.character(rv$matched_seqs),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      DT::datatable(
        df,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel"),
          autoWidth = TRUE
        )
      )
    })

    output$fasta_viewer_ui <- shiny::renderUI({
      if (!isTRUE(rv$has_run)) {
        return(empty_state())
      }

      if (is.null(rv$matched_seqs) || length(rv$matched_seqs) == 0) {
        return(empty_state("No FASTA content available."))
      }

      fasta_text <- paste(
        paste0(">", names(rv$matched_seqs)),
        as.character(rv$matched_seqs),
        sep = "\n",
        collapse = "\n"
      )

      shiny::tagList(
        shiny::tags$label("Matched FASTA Sequences", style = "font-weight: 600;"),
        shiny::tags$textarea(
          class = "form-control",
          style = "width:100%; min-height:420px; resize:vertical; font-family:monospace;",
          readonly = "readonly",
          fasta_text
        )
      )
    })

    output$unmatched_ids_ui <- shiny::renderUI({
      if (!isTRUE(rv$has_run)) {
        return(empty_state())
      }

      if (is.null(rv$unmatched_ids) || length(rv$unmatched_ids) == 0) {
        return(
          bslib::card(
            bslib::card_body(
              shiny::div(
                style = "min-height: 380px; display:flex; align-items:center; justify-content:center; color:#198754; font-size:15px;",
                "All queried IDs were matched successfully."
              )
            )
          )
        )
      }

      show_ids <- if (length(rv$unmatched_ids) > 200) rv$unmatched_ids[1:200] else rv$unmatched_ids

      bslib::card(
        bslib::card_header(
          paste0("Unmatched IDs (", length(rv$unmatched_ids), ")")
        ),
        bslib::card_body(
          shiny::tags$pre(
            style = "white-space: pre-wrap; word-break: break-word; margin-bottom: 0;",
            paste(show_ids, collapse = "\n")
          ),
          if (length(rv$unmatched_ids) > 200) {
            shiny::div(
              style = "margin-top: 10px; color:#6c757d;",
              paste0("Only the first 200 unmatched IDs are shown here.")
            )
          }
        )
      )
    })

    output$download_results <- shiny::downloadHandler(
      filename = function() {
        paste0("protein_extract_results_", Sys.Date(), ".zip")
      },
      content = function(file) {
        shiny::req(rv$matched_seqs)

        temp_dir <- tempfile("protein_extract_")
        dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

        fasta_file <- file.path(temp_dir, "matched_sequences.fasta")
        summary_file <- file.path(temp_dir, "summary.txt")
        unmatched_file <- file.path(temp_dir, "unmatched_ids.txt")

        Biostrings::writeXStringSet(rv$matched_seqs, fasta_file)

        writeLines(
          c(
            "=== Protein Extraction Summary ===",
            sprintf("FASTA file: %s", input$fasta_file$name),
            sprintf("Total proteins in FASTA: %d", length(rv$fasta_data)),
            sprintf("Target proteins queried: %d", length(rv$protein_ids)),
            sprintf("Successfully matched: %d", length(rv$matched_seqs)),
            sprintf("Unmatched IDs: %d", length(rv$unmatched_ids))
          ),
          summary_file
        )

        writeLines(rv$unmatched_ids %||% character(0), unmatched_file)

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(temp_dir)

        utils::zip(
          zipfile = file,
          files = c("matched_sequences.fasta", "summary.txt", "unmatched_ids.txt")
        )
      }
    )

    shiny::reactive({
      list(
        matched_sequences = rv$matched_seqs,
        unmatched_ids = rv$unmatched_ids,
        protein_ids = rv$protein_ids,
        fasta_data = rv$fasta_data,
        has_run = rv$has_run
      )
    })
  })
}
