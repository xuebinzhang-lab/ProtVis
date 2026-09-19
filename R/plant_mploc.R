.plant_mploc_locations <- c(
  "Cell membrane", "Cell wall", "Chloroplast", "Cytoplasm",
  "Endoplasmic reticulum", "Extracellular", "Golgi apparatus",
  "Mitochondrion", "Nucleus", "Peroxisome", "Plastid", "Vacuole"
)

.plant_mploc_url <- "http://www.csbio.sjtu.edu.cn/bioinf/plant-multi/"

.plant_mploc_candidate_urls <- function(url) {
  urls <- url
  if (grepl("^https://", url, ignore.case = TRUE)) {
    urls <- c(urls, sub("^https://", "http://", url, ignore.case = TRUE))
  }
  unique(urls)
}

.plant_mploc_demo_sequence <- paste0(
  "MDKAHLGGGLLALDASPRPLGFLNLLSPPTSSPPFHRTRTMEADDVPAVPGNKGRRSIEVDFFSDDKDKKSAAAAANDKK",
  "EDLTTINLLPGSNANDDDDDEAATRLRLPNDQDRQNRNTNELSAMQSELARMNDENQRLRGMLTQVTTSYQALQMHLVALMQARADGQPMMPPAVAQTLPVTDAAAVMPSPRQFLGLGPAAAAEETSNSSTEVGSPRPSSSAGRRQDQQQQAAERGDSPDPADPSTTARQLAQQQEASMRKARVSVRARSEAPIIADGCQWRKYGQKMAKGNPCPRAYYRCTMANGCPVRKQLSRHAGSALRRRPLHPHHYLRGHPQPPAPARRRGHGLHHLRRRLHAAVRLHAQRGQLPGAGRAAVLLRHGHHLRLRAIPHRHARPHQRPARCSSSRATAAAPAGGRALQRHPPGFVQVLRPAHVIVIDIG"
)

.plant_mploc_clean_sequence <- function(sequence) {
  sequence <- paste(sequence, collapse = "")
  sequence <- toupper(gsub("\\s+", "", sequence))
  if (!nzchar(sequence)) stop("Enter a protein sequence.", call. = FALSE)
  if (!grepl("^[ACDEFGHIKLMNPQRSTVWY]+$", sequence)) {
    stop(
      "Protein sequence contains characters outside the 20 standard amino acids.",
      call. = FALSE
    )
  }
  sequence
}

.plant_mploc_fasta <- function(sequence, id) {
  id <- as.character(id %||% "query_protein")
  id <- if (length(id)) trimws(id[[1L]]) else "query_protein"
  if (!nzchar(id)) id <- "query_protein"
  starts <- seq.int(1L, nchar(sequence), by = 70L)
  lines <- substring(sequence, starts, pmin(starts + 69L, nchar(sequence)))
  paste(c(paste0(">", id), lines), collapse = "\n")
}

.plant_mploc_form_field <- function(forms) {
  candidates <- list()
  for (form_index in seq_along(forms)) {
    fields <- forms[[form_index]]$fields
    if (!length(fields)) next
    field_names <- names(fields)
    for (field_index in seq_along(fields)) {
      field <- fields[[field_index]]
      type <- tolower(as.character(field$type %||% ""))
      if (!type %in% c("textarea", "text", "search")) next
      name <- field_names[[field_index]]
      label <- paste(
        name,
        as.character(field$name %||% ""),
        as.character(field$id %||% ""),
        as.character(field$placeholder %||% "")
      )
      score <- if (identical(type, "textarea")) 100L else 20L
      if (grepl("seq|fasta|protein|query|input", label, ignore.case = TRUE)) {
        score <- score + 50L
      }
      candidates[[length(candidates) + 1L]] <- list(
        form = form_index,
        field = name,
        score = score
      )
    }
  }
  if (!length(candidates)) return(NULL)
  scores <- vapply(candidates, `[[`, numeric(1), "score")
  candidates[[which.max(scores)]]
}

.plant_mploc_submit_name <- function(form) {
  fields <- form$fields
  if (!length(fields)) return(NULL)
  field_names <- names(fields)
  if (is.null(field_names)) return(NULL)
  submit_index <- which(vapply(
    fields,
    function(field) {
      type <- tolower(as.character(field$type %||% ""))
      type %in% c("submit", "image", "button")
    },
    logical(1)
  ))
  if (!length(submit_index)) return(NULL)
  labels <- vapply(submit_index, function(index) {
    field <- fields[[index]]
    paste(
      field_names[[index]],
      as.character(field$value %||% ""),
      as.character(field$name %||% "")
    )
  }, character(1))
  preferred <- which(grepl("submit|predict|run", labels, ignore.case = TRUE))
  index <- submit_index[if (length(preferred)) preferred[[1L]] else 1L]
  field_names[[index]]
}

.plant_mploc_extract_prediction <- function(text) {
  if (is.null(text) || !length(text) || all(is.na(text))) return(NA_character_)
  text <- gsub("\r", "\n", paste(text, collapse = "\n"), fixed = TRUE)
  lines <- trimws(unlist(strsplit(text, "\\n+", perl = TRUE)))
  lines <- gsub("[[:space:]]+", " ", lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return(NA_character_)

  location_pattern <- paste(.plant_mploc_locations, collapse = "|")
  result_lines <- lines[
    grepl(
      "predict(?:ed|ion).*result|subcellular.*location",
      lines,
      ignore.case = TRUE,
      perl = TRUE
    )
  ]
  exact_pattern <- paste0(
    "^(?:", location_pattern, ")",
    "(?:\\s*(?:[.,;/]|and|&)\\s*(?:", location_pattern, "))*[.!]?$"
  )
  exact_lines <- lines[grepl(exact_pattern, lines, ignore.case = TRUE, perl = TRUE)]
  candidates <- unique(c(result_lines, exact_lines))
  if (!length(candidates)) return(NA_character_)

  # Exclude explanatory text that merely lists the server's possible classes.
  candidates <- candidates[
    !grepl(
      "following|location sites|predictor|input|example|server|identify.*among",
      candidates,
      ignore.case = TRUE
    )
  ]
  if (!length(candidates)) return(NA_character_)

  hits <- .plant_mploc_locations[
    vapply(
      .plant_mploc_locations,
      function(location) {
        any(grepl(location, candidates, ignore.case = TRUE, fixed = TRUE))
      },
      logical(1)
    )
  ]
  if (!length(hits)) NA_character_ else hits
}

#' Predict plant protein subcellular localization with Plant-mPLoc
#'
#' Submits one complete protein sequence to the public Plant-mPLoc web server
#' and returns the single- or multi-location prediction. Because this function
#' uses an external academic service, internet access and server availability
#' are required.
#'
#' @param sequence Protein sequence using the 20 standard one-letter amino-acid
#'   codes. Whitespace and line breaks are removed.
#' @param id Protein identifier used in the submitted FASTA header.
#' @param url Plant-mPLoc server URL.
#' @param ignore_ssl Whether to disable certificate verification for this
#'   legacy academic web server. Disable only when the server certificate
#'   cannot be validated.
#' @param timeout Request timeout in seconds.
#' @param verbose Whether to report submission progress.
#'
#' @return A `PlantmPLoc_result` list. Its `prediction` element contains one or
#'   more predicted subcellular locations.
#' @export
predict_plant_mploc <- function(
    sequence,
    id = "query_protein",
    url = .plant_mploc_url,
    ignore_ssl = TRUE,
    timeout = 90,
    verbose = TRUE) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("Package 'rvest' is required.", call. = FALSE)
  }
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required.", call. = FALSE)
  }

  sequence <- .plant_mploc_clean_sequence(sequence)
  id <- as.character(id %||% "query_protein")
  id <- if (length(id)) trimws(id[[1L]]) else "query_protein"
  if (!nzchar(id)) id <- "query_protein"
  url <- as.character(url)
  url <- if (length(url)) trimws(url[[1L]]) else ""
  if (!grepl("^https?://", url, ignore.case = TRUE)) {
    stop("Plant-mPLoc URL must use HTTP or HTTPS.", call. = FALSE)
  }
  fasta <- .plant_mploc_fasta(sequence, id)
  timeout <- suppressWarnings(as.numeric(timeout))
  timeout <- if (length(timeout)) timeout[[1L]] else 90
  if (!is.finite(timeout) || timeout < 5) timeout <- 90

  if (nchar(sequence) < 50L) {
    warning(
      "Plant-mPLoc recommends a complete protein; sequences shorter than 50 aa are usually fragments.",
      call. = FALSE
    )
  }
  if (isTRUE(verbose)) {
    message("Submitting ", id, " (", nchar(sequence), " aa) to Plant-mPLoc")
  }

  request_options <- list(
    httr::timeout(timeout),
    httr::user_agent("ProtVis Plant-mPLoc client")
  )
  if (isTRUE(ignore_ssl)) {
    request_options <- c(
      request_options,
      list(httr::config(ssl_verifypeer = 0L, ssl_verifyhost = 0L))
    )
  }

  ses <- NULL
  forms <- NULL
  selected <- NULL
  connection_errors <- character()
  for (candidate_url in .plant_mploc_candidate_urls(url)) {
    attempt <- tryCatch(
      {
        candidate_session <- do.call(
          rvest::session,
          c(list(url = candidate_url), request_options)
        )
        candidate_forms <- rvest::html_form(candidate_session)
        candidate_selected <- .plant_mploc_form_field(candidate_forms)
        if (is.null(candidate_selected)) {
          stop("FASTA input field was not found")
        }
        list(
          session = candidate_session,
          forms = candidate_forms,
          selected = candidate_selected
        )
      },
      error = function(e) {
        connection_errors <<- c(
          connection_errors,
          paste0(candidate_url, ": ", conditionMessage(e))
        )
        NULL
      }
    )
    if (!is.null(attempt)) {
      ses <- attempt$session
      forms <- attempt$forms
      selected <- attempt$selected
      break
    }
  }
  if (is.null(ses)) {
    stop(
      "Cannot connect to a usable Plant-mPLoc form. ",
      paste(connection_errors, collapse = "; "),
      call. = FALSE
    )
  }

  values <- stats::setNames(list(fasta), selected$field)
  form <- do.call(rvest::html_form_set, c(list(forms[[selected$form]]), values))
  submit_name <- .plant_mploc_submit_name(form)
  result_session <- tryCatch(
    if (is.null(submit_name)) {
      rvest::session_submit(ses, form)
    } else {
      rvest::session_submit(ses, form, submit = submit_name)
    },
    error = function(e) {
      stop("Plant-mPLoc form submission failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  result_nodes <- rvest::html_elements(
    result_session,
    "[id*='result'], [class*='result'], [id*='predict'], [class*='predict']"
  )
  focused_text <- if (length(result_nodes)) {
    paste(rvest::html_text2(result_nodes), collapse = "\n")
  } else {
    ""
  }
  body <- rvest::html_element(result_session, "body")
  result_text <- if (length(body)) rvest::html_text2(body) else ""
  prediction <- .plant_mploc_extract_prediction(focused_text)
  if (all(is.na(prediction))) {
    prediction <- .plant_mploc_extract_prediction(result_text)
  }
  if (all(is.na(prediction))) {
    stop(
      "Plant-mPLoc returned a page, but no prediction could be identified. Inspect the raw server response because the result layout may have changed.",
      call. = FALSE
    )
  }

  result <- list(
    protein_id = id,
    sequence = sequence,
    length = nchar(sequence),
    prediction = prediction,
    raw_result = result_text,
    result_url = result_session$url,
    source = "Plant-mPLoc",
    submitted_at = Sys.time()
  )
  class(result) <- "PlantmPLoc_result"
  result
}

#' @export
print.PlantmPLoc_result <- function(x, ...) {
  cat("Plant-mPLoc prediction\n")
  cat("Protein:", x$protein_id, paste0(" (", x$length, " aa)\n"))
  cat("Location:", paste(x$prediction, collapse = "; "), "\n")
  invisible(x)
}

#' Plant-mPLoc UI module
#' @param id Module namespace.
#' @return Shiny UI.
#' @export
plant_mploc_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      open = "open",
      gap = "12px",
      shiny::div(
        shiny::h4("Plant-mPLoc", style = "font-weight:700; margin-bottom:6px;"),
        shiny::p(
          "Predict single or multiple subcellular locations for a plant protein.",
          style = "color:#667085; font-size:13px;"
        )
      ),
      bslib::accordion(
        open = c("Protein input", "Run"),
        bslib::accordion_panel(
          "Protein input",
          shiny::textInput(ns("protein_id"), "Protein ID", value = "ZmProtein_demo"),
          shiny::textAreaInput(
            ns("sequence"), "Protein sequence", value = .plant_mploc_demo_sequence, rows = 12,
            placeholder = "Paste a complete protein sequence using one-letter amino-acid codes"
          ),
          shiny::actionButton(
            ns("load_demo"), "LOAD DEMO SEQUENCE",
            icon = bsicons::bs_icon("stars"),
            class = "btn-outline-primary w-100 mb-2"
          ),
          shiny::uiOutput(ns("sequence_status"))
        ),
        bslib::accordion_panel(
          "Advanced",
          shiny::numericInput(ns("timeout"), "Timeout (seconds)", value = 90, min = 5, max = 600),
          shiny::checkboxInput(
            ns("ignore_ssl"), "Allow the legacy server certificate", value = TRUE
          )
        ),
        bslib::accordion_panel(
          "Run",
          shiny::div(
            class = "d-grid gap-2",
            shiny::actionButton(
              ns("run"), "PREDICT LOCALIZATION",
              icon = bsicons::bs_icon("play-fill"), class = "btn-primary fw-bold"
            ),
            shiny::actionButton(
              ns("clear"), "CLEAR", icon = bsicons::bs_icon("x-circle"),
              class = "btn-outline-secondary"
            ),
            shiny::tags$a(
              href = .plant_mploc_url,
              target = "_blank", class = "btn btn-outline-info",
              bsicons::bs_icon("box-arrow-up-right"), " Open Plant-mPLoc"
            )
          )
        )
      )
    ),
    bslib::page_fillable(
      fillable = TRUE,
      bslib::layout_column_wrap(
        width = 1 / 3,
        bslib::card(
          bslib::card_body(
            shiny::div(style = "font-size:12px;color:#667085;font-weight:700;", "STATUS"),
            shiny::uiOutput(ns("run_status"))
          )
        ),
        bslib::card(
          bslib::card_body(
            shiny::div(style = "font-size:12px;color:#667085;font-weight:700;", "PROTEIN"),
            shiny::uiOutput(ns("protein_summary"))
          )
        ),
        bslib::card(
          bslib::card_body(
            shiny::div(style = "font-size:12px;color:#667085;font-weight:700;", "PREDICTED LOCATIONS"),
            shiny::uiOutput(ns("location_count"))
          )
        )
      ),
      bslib::card(
        full_screen = TRUE,
        min_height = 650,
        bslib::card_header("Plant-mPLoc Results"),
        bslib::card_body(
          bslib::navset_card_tab(
            bslib::nav_panel("Prediction", shiny::uiOutput(ns("prediction_panel"))),
            bslib::nav_panel("Result table", DT::DTOutput(ns("prediction_table"))),
            bslib::nav_panel(
              "Raw response",
              shiny::div(
                style = "max-height:520px; overflow:auto; white-space:pre-wrap;",
                shiny::verbatimTextOutput(ns("raw_result"))
              )
            ),
            bslib::nav_panel(
              "Method",
              shiny::p(
                "Plant-mPLoc predicts plant protein localization across 12 cellular locations and can report multiple locations. Use a complete protein sequence for the intended accuracy."
              ),
              shiny::tags$a(
                href = "https://doi.org/10.1371/journal.pone.0011335",
                target = "_blank", "Plant-mPLoc publication"
              )
            )
          )
        ),
        bslib::card_footer(
          shiny::downloadButton(ns("download"), "DOWNLOAD RESULT", class = "btn-outline-primary")
        )
      )
    )
  )
}

#' Plant-mPLoc server module
#' @param id Module namespace.
#' @return Shiny server module.
#' @export
plant_mploc_server <- function(id, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal("Ready")
    error_message <- shiny::reactiveVal(NULL)
    running <- shiny::reactiveVal(FALSE)

    output$sequence_status <- shiny::renderUI({
      sequence <- toupper(gsub("\\s+", "", input$sequence %||% ""))
      if (!nzchar(sequence)) {
        return(shiny::div(style = "font-size:12px;color:#667085;", "No sequence entered"))
      }
      valid <- grepl("^[ACDEFGHIKLMNPQRSTVWY]+$", sequence)
      colour <- if (valid) "#157347" else "#b42318"
      label <- if (valid) paste(nchar(sequence), "aa; valid alphabet") else "Invalid characters detected"
      shiny::div(style = paste0("font-size:12px;color:", colour, ";"), label)
    })

    shiny::observeEvent(input$clear, {
      shiny::updateTextAreaInput(session, "sequence", value = "")
      result(NULL)
      error_message(NULL)
      status("Ready")
    })

    shiny::observeEvent(input$load_demo, {
      shiny::updateTextInput(session, "protein_id", value = "ZmProtein_demo")
      shiny::updateTextAreaInput(
        session,
        "sequence",
        value = .plant_mploc_demo_sequence
      )
      result(NULL)
      error_message(NULL)
      status("Ready")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$run, {
      if (isTRUE(running())) return(invisible(NULL))
      running(TRUE)
      shinyjs::disable("run")
      on.exit({
        running(FALSE)
        shinyjs::enable("run")
      }, add = TRUE)

      status("Connecting")
      error_message(NULL)
      result(NULL)
      tryCatch({
        sequence <- .plant_mploc_clean_sequence(input$sequence)
        value <- shiny::withProgress(
          message = "Running Plant-mPLoc...", value = 0.2,
          {
            value <- predict_plant_mploc(
              sequence = sequence,
              id = input$protein_id,
              url = .plant_mploc_url,
              ignore_ssl = isTRUE(input$ignore_ssl),
              timeout = input$timeout,
              verbose = FALSE
            )
            shiny::incProgress(0.8, detail = "Prediction received")
            value
          }
        )
        result(value)
        .protvis_record_shared_run(
          shared_state,
          module = "plant_mploc",
          method = "Plant-mPLoc",
          category = "subcellular_localization",
          parameters = list(
            protein_id = input$protein_id,
            ignore_ssl = isTRUE(input$ignore_ssl),
            timeout = input$timeout
          ),
          tables = list(
            prediction = data.frame(
              protein_id = value$protein_id,
              sequence_length = value$length,
              predicted_location = value$prediction,
              source = value$source,
              stringsAsFactors = FALSE
            )
          ),
          statistics = list(sequence = sequence)
        )
        status("Completed")
        shiny::showNotification("Plant-mPLoc prediction completed.", type = "message")
      }, error = function(e) {
        error_message(conditionMessage(e))
        status("Failed")
        shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
      })
    }, ignoreInit = TRUE)

    output$run_status <- shiny::renderUI({
      colour <- switch(status(), Completed = "#157347", Failed = "#b42318", Connecting = "#0a74b9", "#667085")
      shiny::div(style = paste0("font-size:22px;font-weight:700;color:", colour, ";"), status())
    })
    output$protein_summary <- shiny::renderUI({
      value <- result()
      if (is.null(value)) return(shiny::div(style = "font-size:18px;font-weight:700;", "—"))
      shiny::div(
        shiny::div(style = "font-size:18px;font-weight:700;", value$protein_id),
        shiny::div(style = "font-size:13px;color:#667085;", paste(value$length, "amino acids"))
      )
    })
    output$location_count <- shiny::renderUI({
      value <- result()
      count <- if (is.null(value)) "—" else length(value$prediction)
      shiny::div(style = "font-size:22px;font-weight:700;color:#0a74b9;", count)
    })
    output$prediction_panel <- shiny::renderUI({
      value <- result()
      if (is.null(value)) {
        message <- error_message()
        if (is.null(message)) {
          return(shiny::div(class = "text-muted p-4", "Enter a complete plant protein sequence and run the prediction."))
        }
        return(shiny::div(
          class = "alert alert-danger m-3",
          shiny::strong("Prediction failed"), shiny::br(), message
        ))
      }
      shiny::div(
        class = "p-4",
        shiny::h4(value$protein_id),
        shiny::p(paste(value$length, "amino acids"), class = "text-muted"),
        shiny::div(
          style = "display:flex;gap:10px;flex-wrap:wrap;margin-top:18px;",
          lapply(value$prediction, function(location) {
            shiny::span(
              location,
              style = paste(
                "padding:10px 16px;border-radius:999px;background:#e8f5ff;",
                "border:1px solid #9bd4f5;color:#075985;font-weight:700;"
              )
            )
          })
        )
      )
    })
    output$prediction_table <- DT::renderDT({
      value <- result()
      shiny::req(value)
      DT::datatable(
        data.frame(
          protein_id = value$protein_id,
          sequence_length = value$length,
          predicted_location = value$prediction,
          source = value$source,
          stringsAsFactors = FALSE
        ),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 12)
      )
    })
    output$raw_result <- shiny::renderText({
      value <- result()
      shiny::req(value)
      value$raw_result
    })
    output$download <- shiny::downloadHandler(
      filename = function() paste0("Plant-mPLoc_", gsub("[^A-Za-z0-9_.-]", "_", input$protein_id), ".csv"),
      content = function(file) {
        value <- result()
        shiny::req(value)
        utils::write.csv(
          data.frame(
            protein_id = value$protein_id,
            sequence_length = value$length,
            predicted_location = value$prediction,
            source = value$source,
            stringsAsFactors = FALSE
          ),
          file,
          row.names = FALSE
        )
      }
    )

    result
  })
}
