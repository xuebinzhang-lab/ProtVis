# Protein Workbench STRING interaction diagnostics -----------------------
# Loaded after zzz_protein_workbench_context.R. This replaces only the
# Protein Workbench wrapper UI/server; the original Workbench and standalone
# STRINGdb PPI / Plant-mPLoc modules remain unchanged.

.protvis_pw_string_candidates <- function(entry) {
  if (base::is.null(entry)) return(base::character())
  values <- c(
    base::as.character(entry$primaryAccession %||% ""),
    base::as.character(entry$uniProtkbId %||% ""),
    base::unlist(base::lapply(entry$genes %||% base::list(), function(gene) {
      c(
        gene$geneName$value %||% base::character(),
        base::unlist(base::lapply(gene$synonyms %||% base::list(), function(x) x$value %||% base::character()))
      )
    }), use.names = FALSE)
  )
  values <- base::unique(base::trimws(values))
  values[!base::is.na(values) & base::nzchar(values)]
}

.protvis_pw_string_xref <- function(entry) {
  refs <- .protvis_pw_xrefs_table(entry)
  if (!base::nrow(refs)) return(base::data.frame())
  refs[base::toupper(refs$database) == "STRING", , drop = FALSE]
}

.protvis_pw_string_resolve <- function(entry) {
  if (base::is.null(entry)) {
    return(base::list(mapping = base::data.frame(), tried = base::character(), status = "No UniProt record is loaded."))
  }
  species <- base::as.character(entry$organism$taxonId %||% "")
  if (!base::nzchar(species)) {
    return(base::list(mapping = base::data.frame(), tried = base::character(), status = "UniProt taxonomy ID is unavailable."))
  }

  xrefs <- .protvis_pw_string_xref(entry)
  if (base::nrow(xrefs)) {
    string_id <- base::as.character(xrefs$id[[1L]] %||% "")
    if (base::nzchar(string_id)) {
      mapping <- base::data.frame(
        query = base::as.character(entry$primaryAccession %||% string_id),
        string_id = string_id,
        preferred_name = base::as.character(.protvis_pw_gene_names(entry) %||% entry$primaryAccession %||% string_id),
        taxon_id = species,
        taxon_name = base::as.character(entry$organism$scientificName %||% ""),
        annotation = "Resolved from UniProt STRING cross-reference",
        mapped_from = "UniProt STRING cross-reference",
        stringsAsFactors = FALSE
      )
      return(base::list(mapping = mapping, tried = string_id, status = "Mapped from UniProt STRING cross-reference."))
    }
  }

  candidates <- .protvis_pw_string_candidates(entry)
  errors <- base::character()
  for (candidate in candidates) {
    mapped <- base::tryCatch(
      .protvis_pw_string_map(candidate, species),
      error = function(e) {
        errors <<- c(errors, base::paste0(candidate, ": ", base::conditionMessage(e)))
        base::data.frame()
      }
    )
    if (base::nrow(mapped)) {
      mapped$mapped_from <- candidate
      return(base::list(
        mapping = mapped,
        tried = candidates[base::seq_len(base::match(candidate, candidates))],
        status = base::paste0("Mapped using ", candidate, ".")
      ))
    }
  }

  status <- "No STRING mapping was found for the identifiers supplied by UniProt."
  if (base::length(errors)) status <- base::paste0(status, " API messages: ", base::paste(errors, collapse = " | "))
  base::list(mapping = base::data.frame(), tried = candidates, status = status)
}

.protvis_pw_string_partners_from_mapping <- function(mapping, species, required_score = 400L, limit = 20L) {
  if (base::is.null(mapping) || !base::nrow(mapping)) return(base::data.frame())
  string_id <- base::as.character(mapping$string_id[[1L]] %||% "")
  if (!base::nzchar(string_id)) return(base::data.frame())
  required_score <- base::max(0L, base::min(1000L, base::as.integer(required_score)))
  limit <- base::max(1L, base::min(100L, base::as.integer(limit)))

  raw <- .protvis_pw_http_json(
    "https://string-db.org/api/json/interaction_partners",
    query = base::list(
      identifiers = string_id,
      species = species,
      required_score = required_score,
      limit = limit,
      network_type = "functional",
      caller_identity = "ProtVis"
    ),
    timeout = 60,
    not_found = NULL
  )
  records <- .protvis_pw_string_records(raw)
  if (!base::length(records)) return(base::data.frame())

  query_name <- base::as.character(mapping$preferred_name[[1L]] %||% string_id)
  rows <- base::lapply(records, function(x) {
    a_is_query <- identical(base::as.character(x$stringId_A %||% ""), string_id)
    partner_id <- if (a_is_query) x$stringId_B else x$stringId_A
    partner_name <- if (a_is_query) x$preferredName_B else x$preferredName_A
    partner_annotation <- if (a_is_query) x$annotation_B else x$annotation_A
    base::data.frame(
      query = query_name,
      query_string_id = string_id,
      partner = base::as.character(partner_name %||% partner_id %||% ""),
      partner_string_id = base::as.character(partner_id %||% ""),
      score = base::as.numeric(x$score %||% NA_real_),
      neighborhood = base::as.numeric(x$nscore %||% NA_real_),
      fusion = base::as.numeric(x$fscore %||% NA_real_),
      phylogenetic_profile = base::as.numeric(x$pscore %||% NA_real_),
      coexpression = base::as.numeric(x$ascore %||% NA_real_),
      experimental = base::as.numeric(x$escore %||% NA_real_),
      database = base::as.numeric(x$dscore %||% NA_real_),
      text_mining = base::as.numeric(x$tscore %||% NA_real_),
      annotation = base::as.character(partner_annotation %||% ""),
      stringsAsFactors = FALSE
    )
  })
  table <- base::do.call(base::rbind, rows)
  table <- table[base::order(table$score, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  base::rownames(table) <- NULL
  table
}

.protvis_pw_context_ui_v2 <- function(id) {
  ns <- shiny::NS(id)
  shiny::div(
    style = "padding:0 16px 24px;",
    bslib::card(
      bslib::card_header(
        shiny::div(
          style = "display:flex;align-items:center;justify-content:space-between;gap:12px;",
          shiny::div(
            shiny::tags$strong("Protein context"),
            shiny::div(
              "Localization and interaction views for the currently resolved protein.",
              style = "font-size:12px;color:#657789;font-weight:400;margin-top:2px;"
            )
          ),
          shiny::uiOutput(ns("context_status"))
        )
      ),
      bslib::card_body(
        bslib::navset_card_tab(
          id = ns("context_tabs"),
          bslib::nav_panel(
            "Localization",
            shiny::div(
              style = "color:#657789;font-size:12px;margin-bottom:12px;",
              "Combines curated UniProt subcellular-location annotation, sequence topology features, and optional Plant-mPLoc prediction for plant proteins."
            ),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(bslib::card_header("Curated localization"), DT::DTOutput(ns("localization_table"))),
              bslib::card(bslib::card_header("Localization-related sequence features"), DT::DTOutput(ns("localization_features")))
            ),
            shiny::br(),
            bslib::card(
              bslib::card_header("Plant-mPLoc prediction"),
              bslib::layout_columns(
                col_widths = c(4, 8),
                shiny::div(
                  shiny::p(
                    "Plant-mPLoc is plant-specific and uses the current protein sequence. It is run only when requested; the standalone Plant-mPLoc toolkit remains available.",
                    style = "font-size:12px;color:#657789;"
                  ),
                  shiny::actionButton(ns("run_plant_mploc"), "RUN PLANT-mPLoc", icon = bsicons::bs_icon("geo-alt"), class = "btn-outline-primary"),
                  shiny::uiOutput(ns("plant_mploc_status"))
                ),
                DT::DTOutput(ns("plant_mploc_table"))
              )
            )
          ),
          bslib::nav_panel(
            "Interaction",
            shiny::div(
              style = "color:#657789;font-size:12px;margin-bottom:12px;",
              "Resolve the current protein to STRING, inspect mapping diagnostics, and retrieve confidence-filtered functional partners."
            ),
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              shiny::numericInput(ns("string_score"), "Minimum STRING confidence (0-1000)", value = 400, min = 0, max = 1000, step = 50),
              shiny::numericInput(ns("string_limit"), "Top partners", value = 20, min = 1, max = 100, step = 1),
              shiny::div(style = "padding-top:29px;", shiny::actionButton(ns("run_string"), "LOAD STRING PARTNERS", icon = bsicons::bs_icon("diagram-3"), class = "btn-primary"))
            ),
            bslib::card(
              bslib::card_header("STRING mapping diagnostics"),
              shiny::uiOutput(ns("string_diagnostic_banner")),
              DT::DTOutput(ns("string_diagnostics"))
            ),
            shiny::br(),
            bslib::layout_columns(
              col_widths = c(5, 7),
              bslib::card(bslib::card_header("Interaction database references"), DT::DTOutput(ns("interaction_xrefs"))),
              bslib::card(full_screen = TRUE, bslib::card_header("STRING interaction network"), shiny::plotOutput(ns("interaction_plot"), height = "430px"))
            ),
            shiny::br(),
            bslib::card(
              bslib::card_header("STRING interaction partners"),
              shiny::uiOutput(ns("string_mapping")),
              DT::DTOutput(ns("interaction_table"))
            )
          )
        )
      )
    )
  )
}

# Preserve the fixed-height base Workbench wrapper from the previous layout fix
# while replacing the context UI with the diagnostic-aware version.
protein_workbench_ui <- function(id) {
  shiny::tagList(
    shiny::div(
      class = "pw-main-workbench-shell",
      style = "height:calc(100vh - 95px);min-height:720px;overflow:hidden;",
      .protvis_pw_base_ui(id)
    ),
    .protvis_pw_context_ui_v2(id)
  )
}

protein_workbench_server <- function(id, shared_state = NULL) {
  .protvis_pw_base_server(id, shared_state = shared_state)

  shiny::moduleServer(id, function(input, output, session) {
    rv_context <- shiny::reactiveValues(
      entry = NULL,
      partners = base::data.frame(),
      mapping = base::data.frame(),
      diagnostics = base::data.frame(),
      mapping_status = "Not queried",
      identifiers_tried = base::character(),
      plant_mploc = NULL,
      message = "Resolve a UniProt protein above to populate localization and interaction context."
    )

    current_accession_context <- shiny::reactive({
      base::trimws(base::as.character(input$accession %||% ""))
    })

    current_sequence_context <- shiny::reactive({
      manual <- base::trimws(base::as.character(input$sequence %||% ""))
      if (base::nzchar(manual)) return(.protvis_pw_clean_sequence(manual))
      if (!base::is.null(rv_context$entry)) return(.protvis_pw_sequence(rv_context$entry))
      ""
    })

    reset_interaction <- function() {
      rv_context$partners <- base::data.frame()
      rv_context$mapping <- base::data.frame()
      rv_context$diagnostics <- base::data.frame()
      rv_context$mapping_status <- "Not queried"
      rv_context$identifiers_tried <- base::character()
    }

    shiny::observeEvent(input$accession, {
      accession <- current_accession_context()
      if (!base::nzchar(accession)) {
        rv_context$entry <- NULL
        reset_interaction()
        rv_context$plant_mploc <- NULL
        return()
      }
      base::tryCatch({
        rv_context$entry <- .protvis_pw_get_uniprot(accession)
        reset_interaction()
        rv_context$plant_mploc <- NULL
        rv_context$message <- base::paste("Context loaded for", accession)
      }, error = function(e) {
        rv_context$message <- base::paste("Could not load context:", base::conditionMessage(e))
      })
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$clear, {
      rv_context$entry <- NULL
      reset_interaction()
      rv_context$plant_mploc <- NULL
      rv_context$message <- "Protein context cleared."
    })

    output$context_status <- shiny::renderUI({
      shiny::span(rv_context$message %||% "", style = "font-size:11px;color:#657789;font-weight:400;")
    })

    output$localization_table <- DT::renderDT({
      table <- .protvis_pw_localization_table(rv_context$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No curated UniProt subcellular-location annotation was found.")
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
    })

    output$localization_features <- DT::renderDT({
      table <- .protvis_pw_localization_features(rv_context$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No signal peptide, transit peptide, transmembrane or topology feature was found.")
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
    })

    output$plant_mploc_status <- shiny::renderUI({
      sequence <- base::tryCatch(current_sequence_context(), error = function(e) "")
      if (!base::nzchar(sequence)) return(shiny::div("No protein sequence available.", style = "margin-top:8px;font-size:12px;color:#657789;"))
      if (!base::is.null(rv_context$entry) && !.protvis_pw_is_plant(rv_context$entry)) {
        return(shiny::div("Current UniProt lineage is not recognized as a green plant; Plant-mPLoc is optional and may not be biologically appropriate.", style = "margin-top:8px;font-size:12px;color:#9a6b1a;"))
      }
      shiny::div(base::paste0("Sequence ready: ", base::nchar(sequence), " aa."), style = "margin-top:8px;font-size:12px;color:#2f855a;")
    })

    shiny::observeEvent(input$run_plant_mploc, {
      base::tryCatch({
        sequence <- current_sequence_context()
        if (!base::nzchar(sequence)) base::stop("No protein sequence is available for Plant-mPLoc.")
        accession <- current_accession_context()
        if (!base::nzchar(accession)) accession <- "ProteinWorkbench"
        shiny::withProgress(message = "Plant-mPLoc", value = 0.2, {
          result <- predict_plant_mploc(sequence = sequence, id = accession, timeout = 90, verbose = FALSE)
          shiny::setProgress(1, detail = "Prediction returned")
          rv_context$plant_mploc <- result
        })
        shiny::showNotification("Plant-mPLoc prediction completed.", type = "message", duration = 3)
      }, error = function(e) {
        shiny::showNotification(base::paste("Plant-mPLoc:", base::conditionMessage(e)), type = "error", duration = 7)
      })
    })

    output$plant_mploc_table <- DT::renderDT({
      result <- rv_context$plant_mploc
      if (base::is.null(result)) {
        table <- base::data.frame(Message = "Run Plant-mPLoc to add a sequence-based plant localization prediction.")
      } else {
        predictions <- base::as.character(result$prediction %||% base::character())
        table <- base::data.frame(protein = result$protein_id %||% current_accession_context(), predicted_location = predictions, source = "Plant-mPLoc", stringsAsFactors = FALSE)
      }
      DT::datatable(table, rownames = FALSE, options = base::list(dom = "t", paging = FALSE, scrollX = TRUE))
    })

    output$interaction_xrefs <- DT::renderDT({
      table <- .protvis_pw_interaction_xrefs(rv_context$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No interaction-database cross-reference was found in the UniProt record. This does not by itself mean STRING has no mapping.")
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE))
    })

    shiny::observeEvent(input$run_string, {
      base::tryCatch({
        if (base::is.null(rv_context$entry)) base::stop("Resolve a UniProt protein before querying STRING.")
        accession <- current_accession_context()
        species <- base::as.character(rv_context$entry$organism$taxonId %||% "")
        organism <- base::as.character(rv_context$entry$organism$scientificName %||% "")
        if (!base::nzchar(accession) || !base::nzchar(species)) base::stop("UniProt accession or organism taxonomy ID is unavailable.")
        score <- base::as.integer(input$string_score %||% 400)
        limit <- base::as.integer(input$string_limit %||% 20)

        shiny::withProgress(message = "STRING interaction partners", value = 0.1, {
          resolved <- .protvis_pw_string_resolve(rv_context$entry)
          rv_context$mapping <- resolved$mapping
          rv_context$identifiers_tried <- resolved$tried
          rv_context$mapping_status <- resolved$status
          shiny::setProgress(0.45, detail = "STRING mapping resolved")

          if (base::nrow(resolved$mapping)) {
            partners <- .protvis_pw_string_partners_from_mapping(resolved$mapping, species, required_score = score, limit = limit)
          } else {
            partners <- base::data.frame()
          }
          rv_context$partners <- partners

          mapped_id <- if (base::nrow(resolved$mapping)) resolved$mapping$string_id[[1L]] else ""
          mapped_name <- if (base::nrow(resolved$mapping)) resolved$mapping$preferred_name[[1L]] else ""
          rv_context$diagnostics <- base::data.frame(
            field = c("UniProt accession", "Organism", "Taxon ID", "Identifiers tried", "Mapping status", "STRING ID", "Preferred name", "Minimum confidence", "Partner limit", "Partners returned"),
            value = c(
              accession, organism, species,
              if (base::length(resolved$tried)) base::paste(resolved$tried, collapse = ", ") else "None",
              resolved$status,
              mapped_id,
              mapped_name,
              base::as.character(score),
              base::as.character(limit),
              base::as.character(base::nrow(partners))
            ),
            stringsAsFactors = FALSE
          )
          shiny::setProgress(1, detail = "Interaction result prepared")
        })

        if (!base::nrow(rv_context$mapping)) {
          shiny::showNotification("STRING mapping failed for the current UniProt/gene identifiers. See mapping diagnostics.", type = "warning", duration = 7)
        } else if (!base::nrow(rv_context$partners)) {
          shiny::showNotification(base::paste0("Mapped to ", rv_context$mapping$string_id[[1L]], ", but no partners were returned at confidence >= ", score, "."), type = "warning", duration = 7)
        } else {
          shiny::showNotification(base::paste("Loaded", base::nrow(rv_context$partners), "STRING partners."), type = "message", duration = 3)
        }
      }, error = function(e) {
        shiny::showNotification(base::paste("STRING:", base::conditionMessage(e)), type = "error", duration = 8)
      })
    })

    output$string_diagnostics <- DT::renderDT({
      table <- rv_context$diagnostics
      if (!base::nrow(table)) table <- base::data.frame(Message = "Run STRING partners to display mapping and query diagnostics.")
      DT::datatable(table, rownames = FALSE, options = base::list(dom = "t", paging = FALSE, scrollX = TRUE))
    })

    output$string_diagnostic_banner <- shiny::renderUI({
      mapping <- rv_context$mapping
      if (!base::nrow(rv_context$diagnostics)) return(NULL)
      if (!base::nrow(mapping)) {
        return(shiny::div(rv_context$mapping_status, style = "margin-bottom:8px;padding:9px 12px;border-radius:10px;background:#fff7e6;color:#8a6116;font-size:12px;"))
      }
      id <- base::as.character(mapping$string_id[[1L]] %||% "")
      link <- if (base::nzchar(id)) base::paste0("https://string-db.org/network/", utils::URLencode(id, reserved = TRUE)) else NULL
      shiny::div(
        style = "margin-bottom:8px;padding:9px 12px;border-radius:10px;background:#edf9f3;color:#286749;font-size:12px;display:flex;justify-content:space-between;gap:12px;align-items:center;",
        shiny::span(base::paste0("Mapped to STRING: ", id, " · partners returned: ", base::nrow(rv_context$partners))),
        if (!base::is.null(link)) shiny::tags$a(href = link, target = "_blank", "Open in STRING")
      )
    })

    output$string_mapping <- shiny::renderUI({
      mapping <- rv_context$mapping
      if (base::is.null(mapping) || !base::nrow(mapping)) return(shiny::div("No STRING mapping available. See the diagnostics card above.", style = "font-size:12px;color:#657789;margin-bottom:8px;"))
      row <- mapping[1, , drop = FALSE]
      shiny::div(
        style = "font-size:12px;color:#657789;margin-bottom:8px;",
        shiny::strong(row$preferred_name[[1L]]),
        base::paste0(" · ", row$string_id[[1L]], " · taxon ", row$taxon_id[[1L]]),
        if ("mapped_from" %in% base::names(row)) base::paste0(" · mapped from ", row$mapped_from[[1L]]) else "",
        if (base::nzchar(row$annotation[[1L]])) base::paste0(" · ", row$annotation[[1L]]) else ""
      )
    })

    output$interaction_table <- DT::renderDT({
      table <- rv_context$partners
      if (base::is.null(table) || !base::nrow(table)) table <- base::data.frame(Message = "No STRING interaction partners are currently available. Check mapping diagnostics and confidence threshold.")
      DT::datatable(table, rownames = FALSE, filter = if (base::nrow(table) > 1L) "top" else "none", options = base::list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE))
    })

    output$interaction_plot <- shiny::renderPlot({
      plot <- .protvis_pw_interaction_plot(rv_context$partners)
      if (base::is.null(plot)) {
        graphics::plot.new()
        message <- if (base::nrow(rv_context$diagnostics) && !base::nrow(rv_context$mapping)) "STRING mapping failed. See diagnostics." else "No STRING partners to display at the current threshold."
        graphics::text(0.5, 0.5, message)
        return(invisible(NULL))
      }
      plot
    })
  })
}
