# Protein Workbench context extensions ------------------------------------
#
# Additive extension for the single-protein Protein Workbench. The original
# Workbench UI/server and the standalone Plant-mPLoc / STRINGdb PPI toolkits
# remain unchanged. This file is intentionally collated after
# protein_workbench.R and wraps the existing module with two contextual views:
# Localization and Interaction.

.protvis_pw_base_ui <- protein_workbench_ui
.protvis_pw_base_server <- protein_workbench_server

.protvis_pw_evidence_text <- function(x) {
  if (base::is.null(x) || !base::length(x)) return("")
  values <- base::unique(base::unlist(base::lapply(x, function(item) {
    c(
      item$evidenceCode %||% base::character(),
      item$source %||% base::character(),
      item$id %||% base::character()
    )
  }), use.names = FALSE))
  values <- values[!base::is.na(values) & base::nzchar(values)]
  base::paste(values, collapse = "; ")
}

.protvis_pw_localization_table <- function(entry) {
  if (base::is.null(entry)) return(base::data.frame())
  comments <- entry$comments %||% base::list()
  comments <- comments[base::vapply(
    comments,
    function(x) identical(base::toupper(base::as.character(x$commentType %||% "")), "SUBCELLULAR LOCATION"),
    logical(1)
  )]
  if (!base::length(comments)) return(base::data.frame())

  rows <- base::list()
  row_id <- 0L
  for (comment in comments) {
    locations <- comment$subcellularLocations %||% base::list()
    if (!base::length(locations)) {
      values <- .protvis_pw_leaf_values(comment)
      values <- values[!base::grepl("SUBCELLULAR LOCATION", values, fixed = TRUE)]
      if (base::length(values)) {
        row_id <- row_id + 1L
        rows[[row_id]] <- base::data.frame(
          location = base::paste(base::unique(values), collapse = "; "),
          topology = "",
          orientation = "",
          evidence = "",
          source = "UniProtKB",
          stringsAsFactors = FALSE
        )
      }
      next
    }

    for (item in locations) {
      location <- item$location %||% base::list()
      topology <- item$topology %||% base::list()
      orientation <- item$orientation %||% base::list()
      evidence <- c(
        location$evidences %||% base::list(),
        topology$evidences %||% base::list(),
        orientation$evidences %||% base::list(),
        item$evidences %||% base::list()
      )
      row_id <- row_id + 1L
      rows[[row_id]] <- base::data.frame(
        location = base::as.character(location$value %||% item$location$value %||% ""),
        topology = base::as.character(topology$value %||% ""),
        orientation = base::as.character(orientation$value %||% ""),
        evidence = .protvis_pw_evidence_text(evidence),
        source = "UniProtKB",
        stringsAsFactors = FALSE
      )
    }
  }
  if (!base::length(rows)) return(base::data.frame())
  base::unique(base::do.call(base::rbind, rows))
}

.protvis_pw_localization_features <- function(entry) {
  table <- .protvis_pw_features_table(entry)
  if (!base::nrow(table)) return(table)
  keep <- base::grepl(
    "signal peptide|transit peptide|transmembrane|topological domain|intramembrane|lipidation|propeptide",
    table$type,
    ignore.case = TRUE
  )
  table[keep, , drop = FALSE]
}

.protvis_pw_is_plant <- function(entry) {
  if (base::is.null(entry)) return(FALSE)
  lineage <- base::as.character(entry$organism$lineage %||% base::character())
  organism <- base::as.character(entry$organism$scientificName %||% "")
  base::any(base::grepl(
    "Viridiplantae|Streptophyta|Embryophyta|Tracheophyta|Magnoliopsida|Liliopsida",
    c(lineage, organism),
    ignore.case = TRUE
  ))
}

.protvis_pw_interaction_xrefs <- function(entry) {
  table <- .protvis_pw_xrefs_table(entry)
  if (!base::nrow(table)) return(table)
  keep <- base::toupper(table$database) %in% base::toupper(c(
    "STRING", "IntAct", "BioGRID", "DIP", "MINT", "ComplexPortal", "CORUM"
  ))
  table[keep, , drop = FALSE]
}

.protvis_pw_string_records <- function(raw) {
  if (base::is.null(raw) || !base::length(raw)) return(base::list())
  if (base::is.data.frame(raw)) {
    return(base::lapply(base::seq_len(base::nrow(raw)), function(i) base::as.list(raw[i, , drop = FALSE])))
  }
  if (base::is.list(raw) && !base::is.null(base::names(raw)) &&
      base::any(base::names(raw) %in% c("stringId", "stringId_A", "preferredName_A"))) {
    return(base::list(raw))
  }
  raw
}

.protvis_pw_string_map <- function(identifier, species) {
  identifier <- base::trimws(base::as.character(identifier %||% ""))
  species <- base::trimws(base::as.character(species %||% ""))
  if (!base::nzchar(identifier) || !base::nzchar(species)) return(base::data.frame())

  raw <- .protvis_pw_http_json(
    "https://string-db.org/api/json/get_string_ids",
    query = base::list(
      identifiers = identifier,
      species = species,
      limit = 1,
      echo_query = 1,
      caller_identity = "ProtVis"
    ),
    timeout = 60,
    not_found = NULL
  )
  records <- .protvis_pw_string_records(raw)
  if (!base::length(records)) return(base::data.frame())
  base::do.call(base::rbind, base::lapply(records, function(x) {
    base::data.frame(
      query = base::as.character(x$queryItem %||% identifier),
      string_id = base::as.character(x$stringId %||% ""),
      preferred_name = base::as.character(x$preferredName %||% ""),
      taxon_id = base::as.character(x$ncbiTaxonId %||% species),
      taxon_name = base::as.character(x$taxonName %||% ""),
      annotation = base::as.character(x$annotation %||% ""),
      stringsAsFactors = FALSE
    )
  }))
}

.protvis_pw_string_partners <- function(identifier, species, required_score = 400L, limit = 20L) {
  identifier <- base::trimws(base::as.character(identifier %||% ""))
  species <- base::trimws(base::as.character(species %||% ""))
  if (!base::nzchar(identifier) || !base::nzchar(species)) return(base::data.frame())
  required_score <- base::max(0L, base::min(1000L, base::as.integer(required_score)))
  limit <- base::max(1L, base::min(100L, base::as.integer(limit)))

  mapping <- .protvis_pw_string_map(identifier, species)
  if (!base::nrow(mapping) || !base::nzchar(mapping$string_id[[1L]])) {
    return(base::data.frame())
  }
  query_string <- mapping$string_id[[1L]]
  query_name <- mapping$preferred_name[[1L]]

  raw <- .protvis_pw_http_json(
    "https://string-db.org/api/json/interaction_partners",
    query = base::list(
      identifiers = query_string,
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

  rows <- base::lapply(records, function(x) {
    a_is_query <- identical(base::as.character(x$stringId_A %||% ""), query_string)
    partner_id <- if (a_is_query) x$stringId_B else x$stringId_A
    partner_name <- if (a_is_query) x$preferredName_B else x$preferredName_A
    partner_annotation <- if (a_is_query) x$annotation_B else x$annotation_A
    base::data.frame(
      query = query_name,
      query_string_id = query_string,
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
  attr(table, "mapping") <- mapping
  table
}

.protvis_pw_interaction_plot <- function(partners) {
  if (base::is.null(partners) || !base::nrow(partners)) return(NULL)
  query_name <- base::as.character(partners$query[[1L]] %||% "Query")
  edges <- base::data.frame(
    from = query_name,
    to = partners$partner,
    score = partners$score,
    stringsAsFactors = FALSE
  )
  edges <- edges[base::nzchar(edges$to), , drop = FALSE]
  if (!base::nrow(edges)) return(NULL)

  graph <- igraph::graph_from_data_frame(edges[, c("from", "to"), drop = FALSE], directed = FALSE)
  layout <- igraph::layout_with_fr(graph)
  nodes <- base::data.frame(
    name = igraph::V(graph)$name,
    x = layout[, 1],
    y = layout[, 2],
    stringsAsFactors = FALSE
  )
  nodes$is_query <- nodes$name == query_name
  edge_index <- igraph::as_edgelist(graph, names = TRUE)
  edge_plot <- base::data.frame(
    from = edge_index[, 1],
    to = edge_index[, 2],
    score = edges$score,
    stringsAsFactors = FALSE
  )
  edge_plot$x <- nodes$x[base::match(edge_plot$from, nodes$name)]
  edge_plot$y <- nodes$y[base::match(edge_plot$from, nodes$name)]
  edge_plot$xend <- nodes$x[base::match(edge_plot$to, nodes$name)]
  edge_plot$yend <- nodes$y[base::match(edge_plot$to, nodes$name)]

  ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edge_plot,
      ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = score),
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = nodes,
      ggplot2::aes(x = x, y = y, size = is_query),
      alpha = 0.9
    ) +
    ggplot2::geom_text(
      data = nodes,
      ggplot2::aes(x = x, y = y, label = name),
      nudge_y = 0.08,
      size = 3.2,
      check_overlap = TRUE
    ) +
    ggplot2::scale_size_manual(values = c(`FALSE` = 3.8, `TRUE` = 7.0), guide = "none") +
    ggplot2::scale_linewidth_continuous(range = c(0.3, 2.2), guide = "none") +
    ggplot2::coord_equal() +
    ggplot2::theme_void() +
    ggplot2::labs(subtitle = "STRING functional association partners")
}

.protvis_pw_context_ui <- function(id) {
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
              bslib::card(
                bslib::card_header("Curated localization"),
                DT::DTOutput(ns("localization_table"))
              ),
              bslib::card(
                bslib::card_header("Localization-related sequence features"),
                DT::DTOutput(ns("localization_features"))
              )
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
                  shiny::actionButton(
                    ns("run_plant_mploc"),
                    "RUN PLANT-mPLoc",
                    icon = bsicons::bs_icon("geo-alt"),
                    class = "btn-outline-primary"
                  ),
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
              "Shows interaction database cross-references and retrieves confidence-filtered functional partners from the STRING REST API for the current protein."
            ),
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              shiny::numericInput(ns("string_score"), "Minimum STRING score", value = 400, min = 0, max = 1000, step = 50),
              shiny::numericInput(ns("string_limit"), "Top partners", value = 20, min = 1, max = 100, step = 1),
              shiny::div(
                style = "padding-top:29px;",
                shiny::actionButton(
                  ns("run_string"),
                  "LOAD STRING PARTNERS",
                  icon = bsicons::bs_icon("diagram-3"),
                  class = "btn-primary"
                )
              )
            ),
            bslib::layout_columns(
              col_widths = c(5, 7),
              bslib::card(
                bslib::card_header("Interaction database references"),
                DT::DTOutput(ns("interaction_xrefs"))
              ),
              bslib::card(
                full_screen = TRUE,
                bslib::card_header("STRING interaction network"),
                shiny::plotOutput(ns("interaction_plot"), height = "430px")
              )
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

#' Protein Workbench UI
#'
#' Additive wrapper around the original Protein Workbench UI. Existing tabs are
#' retained unchanged and a contextual Localization / Interaction card is
#' appended below them.
#'
#' @param id Shiny module id.
#' @return Shiny UI.
#' @export
protein_workbench_ui <- function(id) {
  shiny::tagList(
    .protvis_pw_base_ui(id),
    .protvis_pw_context_ui(id)
  )
}

#' Protein Workbench server
#'
#' Additive wrapper that preserves the original Workbench server and registers
#' localization and interaction outputs in the same module namespace.
#'
#' @param id Shiny module id.
#' @return Shiny module server.
#' @export
protein_workbench_server <- function(id) {
  .protvis_pw_base_server(id)

  shiny::moduleServer(id, function(input, output, session) {
    rv_context <- shiny::reactiveValues(
      entry = NULL,
      partners = base::data.frame(),
      mapping = base::data.frame(),
      plant_mploc = NULL,
      message = "Resolve a UniProt protein above to populate localization and interaction context."
    )

    current_accession_context <- shiny::reactive({
      base::trimws(base::as.character(input$accession %||% ""))
    })

    current_sequence_context <- shiny::reactive({
      manual <- base::trimws(base::as.character(input$sequence %||% ""))
      if (base::nzchar(manual)) {
        return(.protvis_pw_clean_sequence(manual))
      }
      if (!base::is.null(rv_context$entry)) {
        return(.protvis_pw_sequence(rv_context$entry))
      }
      ""
    })

    shiny::observeEvent(input$accession, {
      accession <- current_accession_context()
      if (!base::nzchar(accession)) {
        rv_context$entry <- NULL
        rv_context$partners <- base::data.frame()
        rv_context$mapping <- base::data.frame()
        rv_context$plant_mploc <- NULL
        return()
      }
      base::tryCatch({
        rv_context$entry <- .protvis_pw_get_uniprot(accession)
        rv_context$partners <- base::data.frame()
        rv_context$mapping <- base::data.frame()
        rv_context$plant_mploc <- NULL
        rv_context$message <- base::paste("Context loaded for", accession)
      }, error = function(e) {
        rv_context$message <- base::paste("Could not load context:", base::conditionMessage(e))
      })
    }, ignoreInit = FALSE)

    shiny::observeEvent(input$clear, {
      rv_context$entry <- NULL
      rv_context$partners <- base::data.frame()
      rv_context$mapping <- base::data.frame()
      rv_context$plant_mploc <- NULL
      rv_context$message <- "Protein context cleared."
    })

    output$context_status <- shiny::renderUI({
      shiny::span(
        rv_context$message %||% "",
        style = "font-size:11px;color:#657789;font-weight:400;"
      )
    })

    output$localization_table <- DT::renderDT({
      table <- .protvis_pw_localization_table(rv_context$entry)
      if (!base::nrow(table)) {
        table <- base::data.frame(Message = "No curated UniProt subcellular-location annotation was found.")
      }
      DT::datatable(
        table,
        rownames = FALSE,
        options = base::list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
      )
    })

    output$localization_features <- DT::renderDT({
      table <- .protvis_pw_localization_features(rv_context$entry)
      if (!base::nrow(table)) {
        table <- base::data.frame(Message = "No signal peptide, transit peptide, transmembrane or topology feature was found.")
      }
      DT::datatable(
        table,
        rownames = FALSE,
        options = base::list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
      )
    })

    output$plant_mploc_status <- shiny::renderUI({
      sequence <- base::tryCatch(current_sequence_context(), error = function(e) "")
      if (!base::nzchar(sequence)) {
        return(shiny::div("No protein sequence available.", style = "margin-top:8px;font-size:12px;color:#657789;"))
      }
      if (!base::is.null(rv_context$entry) && !.protvis_pw_is_plant(rv_context$entry)) {
        return(shiny::div(
          "Current UniProt lineage is not recognized as a green plant; Plant-mPLoc is therefore optional and may not be biologically appropriate.",
          style = "margin-top:8px;font-size:12px;color:#9a6b1a;"
        ))
      }
      shiny::div(
        base::paste0("Sequence ready: ", base::nchar(sequence), " aa."),
        style = "margin-top:8px;font-size:12px;color:#2f855a;"
      )
    })

    shiny::observeEvent(input$run_plant_mploc, {
      base::tryCatch({
        sequence <- current_sequence_context()
        if (!base::nzchar(sequence)) base::stop("No protein sequence is available for Plant-mPLoc.")
        accession <- current_accession_context()
        if (!base::nzchar(accession)) accession <- "ProteinWorkbench"
        shiny::withProgress(message = "Plant-mPLoc", value = 0.2, {
          result <- predict_plant_mploc(
            sequence = sequence,
            id = accession,
            timeout = 90,
            verbose = FALSE
          )
          shiny::setProgress(1, detail = "Prediction returned")
          rv_context$plant_mploc <- result
        })
        shiny::showNotification("Plant-mPLoc prediction completed.", type = "message", duration = 3)
      }, error = function(e) {
        shiny::showNotification(
          base::paste("Plant-mPLoc:", base::conditionMessage(e)),
          type = "error",
          duration = 7
        )
      })
    })

    output$plant_mploc_table <- DT::renderDT({
      result <- rv_context$plant_mploc
      if (base::is.null(result)) {
        table <- base::data.frame(Message = "Run Plant-mPLoc to add a sequence-based plant localization prediction.")
      } else {
        predictions <- base::as.character(result$prediction %||% base::character())
        table <- base::data.frame(
          protein = result$protein_id %||% current_accession_context(),
          predicted_location = predictions,
          source = "Plant-mPLoc",
          stringsAsFactors = FALSE
        )
      }
      DT::datatable(table, rownames = FALSE, options = base::list(dom = "t", paging = FALSE, scrollX = TRUE))
    })

    output$interaction_xrefs <- DT::renderDT({
      table <- .protvis_pw_interaction_xrefs(rv_context$entry)
      if (!base::nrow(table)) {
        table <- base::data.frame(Message = "No interaction-database cross-reference was found in the UniProt record.")
      }
      DT::datatable(
        table,
        rownames = FALSE,
        options = base::list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE)
      )
    })

    shiny::observeEvent(input$run_string, {
      base::tryCatch({
        if (base::is.null(rv_context$entry)) {
          base::stop("Resolve a UniProt protein before querying STRING.")
        }
        accession <- current_accession_context()
        species <- base::as.character(rv_context$entry$organism$taxonId %||% "")
        if (!base::nzchar(accession) || !base::nzchar(species)) {
          base::stop("UniProt accession or organism taxonomy ID is unavailable.")
        }
        shiny::withProgress(message = "STRING interaction partners", value = 0.15, {
          partners <- .protvis_pw_string_partners(
            accession,
            species,
            required_score = input$string_score %||% 400,
            limit = input$string_limit %||% 20
          )
          shiny::setProgress(0.9, detail = "Preparing interaction network")
          rv_context$partners <- partners
          rv_context$mapping <- attr(partners, "mapping") %||% base::data.frame()
          shiny::setProgress(1)
        })
        if (!base::nrow(rv_context$partners)) {
          shiny::showNotification("STRING returned no partners at the selected threshold.", type = "warning", duration = 5)
        } else {
          shiny::showNotification(
            base::paste("Loaded", base::nrow(rv_context$partners), "STRING partners."),
            type = "message", duration = 3
          )
        }
      }, error = function(e) {
        shiny::showNotification(
          base::paste("STRING:", base::conditionMessage(e)),
          type = "error",
          duration = 7
        )
      })
    })

    output$string_mapping <- shiny::renderUI({
      mapping <- rv_context$mapping
      if (base::is.null(mapping) || !base::nrow(mapping)) {
        return(shiny::div(
          "Run STRING partners to resolve the current UniProt entry to a STRING protein.",
          style = "font-size:12px;color:#657789;margin-bottom:8px;"
        ))
      }
      row <- mapping[1, , drop = FALSE]
      shiny::div(
        style = "font-size:12px;color:#657789;margin-bottom:8px;",
        shiny::strong(row$preferred_name[[1L]]),
        base::paste0(" · ", row$string_id[[1L]], " · taxon ", row$taxon_id[[1L]]),
        if (base::nzchar(row$annotation[[1L]])) base::paste0(" · ", row$annotation[[1L]]) else ""
      )
    })

    output$interaction_table <- DT::renderDT({
      table <- rv_context$partners
      if (base::is.null(table) || !base::nrow(table)) {
        table <- base::data.frame(Message = "No STRING interaction partners loaded yet.")
      }
      DT::datatable(
        table,
        rownames = FALSE,
        filter = if (base::nrow(table) > 1L) "top" else "none",
        options = base::list(pageLength = 20, scrollX = TRUE, autoWidth = TRUE)
      )
    })

    output$interaction_plot <- shiny::renderPlot({
      plot <- .protvis_pw_interaction_plot(rv_context$partners)
      if (base::is.null(plot)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Load STRING partners to display the interaction network.")
        return(invisible(NULL))
      }
      plot
    })
  })
}
