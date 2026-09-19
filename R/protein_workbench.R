# Protein Workbench -------------------------------------------------------
# A standalone single-protein workspace. Existing ProtVis toolkit modules are
# intentionally left unchanged; this module aggregates public annotations and
# local sequence diagnostics in one place.

.protvis_pw_or <- function(x, y) {
  if (base::is.null(x) || base::length(x) == 0L ||
      (base::length(x) == 1L && base::is.na(x))) y else x
}

.protvis_pw_leaf_values <- function(x) {
  if (base::is.null(x)) return(base::character())
  if (base::is.atomic(x)) {
    out <- base::as.character(x)
    return(out[!base::is.na(out) & base::nzchar(out)])
  }
  if (base::is.data.frame(x)) {
    return(base::unique(base::unlist(base::lapply(x, .protvis_pw_leaf_values), use.names = FALSE)))
  }
  if (base::is.list(x)) {
    return(base::unique(base::unlist(base::lapply(x, .protvis_pw_leaf_values), use.names = FALSE)))
  }
  base::character()
}

.protvis_pw_http_json <- function(url, query = NULL, timeout = 60, not_found = NULL) {
  response <- httr::GET(
    url,
    query = query,
    httr::accept_json(),
    httr::user_agent("ProtVis Protein Workbench"),
    httr::timeout(timeout)
  )
  status <- httr::status_code(response)
  if (status == 404L) return(not_found)
  if (status < 200L || status >= 300L) {
    body <- base::tryCatch(
      httr::content(response, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    base::stop(
      base::sprintf("Remote protein service request failed (%s): %s", status, base::substr(body, 1, 700)),
      call. = FALSE
    )
  }
  text <- httr::content(response, as = "text", encoding = "UTF-8")
  if (!base::nzchar(base::trimws(text))) return(base::list())
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

.protvis_pw_http_text <- function(url, timeout = 60, not_found = NULL) {
  response <- httr::GET(
    url,
    httr::user_agent("ProtVis Protein Workbench"),
    httr::timeout(timeout)
  )
  status <- httr::status_code(response)
  if (status == 404L) return(not_found)
  if (status < 200L || status >= 300L) return(not_found)
  httr::content(response, as = "text", encoding = "UTF-8")
}

.protvis_pw_protein_name <- function(entry) {
  description <- entry$proteinDescription
  candidates <- base::list(
    description$recommendedName$fullName$value,
    description$submissionNames[[1]]$fullName$value,
    description$alternativeNames[[1]]$fullName$value
  )
  for (candidate in candidates) {
    if (!base::is.null(candidate) && base::length(candidate) && base::nzchar(base::as.character(candidate[[1]]))) {
      return(base::as.character(candidate[[1]]))
    }
  }
  base::as.character(entry$uniProtkbId %||% entry$primaryAccession %||% NA_character_)
}

# Avoid depending on the SWISS-MODEL module's operator at load time.
`%||%` <- function(x, y) .protvis_pw_or(x, y)

.protvis_pw_gene_names <- function(entry) {
  genes <- entry$genes
  if (base::is.null(genes) || !base::length(genes)) return(NA_character_)
  values <- base::unique(base::unlist(base::lapply(genes, function(gene) {
    c(
      gene$geneName$value %||% base::character(),
      base::unlist(base::lapply(gene$synonyms %||% base::list(), function(x) x$value %||% base::character()))
    )
  }), use.names = FALSE))
  values <- values[!base::is.na(values) & base::nzchar(values)]
  if (!base::length(values)) NA_character_ else base::paste(values, collapse = ", ")
}

.protvis_pw_search_uniprot <- function(query, organism_id = NULL, size = 25L) {
  query <- base::trimws(query %||% "")
  if (!base::nzchar(query)) return(base::data.frame())
  q <- query
  if (!base::is.null(organism_id) && base::nzchar(base::trimws(organism_id))) {
    q <- base::sprintf("(%s) AND (organism_id:%s)", query, base::trimws(organism_id))
  }
  raw <- .protvis_pw_http_json(
    "https://rest.uniprot.org/uniprotkb/search",
    query = base::list(query = q, format = "json", size = base::as.integer(size)),
    timeout = 60
  )
  results <- raw$results %||% base::list()
  if (!base::length(results)) return(base::data.frame())
  base::do.call(base::rbind, base::lapply(results, function(entry) {
    base::data.frame(
      accession = base::as.character(entry$primaryAccession %||% NA_character_),
      entry_name = base::as.character(entry$uniProtkbId %||% NA_character_),
      protein_name = .protvis_pw_protein_name(entry),
      genes = .protvis_pw_gene_names(entry),
      organism = base::as.character(entry$organism$scientificName %||% NA_character_),
      taxon_id = base::as.character(entry$organism$taxonId %||% NA_character_),
      length = base::as.integer(entry$sequence$length %||% NA_integer_),
      entry_type = base::as.character(entry$entryType %||% NA_character_),
      stringsAsFactors = FALSE
    )
  }))
}

.protvis_pw_get_uniprot <- function(accession) {
  accession <- base::trimws(accession %||% "")
  if (!base::nzchar(accession)) return(NULL)
  .protvis_pw_http_json(
    base::paste0("https://rest.uniprot.org/uniprotkb/", utils::URLencode(accession, reserved = TRUE), ".json"),
    timeout = 60,
    not_found = NULL
  )
}

.protvis_pw_comments_table <- function(entry) {
  comments <- entry$comments %||% base::list()
  if (!base::length(comments)) return(base::data.frame())
  rows <- base::lapply(comments, function(comment) {
    type <- base::as.character(comment$commentType %||% "Annotation")
    copy <- comment
    copy$commentType <- NULL
    values <- .protvis_pw_leaf_values(copy)
    values <- base::unique(values[values != type])
    base::data.frame(
      category = type,
      annotation = if (base::length(values)) base::paste(values, collapse = "; ") else "",
      stringsAsFactors = FALSE
    )
  })
  base::do.call(base::rbind, rows)
}

.protvis_pw_features_table <- function(entry) {
  features <- entry$features %||% base::list()
  if (!base::length(features)) return(base::data.frame())
  rows <- base::lapply(features, function(feature) {
    location <- feature$location %||% base::list()
    start <- location$start$value %||% location$position$value %||% NA_integer_
    end <- location$end$value %||% location$position$value %||% NA_integer_
    evidences <- feature$evidences %||% base::list()
    evidence_codes <- base::unique(base::unlist(base::lapply(evidences, function(x) {
      x$evidenceCode %||% x$source %||% base::character()
    }), use.names = FALSE))
    base::data.frame(
      type = base::as.character(feature$type %||% NA_character_),
      description = base::as.character(feature$description %||% ""),
      start = base::as.integer(start),
      end = base::as.integer(end),
      feature_id = base::as.character(feature$featureId %||% ""),
      evidence = if (base::length(evidence_codes)) base::paste(evidence_codes, collapse = "; ") else "",
      stringsAsFactors = FALSE
    )
  })
  base::do.call(base::rbind, rows)
}

.protvis_pw_ptm_table <- function(entry) {
  table <- .protvis_pw_features_table(entry)
  if (!base::nrow(table)) return(table)
  keep <- base::grepl(
    "modified|glycosyl|lipid|cross-link|disulfide|initiator methionine|chain|peptide",
    table$type,
    ignore.case = TRUE
  ) | base::grepl(
    "phospho|acetyl|methyl|ubiquitin|glyco|hydroxy|lipid|sumo|croton|succin|malon|glutaryl|butyryl",
    table$description,
    ignore.case = TRUE
  )
  table[keep, , drop = FALSE]
}

.protvis_pw_xrefs_table <- function(entry) {
  refs <- entry$uniProtKBCrossReferences %||% base::list()
  if (!base::length(refs)) return(base::data.frame())
  rows <- base::lapply(refs, function(ref) {
    props <- ref$properties %||% base::list()
    property_text <- base::unlist(base::lapply(props, function(prop) {
      key <- base::as.character(prop$key %||% "")
      value <- base::as.character(prop$value %||% "")
      if (base::nzchar(key)) base::paste0(key, "=", value) else value
    }), use.names = FALSE)
    base::data.frame(
      database = base::as.character(ref$database %||% NA_character_),
      id = base::as.character(ref$id %||% NA_character_),
      properties = if (base::length(property_text)) base::paste(property_text, collapse = "; ") else "",
      stringsAsFactors = FALSE
    )
  })
  base::do.call(base::rbind, rows)
}

.protvis_pw_interpro <- function(accession) {
  if (base::is.null(accession) || !base::nzchar(accession)) return(base::data.frame())
  raw <- base::tryCatch(
    .protvis_pw_http_json(
      base::paste0(
        "https://www.ebi.ac.uk/interpro/api/entry/all/protein/uniprot/",
        utils::URLencode(accession, reserved = TRUE),
        "/"
      ),
      query = base::list(page_size = 200),
      timeout = 60,
      not_found = NULL
    ),
    error = function(e) NULL
  )
  results <- raw$results %||% base::list()
  if (!base::length(results)) return(base::data.frame())
  rows <- base::list()
  row_id <- 0L
  for (result in results) {
    metadata <- result$metadata %||% base::list()
    accession_id <- base::as.character(metadata$accession %||% "")
    name <- base::as.character(metadata$name %||% "")
    type <- base::as.character(metadata$type %||% "")
    source_database <- base::as.character(metadata$source_database %||% metadata$sourceDatabase %||% "InterPro")
    fragments <- base::list()
    proteins <- result$proteins %||% base::list()
    if (base::length(proteins)) {
      for (protein in proteins) {
        locations <- protein$entry_protein_locations %||% protein$entryProteinLocations %||% base::list()
        for (location in locations) {
          frags <- location$fragments %||% base::list()
          if (base::length(frags)) fragments <- c(fragments, frags)
        }
      }
    }
    if (!base::length(fragments)) fragments <- base::list(base::list(start = NA_integer_, end = NA_integer_))
    for (fragment in fragments) {
      row_id <- row_id + 1L
      rows[[row_id]] <- base::data.frame(
        accession = accession_id,
        name = name,
        type = type,
        database = source_database,
        start = base::as.integer(fragment$start %||% NA_integer_),
        end = base::as.integer(fragment$end %||% NA_integer_),
        stringsAsFactors = FALSE
      )
    }
  }
  base::do.call(base::rbind, rows)
}

.protvis_pw_alphafold <- function(accession) {
  if (base::is.null(accession) || !base::nzchar(accession)) return(NULL)
  base::tryCatch(
    .protvis_pw_http_json(
      base::paste0(
        "https://alphafold.ebi.ac.uk/api/prediction/",
        utils::URLencode(accession, reserved = TRUE)
      ),
      timeout = 60,
      not_found = NULL
    ),
    error = function(e) NULL
  )
}

.protvis_pw_alpha_record <- function(raw) {
  if (base::is.null(raw)) return(NULL)
  if (base::is.list(raw) && base::length(raw) && base::is.null(base::names(raw))) return(raw[[1]])
  if (base::is.list(raw) && base::length(raw) && !base::is.null(raw[[1]]) && base::is.list(raw[[1]])) return(raw[[1]])
  raw
}

.protvis_pw_sequence <- function(entry) {
  base::as.character(entry$sequence$value %||% "")
}

.protvis_pw_clean_sequence <- function(sequence) {
  sequence <- base::toupper(base::gsub("[^A-Za-z*]", "", base::paste(sequence, collapse = "")))
  sequence <- base::gsub("\\*", "", sequence)
  if (!base::nzchar(sequence)) return("")
  if (base::grepl("[^ACDEFGHIKLMNPQRSTVWY]", sequence)) {
    base::stop("Protein sequence contains non-standard amino-acid symbols.", call. = FALSE)
  }
  sequence
}

.protvis_pw_sequence_stats <- function(sequence) {
  sequence <- .protvis_pw_clean_sequence(sequence)
  if (!base::nzchar(sequence)) return(base::data.frame())
  aa <- base::strsplit(sequence, "", fixed = TRUE)[[1]]
  counts <- base::table(base::factor(aa, levels = base::strsplit("ACDEFGHIKLMNPQRSTVWY", "", fixed = TRUE)[[1]]))

  residue_mass <- c(
    A = 71.0788, C = 103.1388, D = 115.0886, E = 129.1155, F = 147.1766,
    G = 57.0519, H = 137.1411, I = 113.1594, K = 128.1741, L = 113.1594,
    M = 131.1926, N = 114.1038, P = 97.1167, Q = 128.1307, R = 156.1875,
    S = 87.0782, T = 101.1051, V = 99.1326, W = 186.2132, Y = 163.1760
  )
  mw <- base::sum(residue_mass[aa]) + 18.01528

  hydropathy <- c(
    A = 1.8, C = 2.5, D = -3.5, E = -3.5, F = 2.8, G = -0.4, H = -3.2,
    I = 4.5, K = -3.9, L = 3.8, M = 1.9, N = -3.5, P = -1.6, Q = -3.5,
    R = -4.5, S = -0.8, T = -0.7, V = 4.2, W = -0.9, Y = -1.3
  )
  gravy <- base::mean(hydropathy[aa])
  aromaticity <- base::sum(aa %in% c("F", "W", "Y")) / base::length(aa)

  charge_at <- function(ph) {
    n_term <- 1 / (1 + 10^(ph - 9.69))
    c_term <- 1 / (1 + 10^(2.34 - ph))
    positive <- n_term +
      counts[["K"]] / (1 + 10^(ph - 10.5)) +
      counts[["R"]] / (1 + 10^(ph - 12.5)) +
      counts[["H"]] / (1 + 10^(ph - 6.0))
    negative <- c_term +
      counts[["D"]] / (1 + 10^(3.86 - ph)) +
      counts[["E"]] / (1 + 10^(4.25 - ph)) +
      counts[["C"]] / (1 + 10^(8.33 - ph)) +
      counts[["Y"]] / (1 + 10^(10.07 - ph))
    positive - negative
  }
  low <- 0
  high <- 14
  for (i in base::seq_len(60)) {
    mid <- (low + high) / 2
    if (charge_at(mid) > 0) low <- mid else high <- mid
  }
  p_i <- (low + high) / 2

  base::data.frame(
    metric = c("Length", "Molecular weight", "Estimated pI", "GRAVY", "Aromaticity"),
    value = c(
      base::sprintf("%d aa", base::nchar(sequence)),
      base::sprintf("%.2f Da", mw),
      base::sprintf("%.2f", p_i),
      base::sprintf("%.3f", gravy),
      base::sprintf("%.3f", aromaticity)
    ),
    stringsAsFactors = FALSE
  )
}

.protvis_pw_composition <- function(sequence) {
  sequence <- .protvis_pw_clean_sequence(sequence)
  if (!base::nzchar(sequence)) return(base::data.frame())
  aa <- base::strsplit(sequence, "", fixed = TRUE)[[1]]
  count <- base::sort(base::table(aa), decreasing = TRUE)
  base::data.frame(
    Residue = base::names(count),
    Count = base::as.numeric(count),
    Fraction = base::as.numeric(count) / base::length(aa),
    stringsAsFactors = FALSE
  )
}

.protvis_pw_hydropathy <- function(sequence, window = 9L) {
  sequence <- .protvis_pw_clean_sequence(sequence)
  if (!base::nzchar(sequence)) return(base::data.frame())
  aa <- base::strsplit(sequence, "", fixed = TRUE)[[1]]
  kd <- c(
    A = 1.8, C = 2.5, D = -3.5, E = -3.5, F = 2.8, G = -0.4, H = -3.2,
    I = 4.5, K = -3.9, L = 3.8, M = 1.9, N = -3.5, P = -1.6, Q = -3.5,
    R = -4.5, S = -0.8, T = -0.7, V = 4.2, W = -0.9, Y = -1.3
  )
  values <- base::as.numeric(kd[aa])
  window <- base::max(1L, base::min(base::as.integer(window), base::length(values)))
  filtered <- base::as.numeric(stats::filter(values, base::rep(1 / window, window), sides = 2))
  base::data.frame(
    position = base::seq_along(values),
    hydropathy = filtered,
    stringsAsFactors = FALSE
  )
}

.protvis_pw_fasta <- function(sequence, accession = "protein") {
  sequence <- .protvis_pw_clean_sequence(sequence)
  chunks <- base::substring(sequence, base::seq(1, base::nchar(sequence), 60), base::seq(60, base::nchar(sequence) + 59, 60))
  base::paste(c(base::paste0(">", accession), chunks), collapse = "\n")
}

.protvis_pw_summary_table <- function(entry) {
  if (base::is.null(entry)) return(base::data.frame())
  sequence <- entry$sequence %||% base::list()
  base::data.frame(
    field = c(
      "UniProt accession", "Entry name", "Protein name", "Gene", "Organism",
      "Taxon ID", "Entry type", "Protein existence", "Annotation score",
      "Sequence length", "UniProt molecular weight"
    ),
    value = c(
      entry$primaryAccession %||% NA_character_,
      entry$uniProtkbId %||% NA_character_,
      .protvis_pw_protein_name(entry),
      .protvis_pw_gene_names(entry),
      entry$organism$scientificName %||% NA_character_,
      entry$organism$taxonId %||% NA_character_,
      entry$entryType %||% NA_character_,
      entry$proteinExistence %||% NA_character_,
      entry$annotationScore %||% NA_character_,
      sequence$length %||% NA_character_,
      sequence$molWeight %||% NA_character_
    ),
    stringsAsFactors = FALSE
  )
}

.protvis_pw_domain_fallback <- function(entry) {
  refs <- .protvis_pw_xrefs_table(entry)
  if (!base::nrow(refs)) return(base::data.frame())
  refs[refs$database %in% c("InterPro", "Pfam", "PROSITE", "SMART", "SUPFAM", "Gene3D"), , drop = FALSE]
}

.protvis_pw_structure_table <- function(entry, alphafold = NULL) {
  refs <- .protvis_pw_xrefs_table(entry)
  structural <- if (base::nrow(refs)) {
    refs[refs$database %in% c("PDB", "AlphaFoldDB", "SMR"), , drop = FALSE]
  } else {
    base::data.frame()
  }
  af <- .protvis_pw_alpha_record(alphafold)
  if (!base::is.null(af)) {
    af_row <- base::data.frame(
      database = "AlphaFold DB API",
      id = base::as.character(af$modelEntityId %||% af$entryId %||% entry$primaryAccession %||% ""),
      properties = base::paste(
        base::c(
          if (!base::is.null(af$globalMetricValue)) base::paste0("globalMetricValue=", af$globalMetricValue),
          if (!base::is.null(af$sequenceStart %||% af$uniprotStart)) base::paste0("start=", af$sequenceStart %||% af$uniprotStart),
          if (!base::is.null(af$sequenceEnd %||% af$uniprotEnd)) base::paste0("end=", af$sequenceEnd %||% af$uniprotEnd)
        ),
        collapse = "; "
      ),
      stringsAsFactors = FALSE
    )
    structural <- if (base::nrow(structural)) base::rbind(structural, af_row) else af_row
  }
  structural
}

.protvis_pw_external_links <- function(accession) {
  if (base::is.null(accession) || !base::nzchar(accession)) return(base::list())
  id <- utils::URLencode(accession, reserved = TRUE)
  base::list(
    UniProt = base::paste0("https://www.uniprot.org/uniprotkb/", id, "/entry"),
    InterPro = base::paste0("https://www.ebi.ac.uk/interpro/protein/UniProt/", id, "/"),
    AlphaFold_DB = base::paste0("https://alphafold.ebi.ac.uk/entry/", id),
    SWISS_MODEL_Repository = base::paste0("https://swissmodel.expasy.org/repository/uniprot/", id),
    STRING = base::paste0("https://string-db.org/network/", id),
    PDBe_KB = base::paste0("https://www.ebi.ac.uk/pdbe/pdbe-kb/proteins/", id)
  )
}

.protvis_pw_model_view <- function(pdb_text) {
  if (base::is.null(pdb_text) || !base::nzchar(pdb_text)) return(r3dmol::r3dmol())
  r3dmol::r3dmol() |>
    r3dmol::m_add_model(data = pdb_text, format = "pdb") |>
    r3dmol::m_set_style(style = r3dmol::m_style_cartoon(color = "spectrum")) |>
    r3dmol::m_zoom_to()
}

utils::globalVariables(c("Residue", "Count", "position", "hydropathy", "start", "end", "name"))

#' Protein Workbench UI
#'
#' Single-protein workspace that combines UniProt annotation, sequence
#' properties, PTM/features, InterPro domains, structural resources and
#' database links without replacing any existing ProtVis toolkit.
#'
#' @param id Shiny module id.
#' @return Shiny UI.
#' @export
protein_workbench_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(shiny::HTML("\n      .pw-note {color:#657789;font-size:12px;line-height:1.55;}\n      .pw-kpis {display:grid;grid-template-columns:repeat(5,minmax(120px,1fr));gap:10px;margin-bottom:14px;}\n      .pw-kpi {border:1px solid #dbe8f3;border-radius:14px;background:#f8fbff;padding:13px 15px;}\n      .pw-kpi strong {display:block;color:#1787c9;font-size:19px;line-height:1.2;overflow-wrap:anywhere;}\n      .pw-kpi span {display:block;color:#657789;font-size:11px;margin-top:5px;text-transform:uppercase;letter-spacing:.05em;}\n      .pw-resource-grid {display:grid;grid-template-columns:repeat(3,minmax(180px,1fr));gap:12px;}\n      .pw-resource {display:block;border:1px solid #dbe8f3;border-radius:14px;background:#fff;padding:16px;text-decoration:none!important;}\n      .pw-resource:hover {border-color:#9ccce8;background:#f8fbff;}\n      .pw-resource strong {display:block;color:#1f3447;margin-bottom:5px;}\n      .pw-resource span {color:#657789;font-size:12px;}\n      @media(max-width:1000px){.pw-kpis{grid-template-columns:repeat(2,1fr)}.pw-resource-grid{grid-template-columns:1fr 1fr}}\n    ")),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 365,
        open = "open",
        shiny::h4("Protein Workbench"),
        shiny::p(
          "Resolve one protein and inspect sequence, annotation, PTM, domains, structures and external resources in a single workspace.",
          class = "pw-note"
        ),
        shiny::textInput(
          ns("query"), "Protein / gene / UniProt ID",
          value = "", placeholder = "e.g. P68871, HBB, Zm00001..."
        ),
        shiny::textInput(
          ns("organism_id"), "Organism taxon ID (optional)",
          value = "", placeholder = "e.g. 9606 or 4577"
        ),
        shiny::textAreaInput(
          ns("sequence"), "Protein sequence (optional)",
          value = "", rows = 7,
          placeholder = "Paste an amino-acid sequence for local sequence analysis"
        ),
        shiny::fileInput(
          ns("fasta_file"), "Or upload FASTA",
          accept = c(".fa", ".faa", ".fasta", ".fas")
        ),
        shiny::div(
          style = "display:flex;gap:8px;flex-wrap:wrap;",
          shiny::actionButton(ns("run"), "RUN / RESOLVE", icon = bsicons::bs_icon("play-fill"), class = "btn-primary pv-run-button"),
          shiny::actionButton(ns("example"), "EXAMPLE", icon = bsicons::bs_icon("stars")),
          shiny::actionButton(ns("clear"), "CLEAR", icon = bsicons::bs_icon("x-circle"))
        ),
        shiny::hr(),
        shiny::selectInput(ns("accession"), "Resolved UniProt entry", choices = character()),
        shiny::uiOutput(ns("status")),
        shiny::hr(),
        shiny::downloadButton(ns("download_fasta"), "Download FASTA"),
        shiny::downloadButton(ns("download_json"), "Download UniProt JSON")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(
          shiny::div(
            style = "display:flex;align-items:center;justify-content:space-between;gap:15px;",
            shiny::span("Protein-centric analysis workspace"),
            shiny::uiOutput(ns("header_links"))
          )
        ),
        bslib::card_body(
          shiny::uiOutput(ns("protein_header")),
          bslib::navset_card_tab(
            id = ns("tabs"),
            bslib::nav_panel(
              "Overview",
              bslib::layout_columns(
                col_widths = c(5, 7),
                bslib::card(bslib::card_header("Protein summary"), DT::DTOutput(ns("summary_table"))),
                bslib::card(bslib::card_header("Functional annotation"), DT::DTOutput(ns("comments_table")))
              )
            ),
            bslib::nav_panel(
              "Sequence",
              bslib::layout_columns(
                col_widths = c(4, 8),
                bslib::card(bslib::card_header("Sequence properties"), DT::DTOutput(ns("sequence_stats"))),
                bslib::card(bslib::card_header("Amino-acid sequence"), shiny::verbatimTextOutput(ns("sequence_text")))
              ),
              shiny::br(),
              bslib::layout_columns(
                col_widths = c(5, 7),
                bslib::card(bslib::card_header("Residue composition"), shiny::plotOutput(ns("composition_plot"), height = "360px")),
                bslib::card(bslib::card_header("Kyte-Doolittle hydropathy"), shiny::sliderInput(ns("hydro_window"), "Window", min = 3, max = 31, value = 9, step = 2), shiny::plotOutput(ns("hydropathy_plot"), height = "310px"))
              )
            ),
            bslib::nav_panel(
              "Annotations",
              shiny::p("UniProt sequence features including regions, active sites, binding sites, variants and processing features.", class = "pw-note"),
              DT::DTOutput(ns("features_table"))
            ),
            bslib::nav_panel(
              "PTM & sites",
              shiny::p("PTM-related and site-level UniProt features are separated here for rapid proteomics interpretation.", class = "pw-note"),
              DT::DTOutput(ns("ptm_table"))
            ),
            bslib::nav_panel(
              "Domains",
              shiny::p("InterPro is queried when a UniProt accession is available; UniProt cross-references remain available as a fallback.", class = "pw-note"),
              shiny::plotOutput(ns("domain_plot"), height = "280px"),
              DT::DTOutput(ns("domain_table"))
            ),
            bslib::nav_panel(
              "Structure",
              bslib::layout_columns(
                col_widths = c(5, 7),
                bslib::card(bslib::card_header("Structure resources"), DT::DTOutput(ns("structure_table"))),
                bslib::card(bslib::card_header("AlphaFold structure"), r3dmol::r3dmolOutput(ns("alphafold_view"), height = "470px"))
              ),
              shiny::br(),
              DT::DTOutput(ns("alphafold_table"))
            ),
            bslib::nav_panel(
              "Cross-references",
              DT::DTOutput(ns("xrefs_table"))
            ),
            bslib::nav_panel(
              "Resources",
              shiny::uiOutput(ns("resource_cards"))
            ),
            bslib::nav_panel(
              "Raw record",
              shiny::verbatimTextOutput(ns("raw_json"))
            )
          )
        )
      )
    )
  )
}

#' Protein Workbench server
#'
#' @param id Shiny module id.
#' @return A Shiny module server.
#' @export
protein_workbench_server <- function(id, shared_state = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      search = base::data.frame(),
      entry = NULL,
      interpro = base::data.frame(),
      alphafold = NULL,
      alphafold_pdb = NULL,
      local_sequence = "",
      message = "Enter a protein identifier or sequence to begin."
    )

    load_accession <- function(accession) {
      if (base::is.null(accession) || !base::nzchar(accession)) return(invisible(NULL))
      rv$message <- base::paste("Loading", accession, "...")
      entry <- .protvis_pw_get_uniprot(accession)
      if (base::is.null(entry)) base::stop("The selected UniProt entry could not be retrieved.", call. = FALSE)
      rv$entry <- entry
      rv$local_sequence <- .protvis_pw_sequence(entry)
      rv$interpro <- .protvis_pw_interpro(accession)
      rv$alphafold <- .protvis_pw_alphafold(accession)
      rv$alphafold_pdb <- NULL
      af <- .protvis_pw_alpha_record(rv$alphafold)
      pdb_url <- af$pdbUrl %||% af$pdb_url %||% NULL
      if (!base::is.null(pdb_url) && base::nzchar(base::as.character(pdb_url))) {
        rv$alphafold_pdb <- .protvis_pw_http_text(base::as.character(pdb_url), timeout = 60, not_found = NULL)
      }
      rv$message <- base::paste("Loaded UniProt", accession)
      invisible(entry)
    }

    shiny::observeEvent(input$fasta_file, {
      file <- input$fasta_file
      if (base::is.null(file) || !base::nrow(file)) return()
      base::tryCatch({
        fasta <- Biostrings::readAAStringSet(file$datapath)
        if (!base::length(fasta)) base::stop("No protein sequence was found in the FASTA file.")
        sequence <- base::as.character(fasta[[1]])
        shiny::updateTextAreaInput(session, "sequence", value = sequence)
        rv$local_sequence <- .protvis_pw_clean_sequence(sequence)
        rv$message <- base::paste0("Loaded FASTA sequence: ", base::nchar(rv$local_sequence), " aa")
      }, error = function(e) {
        shiny::showNotification(base::conditionMessage(e), type = "error", duration = 5)
      })
    })

    shiny::observeEvent(input$example, {
      shiny::updateTextInput(session, "query", value = "P68871")
      shiny::updateTextInput(session, "organism_id", value = "9606")
      shiny::updateTextAreaInput(session, "sequence", value = "")
      shiny::showNotification("Example loaded: human hemoglobin subunit beta (P68871).", type = "message", duration = 3)
    })

    shiny::observeEvent(input$clear, {
      shiny::updateTextInput(session, "query", value = "")
      shiny::updateTextInput(session, "organism_id", value = "")
      shiny::updateTextAreaInput(session, "sequence", value = "")
      shiny::updateSelectInput(session, "accession", choices = character())
      rv$search <- base::data.frame()
      rv$entry <- NULL
      rv$interpro <- base::data.frame()
      rv$alphafold <- NULL
      rv$alphafold_pdb <- NULL
      rv$local_sequence <- ""
      rv$message <- "Protein Workbench cleared."
    })

    shiny::observeEvent(input$run, {
      base::tryCatch({
        query <- base::trimws(input$query %||% "")
        sequence <- base::trimws(input$sequence %||% "")
        if (!base::nzchar(query) && !base::nzchar(sequence)) {
          base::stop("Enter a protein/gene/UniProt ID or a protein sequence.")
        }

        if (base::nzchar(sequence)) {
          rv$local_sequence <- .protvis_pw_clean_sequence(sequence)
        }

        if (base::nzchar(query)) {
          rv$message <- "Searching UniProt..."
          hits <- .protvis_pw_search_uniprot(query, input$organism_id %||% NULL, size = 25L)
          rv$search <- hits
          if (!base::nrow(hits)) base::stop("No UniProt entries matched the query.")
          labels <- base::paste0(
            hits$accession, " · ", hits$protein_name,
            ifelse(base::is.na(hits$genes) | !base::nzchar(hits$genes), "", base::paste0(" · ", hits$genes)),
            ifelse(base::is.na(hits$organism) | !base::nzchar(hits$organism), "", base::paste0(" · ", hits$organism))
          )
          choices <- stats::setNames(hits$accession, labels)
          shiny::updateSelectInput(session, "accession", choices = choices, selected = hits$accession[[1]])
          load_accession(hits$accession[[1]])
        } else {
          rv$entry <- NULL
          rv$interpro <- base::data.frame()
          rv$alphafold <- NULL
          rv$alphafold_pdb <- NULL
          rv$message <- base::paste0("Local sequence loaded: ", base::nchar(rv$local_sequence), " aa")
        }

        sequence_value <- rv$local_sequence %||% ""
        if (!base::nzchar(sequence_value) && !base::is.null(rv$entry)) {
          sequence_value <- .protvis_pw_sequence(rv$entry)
        }
        table_candidates <- list(
          search = rv$search,
          summary = .protvis_pw_summary_table(rv$entry),
          comments = .protvis_pw_comments_table(rv$entry),
          interpro = rv$interpro,
          sequence_stats = .protvis_pw_sequence_stats(sequence_value)
        )
        table_candidates <- table_candidates[
          vapply(table_candidates, is.data.frame, logical(1))
        ]
        .protvis_record_shared_run(
          shared_state,
          module = "protein_workbench",
          method = if (base::nzchar(query)) "UniProt_query" else "local_sequence",
          category = "toolkits",
          parameters = list(
            query = query,
            organism_id = input$organism_id %||% NA_character_
          ),
          tables = table_candidates,
          statistics = list(
            accession = if (base::is.null(rv$entry)) NA_character_ else
              rv$entry$primaryAccession %||% NA_character_,
            sequence = sequence_value,
            alphafold = rv$alphafold
          )
        )
      }, error = function(e) {
        rv$message <- base::conditionMessage(e)
        shiny::showModal(shiny::modalDialog(
          title = "Protein Workbench",
          base::conditionMessage(e),
          easyClose = TRUE,
          footer = NULL
        ))
      })
    })

    shiny::observeEvent(input$accession, {
      accession <- input$accession %||% ""
      if (!base::nzchar(accession)) return()
      if (!base::is.null(rv$entry) && identical(base::as.character(rv$entry$primaryAccession), accession)) return()
      base::tryCatch(
        load_accession(accession),
        error = function(e) shiny::showNotification(base::conditionMessage(e), type = "error", duration = 5)
      )
    }, ignoreInit = TRUE)

    current_sequence <- shiny::reactive({
      sequence <- rv$local_sequence %||% ""
      if (!base::nzchar(sequence) && !base::is.null(rv$entry)) sequence <- .protvis_pw_sequence(rv$entry)
      sequence
    })

    current_accession <- shiny::reactive({
      if (base::is.null(rv$entry)) "" else base::as.character(rv$entry$primaryAccession %||% "")
    })

    output$status <- shiny::renderUI({
      shiny::div(class = "pw-note", rv$message %||% "")
    })

    output$protein_header <- shiny::renderUI({
      sequence <- current_sequence()
      if (base::is.null(rv$entry)) {
        if (!base::nzchar(sequence)) return(shiny::div(class = "pw-note", "No protein loaded."))
        stats <- .protvis_pw_sequence_stats(sequence)
        return(shiny::div(
          class = "pw-kpis",
          shiny::div(class = "pw-kpi", shiny::strong(base::nchar(sequence)), shiny::span("Amino acids")),
          shiny::div(class = "pw-kpi", shiny::strong(stats$value[stats$metric == "Molecular weight"]), shiny::span("Approx. MW")),
          shiny::div(class = "pw-kpi", shiny::strong(stats$value[stats$metric == "Estimated pI"]), shiny::span("Estimated pI")),
          shiny::div(class = "pw-kpi", shiny::strong(stats$value[stats$metric == "GRAVY"]), shiny::span("GRAVY")),
          shiny::div(class = "pw-kpi", shiny::strong("Local"), shiny::span("Sequence source"))
        ))
      }
      entry <- rv$entry
      shiny::div(
        shiny::h3(.protvis_pw_protein_name(entry), style = "margin-bottom:4px;"),
        shiny::p(
          base::paste(
            entry$primaryAccession %||% "",
            .protvis_pw_gene_names(entry) %||% "",
            entry$organism$scientificName %||% "",
            sep = " · "
          ),
          class = "pw-note"
        ),
        shiny::div(
          class = "pw-kpis",
          shiny::div(class = "pw-kpi", shiny::strong(entry$primaryAccession %||% "—"), shiny::span("UniProt")),
          shiny::div(class = "pw-kpi", shiny::strong(entry$sequence$length %||% base::nchar(sequence)), shiny::span("Amino acids")),
          shiny::div(class = "pw-kpi", shiny::strong(entry$sequence$molWeight %||% "—"), shiny::span("Molecular weight")),
          shiny::div(class = "pw-kpi", shiny::strong(entry$annotationScore %||% "—"), shiny::span("Annotation score")),
          shiny::div(class = "pw-kpi", shiny::strong(if (base::grepl("reviewed", entry$entryType %||% "", ignore.case = TRUE)) "Reviewed" else "Unreviewed"), shiny::span("UniProt status"))
        )
      )
    })

    output$header_links <- shiny::renderUI({
      accession <- current_accession()
      if (!base::nzchar(accession)) return(NULL)
      links <- .protvis_pw_external_links(accession)
      shiny::tags$a(href = links$UniProt, target = "_blank", class = "btn btn-outline-primary btn-sm", "Open UniProt")
    })

    output$summary_table <- DT::renderDT({
      table <- .protvis_pw_summary_table(rv$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "Resolve a UniProt entry to display database metadata.")
      DT::datatable(table, rownames = FALSE, options = base::list(dom = "t", paging = FALSE, scrollX = TRUE))
    })

    output$comments_table <- DT::renderDT({
      table <- .protvis_pw_comments_table(rv$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No UniProt comments are available for this protein.")
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 8, scrollX = TRUE))
    })

    output$sequence_stats <- DT::renderDT({
      table <- .protvis_pw_sequence_stats(current_sequence())
      if (!base::nrow(table)) table <- base::data.frame(Message = "No protein sequence loaded.")
      DT::datatable(table, rownames = FALSE, options = base::list(dom = "t", paging = FALSE))
    })

    output$sequence_text <- shiny::renderText({
      sequence <- current_sequence()
      if (!base::nzchar(sequence)) return("No protein sequence loaded.")
      base::paste(base::substring(sequence, base::seq(1, base::nchar(sequence), 60), base::seq(60, base::nchar(sequence) + 59, 60)), collapse = "\n")
    })

    output$composition_plot <- shiny::renderPlot({
      table <- .protvis_pw_composition(current_sequence())
      if (!base::nrow(table)) {
        graphics::plot.new(); graphics::text(0.5, 0.5, "No sequence loaded."); return(invisible(NULL))
      }
      ggplot2::ggplot(table, ggplot2::aes(x = stats::reorder(Residue, -Count), y = Count)) +
        ggplot2::geom_col() +
        ggplot2::labs(x = "Amino acid", y = "Count") +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$hydropathy_plot <- shiny::renderPlot({
      table <- .protvis_pw_hydropathy(current_sequence(), input$hydro_window %||% 9L)
      table <- table[!base::is.na(table$hydropathy), , drop = FALSE]
      if (!base::nrow(table)) {
        graphics::plot.new(); graphics::text(0.5, 0.5, "No sequence loaded."); return(invisible(NULL))
      }
      ggplot2::ggplot(table, ggplot2::aes(x = position, y = hydropathy)) +
        ggplot2::geom_line(linewidth = 0.6) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.35) +
        ggplot2::labs(x = "Residue position", y = "Hydropathy") +
        ggplot2::theme_minimal(base_size = 12)
    })

    output$features_table <- DT::renderDT({
      table <- .protvis_pw_features_table(rv$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No UniProt sequence features are available.")
      DT::datatable(table, rownames = FALSE, filter = if (base::nrow(table) > 1L) "top" else "none", options = base::list(pageLength = 15, scrollX = TRUE))
    })

    output$ptm_table <- DT::renderDT({
      table <- .protvis_pw_ptm_table(rv$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No PTM/site features were found in the UniProt record.")
      DT::datatable(table, rownames = FALSE, filter = if (base::nrow(table) > 1L) "top" else "none", options = base::list(pageLength = 15, scrollX = TRUE))
    })

    output$domain_table <- DT::renderDT({
      table <- rv$interpro
      if (!base::nrow(table) && !base::is.null(rv$entry)) table <- .protvis_pw_domain_fallback(rv$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No domain annotation is available for the current protein.")
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 15, scrollX = TRUE))
    })

    output$domain_plot <- shiny::renderPlot({
      table <- rv$interpro
      if (!base::nrow(table) || !base::all(c("start", "end", "name") %in% base::names(table))) {
        graphics::plot.new(); graphics::text(0.5, 0.5, "InterPro positional domains will appear here when available."); return(invisible(NULL))
      }
      plot_data <- table[!base::is.na(table$start) & !base::is.na(table$end), , drop = FALSE]
      if (!base::nrow(plot_data)) {
        graphics::plot.new(); graphics::text(0.5, 0.5, "No positional domain coordinates returned by InterPro."); return(invisible(NULL))
      }
      plot_data$label <- ifelse(base::nzchar(plot_data$name), plot_data$name, plot_data$accession)
      plot_data$track <- base::seq_len(base::nrow(plot_data))
      ggplot2::ggplot(plot_data) +
        ggplot2::geom_segment(ggplot2::aes(x = start, xend = end, y = track, yend = track), linewidth = 7, lineend = "round") +
        ggplot2::scale_y_continuous(breaks = plot_data$track, labels = plot_data$label) +
        ggplot2::labs(x = "Residue position", y = NULL) +
        ggplot2::theme_minimal(base_size = 11)
    })

    output$structure_table <- DT::renderDT({
      table <- if (base::is.null(rv$entry)) base::data.frame() else .protvis_pw_structure_table(rv$entry, rv$alphafold)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No structure cross-reference is available.")
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 12, scrollX = TRUE))
    })

    output$alphafold_view <- r3dmol::renderR3dmol({
      .protvis_pw_model_view(rv$alphafold_pdb)
    })

    output$alphafold_table <- DT::renderDT({
      af <- .protvis_pw_alpha_record(rv$alphafold)
      if (base::is.null(af)) {
        table <- base::data.frame(Message = "No AlphaFold DB record was returned for this accession.")
      } else {
        leaves <- .protvis_pw_leaf_values(af)
        paths <- base::names(base::unlist(af, recursive = TRUE, use.names = TRUE))
        values <- base::as.character(base::unlist(af, recursive = TRUE, use.names = FALSE))
        if (base::length(paths) == base::length(values) && base::length(paths)) {
          table <- base::data.frame(field = paths, value = values, stringsAsFactors = FALSE)
        } else {
          table <- base::data.frame(value = leaves, stringsAsFactors = FALSE)
        }
      }
      DT::datatable(table, rownames = FALSE, options = base::list(pageLength = 12, scrollX = TRUE))
    })

    output$xrefs_table <- DT::renderDT({
      table <- .protvis_pw_xrefs_table(rv$entry)
      if (!base::nrow(table)) table <- base::data.frame(Message = "No UniProt cross-references are available.")
      DT::datatable(table, rownames = FALSE, filter = if (base::nrow(table) > 1L) "top" else "none", options = base::list(pageLength = 20, scrollX = TRUE))
    })

    output$resource_cards <- shiny::renderUI({
      accession <- current_accession()
      if (!base::nzchar(accession)) {
        return(shiny::div(class = "pw-note", "Resolve a UniProt accession to enable linked protein resources."))
      }
      links <- .protvis_pw_external_links(accession)
      descriptions <- c(
        UniProt = "Curated sequence and functional annotation",
        InterPro = "Domains, families and signatures",
        AlphaFold_DB = "Predicted protein structures and confidence",
        SWISS_MODEL_Repository = "Homology models and experimental structure links",
        STRING = "Protein association network",
        PDBe_KB = "Experimental structural knowledge"
      )
      shiny::div(
        class = "pw-resource-grid",
        base::lapply(base::names(links), function(label) {
          shiny::tags$a(
            class = "pw-resource", href = links[[label]], target = "_blank",
            shiny::strong(base::gsub("_", " ", label)),
            shiny::span(descriptions[[label]])
          )
        })
      )
    })

    output$raw_json <- shiny::renderText({
      if (base::is.null(rv$entry)) return("No UniProt record loaded.")
      jsonlite::toJSON(rv$entry, auto_unbox = TRUE, pretty = TRUE, null = "null", digits = NA)
    })

    output$download_fasta <- shiny::downloadHandler(
      filename = function() base::paste0(current_accession() %||% "protein", ".fasta"),
      content = function(file) {
        sequence <- current_sequence()
        if (!base::nzchar(sequence)) base::stop("No protein sequence available.")
        accession <- current_accession()
        if (!base::nzchar(accession)) accession <- "protein"
        base::writeLines(.protvis_pw_fasta(sequence, accession), file)
      }
    )

    output$download_json <- shiny::downloadHandler(
      filename = function() base::paste0(current_accession() %||% "protein", "_uniprot.json"),
      content = function(file) {
        if (base::is.null(rv$entry)) base::stop("No UniProt record available.")
        jsonlite::write_json(rv$entry, file, pretty = TRUE, auto_unbox = TRUE, null = "null")
      }
    )
  })
}
