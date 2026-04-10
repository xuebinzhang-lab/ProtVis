#' BLAST UniProt Search Function
#'
#' Performs BLASTp search against SwissProt and TrEMBL databases to find
#' matching UniProt entries for protein sequences.
#'
#' @param input Character vector of protein sequences, list of sequences,
#'   or path to FASTA file
#' @param db_swiss Character string path to SwissProt BLAST database
#' @param db_trembl Character string path to TrEMBL BLAST database
#' @param top_n Integer number of top hits to return (default: 1)
#' @param tmp_fasta Character string temporary FASTA file name (default: "query_tmp.fasta")
#' @param blastp_path Character string path to BLASTp executable
#' @name blast_uniprot
#' @export
#'

blast_uniprot <- function(input,
                          db_swiss,
                          db_trembl,
                          top_n = 1,
                          tmp_fasta = "query_tmp.fasta",
                          blastp_path) {
  if (base::is.list(input)) input <- base::unlist(input)
  if (base::is.character(input) && base::length(input) == 1 && base::file.exists(input)) {
    seqs <- Biostrings::readAAStringSet(input)
    seq_names <- nbase::ames(seqs)
    seq_list <- base::as.character(seqs)
  } else if (base::is.character(input)) {
    seq_list <- input
    seq_names <- base::paste0("Seq", base::seq_along(seq_list))
  } else {
    stop("Input must be a character vector, a list, or a path to a FASTA file.")
  }
  results <- base::list()
  for (i in base::seq_along(seq_list)) {
    seq <- base::gsub("\\s", "", seq_list[i])
    seq_name <- seq_names[i]
    base::writeLines(base::paste0(">", seq_name, "\n", seq), tmp_fasta)
    run_blast <- function(db_path) {
      xml_out <- base::paste0("blast_result_", seq_name, ".xml")
      args <- c(
        "-query", tmp_fasta,
        "-db", db_path,
        "-out", xml_out,
        "-outfmt", "5",
        "-max_target_seqs", base::as.character(top_n)
      )
      base::system2(blastp_path, args = args, stdout = TRUE, stderr = TRUE)
      if (!base::file.exists(xml_out) || base::file.info(xml_out)$size == 0) {
        return(list())
      }
      doc <- xml2::read_xml(xml_out)
      xml2::xml_find_all(doc, ".//Hit")
    }
    hits <- run_blast(db_swiss)
    final_db <- "SwissProt"
    if (base::length(hits) == 0) {
      hits <- run_blast(db_trembl)
      final_db <- "TrEMBL"
    }
    if (base::length(hits) == 0) {
      df <- base::data.frame(
        SeqID = seq_name,
        UniProtAcc = NA,
        EntryName = NA,
        Description = NA,
        Identity = NA,
        Database = NA,
        stringsAsFactors = FALSE
      )
    } else {
      top_hit <- hits[[1]]
      acc <- xml2::xml_text(xml2::xml_find_first(top_hit, ".//Hit_accession"))
      entry <- xml2::xml_text(xml2::xml_find_first(top_hit, ".//Hit_id"))
      desc <- xml2::xml_text(xml2::xml_find_first(top_hit, ".//Hit_def"))
      hsp_identity <- base::as.numeric(xml2::xml_text(xml2::xml_find_first(top_hit, ".//Hsp_identity")))
      hsp_align_len <- base::as.numeric(xml2::xml_text(xml2::xml_find_first(top_hit, ".//Hsp_align-len")))
      identity <- if (!base::is.na(hsp_identity) & !base::is.na(hsp_align_len)) base::round(hsp_identity / hsp_align_len * 100, 2) else NA
      df <- base::data.frame(
        SeqID = seq_name,
        UniProtAcc = acc,
        EntryName = entry,
        Description = desc,
        Identity = identity,
        Database = final_db,
        stringsAsFactors = FALSE
      )
    }
    results[[seq_name]] <- df
  }
  dplyr::bind_rows(results)
}

#' Get UniProt Entry from Gene ID
#' Queries UniProt REST API to retrieve primary accession number for a given gene ID.
#' @param gene_id Character string containing the gene identifier to search
#' @return Character string of the primary UniProt accession, or NA if not found
#' @name get_uniprot_entry
#' @export
#'
get_uniprot_entry <- function(gene_id) {
  url <- base::paste0("https://rest.uniprot.org/uniprotkb/search?query=", gene_id, "&format=json")
  res <- httr::GET(url)
  httr::stop_for_status(res)
  data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  if (base::is.null(data$results) || base::nrow(data$results) == 0) {
    return(NA)
  }
  return(data$results$primaryAccession[1])
}

#' Protein Links UI Module
#' Creates a user interface for entering a UniProt ID or protein sequence
#' and displays quick links to popular protein analysis tools (UniProt, InterPro, PSIPRED, Swiss-Model).
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing a text input and a dynamic links panel
#' @name protein_links_ui
#' @export
#'
protein_links_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput(ns("protein_input"), "Enter Gene ID or Protein Sequence", value = ""),
        shiny::textAreaInput(ns("seq_input"), "Enter protein sequence:", value = "", rows = 8),
        shiny::textInput(ns("db_swiss"), "SwissProt database path:", value = "G:/blastdb/UniProt/uniprot_sprot"),
        shiny::textInput(ns("db_trembl"), "TrEMBL database path:", value = "G:/blastdb/UniProt/uniprot_trembl"),
        shiny::textInput(ns("blastp_path"), "BLASTp executable path:", value = "F:/NCBI/blast-2.17.0+/bin/blastp.exe"),
        shiny::actionButton(ns("run_blast"), "Run BLASTp")
      ),
      shiny::mainPanel(
        shiny::uiOutput(ns("links_panel"))
      )
    )
  )
}

#' Protein Links Server Module
#' Server-side logic that detects whether the input is a UniProt ID (via pattern or database lookup)
#' or a raw protein sequence, and generates links for UniProt, InterPro, and Swiss-Model accordingly.
#' If no UniProt ID is provided directly, it runs BLASTp against SwissProt/TrEMBL databases to find matches.
#' @param id The namespace identifier for the module
#' @return A Shiny server module function
#' @name protein_links_server
#' @export
#'

utils::globalVariables(c("Description", "UniProtID"))

protein_links_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    #' Render Links Panel
    #'
    #' Reactive output that generates protein database links based on user input.
    #' Priority: 1) Direct UniProt ID input, 2) BLASTp search from sequence
    output$links_panel <- shiny::renderUI({
      prot <- NULL
      # Priority 1: Use direct UniProt ID input if provided
      if (base::nzchar(input$protein_input)) {
        shiny::req(input$protein_input)
        prot <- get_uniprot_entry(input$protein_input)
      }
      # Priority 2: Run BLASTp search if sequence provided but no UniProt ID
      if (base::is.null(prot) && base::nzchar(input$seq_input)) {
        blast_result <- shiny::eventReactive(input$run_blast, {
          shiny::req(input$seq_input)
          shiny::req(input$db_swiss)
          shiny::req(input$db_trembl)
          seq_list <- base::list(input$seq_input)
          result <- blast_uniprot(
            base::list(seq_list),
            db_swiss = input$db_swiss,
            db_trembl = input$db_trembl,
            blastp_path = input$blastp_path
          )
          result
        })
        prot <- blast_result() %>%
          dplyr::select(Description) %>%
          dplyr::mutate(UniProtID = stringr::str_extract(Description, "(?<=sp\\|)[A-Z0-9]+(?=\\|)")) %>%
          dplyr::pull(UniProtID)
      }
      # Priority 3: Display warning if no UniProt ID found
      if (is.null(prot) || is.na(prot) || prot == "") {
        return(shiny::tags$p("No UniProt ID found. Please check your input.", style = "color:green"))
      }
      # Generate external database links
      uniprot_url    <- paste0("https://www.uniprot.org/uniprotkb/", prot)
      interpro_url   <- paste0("https://www.ebi.ac.uk/interpro/protein/UniProt/", prot)
      swissmodel_url <- paste0("https://www.swissmodel.expasy.org/repository/uniprot/", prot)
      shiny::tagList(
        shiny::h4("Quick Links"),
        shiny::tags$a(href = uniprot_url, "UniProt", target = "_blank", class = "btn btn-primary m-1"),
        shiny::tags$a(href = interpro_url, "InterPro", target = "_blank", class = "btn btn-info m-1"),
        shiny::tags$a(href = swissmodel_url, "Swiss-Model", target = "_blank", class = "btn btn-warning m-1")
      )
    })
  })
}
