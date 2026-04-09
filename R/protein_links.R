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
#' @return A data frame containing BLAST results with columns: SeqID,
#'   UniProtAcc, EntryName, Description, Identity, Database
#' @export
#' @examples
#' \dontrun{
#' result <- blast_uniprot(
#'   input = "MAEGEITTFTALTEKFNLPPGNYKKPKLLYCSNGGHFLRILPDGTVDGTRDRSDQHIQLQLSAESIGEG",
#'   db_swiss = "/path/to/uniprot_sprot",
#'   db_trembl = "/path/to/uniprot_trembl",
#'   blastp_path = "/path/to/blastp"
#' )
#' }
blast_uniprot <- function(input,
                          db_swiss,
                          db_trembl,
                          top_n = 1,
                          tmp_fasta = "query_tmp.fasta",
                          blastp_path) {

  if (is.list(input)) input <- unlist(input)

  if (is.character(input) && length(input) == 1 && file.exists(input)) {
    seqs <- Biostrings::readAAStringSet(input)
    seq_names <- names(seqs)
    seq_list <- as.character(seqs)
  } else if (is.character(input)) {
    seq_list <- input
    seq_names <- paste0("Seq", seq_along(seq_list))
  } else {
    stop("Input must be a character vector, a list, or a path to a FASTA file.")
  }

  results <- list()

  for (i in seq_along(seq_list)) {
    seq <- gsub("\\s", "", seq_list[i])
    seq_name <- seq_names[i]
    writeLines(paste0(">", seq_name, "\n", seq), tmp_fasta)

    run_blast <- function(db_path) {
      xml_out <- paste0("blast_result_", seq_name, ".xml")

      args <- c(
        "-query", tmp_fasta,
        "-db", db_path,
        "-out", xml_out,
        "-outfmt", "5",
        "-max_target_seqs", as.character(top_n)
      )

      system2(blastp_path, args = args, stdout = TRUE, stderr = TRUE)

      if (!file.exists(xml_out) || file.info(xml_out)$size == 0) {
        return(list())
      }

      doc <- xml2::read_xml(xml_out)
      xml2::xml_find_all(doc, ".//Hit")
    }

    hits <- run_blast(db_swiss)
    final_db <- "SwissProt"

    if (length(hits) == 0) {
      hits <- run_blast(db_trembl)
      final_db <- "TrEMBL"
    }

    if (length(hits) == 0) {
      df <- data.frame(
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

      hsp_identity <- as.numeric(xml2::xml_text(xml2::xml_find_first(top_hit, ".//Hsp_identity")))
      hsp_align_len <- as.numeric(xml2::xml_text(xml2::xml_find_first(top_hit, ".//Hsp_align-len")))
      identity <- if (!is.na(hsp_identity) & !is.na(hsp_align_len)) round(hsp_identity / hsp_align_len * 100, 2) else NA

      df <- data.frame(
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
#'
#' Queries UniProt REST API to retrieve primary accession number for a given gene ID.
#'
#' @param gene_id Character string containing the gene identifier to search
#' @return Character string of the primary UniProt accession, or NA if not found
#' @export
#' @examples
#' \dontrun{
#' accession <- get_uniprot_entry("TP53")
#' }
get_uniprot_entry <- function(gene_id) {
  url <- paste0("https://rest.uniprot.org/uniprotkb/search?query=", gene_id, "&format=json")
  res <- httr::GET(url)
  httr::stop_for_status(res)
  data <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  if (is.null(data$results) || nrow(data$results) == 0) {
    return(NA)
  }
  return(data$results$primaryAccession[1])
}

#' Protein Links UI Module
#'
#' Creates a user interface for entering a UniProt ID or protein sequence
#' and displays quick links to popular protein analysis tools (UniProt, InterPro, PSIPRED, Swiss-Model).
#'
#' @param id The namespace identifier for the module
#' @return A Shiny UI tagList containing a text input and a dynamic links panel
#' @export
#' @examples
#' \dontrun{
#' ui <- protein_links_ui("protein_links")
#' }
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
#'
#' Server-side logic that detects whether the input is a UniProt ID (via pattern or database lookup)
#' or a raw protein sequence, and generates links for UniProt, InterPro, and Swiss-Model accordingly.
#' If no UniProt ID is provided directly, it runs BLASTp against SwissProt/TrEMBL databases to find matches.
#'
#' @param id The namespace identifier for the module
#' @return A Shiny server module function
#' @export
#' @name protein_links_server
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   protein_links_server("protein_links")
#' }
#' }

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
      if (nzchar(input$protein_input)) {
        shiny::req(input$protein_input)
        prot <- get_uniprot_entry(input$protein_input)
      }

      # Priority 2: Run BLASTp search if sequence provided but no UniProt ID
      if (is.null(prot) && nzchar(input$seq_input)) {

        blast_result <- shiny::eventReactive(input$run_blast, {
          shiny::req(input$seq_input)
          shiny::req(input$db_swiss)
          shiny::req(input$db_trembl)

          seq_list <- list(input$seq_input)

          result <- blast_uniprot(
            list(seq_list),
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
