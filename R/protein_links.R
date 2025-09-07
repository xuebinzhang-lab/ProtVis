library(shiny)
library(bslib)
library(xml2)
library(dplyr)
library(Biostrings)

# 改写的 blast_uniprot，使用 system2()，安全处理空结果
blast_uniprot <- function(input,
                          db_swiss,
                          db_trembl,
                          top_n = 1,
                          tmp_fasta = "query_tmp.fasta",
                          blastp_path) {

  if (is.list(input)) input <- unlist(input)

  if (is.character(input) && length(input) == 1 && file.exists(input)) {
    seqs <- readAAStringSet(input)
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

      doc <- read_xml(xml_out)
      xml_find_all(doc, ".//Hit")
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
      acc <- xml_text(xml_find_first(top_hit, ".//Hit_accession"))
      entry <- xml_text(xml_find_first(top_hit, ".//Hit_id"))
      desc <- xml_text(xml_find_first(top_hit, ".//Hit_def"))

      hsp_identity <- as.numeric(xml_text(xml_find_first(top_hit, ".//Hsp_identity")))
      hsp_align_len <- as.numeric(xml_text(xml_find_first(top_hit, ".//Hsp_align-len")))
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

  bind_rows(results)
}

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
protein_links_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        textInput(ns("protein_input"), "Enter Gene ID or Protein Sequence", value = ""),
        textAreaInput(ns("seq_input"), "Enter protein sequence:", value = "", rows = 8),
        textInput(ns("db_swiss"), "SwissProt database path:", value = "G:/blastdb/UniProt/uniprot_sprot"),
        textInput(ns("db_trembl"), "TrEMBL database path:", value = "G:/blastdb/UniProt/uniprot_trembl"),
        textInput(ns("blastp_path"), "BLASTp executable path:", value = "F:/NCBI/blast-2.17.0+/bin/blastp.exe"),
        actionButton(ns("run_blast"), "Run BLASTp")
      ),
      mainPanel(
        # h4("BLASTp Result"),
        # tableOutput(ns("blast_result")),
        # br(),
        uiOutput(ns("links_panel"))  # 保留原来动态生成链接的区域
      )
    )
  )
}


#' Protein Links Server Module
#'
#' Detects whether the input is a UniProt ID (via pattern or database lookup)
#' or a raw protein sequence, and generates links for UniProt, InterPro, PSIPRED,
#' and Swiss-Model accordingly.
#'
#' @param id The namespace identifier for the module
#' @return A Shiny server module
#' @export
protein_links_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$links_panel <- renderUI({
      prot <- NULL
      # 1️⃣ 如果用户输入了 UniProt ID，优先使用
      if (nzchar(input$protein_input)) {
        req(input$protein_input)
        prot <- get_uniprot_entry(input$protein_input)
      }
      # 2️⃣ 如果没有 UniProt ID 输入，但有序列输入并运行 BLASTp
      if (is.null(prot) && nzchar(input$seq_input)) {

        blast_result <- eventReactive(input$run_blast, {
          req(input$seq_input)
          req(input$db_swiss)
          req(input$db_trembl)

          seq_list <- list(input$seq_input)

          result <- blast_uniprot(
            list(seq_list),
            db_swiss = input$db_swiss,
            db_trembl = input$db_trembl,
            blastp_path = input$blastp_path
          )

          result
        })
        prot <- result %>%
          dplyr::select(Description) %>%
          dplyr::mutate(UniProtID = stringr::str_extract(Description, "(?<=sp\\|)[A-Z0-9]+(?=\\|)")) %>%
          dplyr::pull(UniProtID)
      }

      # 3️⃣ 如果仍然没有得到 UniProt ID，显示提示
      if (is.null(prot) || is.na(prot) || prot == "") {
        return(tags$p("No UniProt ID found. Please check your input.", style = "color:green"))
      }




      # 生成链接
      uniprot_url    <- paste0("https://www.uniprot.org/uniprotkb/", prot)
      interpro_url   <- paste0("https://www.ebi.ac.uk/interpro/protein/UniProt/", prot)
      swissmodel_url <- paste0("https://www.swissmodel.expasy.org/repository/uniprot/", prot)
      # psipred_url    <- "http://bioinf.cs.ucl.ac.uk/psipred/"

      tagList(
        h4("Quick Links"),
        tags$a(href = uniprot_url, "UniProt", target = "_blank", class = "btn btn-primary m-1"),
        tags$a(href = interpro_url, "InterPro", target = "_blank", class = "btn btn-info m-1"),
        # tags$a(href = psipred_url, "PSIPRED", target = "_blank", class = "btn btn-success m-1"),
        tags$a(href = swissmodel_url, "Swiss-Model", target = "_blank", class = "btn btn-warning m-1")
      )
    })
  })
}

