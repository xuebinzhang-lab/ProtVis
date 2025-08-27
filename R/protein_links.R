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
    textInput(ns("protein_input"), "Enter UniProt ID or Protein Sequence", value = ""),
    br(),
    uiOutput(ns("links_panel"))
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
      req(input$protein_input)
      input_id <- input$protein_input

      # 查数据库
      prot <- ProtVisDatabase::Arabidopsis_thaliana_id %>%
        dplyr::filter(V3 == input_id) %>%
        dplyr::pull(V1) %>%
        dplyr::first()

      # 如果找不到就不显示任何链接
      if (is.na(prot)) {
        return(NULL)
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

