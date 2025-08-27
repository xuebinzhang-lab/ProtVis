#' Background Maker Module UI
#'
#' Creates the user interface for the background generation module
#' that processes EggNOG output files to create GO and KEGG backgrounds.
#'
#' @param id Character string specifying the namespace id for the module
#' @return A Shiny UI tagList containing the module interface
#' @export
#' @examples
#' background_make_ui("my_background")
background_make_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 350,
        div(style = "margin-bottom: 15px;",
            fileInput(ns("eggnog_output"), "Upload Eggnog Output File",
                      accept = c(".csv", ".xlsx"),
                      buttonLabel = "Browse..."),
            actionButton(ns("check_file"), "Check File",
                         class = "btn btn-success fw-bold mb-2"),
            div(style = "border-top: 3px solid #ff0000; margin: 10px 0;"),
            textInput(
              inputId = "separator",
              label = "Separator:",
              value = "_"
            ),
            actionButton(ns("extract"), "Make Background",
                         class = "btn btn-light fw-bold mb-3"),
            br(style = "line-height: 100px;"),
            downloadButton(ns("download_background"), "Download",
                           class = "btn btn-light fw-bold")
        )
      ),
      page_fluid(
        card(
          card_header("File Check Result"),
          card_body(
            textOutput(ns("file_check_result"))
          )
        ),
        layout_column_wrap(
          width = 1/2,
          height = 600,
          card(
            height = "800px",
            card_header("GO Background"),
            card_body(
              dataTableOutput(ns("go_background"))
            )
          ),
          card(
            height = "800px",
            card_header("KEGG Background"),
            card_body(
              dataTableOutput(ns("kegg_background"))
            )
          )
        )
      )
    )
  )
}

#' Background Maker Module Server
#'
#' Server-side logic for the background generation module that processes
#' EggNOG output files to create GO and KEGG pathway backgrounds for
#' enrichment analysis.
#'
#' @param id Character string specifying the namespace id for the module
#' @return A Shiny module server function
#' @export
#' @importFrom dplyr select filter mutate rename distinct left_join
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_extract str_remove str_detect
#' @importFrom GO.db GOTERM
#' @importFrom AnnotationDbi Term
#' @importFrom clusterProfiler ko2name
#' @importFrom openxlsx write.xlsx
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
background_make_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    go_bg <- reactiveVal(NULL)
    kegg_bg <- reactiveVal(NULL)
    file_valid <- reactiveVal(FALSE)   # Track if file validation passed

    # ---- File Validation ----
    observeEvent(input$check_file, {
      req(input$eggnog_output)

      infile <- input$eggnog_output$datapath
      ext <- tools::file_ext(infile)
      if (ext == "csv") {
        df <- read.csv(infile, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(infile)
      } else {
        output$file_check_result <- renderText("❌ Unsupported file type")
        file_valid(FALSE)
        return(NULL)
      }

      # Check for required columns
      required_cols <- c("query", "GOs", "KEGG_Pathway")
      missing_cols <- setdiff(required_cols, colnames(df))

      if (length(missing_cols) > 0) {
        output$file_check_result <- renderText(
          paste("❌ Missing required columns:", paste(missing_cols, collapse = ", "))
        )
        file_valid(FALSE)
      } else {
        output$file_check_result <- renderText("✅ File format check passed!")
        file_valid(TRUE)
      }
    })

    # ---- Background Generation ----
    observeEvent(input$extract, {
      req(input$eggnog_output)
      if (!file_valid()) {
        showNotification("Please check file first or fix column issues!", type = "error")
        return(NULL)
      }

      infile <- input$eggnog_output$datapath
      ext <- tools::file_ext(infile)
      if (ext == "csv") {
        df <- read.csv(infile, stringsAsFactors = FALSE)
      } else {
        df <- readxl::read_excel(infile)
      }

      # 分隔符（用户输入）
      sep <- input$separator
      if (is.null(sep) || sep == "") sep <- "_"   # 默认 "_"
      pattern <- paste0("^[^", sep, "]+")        # 动态正则

      # ---- GO Background Processing ----
      goterms <- Term(GOTERM)
      GOlist <- as.data.frame(goterms) %>%
        tibble::rownames_to_column("TERM") %>%
        dplyr::rename(NAME = goterms)

      go_background <- df %>%
        dplyr::select(query, GOs) %>%
        tidyr::separate_rows(GOs, sep = ",") %>%
        dplyr::filter(GOs != "-") %>%
        dplyr::mutate(query = stringr::str_extract(query, pattern)) %>%  # 动态分隔符
        dplyr::rename(GENE = query, TERM = GOs) %>%
        dplyr::left_join(GOlist, by = "TERM") %>%
        dplyr::filter(NAME != "NA") %>%
        dplyr::mutate(GENE = stringr::str_extract(GENE, "^[^\\.]+")) %>%
        dplyr::distinct() %>%
        dplyr::select(GENE, TERM, NAME)

      go_bg(go_background)

      # ---- KEGG Background Processing ----
      map_list <- df %>%
        dplyr::select(query, KEGG_Pathway) %>%
        tidyr::separate_rows(KEGG_Pathway, sep = ",") %>%
        dplyr::filter(KEGG_Pathway != "-") %>%
        dplyr::filter(stringr::str_detect(KEGG_Pathway, "map")) %>%
        dplyr::mutate(query = stringr::str_remove(query, "\\..*")) %>%
        dplyr::distinct() %>%
        dplyr::pull() %>%
        unique()

      result <- clusterProfiler::ko2name(map_list)

      kegg_background <- df %>%
        dplyr::select(query, KEGG_Pathway) %>%
        dplyr::mutate(query = stringr::str_extract(query, pattern)) %>%  # 动态分隔符
        tidyr::separate_rows(KEGG_Pathway, sep = ",") %>%
        dplyr::filter(KEGG_Pathway != "-") %>%
        dplyr::filter(stringr::str_detect(KEGG_Pathway, "map")) %>%
        dplyr::select(query, ko = KEGG_Pathway) %>%
        dplyr::left_join(result, by = "ko") %>%
        dplyr::filter(name != "NA") %>%
        dplyr::mutate(query = stringr::str_extract(query, "^[^.]+")) %>%
        dplyr::distinct() %>%
        dplyr::select(GENE = query, TERM = ko, NAME = name) %>%
        dplyr::select(GENE, TERM, NAME)

      kegg_bg(kegg_background)
    })

    # ---- UI Output Rendering ----
    output$go_background <- renderDataTable({
      req(go_bg())
      go_bg()
    })

    output$kegg_background <- renderDataTable({
      req(kegg_bg())
      kegg_bg()
    })

    # ---- Download Handler ----
    output$download_background <- downloadHandler(
      filename = function() {
        paste0("background_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        go_data <- go_bg()
        kegg_data <- kegg_bg()
        openxlsx::write.xlsx(
          list(
            GO_background = go_data,
            KEGG_background = kegg_data
          ),
          file = file,
          overwrite = TRUE
        )
      }
    )
  })
}
