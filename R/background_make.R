#' Background Maker Module UI
#'
#' Creates the user interface for the background generation module
#' that processes EggNOG output files to create GO and KEGG backgrounds.
#'
#' @param id Character string specifying the namespace id for the module
#' @return A shiny tagList
#' @import shiny
#' @export
background_make_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      class = "background-maker-module",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          width = 360,
          open = "open",
          gap = "12px",

          shiny::div(
            style = "margin-bottom: 10px;",
            shiny::h4(
              "Background Maker",
              style = "font-weight: 700; margin-bottom: 6px;"
            ),
            shiny::p(
              "Generate GO and KEGG background annotation tables from EggNOG output files.",
              style = "color: #6c757d; font-size: 14px; margin-bottom: 0;"
            )
          ),

          bslib::accordion(
            id = ns("bg_accordion"),
            open = c("upload", "params", "actions"),

            bslib::accordion_panel(
              title = "File Upload",
              value = "upload",
              shiny::fileInput(
                ns("eggnog_output"),
                "Upload EggNOG Output File",
                accept = c(".csv", ".xlsx", ".xls"),
                buttonLabel = "Browse..."
              ),
              shiny::div(
                style = "font-size: 13px; color: #6c757d; margin-top: 8px;",
                shiny::tags$b("Required columns: "),
                "query, GOs, KEGG_Pathway"
              )
            ),

            bslib::accordion_panel(
              title = "Parameters",
              value = "params",
              shiny::textInput(
                inputId = ns("separator"),
                label = "Gene ID Separator",
                value = "_",
                placeholder = "Example: _"
              ),
              shiny::div(
                style = "font-size: 13px; color: #6c757d;",
                "Used to extract gene IDs from the query column."
              )
            ),

            bslib::accordion_panel(
              title = "Actions",
              value = "actions",
              shiny::div(
                class = "d-grid gap-2",

                shiny::actionButton(
                  ns("check_file"),
                  "Check File",
                  class = "btn btn-success fw-bold"
                ),

                shiny::actionButton(
                  ns("extract"),
                  "Run Background Maker",
                  class = "btn btn-primary fw-bold"
                ),

                shiny::downloadButton(
                  ns("download_background"),
                  "Download Background",
                  class = "btn btn-outline-secondary fw-bold"
                ),

                shiny::downloadButton(
                  ns("download_demo"),
                  "Download Demo Data",
                  class = "btn btn-outline-info fw-bold"
                )
              )
            )
          )
        ),

        bslib::page_fillable(
          fillable = TRUE,

          bslib::layout_column_wrap(
            width = 1/3,

            bslib::card(
              class = "shadow-sm border-0",
              style = "background: linear-gradient(135deg, #eef6ff 0%, #f8fbff 100%);",
              bslib::card_body(
                shiny::div(
                  style = "font-size: 13px; color: #4f5b67; font-weight: 600;",
                  "FILE STATUS"
                ),
                shiny::uiOutput(ns("file_status_ui"))
              )
            ),

            bslib::card(
              class = "shadow-sm border-0",
              style = "background: linear-gradient(135deg, #eefaf3 0%, #f7fcf9 100%);",
              bslib::card_body(
                shiny::div(
                  style = "font-size: 13px; color: #4f5b67; font-weight: 600;",
                  "GO SUMMARY"
                ),
                shiny::uiOutput(ns("go_summary_ui"))
              )
            ),

            bslib::card(
              class = "shadow-sm border-0",
              style = "background: linear-gradient(135deg, #fff7ec 0%, #fffdf8 100%);",
              bslib::card_body(
                shiny::div(
                  style = "font-size: 13px; color: #4f5b67; font-weight: 600;",
                  "KEGG SUMMARY"
                ),
                shiny::uiOutput(ns("kegg_summary_ui"))
              )
            )
          ),

          shiny::br(),

          bslib::layout_column_wrap(
            width = 1/2,

            bslib::card(
              full_screen = TRUE,
              height = "760px",
              class = "shadow-sm",
              bslib::card_header(
                shiny::div(
                  style = "font-weight: 700;",
                  "GO Background"
                )
              ),
              bslib::card_body(
                shiny::uiOutput(ns("go_panel_ui"))
              )
            ),

            bslib::card(
              full_screen = TRUE,
              height = "760px",
              class = "shadow-sm",
              bslib::card_header(
                shiny::div(
                  style = "font-weight: 700;",
                  "KEGG Background"
                )
              ),
              bslib::card_body(
                shiny::uiOutput(ns("kegg_panel_ui"))
              )
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
#' EggNOG output files to create GO and KEGG pathway backgrounds.
#'
#' @param id Character string specifying the namespace id for the module
#' @return A shiny module server
#' @export
#' @importFrom shiny moduleServer observeEvent req reactiveVal renderUI renderText showNotification downloadHandler
#' @importFrom dplyr select filter mutate rename distinct left_join pull
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_extract str_detect
#' @importFrom GO.db GOTERM
#' @importFrom AnnotationDbi Term
#' @importFrom clusterProfiler ko2name
#' @importFrom openxlsx write.xlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom tools file_ext
#' @importFrom readxl read_excel
#' @importFrom DT renderDT datatable DTOutput
#' @importFrom tibble rownames_to_column
#' @export

utils::globalVariables(c(
  "query", "GOs", "NAME", "TERM", "KEGG_Pathway",
  "name", "ko", "GENE"
))

background_make_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    go_bg <- shiny::reactiveVal(NULL)
    kegg_bg <- shiny::reactiveVal(NULL)
    file_valid <- shiny::reactiveVal(FALSE)
    run_done <- shiny::reactiveVal(FALSE)
    checked_msg <- shiny::reactiveVal("No file checked yet.")
    uploaded_name <- shiny::reactiveVal("No file uploaded")

    read_input_file <- function(fileinfo) {
      if (is.null(fileinfo)) {
        return(NULL)
      }

      infile <- fileinfo$datapath
      ext <- tolower(tools::file_ext(infile))

      if (ext == "csv") {
        df <- utils::read.csv(
          infile,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      } else if (ext %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(infile)
        df <- as.data.frame(df, stringsAsFactors = FALSE)
      } else {
        return(NULL)
      }

      df
    }

    summary_df <- function() {
      data.frame(
        Item = c(
          "Uploaded file",
          "File checked",
          "Background generated",
          "GO records",
          "GO unique genes",
          "GO unique terms",
          "KEGG records",
          "KEGG unique genes",
          "KEGG unique terms"
        ),
        Value = c(
          uploaded_name(),
          ifelse(isTRUE(file_valid()), "Yes", "No"),
          ifelse(isTRUE(run_done()), "Yes", "No"),
          ifelse(is.null(go_bg()), 0, nrow(go_bg())),
          ifelse(is.null(go_bg()), 0, length(unique(go_bg()$GENE))),
          ifelse(is.null(go_bg()), 0, length(unique(go_bg()$TERM))),
          ifelse(is.null(kegg_bg()), 0, nrow(kegg_bg())),
          ifelse(is.null(kegg_bg()), 0, length(unique(kegg_bg()$GENE))),
          ifelse(is.null(kegg_bg()), 0, length(unique(kegg_bg()$TERM)))
        ),
        stringsAsFactors = FALSE
      )
    }

    observeEvent(input$eggnog_output, {
      uploaded_name(
        if (!is.null(input$eggnog_output)) input$eggnog_output$name else "No file uploaded"
      )
      file_valid(FALSE)
      run_done(FALSE)
      checked_msg("No file checked yet.")
      go_bg(NULL)
      kegg_bg(NULL)
    })

    observeEvent(input$check_file, {
      shiny::req(input$eggnog_output)

      df <- read_input_file(input$eggnog_output)

      if (is.null(df)) {
        checked_msg("❌ Unsupported file type.")
        file_valid(FALSE)
        shiny::showNotification("Unsupported file type.", type = "error")
        return(NULL)
      }

      required_cols <- c("query", "GOs", "KEGG_Pathway")
      missing_cols <- base::setdiff(required_cols, base::colnames(df))

      if (length(missing_cols) > 0) {
        checked_msg(
          paste0(
            "❌ Missing required columns: ",
            paste(missing_cols, collapse = ", ")
          )
        )
        file_valid(FALSE)
        shiny::showNotification(
          paste("Missing required columns:", paste(missing_cols, collapse = ", ")),
          type = "error"
        )
      } else {
        checked_msg("✅ File format check passed.")
        file_valid(TRUE)
        shiny::showNotification("File check passed.", type = "message")
      }
    })

    observeEvent(input$extract, {
      shiny::req(input$eggnog_output)

      if (!isTRUE(file_valid())) {
        shiny::showNotification(
          "Please click 'Check File' first and ensure the required columns are present.",
          type = "error"
        )
        return(NULL)
      }

      df <- read_input_file(input$eggnog_output)

      if (is.null(df)) {
        shiny::showNotification("Failed to read the input file.", type = "error")
        return(NULL)
      }

      sep <- input$separator
      if (is.null(sep) || sep == "") {
        sep <- "_"
      }

      pattern <- paste0("^[^", sep, "]+")
      goterms <- AnnotationDbi::Term(GO.db::GOTERM)

      golist <- as.data.frame(goterms) |>
        tibble::rownames_to_column("TERM") |>
        dplyr::rename(NAME = goterms)

      go_background <- df |>
        dplyr::select(query, GOs) |>
        tidyr::separate_rows(GOs, sep = ",") |>
        dplyr::filter(!is.na(GOs), GOs != "-") |>
        dplyr::mutate(query = stringr::str_extract(query, pattern)) |>
        dplyr::rename(GENE = query, TERM = GOs) |>
        dplyr::left_join(golist, by = "TERM") |>
        dplyr::filter(!is.na(NAME), NAME != "NA") |>
        dplyr::mutate(GENE = stringr::str_extract(GENE, "^[^\\.]+")) |>
        dplyr::distinct() |>
        dplyr::select(GENE, TERM, NAME)

      map_list <- df |>
        dplyr::select(query, KEGG_Pathway) |>
        tidyr::separate_rows(KEGG_Pathway, sep = ",") |>
        dplyr::filter(!is.na(KEGG_Pathway), KEGG_Pathway != "-") |>
        dplyr::filter(stringr::str_detect(KEGG_Pathway, "map")) |>
        dplyr::pull(KEGG_Pathway) |>
        unique()

      result <- clusterProfiler::ko2name(map_list)

      kegg_background <- df |>
        dplyr::select(query, KEGG_Pathway) |>
        dplyr::mutate(query = stringr::str_extract(query, pattern)) |>
        tidyr::separate_rows(KEGG_Pathway, sep = ",") |>
        dplyr::filter(!is.na(KEGG_Pathway), KEGG_Pathway != "-") |>
        dplyr::filter(stringr::str_detect(KEGG_Pathway, "map")) |>
        dplyr::select(query, ko = KEGG_Pathway) |>
        dplyr::left_join(result, by = "ko") |>
        dplyr::filter(!is.na(name), name != "NA") |>
        dplyr::mutate(query = stringr::str_extract(query, "^[^.]+")) |>
        dplyr::distinct() |>
        dplyr::select(GENE = query, TERM = ko, NAME = name)

      go_bg(go_background)
      kegg_bg(kegg_background)
      run_done(TRUE)

      shiny::showNotification(
        "Background generation completed successfully.",
        type = "message"
      )
    })

    output$file_status_ui <- shiny::renderUI({
      status_color <- if (isTRUE(file_valid())) "#198754" else "#6c757d"

      shiny::tagList(
        shiny::div(
          style = "font-size: 28px; font-weight: 700; margin-top: 4px; color: #1f2d3d;",
          if (isTRUE(file_valid())) "Ready" else "Waiting"
        ),
        shiny::div(
          style = "margin-top: 8px; color: #495057; font-size: 14px;",
          shiny::tags$b("File: "), uploaded_name()
        ),
        shiny::div(
          style = paste0(
            "margin-top: 8px; font-size: 14px; color: ", status_color, ";"
          ),
          checked_msg()
        )
      )
    })

    output$go_summary_ui <- shiny::renderUI({
      n_records <- if (is.null(go_bg())) 0 else nrow(go_bg())
      n_genes <- if (is.null(go_bg())) 0 else length(unique(go_bg()$GENE))
      n_terms <- if (is.null(go_bg())) 0 else length(unique(go_bg()$TERM))

      shiny::tagList(
        shiny::div(
          style = "font-size: 30px; font-weight: 700; margin-top: 4px; color: #1f2d3d;",
          format(n_records, big.mark = ",")
        ),
        shiny::div(
          style = "color: #6c757d; font-size: 14px;",
          "GO annotation records"
        ),
        shiny::hr(style = "margin: 10px 0;"),
        shiny::div(style = "font-size: 14px;", shiny::tags$b("Unique genes: "), n_genes),
        shiny::div(style = "font-size: 14px; margin-top: 4px;", shiny::tags$b("Unique terms: "), n_terms)
      )
    })

    output$kegg_summary_ui <- shiny::renderUI({
      n_records <- if (is.null(kegg_bg())) 0 else nrow(kegg_bg())
      n_genes <- if (is.null(kegg_bg())) 0 else length(unique(kegg_bg()$GENE))
      n_terms <- if (is.null(kegg_bg())) 0 else length(unique(kegg_bg()$TERM))

      shiny::tagList(
        shiny::div(
          style = "font-size: 30px; font-weight: 700; margin-top: 4px; color: #1f2d3d;",
          format(n_records, big.mark = ",")
        ),
        shiny::div(
          style = "color: #6c757d; font-size: 14px;",
          "KEGG annotation records"
        ),
        shiny::hr(style = "margin: 10px 0;"),
        shiny::div(style = "font-size: 14px;", shiny::tags$b("Unique genes: "), n_genes),
        shiny::div(style = "font-size: 14px; margin-top: 4px;", shiny::tags$b("Unique terms: "), n_terms)
      )
    })

    output$go_panel_ui <- shiny::renderUI({
      if (!isTRUE(run_done()) || is.null(go_bg())) {
        return(
          shiny::div(
            style = paste(
              "height: 100%; min-height: 620px; display: flex;",
              "align-items: center; justify-content: center;",
              "border: 1px dashed #d9dee3; border-radius: 12px;",
              "background: #fafbfc; color: #6c757d; font-size: 15px;"
            ),
            "Run Background Maker to display the GO background table."
          )
        )
      }

      DT::DTOutput(session$ns("go_background"))
    })

    output$kegg_panel_ui <- shiny::renderUI({
      if (!isTRUE(run_done()) || is.null(kegg_bg())) {
        return(
          shiny::div(
            style = paste(
              "height: 100%; min-height: 620px; display: flex;",
              "align-items: center; justify-content: center;",
              "border: 1px dashed #d9dee3; border-radius: 12px;",
              "background: #fafbfc; color: #6c757d; font-size: 15px;"
            ),
            "Run Background Maker to display the KEGG background table."
          )
        )
      }

      DT::DTOutput(session$ns("kegg_background"))
    })

    output$go_background <- DT::renderDT({
      shiny::req(run_done(), go_bg())

      DT::datatable(
        go_bg(),
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 15,
          lengthMenu = c(10, 15, 25, 50),
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "tip"
        )
      )
    })

    output$kegg_background <- DT::renderDT({
      shiny::req(run_done(), kegg_bg())

      DT::datatable(
        kegg_bg(),
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 15,
          lengthMenu = c(10, 15, 25, 50),
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "tip"
        )
      )
    })

    output$download_background <- shiny::downloadHandler(
      filename = function() {
        paste0("background_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        shiny::req(go_bg(), kegg_bg())

        wb <- openxlsx::createWorkbook()

        openxlsx::addWorksheet(wb, "Summary")
        openxlsx::writeData(wb, "Summary", summary_df())

        openxlsx::addWorksheet(wb, "GO_background")
        openxlsx::writeData(wb, "GO_background", go_bg())

        openxlsx::addWorksheet(wb, "KEGG_background")
        openxlsx::writeData(wb, "KEGG_background", kegg_bg())

        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )

    output$download_demo <- shiny::downloadHandler(
      filename = function() {
        "background_maker_demo.csv"
      },
      content = function(file) {
        demo_df <- data.frame(
          query = c(
            "GeneA_1",
            "GeneB_1",
            "GeneC_1",
            "GeneD_1"
          ),
          GOs = c(
            "GO:0008150,GO:0003674",
            "GO:0009987",
            "-",
            "GO:0008152,GO:0003824"
          ),
          KEGG_Pathway = c(
            "map00010,map01100",
            "map04075",
            "-",
            "map00941,map01110"
          ),
          stringsAsFactors = FALSE
        )

        utils::write.csv(demo_df, file, row.names = FALSE)
      }
    )

    observe({
      if (requireNamespace("shinyjs", quietly = TRUE)) {
        shinyjs::toggleState(
          id = "download_background",
          condition = isTRUE(run_done())
        )
      }
    })
  })
}
