#' Co-enrichment Visualization UI Module
#'
#' Publication-ready ProtVis module for visualizing shared and specific
#' enrichment patterns across two-omics or three-omics datasets.
#'
#' @param id A unique module id.
#'
#' @return A Shiny UI component.
#'
#' @import shiny
#' @import bslib
#' @export
co_enrichment_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 360,
        open = "open",

        shiny::div(
          style = "margin-bottom: 14px;",
          shiny::h4("Co-enrichment Visualization"),
          shiny::p(
            style = "color: #6b7280; font-size: 13px; margin-bottom: 0;",
            "Compare shared and omics-specific enriched pathways across two or three omics datasets with publication-style summary tables and figures."
          )
        ),

        bslib::accordion(
          multiple = TRUE,
          open = c("Data Input", "Display Control", "Color & Export", "Run & Download"),

          bslib::accordion_panel(
            title = "Data Input",

            shiny::selectInput(
              inputId = ns("omics_mode"),
              label = "Co-enrichment Mode",
              choices = c(
                "Two-omics" = "two",
                "Three-omics" = "three"
              ),
              selected = "two"
            ),

            shiny::textInput(
              inputId = ns("omics1_name"),
              label = "Omics 1 Name",
              value = "Transcriptome"
            ),

            shiny::fileInput(
              inputId = ns("file1"),
              label = "Upload Omics 1 Enrichment File",
              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
            ),

            shiny::textInput(
              inputId = ns("omics2_name"),
              label = "Omics 2 Name",
              value = "Proteome"
            ),

            shiny::fileInput(
              inputId = ns("file2"),
              label = "Upload Omics 2 Enrichment File",
              accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
            ),

            shiny::conditionalPanel(
              condition = "input.omics_mode === 'three'",
              ns = ns,
              shiny::textInput(
                inputId = ns("omics3_name"),
                label = "Omics 3 Name",
                value = "Metabolome"
              ),
              shiny::fileInput(
                inputId = ns("file3"),
                label = "Upload Omics 3 Enrichment File",
                accept = c(".csv", ".tsv", ".txt", ".xlsx", ".xls")
              )
            ),

            shiny::sliderInput(
              inputId = ns("padj_cutoff"),
              label = "Adjusted p-value cutoff",
              min = 0.001,
              max = 0.2,
              value = 0.05,
              step = 0.001
            ),

            shiny::checkboxInput(
              inputId = ns("keep_unique_only"),
              label = "Remove duplicated pathway names within each omics",
              value = TRUE
            )
          ),

          bslib::accordion_panel(
            title = "Column Mapping",

            shiny::uiOutput(ns("mapping_ui_1")),
            shiny::tags$hr(),
            shiny::uiOutput(ns("mapping_ui_2")),

            shiny::conditionalPanel(
              condition = "input.omics_mode === 'three'",
              ns = ns,
              shiny::tags$hr(),
              shiny::uiOutput(ns("mapping_ui_3"))
            )
          ),

          bslib::accordion_panel(
            title = "Display Control",

            shiny::selectInput(
              inputId = ns("pathway_filter"),
              label = "Pathway Display Mode",
              choices = c(
                "All" = "all",
                "Shared only" = "shared",
                "Specific only" = "specific"
              ),
              selected = "all"
            ),

            shiny::uiOutput(ns("group_filter_ui")),

            shiny::numericInput(
              inputId = ns("top_n"),
              label = "Top N pathways in bubble plot",
              value = 20,
              min = 5,
              max = 100,
              step = 1
            ),

            shiny::checkboxInput(
              inputId = ns("show_value_label"),
              label = "Show value labels",
              value = TRUE
            ),

            shiny::checkboxInput(
              inputId = ns("sort_desc"),
              label = "Sort summary count descending",
              value = FALSE
            )
          ),

          bslib::accordion_panel(
            title = "Color & Export",

            colourpicker::colourInput(
              inputId = ns("color_shared"),
              label = "Shared color",
              value = "#1f77b4"
            ),

            colourpicker::colourInput(
              inputId = ns("color_pairwise"),
              label = "Pairwise-shared color",
              value = "#ff7f0e"
            ),

            colourpicker::colourInput(
              inputId = ns("color_specific1"),
              label = "Specific color: Omics 1",
              value = "#d62728"
            ),

            colourpicker::colourInput(
              inputId = ns("color_specific2"),
              label = "Specific color: Omics 2",
              value = "#2ca02c"
            ),

            shiny::conditionalPanel(
              condition = "input.omics_mode === 'three'",
              ns = ns,
              colourpicker::colourInput(
                inputId = ns("color_specific3"),
                label = "Specific color: Omics 3",
                value = "#9467bd"
              )
            ),

            colourpicker::colourInput(
              inputId = ns("color_upset_active"),
              label = "UpSet active dot/line color",
              value = "#111827"
            ),

            colourpicker::colourInput(
              inputId = ns("color_upset_inactive"),
              label = "UpSet inactive dot color",
              value = "#d1d5db"
            ),

            colourpicker::colourInput(
              inputId = ns("bubble_low"),
              label = "Bubble low color",
              value = "#cfe8f3"
            ),

            colourpicker::colourInput(
              inputId = ns("bubble_high"),
              label = "Bubble high color",
              value = "#08519c"
            ),

            shiny::radioButtons(
              inputId = ns("download_format"),
              label = "Plot download format",
              choices = c("PDF" = "pdf", "PNG" = "png"),
              selected = "pdf",
              inline = TRUE
            ),

            shiny::numericInput(
              inputId = ns("plot_width"),
              label = "Download width (inch)",
              value = 10,
              min = 4,
              max = 30,
              step = 1
            ),

            shiny::numericInput(
              inputId = ns("plot_height"),
              label = "Download height (inch)",
              value = 7,
              min = 4,
              max = 30,
              step = 1
            ),

            shiny::numericInput(
              inputId = ns("plot_dpi"),
              label = "PNG dpi",
              value = 300,
              min = 72,
              max = 1200,
              step = 10
            )
          ),

          bslib::accordion_panel(
            title = "Run & Download",

            shiny::actionButton(
              inputId = ns("run_analysis"),
              label = "Run",
              icon = shiny::icon("play"),
              width = "100%",
              class = "btn-primary"
            ),

            shiny::br(),
            shiny::br(),

            shiny::downloadButton(
              outputId = ns("download_two_demo"),
              label = "Download Two-omics Demo",
              width = "100%"
            ),

            shiny::br(),
            shiny::br(),

            shiny::downloadButton(
              outputId = ns("download_three_demo"),
              label = "Download Three-omics Demo",
              width = "100%"
            ),

            shiny::br(),
            shiny::br(),

            shiny::downloadButton(
              outputId = ns("download_summary"),
              label = "Download Summary Table",
              width = "100%"
            ),

            shiny::br(),
            shiny::br(),

            shiny::downloadButton(
              outputId = ns("download_shared"),
              label = "Download Pathway Table",
              width = "100%"
            ),

            shiny::br(),
            shiny::tags$hr(),

            shiny::div(
              style = "font-weight: 600; margin-bottom: 8px;",
              "Plot Downloads"
            ),

            shiny::conditionalPanel(
              condition = "input.download_format === 'pdf'",
              ns = ns,
              shiny::downloadButton(
                outputId = ns("download_summary_pdf"),
                label = "Download Summary Plot",
                width = "100%"
              ),
              shiny::br(),
              shiny::br(),
              shiny::downloadButton(
                outputId = ns("download_upset_pdf"),
                label = "Download UpSet Plot",
                width = "100%"
              ),
              shiny::br(),
              shiny::br(),
              shiny::downloadButton(
                outputId = ns("download_bubble_pdf"),
                label = "Download Bubble Plot",
                width = "100%"
              )
            ),

            shiny::conditionalPanel(
              condition = "input.download_format === 'png'",
              ns = ns,
              shiny::downloadButton(
                outputId = ns("download_summary_png"),
                label = "Download Summary Plot",
                width = "100%"
              ),
              shiny::br(),
              shiny::br(),
              shiny::downloadButton(
                outputId = ns("download_upset_png"),
                label = "Download UpSet Plot",
                width = "100%"
              ),
              shiny::br(),
              shiny::br(),
              shiny::downloadButton(
                outputId = ns("download_bubble_png"),
                label = "Download Bubble Plot",
                width = "100%"
              )
            )
          )
        )
      ),

      shiny::div(
        style = "padding: 6px 4px 10px 4px;",

        bslib::card(
          style = "margin-bottom: 14px; border-radius: 16px;",
          full_screen = TRUE,
          bslib::card_body(
            shiny::div(
              style = "padding: 4px 6px 2px 6px;",
              shiny::h3(
                style = "margin-bottom: 6px;",
                "Co-enrichment Overview"
              ),
              shiny::p(
                style = "color: #6b7280; margin-bottom: 0;",
                "Integrated summary of shared and omics-specific enriched pathways with matrix-style overlap visualization and publication-ready plots."
              )
            )
          )
        ),

        bslib::layout_columns(
          col_widths = c(7, 5),

          bslib::card(
            style = "border-radius: 16px;",
            full_screen = TRUE,
            bslib::card_header("Overlap Summary"),
            bslib::card_body(
              shiny::uiOutput(ns("summary_note")),
              DT::DTOutput(ns("summary_table"))
            )
          ),

          bslib::card(
            style = "border-radius: 16px;",
            full_screen = TRUE,
            bslib::card_header("Uploaded Data Preview"),
            bslib::card_body(
              shiny::tabsetPanel(
                shiny::tabPanel("Omics 1", DT::DTOutput(ns("preview1"))),
                shiny::tabPanel("Omics 2", DT::DTOutput(ns("preview2"))),
                shiny::tabPanel("Omics 3", DT::DTOutput(ns("preview3")))
              )
            )
          )
        ),

        shiny::br(),

        bslib::layout_columns(
          col_widths = c(6, 6),

          bslib::card(
            style = "border-radius: 16px;",
            full_screen = TRUE,
            bslib::card_header("Summary Bar Plot"),
            bslib::card_body(
              shiny::plotOutput(ns("summary_plot"), height = "430px")
            )
          ),

          bslib::card(
            style = "border-radius: 16px;",
            full_screen = TRUE,
            bslib::card_header("Matrix-style UpSet Plot"),
            bslib::card_body(
              shiny::plotOutput(ns("upset_plot"), height = "520px")
            )
          )
        ),

        shiny::br(),

        bslib::card(
          style = "border-radius: 16px;",
          full_screen = TRUE,
          bslib::card_header("Significance Bubble Plot"),
          bslib::card_body(
            shiny::plotOutput(ns("bubble_plot"), height = "560px")
          )
        ),

        shiny::br(),

        bslib::card(
          style = "border-radius: 16px;",
          full_screen = TRUE,
          bslib::card_header("Shared and Specific Pathways"),
          bslib::card_body(
            DT::DTOutput(ns("shared_table"))
          )
        )
      )
    )
  )
}
#' Co-enrichment Visualization Server Module
#'
#' Server logic for the ProtVis co-enrichment visualization module.
#'
#' @param id A unique module id.
#'
#' @return Internal reactive objects only.
#'
#' @import shiny
#' @import bslib
#' @export
co_enrichment_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    rv <- shiny::reactiveValues(
      df1 = NULL,
      df2 = NULL,
      df3 = NULL,
      summary_df = NULL,
      pathway_df = NULL,
      bubble_df = NULL,
      intersection_df = NULL,
      matrix_df = NULL
    )

    read_table_flexible <- function(file_path) {
      ext <- tolower(tools::file_ext(file_path))

      if (ext %in% c("csv")) {
        df <- utils::read.csv(
          file_path,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else if (ext %in% c("tsv", "txt")) {
        df <- utils::read.delim(
          file_path,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          stop("Package 'readxl' is required to read Excel files.")
        }
        df <- readxl::read_excel(file_path)
        df <- as.data.frame(
          df,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        stop("Unsupported file format. Please upload csv, tsv, txt, xlsx, or xls.")
      }

      df
    }

    normalize_colnames <- function(x) {
      out <- tolower(trimws(x))
      out <- gsub("[[:space:]]+", "_", out)
      out <- gsub("\\.+", "_", out)
      out
    }

    guess_pathway_col <- function(df) {
      cn <- base::colnames(df)
      if (is.null(cn) || length(cn) == 0) {
        return(NULL)
      }

      cn_norm <- normalize_colnames(cn)
      candidate_patterns <- c("pathway", "description", "term", "category", "name")

      for (p in candidate_patterns) {
        hit <- which(grepl(p, cn_norm, fixed = TRUE))
        if (length(hit) > 0) {
          return(cn[hit[1]])
        }
      }

      cn[1]
    }

    guess_padj_col <- function(df) {
      cn <- base::colnames(df)
      if (is.null(cn) || length(cn) == 0) {
        return(NULL)
      }

      cn_norm <- normalize_colnames(cn)

      candidate_patterns <- c(
        "p_adjust", "p.adjust", "padj", "adj_p", "adjusted_p",
        "qvalue", "fdr", "pvalue", "p_value", "p.value"
      )

      for (p in candidate_patterns) {
        hit <- which(grepl(gsub("\\.", "_", p), cn_norm, fixed = TRUE))
        if (length(hit) > 0) {
          return(cn[hit[1]])
        }
      }

      numeric_cols <- cn[vapply(df, is.numeric, logical(1))]
      if (length(numeric_cols) > 0) {
        return(numeric_cols[1])
      }

      cn[1]
    }

    safe_numeric <- function(x) {
      suppressWarnings(as.numeric(as.character(x)))
    }

    make_standard_df <- function(df, pathway_col, padj_col, omics_name, keep_unique_only = TRUE) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        stop(paste0("No valid data found for ", omics_name, "."))
      }

      if (is.null(pathway_col) || length(pathway_col) != 1 || is.na(pathway_col) || pathway_col == "") {
        stop(paste0("Please select a valid pathway column for ", omics_name, "."))
      }

      if (is.null(padj_col) || length(padj_col) != 1 || is.na(padj_col) || padj_col == "") {
        stop(paste0("Please select a valid adjusted p-value column for ", omics_name, "."))
      }

      if (!(pathway_col %in% colnames(df))) {
        stop(paste0("Selected pathway column not found in ", omics_name, "."))
      }

      if (!(padj_col %in% colnames(df))) {
        stop(paste0("Selected adjusted p-value column not found in ", omics_name, "."))
      }

      out <- data.frame(
        pathway = as.character(df[[pathway_col]]),
        padj = safe_numeric(df[[padj_col]]),
        omics = omics_name,
        stringsAsFactors = FALSE
      )

      out <- out[!is.na(out$pathway) & trimws(out$pathway) != "", , drop = FALSE]
      out <- out[!is.na(out$padj), , drop = FALSE]
      out$pathway <- trimws(out$pathway)

      if (keep_unique_only && nrow(out) > 0) {
        out <- out[order(out$padj, decreasing = FALSE), , drop = FALSE]
        out <- out[!duplicated(out$pathway), , drop = FALSE]
      }

      rownames(out) <- NULL
      out
    }

    add_type_column <- function(pathway_df) {
      if (is.null(pathway_df) || nrow(pathway_df) == 0) {
        return(pathway_df)
      }

      pathway_df$Type <- ifelse(grepl("^Specific: ", pathway_df$Group), "Specific", "Shared")
      pathway_df
    }

    build_matrix_df <- function(intersection_df, omics_names) {
      if (is.null(intersection_df) || nrow(intersection_df) == 0) {
        return(data.frame(
          Combination = character(0),
          Omics = character(0),
          Present = logical(0),
          Count = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      res_list <- lapply(seq_len(nrow(intersection_df)), function(i) {
        data.frame(
          Combination = rep(intersection_df$Combination[i], length(omics_names)),
          Omics = omics_names,
          Present = vapply(
            omics_names,
            function(x) {
              isTRUE(intersection_df[i, x, drop = TRUE])
            },
            logical(1)
          ),
          Count = rep(intersection_df$Count[i], length(omics_names)),
          stringsAsFactors = FALSE
        )
      })

      out <- do.call(rbind, res_list)
      rownames(out) <- NULL
      out
    }

    compute_two_omics <- function(df1, df2, cutoff) {
      n1 <- unique(df1$omics)[1]
      n2 <- unique(df2$omics)[1]

      s1 <- unique(df1$pathway[df1$padj <= cutoff])
      s2 <- unique(df2$pathway[df2$padj <= cutoff])

      shared <- intersect(s1, s2)
      only1 <- setdiff(s1, s2)
      only2 <- setdiff(s2, s1)

      summary_df <- data.frame(
        Category = c(
          paste0(n1, " significant pathways"),
          paste0(n2, " significant pathways"),
          "Shared pathways",
          paste0("Specific to ", n1),
          paste0("Specific to ", n2)
        ),
        Count = c(length(s1), length(s2), length(shared), length(only1), length(only2)),
        stringsAsFactors = FALSE
      )

      pathway_df <- data.frame(
        Pathway = c(shared, only1, only2),
        Group = c(
          rep("Shared", length(shared)),
          rep(paste0("Specific: ", n1), length(only1)),
          rep(paste0("Specific: ", n2), length(only2))
        ),
        stringsAsFactors = FALSE
      )

      intersection_df <- data.frame(
        Combination = c(
          paste0(n1, " only"),
          "Shared",
          paste0(n2, " only")
        ),
        Count = c(length(only1), length(shared), length(only2)),
        stringsAsFactors = FALSE
      )
      intersection_df[[n1]] <- c(TRUE, TRUE, FALSE)
      intersection_df[[n2]] <- c(FALSE, TRUE, TRUE)

      bubble_shared <- merge(
        df1[df1$pathway %in% shared, c("pathway", "padj"), drop = FALSE],
        df2[df2$pathway %in% shared, c("pathway", "padj"), drop = FALSE],
        by = "pathway",
        suffixes = c("_1", "_2")
      )

      if (nrow(bubble_shared) > 0) {
        bubble_df <- data.frame(
          pathway = c(bubble_shared$pathway, bubble_shared$pathway),
          omics = c(rep(n1, nrow(bubble_shared)), rep(n2, nrow(bubble_shared))),
          neglog10_padj = c(-log10(bubble_shared$padj_1), -log10(bubble_shared$padj_2)),
          stringsAsFactors = FALSE
        )
      } else {
        bubble_df <- data.frame(
          pathway = character(0),
          omics = character(0),
          neglog10_padj = numeric(0),
          stringsAsFactors = FALSE
        )
      }

      matrix_df <- build_matrix_df(intersection_df, c(n1, n2))

      list(
        summary_df = summary_df,
        pathway_df = pathway_df,
        bubble_df = bubble_df,
        intersection_df = intersection_df,
        matrix_df = matrix_df
      )
    }

    compute_three_omics <- function(df1, df2, df3, cutoff) {
      n1 <- unique(df1$omics)[1]
      n2 <- unique(df2$omics)[1]
      n3 <- unique(df3$omics)[1]

      s1 <- unique(df1$pathway[df1$padj <= cutoff])
      s2 <- unique(df2$pathway[df2$padj <= cutoff])
      s3 <- unique(df3$pathway[df3$padj <= cutoff])

      all_pathways <- sort(unique(c(s1, s2, s3)))

      if (length(all_pathways) == 0) {
        summary_df <- data.frame(
          Category = c(
            paste0(n1, " significant pathways"),
            paste0(n2, " significant pathways"),
            paste0(n3, " significant pathways"),
            "Shared by all three",
            paste0("Shared: ", n1, " & ", n2),
            paste0("Shared: ", n1, " & ", n3),
            paste0("Shared: ", n2, " & ", n3),
            paste0("Specific to ", n1),
            paste0("Specific to ", n2),
            paste0("Specific to ", n3)
          ),
          Count = 0,
          stringsAsFactors = FALSE
        )

        pathway_df <- data.frame(
          Pathway = character(0),
          Group = character(0),
          stringsAsFactors = FALSE
        )

        intersection_df <- data.frame(
          Combination = c(
            paste0(n1, " only"),
            paste0(n2, " only"),
            paste0(n3, " only"),
            paste0(n1, " & ", n2),
            paste0(n1, " & ", n3),
            paste0(n2, " & ", n3),
            "All three"
          ),
          Count = c(0, 0, 0, 0, 0, 0, 0),
          stringsAsFactors = FALSE
        )
        intersection_df[[n1]] <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
        intersection_df[[n2]] <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
        intersection_df[[n3]] <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)

        matrix_df <- build_matrix_df(intersection_df, c(n1, n2, n3))

        bubble_df <- data.frame(
          pathway = character(0),
          omics = character(0),
          neglog10_padj = numeric(0),
          stringsAsFactors = FALSE
        )

        return(list(
          summary_df = summary_df,
          pathway_df = pathway_df,
          bubble_df = bubble_df,
          intersection_df = intersection_df,
          matrix_df = matrix_df
        ))
      }

      membership <- data.frame(
        Pathway = all_pathways,
        stringsAsFactors = FALSE
      )
      membership[[n1]] <- membership$Pathway %in% s1
      membership[[n2]] <- membership$Pathway %in% s2
      membership[[n3]] <- membership$Pathway %in% s3

      membership$Combination <- vapply(
        seq_len(nrow(membership)),
        function(i) {
          present <- c(
            membership[[n1]][i],
            membership[[n2]][i],
            membership[[n3]][i]
          )
          active_names <- c(n1, n2, n3)[present]

          if (length(active_names) == 3) {
            "All three"
          } else if (length(active_names) == 2) {
            paste(active_names, collapse = " & ")
          } else if (length(active_names) == 1) {
            paste0(active_names, " only")
          } else {
            NA_character_
          }
        },
        character(1)
      )

      membership <- membership[!is.na(membership$Combination), , drop = FALSE]

      comb_order <- c(
        paste0(n1, " only"),
        paste0(n2, " only"),
        paste0(n3, " only"),
        paste0(n1, " & ", n2),
        paste0(n1, " & ", n3),
        paste0(n2, " & ", n3),
        "All three"
      )

      comb_count <- stats::aggregate(
        Pathway ~ Combination,
        data = membership,
        FUN = length
      )
      colnames(comb_count)[2] <- "Count"

      intersection_df <- merge(
        data.frame(
          Combination = comb_order,
          stringsAsFactors = FALSE
        ),
        comb_count,
        by = "Combination",
        all.x = TRUE,
        sort = FALSE
      )

      intersection_df$Count[is.na(intersection_df$Count)] <- 0
      intersection_df[[n1]] <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE)
      intersection_df[[n2]] <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE)
      intersection_df[[n3]] <- c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)

      shared_all <- membership$Pathway[membership$Combination == "All three"]
      shared_12 <- membership$Pathway[membership$Combination == paste0(n1, " & ", n2)]
      shared_13 <- membership$Pathway[membership$Combination == paste0(n1, " & ", n3)]
      shared_23 <- membership$Pathway[membership$Combination == paste0(n2, " & ", n3)]
      only1 <- membership$Pathway[membership$Combination == paste0(n1, " only")]
      only2 <- membership$Pathway[membership$Combination == paste0(n2, " only")]
      only3 <- membership$Pathway[membership$Combination == paste0(n3, " only")]

      summary_df <- data.frame(
        Category = c(
          paste0(n1, " significant pathways"),
          paste0(n2, " significant pathways"),
          paste0(n3, " significant pathways"),
          "Shared by all three",
          paste0("Shared: ", n1, " & ", n2),
          paste0("Shared: ", n1, " & ", n3),
          paste0("Shared: ", n2, " & ", n3),
          paste0("Specific to ", n1),
          paste0("Specific to ", n2),
          paste0("Specific to ", n3)
        ),
        Count = c(
          length(s1), length(s2), length(s3),
          length(shared_all), length(shared_12), length(shared_13), length(shared_23),
          length(only1), length(only2), length(only3)
        ),
        stringsAsFactors = FALSE
      )

      pathway_df <- data.frame(
        Pathway = c(shared_all, shared_12, shared_13, shared_23, only1, only2, only3),
        Group = c(
          rep("Shared by all three", length(shared_all)),
          rep(paste0("Shared: ", n1, " & ", n2), length(shared_12)),
          rep(paste0("Shared: ", n1, " & ", n3), length(shared_13)),
          rep(paste0("Shared: ", n2, " & ", n3), length(shared_23)),
          rep(paste0("Specific: ", n1), length(only1)),
          rep(paste0("Specific: ", n2), length(only2)),
          rep(paste0("Specific: ", n3), length(only3))
        ),
        stringsAsFactors = FALSE
      )

      bubble_pool <- unique(c(shared_all, shared_12, shared_13, shared_23))

      d1 <- df1[df1$pathway %in% bubble_pool, c("pathway", "padj", "omics"), drop = FALSE]
      d2 <- df2[df2$pathway %in% bubble_pool, c("pathway", "padj", "omics"), drop = FALSE]
      d3 <- df3[df3$pathway %in% bubble_pool, c("pathway", "padj", "omics"), drop = FALSE]

      bubble_df <- rbind(d1, d2, d3)
      if (nrow(bubble_df) > 0) {
        bubble_df$neglog10_padj <- -log10(bubble_df$padj)
        bubble_df <- bubble_df[, c("pathway", "omics", "neglog10_padj"), drop = FALSE]
      } else {
        bubble_df <- data.frame(
          pathway = character(0),
          omics = character(0),
          neglog10_padj = numeric(0),
          stringsAsFactors = FALSE
        )
      }

      matrix_df <- build_matrix_df(intersection_df, c(n1, n2, n3))

      list(
        summary_df = summary_df,
        pathway_df = pathway_df,
        bubble_df = bubble_df,
        intersection_df = intersection_df,
        matrix_df = matrix_df
      )
    }

    observe_file_and_guess <- function(file_input, prefix) {
      shiny::observeEvent(file_input(), {
        req_file <- file_input()
        if (is.null(req_file)) {
          return(NULL)
        }

        df <- tryCatch(
          read_table_flexible(req_file$datapath),
          error = function(e) {
            shiny::showNotification(
              paste("Failed to read uploaded file:", e$message),
              type = "error"
            )
            NULL
          }
        )

        if (is.null(df)) {
          return(NULL)
        }

        rv[[paste0("df", prefix)]] <- df

        cn <- colnames(df)
        guessed_pathway <- guess_pathway_col(df)
        guessed_padj <- guess_padj_col(df)

        shiny::updateSelectInput(
          session,
          paste0("pathway_col_", prefix),
          choices = cn,
          selected = guessed_pathway
        )
        shiny::updateSelectInput(
          session,
          paste0("padj_col_", prefix),
          choices = cn,
          selected = guessed_padj
        )
      }, ignoreNULL = TRUE)
    }

    observe_file_and_guess(shiny::reactive(input$file1), "1")
    observe_file_and_guess(shiny::reactive(input$file2), "2")
    observe_file_and_guess(shiny::reactive(input$file3), "3")

    output$mapping_ui_1 <- shiny::renderUI({
      df <- rv$df1
      cn <- if (!is.null(df)) colnames(df) else character(0)

      selected_pathway <- NULL
      selected_padj <- NULL

      if (!is.null(df) && length(cn) > 0) {
        selected_pathway <- guess_pathway_col(df)
        selected_padj <- guess_padj_col(df)
      }

      shiny::tagList(
        shiny::h5(input$omics1_name),
        shiny::selectInput(
          inputId = ns("pathway_col_1"),
          label = "Pathway column",
          choices = cn,
          selected = selected_pathway
        ),
        shiny::selectInput(
          inputId = ns("padj_col_1"),
          label = "Adjusted p-value column",
          choices = cn,
          selected = selected_padj
        )
      )
    })

    output$mapping_ui_2 <- shiny::renderUI({
      df <- rv$df2
      cn <- if (!is.null(df)) colnames(df) else character(0)

      selected_pathway <- NULL
      selected_padj <- NULL

      if (!is.null(df) && length(cn) > 0) {
        selected_pathway <- guess_pathway_col(df)
        selected_padj <- guess_padj_col(df)
      }

      shiny::tagList(
        shiny::h5(input$omics2_name),
        shiny::selectInput(
          inputId = ns("pathway_col_2"),
          label = "Pathway column",
          choices = cn,
          selected = selected_pathway
        ),
        shiny::selectInput(
          inputId = ns("padj_col_2"),
          label = "Adjusted p-value column",
          choices = cn,
          selected = selected_padj
        )
      )
    })

    output$mapping_ui_3 <- shiny::renderUI({
      df <- rv$df3
      cn <- if (!is.null(df)) colnames(df) else character(0)

      selected_pathway <- NULL
      selected_padj <- NULL

      if (!is.null(df) && length(cn) > 0) {
        selected_pathway <- guess_pathway_col(df)
        selected_padj <- guess_padj_col(df)
      }

      shiny::tagList(
        shiny::h5(input$omics3_name),
        shiny::selectInput(
          inputId = ns("pathway_col_3"),
          label = "Pathway column",
          choices = cn,
          selected = selected_pathway
        ),
        shiny::selectInput(
          inputId = ns("padj_col_3"),
          label = "Adjusted p-value column",
          choices = cn,
          selected = selected_padj
        )
      )
    })

    output$group_filter_ui <- shiny::renderUI({
      choices <- "All groups"

      if (!is.null(rv$pathway_df) && nrow(rv$pathway_df) > 0) {
        g <- unique(rv$pathway_df$Group)

        if (input$pathway_filter == "shared") {
          g <- g[!grepl("^Specific: ", g)]
        } else if (input$pathway_filter == "specific") {
          g <- g[grepl("^Specific: ", g)]
        }

        choices <- c("All groups", g)
      }

      shiny::selectInput(
        inputId = ns("group_filter"),
        label = "Group Filter",
        choices = choices,
        selected = "All groups"
      )
    })

    filter_pathway_df <- function(pathway_df, pathway_filter, group_filter) {
      if (is.null(pathway_df) || nrow(pathway_df) == 0) {
        return(pathway_df)
      }

      pathway_df <- add_type_column(pathway_df)

      if (pathway_filter == "shared") {
        pathway_df <- pathway_df[pathway_df$Type == "Shared", , drop = FALSE]
      } else if (pathway_filter == "specific") {
        pathway_df <- pathway_df[pathway_df$Type == "Specific", , drop = FALSE]
      }

      if (!is.null(group_filter) && group_filter != "All groups") {
        pathway_df <- pathway_df[pathway_df$Group == group_filter, , drop = FALSE]
      }

      rownames(pathway_df) <- NULL
      pathway_df
    }

    filtered_pathway_df <- shiny::reactive({
      filter_pathway_df(rv$pathway_df, input$pathway_filter, input$group_filter)
    })

    filtered_summary_df <- shiny::reactive({
      df <- rv$summary_df

      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }

      if (input$pathway_filter == "shared") {
        df <- df[grepl("^Shared", df$Category), , drop = FALSE]
      } else if (input$pathway_filter == "specific") {
        df <- df[grepl("^Specific", df$Category), , drop = FALSE]
      }

      if (!is.null(input$group_filter) && input$group_filter != "All groups") {
        target <- input$group_filter

        if (target == "Shared") {
          df <- df[df$Category %in% c("Shared pathways"), , drop = FALSE]
        } else if (target == "Shared by all three") {
          df <- df[df$Category %in% c("Shared by all three"), , drop = FALSE]
        } else if (grepl("^Shared: ", target)) {
          df <- df[df$Category == target, , drop = FALSE]
        } else if (grepl("^Specific: ", target)) {
          target2 <- sub("^Specific: ", "Specific to ", target)
          df <- df[df$Category == target2, , drop = FALSE]
        }
      }

      if (isTRUE(input$sort_desc) && nrow(df) > 0) {
        df <- df[order(df$Count, decreasing = TRUE), , drop = FALSE]
      }

      rownames(df) <- NULL
      df
    })

    filtered_intersection_df <- shiny::reactive({
      df <- rv$intersection_df

      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }

      if (input$pathway_filter == "shared") {
        df <- df[!grepl(" only$", df$Combination), , drop = FALSE]
      } else if (input$pathway_filter == "specific") {
        df <- df[grepl(" only$", df$Combination), , drop = FALSE]
      }

      if (!is.null(input$group_filter) && input$group_filter != "All groups") {
        target <- input$group_filter

        if (target == "Shared") {
          df <- df[df$Combination == "Shared", , drop = FALSE]
        } else if (target == "Shared by all three") {
          df <- df[df$Combination == "All three", , drop = FALSE]
        } else if (grepl("^Specific: ", target)) {
          target2 <- sub("^Specific: ", "", target)
          df <- df[df$Combination == paste0(target2, " only"), , drop = FALSE]
        } else {
          df <- df[df$Combination == target, , drop = FALSE]
        }
      }

      rownames(df) <- NULL
      df
    })

    filtered_matrix_df <- shiny::reactive({
      mdf <- rv$matrix_df
      idf <- filtered_intersection_df()

      if (is.null(mdf) || is.null(idf) || nrow(mdf) == 0 || nrow(idf) == 0) {
        return(data.frame(
          Combination = character(0),
          Omics = character(0),
          Present = logical(0),
          Count = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      mdf <- mdf[mdf$Combination %in% idf$Combination, , drop = FALSE]
      rownames(mdf) <- NULL
      mdf
    })

    filtered_bubble_df <- shiny::reactive({
      df <- rv$bubble_df
      ptab <- filtered_pathway_df()

      if (is.null(df) || nrow(df) == 0 || is.null(ptab) || nrow(ptab) == 0) {
        return(data.frame(
          pathway = character(0),
          omics = character(0),
          neglog10_padj = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      keep_pathways <- unique(ptab$Pathway)
      df <- df[df$pathway %in% keep_pathways, , drop = FALSE]

      if (nrow(df) == 0) {
        return(df)
      }

      rank_df <- stats::aggregate(
        neglog10_padj ~ pathway,
        data = df,
        FUN = max
      )

      rank_df <- rank_df[order(rank_df$neglog10_padj, decreasing = TRUE), , drop = FALSE]
      keep_n <- min(input$top_n, nrow(rank_df))
      keep_pathways2 <- rank_df$pathway[seq_len(keep_n)]

      df <- df[df$pathway %in% keep_pathways2, , drop = FALSE]
      rownames(df) <- NULL
      df
    })

    summary_plot_obj <- shiny::reactive({
      df <- filtered_summary_df()

      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      fill_color <- rep(input$color_shared, nrow(df))

      fill_color[df$Category == paste0("Specific to ", input$omics1_name)] <- input$color_specific1
      fill_color[df$Category == paste0("Specific to ", input$omics2_name)] <- input$color_specific2

      if (!is.null(input$omics3_name) && nzchar(input$omics3_name) && !is.null(input$color_specific3)) {
        fill_color[df$Category == paste0("Specific to ", input$omics3_name)] <- input$color_specific3
      }

      fill_color[grepl("^Shared: ", df$Category)] <- input$color_pairwise
      fill_color[df$Category %in% c("Shared pathways", "Shared by all three")] <- input$color_shared

      df$fill_color <- fill_color
      df$Category <- factor(df$Category, levels = df$Category)

      p <- ggplot2::ggplot(
        df,
        ggplot2::aes(x = Category, y = Count, fill = fill_color)
      ) +
        ggplot2::geom_col(width = 0.75, show.legend = FALSE) +
        ggplot2::scale_fill_identity() +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold")
        ) +
        ggplot2::labs(
          x = NULL,
          y = "Pathway Count",
          title = "Co-enrichment Summary"
        )

      if (isTRUE(input$show_value_label)) {
        p <- p + ggplot2::geom_text(
          ggplot2::aes(label = Count),
          vjust = -0.25,
          size = 4
        )
      }

      p
    })

    upset_plot_obj <- shiny::reactive({
      idf <- filtered_intersection_df()
      mdf <- filtered_matrix_df()

      if (is.null(idf) || nrow(idf) == 0 || is.null(mdf) || nrow(mdf) == 0) {
        return(NULL)
      }

      omics_cols <- setdiff(colnames(idf), c("Combination", "Count"))
      combo_levels <- as.character(idf$Combination)

      idf$Combination <- factor(as.character(idf$Combination), levels = combo_levels)
      mdf$Combination <- factor(as.character(mdf$Combination), levels = combo_levels)
      mdf$Omics <- factor(as.character(mdf$Omics), levels = rev(omics_cols))
      mdf$y_num <- as.numeric(mdf$Omics)

      bar_color <- rep(input$color_shared, nrow(idf))

      only_omics1 <- paste0(input$omics1_name, " only")
      only_omics2 <- paste0(input$omics2_name, " only")
      only_omics3 <- paste0(input$omics3_name, " only")

      bar_color[as.character(idf$Combination) == only_omics1] <- input$color_specific1
      bar_color[as.character(idf$Combination) == only_omics2] <- input$color_specific2

      if (!is.null(input$color_specific3) && input$omics_mode == "three") {
        bar_color[as.character(idf$Combination) == only_omics3] <- input$color_specific3
      }

      bar_color[grepl(" & ", as.character(idf$Combination))] <- input$color_pairwise
      bar_color[as.character(idf$Combination) %in% c("Shared", "All three")] <- input$color_shared
      idf$bar_color <- bar_color

      line_df_raw <- mdf[mdf$Present, , drop = FALSE]

      if (nrow(line_df_raw) > 0) {
        line_df <- data.frame(
          Combination = unique(as.character(line_df_raw$Combination)),
          ymin = NA_real_,
          ymax = NA_real_,
          stringsAsFactors = FALSE
        )

        for (i in seq_len(nrow(line_df))) {
          tmp_y <- line_df_raw$y_num[as.character(line_df_raw$Combination) == line_df$Combination[i]]
          line_df$ymin[i] <- min(tmp_y, na.rm = TRUE)
          line_df$ymax[i] <- max(tmp_y, na.rm = TRUE)
        }

        line_df$Combination <- factor(line_df$Combination, levels = combo_levels)
      } else {
        line_df <- data.frame(
          Combination = factor(character(0), levels = combo_levels),
          ymin = numeric(0),
          ymax = numeric(0),
          stringsAsFactors = FALSE
        )
      }

      point_df <- mdf
      point_df$point_color <- ifelse(
        point_df$Present,
        input$color_upset_active,
        input$color_upset_inactive
      )

      max_count <- max(idf$Count, na.rm = TRUE)
      if (!is.finite(max_count)) {
        max_count <- 1
      }
      if (max_count <= 0) {
        max_count <- 1
      }

      matrix_top <- max_count + max(1, max_count * 0.25)
      point_df$y_plot <- matrix_top + point_df$y_num - 1

      if (nrow(line_df) > 0) {
        line_df$ymin_plot <- matrix_top + line_df$ymin - 1
        line_df$ymax_plot <- matrix_top + line_df$ymax - 1
      }

      axis_break_counts <- pretty(c(0, max_count), n = 4)
      axis_break_counts <- axis_break_counts[axis_break_counts >= 0]

      axis_breaks <- c(
        axis_break_counts,
        matrix_top + seq_len(length(omics_cols)) - 1
      )
      axis_labels <- c(
        axis_break_counts,
        rev(omics_cols)
      )

      p <- ggplot2::ggplot() +
        ggplot2::geom_col(
          data = idf,
          ggplot2::aes(x = Combination, y = Count),
          fill = idf$bar_color,
          width = 0.72
        )

      if (nrow(line_df) > 0) {
        p <- p + ggplot2::geom_segment(
          data = line_df,
          ggplot2::aes(
            x = Combination,
            xend = Combination,
            y = ymin_plot,
            yend = ymax_plot
          ),
          linewidth = 0.8,
          color = input$color_upset_active
        )
      }

      p <- p +
        ggplot2::geom_point(
          data = point_df,
          ggplot2::aes(
            x = Combination,
            y = y_plot
          ),
          size = 3.5,
          color = point_df$point_color
        ) +
        ggplot2::scale_y_continuous(
          breaks = axis_breaks,
          labels = axis_labels,
          expand = ggplot2::expansion(mult = c(0.02, 0.08))
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
          plot.title = ggplot2::element_text(face = "bold")
        ) +
        ggplot2::labs(
          x = NULL,
          y = NULL,
          title = "Matrix-style UpSet Plot"
        )

      if (isTRUE(input$show_value_label)) {
        p <- p + ggplot2::geom_text(
          data = idf,
          ggplot2::aes(
            x = Combination,
            y = Count,
            label = Count
          ),
          vjust = -0.3,
          size = 4
        )
      }

      p
    })

    bubble_plot_obj <- shiny::reactive({
      plot_df <- filtered_bubble_df()

      if (is.null(plot_df) || nrow(plot_df) == 0) {
        return(NULL)
      }

      pathway_order <- stats::aggregate(
        neglog10_padj ~ pathway,
        data = plot_df,
        FUN = max
      )

      pathway_order <- pathway_order[order(pathway_order$neglog10_padj, decreasing = TRUE), , drop = FALSE]
      plot_df$pathway <- factor(plot_df$pathway, levels = rev(pathway_order$pathway))

      p <- ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = omics,
          y = pathway,
          size = neglog10_padj,
          color = neglog10_padj
        )
      ) +
        ggplot2::geom_point(alpha = 0.9) +
        ggplot2::scale_color_gradient(
          low = input$bubble_low,
          high = input$bubble_high
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(
          panel.grid.minor = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold")
        ) +
        ggplot2::labs(
          x = NULL,
          y = NULL,
          size = expression(-log[10]("adj.P")),
          color = expression(-log[10]("adj.P")),
          title = "Shared Pathway Significance Across Omics"
        )

      p
    })

    output$summary_note <- shiny::renderUI({
      if (is.null(rv$summary_df)) {
        return(
          shiny::div(
            style = "padding: 12px; color: #666;",
            "Click 'Run' to generate co-enrichment summary."
          )
        )
      }

      shiny::div(
        style = "padding-bottom: 8px; color: #666;",
        paste0(
          "Adjusted p-value cutoff = ",
          input$padj_cutoff,
          "; display mode = ",
          switch(
            input$pathway_filter,
            all = "All",
            shared = "Shared only",
            specific = "Specific only"
          ),
          "; group = ",
          ifelse(is.null(input$group_filter), "All groups", input$group_filter),
          "; Top N = ",
          input$top_n,
          "."
        )
      )
    })

    output$summary_table <- DT::renderDT({
      df <- filtered_summary_df()

      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Message = "No results available. Please upload files and click Run."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        df,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        rownames = FALSE
      )
    })

    output$shared_table <- DT::renderDT({
      df <- filtered_pathway_df()

      if (is.null(df) || nrow(df) == 0) {
        return(
          DT::datatable(
            data.frame(Message = "No pathway results available. Please click Run."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        df,
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 15,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        rownames = FALSE
      )
    })

    output$preview1 <- DT::renderDT({
      if (is.null(rv$df1)) {
        return(
          DT::datatable(
            data.frame(Message = "Upload Omics 1 enrichment file to preview."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        utils::head(rv$df1, 10),
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        rownames = FALSE
      )
    })

    output$preview2 <- DT::renderDT({
      if (is.null(rv$df2)) {
        return(
          DT::datatable(
            data.frame(Message = "Upload Omics 2 enrichment file to preview."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        utils::head(rv$df2, 10),
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        rownames = FALSE
      )
    })

    output$preview3 <- DT::renderDT({
      if (input$omics_mode != "three") {
        return(
          DT::datatable(
            data.frame(Message = "Three-omics mode is not selected."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      if (is.null(rv$df3)) {
        return(
          DT::datatable(
            data.frame(Message = "Upload Omics 3 enrichment file to preview."),
            options = list(dom = "t"),
            rownames = FALSE
          )
        )
      }

      DT::datatable(
        utils::head(rv$df3, 10),
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          pageLength = 10,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        ),
        rownames = FALSE
      )
    })

    shiny::observeEvent(input$run_analysis, {
      tryCatch({
        shiny::req(rv$df1, rv$df2)

        if (is.null(input$pathway_col_1) || is.null(input$padj_col_1)) {
          stop("Please select pathway and adjusted p-value columns for Omics 1.")
        }

        if (is.null(input$pathway_col_2) || is.null(input$padj_col_2)) {
          stop("Please select pathway and adjusted p-value columns for Omics 2.")
        }

        if (input$omics_mode == "three") {
          if (is.null(rv$df3)) {
            stop("Three-omics mode requires three uploaded enrichment files.")
          }
          if (is.null(input$pathway_col_3) || is.null(input$padj_col_3)) {
            stop("Please select pathway and adjusted p-value columns for Omics 3.")
          }
        }

        std1 <- make_standard_df(
          df = rv$df1,
          pathway_col = input$pathway_col_1,
          padj_col = input$padj_col_1,
          omics_name = input$omics1_name,
          keep_unique_only = isTRUE(input$keep_unique_only)
        )

        std2 <- make_standard_df(
          df = rv$df2,
          pathway_col = input$pathway_col_2,
          padj_col = input$padj_col_2,
          omics_name = input$omics2_name,
          keep_unique_only = isTRUE(input$keep_unique_only)
        )

        if (input$omics_mode == "two") {
          res <- compute_two_omics(std1, std2, input$padj_cutoff)
        } else {
          std3 <- make_standard_df(
            df = rv$df3,
            pathway_col = input$pathway_col_3,
            padj_col = input$padj_col_3,
            omics_name = input$omics3_name,
            keep_unique_only = isTRUE(input$keep_unique_only)
          )

          res <- compute_three_omics(std1, std2, std3, input$padj_cutoff)
        }

        rv$summary_df <- res$summary_df
        rv$pathway_df <- res$pathway_df
        rv$bubble_df <- res$bubble_df
        rv$intersection_df <- res$intersection_df
        rv$matrix_df <- res$matrix_df

        shiny::showNotification(
          "Co-enrichment analysis completed successfully.",
          type = "message"
        )
      }, error = function(e) {
        rv$summary_df <- NULL
        rv$pathway_df <- NULL
        rv$bubble_df <- NULL
        rv$intersection_df <- NULL
        rv$matrix_df <- NULL

        shiny::showNotification(
          paste("Analysis failed:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    output$summary_plot <- shiny::renderPlot({
      p <- summary_plot_obj()

      if (is.null(p)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Click 'Run' to generate summary plot.")
        return()
      }

      print(p)
    })

    output$upset_plot <- shiny::renderPlot({
      p <- upset_plot_obj()

      if (is.null(p)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Click 'Run' to generate matrix-style UpSet plot.")
        return()
      }

      print(p)
    })

    output$bubble_plot <- shiny::renderPlot({
      p <- bubble_plot_obj()

      if (is.null(p)) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "No shared/significant pathway data available for bubble plot.")
        return()
      }

      print(p)
    })

    save_plot_pdf <- function(plot_obj, file) {
      ggplot2::ggsave(
        filename = file,
        plot = plot_obj,
        width = input$plot_width,
        height = input$plot_height,
        units = "in",
        device = grDevices::cairo_pdf
      )
    }

    save_plot_png <- function(plot_obj, file) {
      ggplot2::ggsave(
        filename = file,
        plot = plot_obj,
        width = input$plot_width,
        height = input$plot_height,
        units = "in",
        dpi = input$plot_dpi
      )
    }

    output$download_summary <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_summary_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- filtered_summary_df()
        if (is.null(df) || nrow(df) == 0) {
          utils::write.csv(data.frame(Message = "No results available."), file, row.names = FALSE)
        } else {
          utils::write.csv(df, file, row.names = FALSE)
        }
      }
    )

    output$download_shared <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_pathway_table_", Sys.Date(), ".csv")
      },
      content = function(file) {
        df <- filtered_pathway_df()
        if (is.null(df) || nrow(df) == 0) {
          utils::write.csv(data.frame(Message = "No results available."), file, row.names = FALSE)
        } else {
          utils::write.csv(df, file, row.names = FALSE)
        }
      }
    )

    output$download_summary_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_summary_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- summary_plot_obj()
        if (is.null(p)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No data")
        }
        save_plot_pdf(p, file)
      }
    )

    output$download_summary_png <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_summary_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- summary_plot_obj()
        if (is.null(p)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No data")
        }
        save_plot_png(p, file)
      }
    )

    output$download_upset_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_upset_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- upset_plot_obj()
        if (is.null(p)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No data")
        }
        save_plot_pdf(p, file)
      }
    )

    output$download_upset_png <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_upset_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- upset_plot_obj()
        if (is.null(p)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No data")
        }
        save_plot_png(p, file)
      }
    )

    output$download_bubble_pdf <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_bubble_plot_", Sys.Date(), ".pdf")
      },
      content = function(file) {
        p <- bubble_plot_obj()
        if (is.null(p)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No data")
        }
        save_plot_pdf(p, file)
      }
    )

    output$download_bubble_png <- shiny::downloadHandler(
      filename = function() {
        paste0("co_enrichment_bubble_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- bubble_plot_obj()
        if (is.null(p)) {
          p <- ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No data")
        }
        save_plot_png(p, file)
      }
    )

    output$download_two_demo <- shiny::downloadHandler(
      filename = function() {
        paste0("two_omics_co_enrichment_demo_", Sys.Date(), ".zip")
      },
      content = function(file) {
        td <- tempdir()

        demo1 <- data.frame(
          Pathway = c(
            "Plant hormone signal transduction",
            "MAPK signaling pathway",
            "Phenylpropanoid biosynthesis",
            "Carbon metabolism",
            "Glutathione metabolism",
            "Starch and sucrose metabolism",
            "Biosynthesis of amino acids"
          ),
          p.adjust = c(0.001, 0.004, 0.007, 0.012, 0.018, 0.043, 0.071),
          Count = c(15, 12, 10, 13, 9, 11, 8),
          stringsAsFactors = FALSE
        )

        demo2 <- data.frame(
          Pathway = c(
            "Plant hormone signal transduction",
            "MAPK signaling pathway",
            "Phenylpropanoid biosynthesis",
            "Proteasome",
            "Ribosome",
            "Carbon metabolism",
            "Peroxisome"
          ),
          p.adjust = c(0.002, 0.006, 0.010, 0.013, 0.020, 0.030, 0.080),
          Count = c(13, 9, 8, 10, 14, 12, 6),
          stringsAsFactors = FALSE
        )

        f1 <- file.path(td, "Transcriptome_demo.csv")
        f2 <- file.path(td, "Proteome_demo.csv")

        utils::write.csv(demo1, f1, row.names = FALSE)
        utils::write.csv(demo2, f2, row.names = FALSE)

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(td)

        utils::zip(
          zipfile = file,
          files = c("Transcriptome_demo.csv", "Proteome_demo.csv")
        )
      }
    )

    output$download_three_demo <- shiny::downloadHandler(
      filename = function() {
        paste0("three_omics_co_enrichment_demo_", Sys.Date(), ".zip")
      },
      content = function(file) {
        td <- tempdir()

        demo1 <- data.frame(
          Pathway = c(
            "Plant hormone signal transduction",
            "MAPK signaling pathway",
            "Phenylpropanoid biosynthesis",
            "Carbon metabolism",
            "Glutathione metabolism",
            "Starch and sucrose metabolism",
            "Biosynthesis of amino acids"
          ),
          p.adjust = c(0.001, 0.004, 0.007, 0.012, 0.018, 0.043, 0.071),
          Count = c(15, 12, 10, 13, 9, 11, 8),
          stringsAsFactors = FALSE
        )

        demo2 <- data.frame(
          Pathway = c(
            "Plant hormone signal transduction",
            "MAPK signaling pathway",
            "Phenylpropanoid biosynthesis",
            "Proteasome",
            "Ribosome",
            "Carbon metabolism",
            "Peroxisome"
          ),
          p.adjust = c(0.002, 0.006, 0.010, 0.013, 0.020, 0.030, 0.080),
          Count = c(13, 9, 8, 10, 14, 12, 6),
          stringsAsFactors = FALSE
        )

        demo3 <- data.frame(
          Pathway = c(
            "Plant hormone signal transduction",
            "Phenylpropanoid biosynthesis",
            "Carbon metabolism",
            "Flavonoid biosynthesis",
            "Glutathione metabolism",
            "ABC transporters",
            "MAPK signaling pathway"
          ),
          p.adjust = c(0.003, 0.005, 0.009, 0.016, 0.024, 0.031, 0.049),
          Count = c(11, 10, 12, 8, 7, 9, 6),
          stringsAsFactors = FALSE
        )

        f1 <- file.path(td, "Transcriptome_demo.csv")
        f2 <- file.path(td, "Proteome_demo.csv")
        f3 <- file.path(td, "Metabolome_demo.csv")

        utils::write.csv(demo1, f1, row.names = FALSE)
        utils::write.csv(demo2, f2, row.names = FALSE)
        utils::write.csv(demo3, f3, row.names = FALSE)

        old_wd <- getwd()
        on.exit(setwd(old_wd), add = TRUE)
        setwd(td)

        utils::zip(
          zipfile = file,
          files = c("Transcriptome_demo.csv", "Proteome_demo.csv", "Metabolome_demo.csv")
        )
      }
    )
  })
}
