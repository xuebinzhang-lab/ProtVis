#' Protein Structure Analysis UI Module
#'
#' Creates a user interface for protein secondary structure analysis using
#' bio3d package to perform Normal Mode Analysis (NMA) on PDB files.
#'
#' @title Protein Structure UI
#' @param id A unique identifier for the Shiny namespace
#' @import shiny
#' @import bslib
#' @name protein_structure_ui
#' @export
protein_structure_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 320,
        open = TRUE,
        bslib::accordion(
          open = c("File Upload", "Demo Data", "Parameters"),

          bslib::accordion_panel(
            title = "File Upload",
            icon = bsicons::bs_icon("upload"),
            shiny::fileInput(
              inputId = ns("file"),
              label = "Upload PDB file",
              multiple = FALSE,
              accept = c(".pdb")
            ),
            shiny::div(
              class = "text-muted",
              style = "font-size: 12px;",
              "Supported format: .pdb"
            )
          ),

          bslib::accordion_panel(
            title = "Demo Data",
            icon = bsicons::bs_icon("database"),
            shiny::actionButton(
              inputId = ns("use_demo"),
              label = "Use Demo PDB",
              class = "btn-outline-primary",
              width = "100%"
            ),
            shiny::br(),
            shiny::br(),
            shiny::downloadButton(
              outputId = ns("download_demo"),
              label = "Download Demo PDB"
            ),
            shiny::br(),
            shiny::br(),
            shiny::uiOutput(ns("demo_status")),
            shiny::br(),
            shiny::uiOutput(ns("active_source_ui"))
          ),

          bslib::accordion_panel(
            title = "Parameters",
            icon = bsicons::bs_icon("sliders"),
            colourpicker::colourInput(
              inputId = ns("sheet_color"),
              label = "Beta sheet color",
              value = "#E69F00"
            ),
            colourpicker::colourInput(
              inputId = ns("helix_color"),
              label = "Alpha helix color",
              value = "#7B61FF"
            ),
            shiny::numericInput(
              inputId = ns("line_width"),
              label = "Line width",
              value = 2.5,
              min = 0.5,
              max = 6,
              step = 0.5
            )
          ),

          bslib::accordion_panel(
            title = "Run",
            icon = bsicons::bs_icon("play-circle"),
            shiny::actionButton(
              inputId = ns("run"),
              label = "Run",
              class = "btn-primary",
              width = "100%"
            )
          ),

          bslib::accordion_panel(
            title = "Download",
            icon = bsicons::bs_icon("download"),
            shiny::numericInput(
              inputId = ns("plot_width"),
              label = "Plot width",
              value = 10,
              min = 5,
              max = 20,
              step = 0.5
            ),
            shiny::numericInput(
              inputId = ns("plot_height"),
              label = "Plot height",
              value = 6,
              min = 4,
              max = 15,
              step = 0.5
            ),
            shiny::selectInput(
              inputId = ns("plot_units"),
              label = "Plot units",
              choices = c("inches" = "in", "centimeters" = "cm", "millimeters" = "mm"),
              selected = "in"
            ),
            shiny::numericInput(
              inputId = ns("plot_dpi"),
              label = "Resolution (DPI)",
              value = 300,
              min = 72,
              max = 1200,
              step = 1
            ),
            shiny::selectInput(
              inputId = ns("plot_format"),
              label = "File format",
              choices = c(
                "PDF" = "pdf",
                "PNG" = "png",
                "JPEG" = "jpeg",
                "TIFF" = "tiff",
                "SVG" = "svg"
              ),
              selected = "pdf"
            ),
            shiny::downloadButton(ns("downloadPlot"), "Download"),
            shiny::div(
              style = "font-size: 12px; color: #6c757d; margin-top: 8px;",
              "PDF/SVG support vector output; PNG/JPEG/TIFF are raster formats."
            )
          )
        )
      ),

      bslib::page_fluid(
        bslib::layout_column_wrap(
          width = 1/2,

          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Secondary Structure Plot"),
            bslib::card_body(
              style = "min-height: 620px;",
              shiny::uiOutput(ns("plot_ui"))
            )
          ),

          bslib::card(
            full_screen = TRUE,
            bslib::card_header("Structure Summary"),
            bslib::card_body(
              style = "min-height: 620px;",
              DT::DTOutput(ns("summary_table"))
            )
          )
        )
      )
    )
}

#' Protein Structure Analysis Server Module
#'
#' Server-side logic for protein secondary structure analysis. Performs
#' Normal Mode Analysis (NMA) on uploaded PDB files and generates fluctuation
#' plots with secondary structure elements highlighted.
#'
#' @description Server logic for protein_structure module
#' @param id Standard shiny server identifier
#' @return A Shiny server module function
#' @import shiny
#' @name protein_structure_server
#' @export
protein_structure_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    if (!base::exists("elements") || !base::exists("atom.index")) {
      utils::data(elements, package = "bio3d", envir = environment())
      utils::data(atom.index, package = "bio3d", envir = environment())
    }

    rv <- shiny::reactiveValues(
      pdb = NULL,
      modes = NULL,
      summary = NULL,
      analysis_done = FALSE,
      use_demo = FALSE,
      active_file_path = NULL,
      active_file_name = NULL,
      active_source = "None selected"
    )

    demo_pdb_path <- system.file("extdata", "1hel.pdb", package = "ProtVis")

    output$demo_status <- shiny::renderUI({
      if (nzchar(demo_pdb_path) && file.exists(demo_pdb_path)) {
        shiny::div(
          style = "font-size: 12px; color: #6c757d;",
          "Built-in demo file: 1hel.pdb"
        )
      } else {
        shiny::div(
          style = "font-size: 12px; color: #dc3545;",
          "Demo PDB not found: inst/extdata/1hel.pdb"
        )
      }
    })

    output$active_source_ui <- shiny::renderUI({
      source_text <- if (is.null(rv$active_source)) "None selected" else rv$active_source
      file_text <- if (is.null(rv$active_file_name)) "No file selected" else rv$active_file_name

      shiny::div(
        style = "padding: 10px 12px; background: #f8f9fa; border-radius: 10px; border: 1px solid #e9ecef;",
        shiny::div(
          style = "font-size: 12px; color: #6c757d; margin-bottom: 4px;",
          "Current source"
        ),
        shiny::div(
          style = "font-weight: 600; color: #212529;",
          source_text
        ),
        shiny::div(
          style = "font-size: 12px; color: #6c757d; margin-top: 6px;",
          file_text
        )
      )
    })

    shiny::observe({
      shinyjs::toggleState(id = "downloadPlot", condition = isTRUE(rv$analysis_done))
    })

    shiny::observeEvent(input$use_demo, {
      if (!nzchar(demo_pdb_path) || !file.exists(demo_pdb_path)) {
        shiny::showNotification(
          "Demo PDB file was not found. Please add inst/extdata/1hel.pdb",
          type = "error",
          duration = 5
        )
        return(NULL)
      }

      rv$use_demo <- TRUE
      rv$active_file_path <- demo_pdb_path
      rv$active_file_name <- "1hel.pdb"
      rv$active_source <- "Demo file"

      shiny::showNotification(
        "Demo PDB selected. Click Run to start analysis.",
        type = "message",
        duration = 3
      )
    })

    shiny::observeEvent(input$file, {
      shiny::req(input$file)

      file_ext <- tools::file_ext(input$file$name)
      if (!tolower(file_ext) %in% c("pdb")) {
        shiny::showNotification(
          "Please upload a valid .pdb file.",
          type = "error",
          duration = 4
        )
        return(NULL)
      }

      rv$use_demo <- FALSE
      rv$active_file_path <- input$file$datapath
      rv$active_file_name <- input$file$name
      rv$active_source <- "Uploaded file"

      shiny::showNotification(
        "Uploaded PDB selected. Click Run to start analysis.",
        type = "message",
        duration = 3
      )
    })

    output$plot_ui <- shiny::renderUI({
      if (isTRUE(rv$analysis_done)) {
        shiny::plotOutput(session$ns("protein_plot"), height = "560px")
      } else {
        bslib::card(
          class = "border-0 shadow-sm",
          style = "margin-top: 10px;",
          bslib::card_body(
            style = "display:flex; align-items:center; justify-content:center; height:560px;",
            shiny::div(
              style = "text-align:center; color:#6c757d;",
              bsicons::bs_icon("bar-chart-line", size = "2em"),
              shiny::h5("No analysis result available", style = "margin-top: 12px;"),
              shiny::p("Upload a PDB file or use the demo PDB, then click Run.")
            )
          )
        )
      }
    })

    output$protein_plot <- shiny::renderPlot({
      shiny::req(rv$analysis_done, rv$modes, rv$pdb)

      if (!base::is.null(rv$modes$fluctuations)) {
        bio3d::plot.bio3d(
          rv$modes$fluctuations,
          sse = rv$pdb,
          sheet.col = input$sheet_color,
          helix.col = input$helix_color,
          typ = "l",
          lwd = input$line_width,
          xlab = "Residue Index",
          ylab = expression("Fluctuations from NMA (" * ring(A) * ")")
        )
        graphics::title(
          main = "Protein Secondary Structure Analysis",
          sub = paste("Source:", rv$active_source)
        )
      } else {
        graphics::plot.new()
        graphics::text(
          0.5, 0.5,
          "No fluctuation data available",
          cex = 1.2,
          col = "red"
        )
      }
    })

    shiny::observeEvent(input$run, {
      if (is.null(rv$active_file_path) || !nzchar(rv$active_file_path)) {
        shiny::showNotification(
          "Please upload a PDB file or choose the demo PDB first.",
          type = "warning",
          duration = 4
        )
        return(NULL)
      }

      rv$analysis_done <- FALSE
      rv$pdb <- NULL
      rv$modes <- NULL
      rv$summary <- NULL

      shiny::showNotification("Reading PDB file...", type = "message", duration = 2)

      tryCatch({
        pdb <- bio3d::read.pdb(rv$active_file_path)

        shiny::showNotification(
          "Performing Normal Mode Analysis...",
          type = "message",
          duration = 2
        )

        modes <- bio3d::nma(pdb)

        residue_count <- NA_integer_
        atom_count <- NA_integer_
        chain_count <- NA_integer_
        sse_detected <- "No"

        if (!base::is.null(pdb$atom)) {
          atom_count <- nrow(pdb$atom)

          if ("resno" %in% colnames(pdb$atom)) {
            residue_count <- length(unique(pdb$atom[, "resno"]))
          }

          if ("chain" %in% colnames(pdb$atom)) {
            chain_vals <- unique(stats::na.omit(pdb$atom[, "chain"]))
            chain_count <- length(chain_vals)
          }
        }

        if (!base::is.null(pdb$helix) || !base::is.null(pdb$sheet)) {
          sse_detected <- "Yes"
        }

        mode_count <- NA_integer_
        if (!base::is.null(modes$modes)) {
          mode_count <- ncol(modes$modes)
        }

        rv$pdb <- pdb
        rv$modes <- modes
        rv$summary <- data.frame(
          Item = c(
            "Source",
            "File name",
            "Number of atoms",
            "Number of residues",
            "Number of chains",
            "Secondary structure detected",
            "NMA mode count"
          ),
          Value = c(
            rv$active_source,
            rv$active_file_name,
            atom_count,
            residue_count,
            chain_count,
            sse_detected,
            mode_count
          ),
          stringsAsFactors = FALSE
        )
        rv$analysis_done <- TRUE

        shiny::showNotification(
          "Analysis completed successfully!",
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        rv$analysis_done <- FALSE
        rv$pdb <- NULL
        rv$modes <- NULL
        rv$summary <- data.frame(
          Item = c("Source", "File name", "Error"),
          Value = c(
            if (is.null(rv$active_source)) NA_character_ else rv$active_source,
            if (is.null(rv$active_file_name)) NA_character_ else rv$active_file_name,
            e$message
          ),
          stringsAsFactors = FALSE
        )

        shiny::showNotification(
          paste("Error:", e$message),
          type = "error",
          duration = 6
        )
      })
    })

    output$summary_table <- DT::renderDT({
      if (isFALSE(rv$analysis_done) || is.null(rv$summary)) {
        return(
          DT::datatable(
            data.frame(
              Item = "Status",
              Value = "No analysis has been run yet.",
              stringsAsFactors = FALSE
            ),
            rownames = FALSE,
            options = list(
              dom = "t",
              paging = FALSE,
              ordering = FALSE,
              autoWidth = TRUE
            )
          )
        )
      }

      DT::datatable(
        rv$summary,
        rownames = FALSE,
        options = list(
          dom = "t",
          paging = FALSE,
          ordering = FALSE,
          autoWidth = TRUE
        )
      )
    })

    output$download_demo <- shiny::downloadHandler(
      filename = function() {
        "1hel.pdb"
      },
      content = function(file) {
        if (!nzchar(demo_pdb_path) || !file.exists(demo_pdb_path)) {
          stop("Demo PDB file not found.")
        }
        file.copy(demo_pdb_path, file, overwrite = TRUE)
      }
    )

    output$downloadPlot <- shiny::downloadHandler(
      filename = function() {
        base_name <- paste0("protein_structure_", Sys.Date())
        switch(
          input$plot_format,
          "pdf" = paste0(base_name, ".pdf"),
          "png" = paste0(base_name, ".png"),
          "jpeg" = paste0(base_name, ".jpeg"),
          "tiff" = paste0(base_name, ".tiff"),
          "svg" = paste0(base_name, ".svg")
        )
      },
      content = function(file) {
        shiny::req(rv$analysis_done, rv$modes, rv$pdb)

        format <- input$plot_format
        width <- as.numeric(input$plot_width)
        height <- as.numeric(input$plot_height)
        units <- input$plot_units
        dpi <- as.numeric(input$plot_dpi)

        if (format == "pdf") {
          grDevices::pdf(file, width = width, height = height)
        } else if (format == "png") {
          grDevices::png(file, width = width, height = height, units = units, res = dpi)
        } else if (format == "jpeg") {
          grDevices::jpeg(file, width = width, height = height, units = units, res = dpi, quality = 100)
        } else if (format == "tiff") {
          grDevices::tiff(file, width = width, height = height, units = units, res = dpi, compression = "lzw")
        } else if (format == "svg") {
          grDevices::svg(file, width = width, height = height)
        }

        bio3d::plot.bio3d(
          rv$modes$fluctuations,
          sse = rv$pdb,
          sheet.col = input$sheet_color,
          helix.col = input$helix_color,
          typ = "l",
          lwd = input$line_width,
          xlab = "Residue Index",
          ylab = expression("Fluctuations from NMA (" * ring(A) * ")")
        )

        graphics::title(
          main = "Protein Secondary Structure Analysis",
          sub = paste("Generated:", Sys.Date(), "| Source:", rv$active_source)
        )

        grDevices::dev.off()
      },
      contentType = function() {
        switch(
          input$plot_format,
          "pdf" = "application/pdf",
          "png" = "image/png",
          "jpeg" = "image/jpeg",
          "tiff" = "image/tiff",
          "svg" = "image/svg+xml"
        )
      }
    )
  })
}
