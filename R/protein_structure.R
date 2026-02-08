#' protein_structure
#' @title protein_structure_ui
#' @name protein_structure_ui
#' @param id A unique identifier for the Shiny namespace.
#' @import shiny
#' @import bslib
#' @import bsicons
#' @export
#'
protein_structure_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = 'Secondary structure of protein',
    icon = bs_icon("play-circle"),
    layout_sidebar(
      sidebar = accordion(
        accordion_panel(
          title = "File Upload",
          icon = bs_icon("upload"),
          fileInput(
            inputId = ns('file'),
            label = 'File(.pdb)',
            multiple = FALSE,
            accept = '.pdb'
          )
        )
      ),
      page_fluid(
        layout_column_wrap(
          width = 1,
          height = 750,
          navset_card_tab(
            height = 600,
            full_screen = TRUE,
            title = "Secondary structure of protein",
            sidebar = accordion(
              open = 'closed',
              accordion_panel(
                title = 'Parameter',
                colourpicker::colourInput(
                  inputId = ns("sheet_color"),
                  label = "beta sheet color",
                  value = "orange"),
                colourpicker::colourInput(
                  inputId = ns("helix_color"),
                  label = "alpha helix color",
                  value = "purple")
              ),
              accordion_panel(
                title = 'Run',
                actionButton(ns("run"), "Run")
              ),
              accordion_panel(
                title = 'Download',
                # 添加下载尺寸设置
                numericInput(
                  inputId = ns("plot_width"),
                  label = "Plot width (inches)",
                  value = 10,
                  min = 5,
                  max = 20,
                  step = 0.5
                ),
                numericInput(
                  inputId = ns("plot_height"),
                  label = "Plot height (inches)",
                  value = 6,
                  min = 4,
                  max = 15,
                  step = 0.5
                ),
                selectInput(
                  inputId = ns("plot_units"),
                  label = "Plot units",
                  choices = c("inches" = "in", "centimeters" = "cm", "millimeters" = "mm"),
                  selected = "in"
                ),
                numericInput(
                  inputId = ns("plot_dpi"),
                  label = "Resolution (DPI)",
                  value = 300,
                  min = 72,
                  max = 1200,
                  step = 1
                ),
                selectInput(
                  inputId = ns("plot_format"),
                  label = "File format",
                  choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "TIFF" = "tiff", "SVG" = "svg"),
                  selected = "pdf"
                ),
                hr(),
                downloadButton(ns("downloadPlot"), "Download Plot"),
                p(style = "font-size: 12px; color: #666; margin-top: 10px;",
                  "Note: PDF format supports vector graphics, PNG/JPEG/TIFF are raster images.")
              )
            ),
            mainPanel(
              plotOutput(ns("protein_2"))
            )
          )
        )
      )
    )
  )
}
#' protein_structure Server Module
#' @description Server logic for protein_structure
#' @param id Standard shiny server arguments
#' @import shiny
#' @import bio3d
#' @export
#'
protein_structure_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 加载 bio3d 包的数据
    if (!exists("elements") || !exists("atom.index")) {
      data(elements, package = "bio3d")
      data(atom.index, package = "bio3d")
    }

    # 存储分析结果的reactive value
    plot_data <- reactiveValues(
      modes = NULL,
      pdb = NULL
    )

    observeEvent(input$run, {
      req(input$file)

      # 添加进度指示
      showNotification("Reading PDB file...", type = "message", duration = 2)

      tryCatch({
        # 读取 PDB 文件
        pdb <- bio3d::read.pdb(input$file$datapath)

        showNotification("Performing Normal Mode Analysis...", type = "message", duration = 2)

        # 进行 NMA 分析
        modes <- bio3d::nma(pdb)

        # 存储结果
        plot_data$modes <- modes
        plot_data$pdb <- pdb

        # 渲染图形
        output$protein_2 <- renderPlot({
          if (!is.null(modes) && !is.null(modes$fluctuations)) {
            bio3d::plot.bio3d(
              modes$fluctuations,
              sse = pdb,
              sheet.col = input$sheet_color,
              helix.col = input$helix_color,
              typ = "l",
              lwd = 3,
              ylab = "Fluctuations from NMA (Å)"
            )
          } else {
            plot.new()
            text(0.5, 0.5, "Analysis failed or no fluctuations data",
                 cex = 1.2, col = "red")
          }
        })

        # 下载处理
        output$downloadPlot <- downloadHandler(
          filename = function() {
            # 根据选择的格式生成文件名
            base_name <- paste0("protein_structure_", Sys.Date())
            format <- input$plot_format

            switch(format,
                   "pdf" = paste0(base_name, ".pdf"),
                   "png" = paste0(base_name, ".png"),
                   "jpeg" = paste0(base_name, ".jpeg"),
                   "tiff" = paste0(base_name, ".tiff"),
                   "svg" = paste0(base_name, ".svg"))
          },
          content = function(file) {
            req(plot_data$modes, plot_data$pdb)

            format <- input$plot_format
            width <- as.numeric(input$plot_width)
            height <- as.numeric(input$plot_height)
            units <- input$plot_units
            dpi <- as.numeric(input$plot_dpi)

            # 根据格式设置设备参数
            if (format == "pdf") {
              pdf(file, width = width, height = height)
            } else if (format == "png") {
              png(file, width = width, height = height,
                  units = units, res = dpi)
            } else if (format == "jpeg") {
              jpeg(file, width = width, height = height,
                   units = units, res = dpi, quality = 100)
            } else if (format == "tiff") {
              tiff(file, width = width, height = height,
                   units = units, res = dpi, compression = "lzw")
            } else if (format == "svg") {
              svg(file, width = width, height = height)
            }

            # 绘制图形
            bio3d::plot.bio3d(
              plot_data$modes$fluctuations,
              sse = plot_data$pdb,
              sheet.col = input$sheet_color,
              helix.col = input$helix_color,
              typ = "l",
              lwd = 3,
              ylab = "Fluctuations from NMA (Å)"
            )

            # 添加标题和元数据
            title(main = "Protein Secondary Structure Analysis",
                  sub = paste("Generated:", Sys.Date()))

            dev.off()
          },
          contentType = switch(input$plot_format,
                               "pdf" = "application/pdf",
                               "png" = "image/png",
                               "jpeg" = "image/jpeg",
                               "tiff" = "image/tiff",
                               "svg" = "image/svg+xml")
        )

        showNotification("Analysis completed successfully!",
                         type = "message", duration = 3)

      }, error = function(e) {
        output$protein_2 <- renderPlot({
          plot.new()
          text(0.5, 0.5, paste("Error:", e$message),
               cex = 1, col = "red")
        })
        showNotification(paste("Error:", e$message),
                         type = "error", duration = 5)
      })
    })
  })
}
