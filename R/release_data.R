library(shiny)
library(openxlsx)

release_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("workdir_display")),
    actionButton(ns("load_files"), "Load .rda Files"),
    uiOutput(ns("rda_files_ui")),
    selectInput(ns("output_format"), "Output Format", choices = c("xlsx", "csv")),
    actionButton(ns("export_btn"), "Export")
  )
}

release_data_server <- function(id, shared_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 显示当前工作目录
    output$workdir_display <- renderText({
      wd <- shared_state$workdir
      if (is.null(wd) || wd == "") {
        "Working directory not set"
      } else {
        paste("Current working directory:", wd)
      }
    })

    # 读取工作目录下所有 .rda 文件
    rda_files <- eventReactive(input$load_files, {
      wd <- shared_state$workdir
      req(wd)
      if (!dir.exists(wd)) {
        showNotification("Directory does not exist", type = "error")
        return(NULL)
      }
      files <- list.files(wd, pattern = "\\.rda$", full.names = FALSE)
      if (length(files) == 0) {
        showNotification("No .rda files found", type = "warning")
        return(NULL)
      }
      files
    })

    # 显示复选框供用户选择 .rda 文件
    output$rda_files_ui <- renderUI({
      files <- rda_files()
      req(files)
      checkboxGroupInput(ns("selected_rda"), "Select .rda files to export", choices = files)
    })

    observeEvent(input$export_btn, {
      wd <- shared_state$workdir
      req(wd)
      req(input$selected_rda)
      format <- input$output_format

      for (rda_file in input$selected_rda) {
        rda_path <- file.path(wd, rda_file)
        env <- new.env()
        load(rda_path, envir = env)

        folder_name <- tools::file_path_sans_ext(rda_file)
        out_dir <- file.path(wd, folder_name)
        if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

        obj_names <- ls(env)
        for (obj_name in obj_names) {
          obj <- env[[obj_name]]
          out_file <- file.path(out_dir, paste0(obj_name, ".", format))

          if (format == "xlsx") {
            if (is.data.frame(obj)) {
              openxlsx::write.xlsx(obj, out_file)
            } else {
              # 尝试转换为data.frame
              tryCatch({
                df <- as.data.frame(obj)
                openxlsx::write.xlsx(df, out_file)
              }, error = function(e) {
                showNotification(paste("Cannot export object", obj_name, "- not a data.frame"), type = "warning")
              })
            }
          } else if (format == "csv") {
            if (is.data.frame(obj)) {
              write.csv(obj, out_file, row.names = FALSE)
            } else {
              tryCatch({
                df <- as.data.frame(obj)
                write.csv(df, out_file, row.names = FALSE)
              }, error = function(e) {
                showNotification(paste("Cannot export object", obj_name, "- not a data.frame"), type = "warning")
              })
            }
          }
        }
      }

      showNotification("Export completed", type = "message")
    })

  })
}
